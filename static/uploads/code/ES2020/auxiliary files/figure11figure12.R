library(cowplot)

# Preparations for loop
treatments <- c("Coalition ALLIANCE", "Coalition M-SD", "Coalition SAP-MP", "Coalition SAP-MP-L-C")
evaluations <- c("rat_coal_ALLIANCE", "rat_coal_MSD", "rat_coal_SAPMP", "rat_coal_SAPMPLC")
evaluations_comp <- c("rat_coal_MSD", "rat_coal_ALLIANCE", "rat_coal_SAPMPLC", "rat_coal_SAPMP")
parties <- c("M", "SAP", "V", "MP", "C", "L", "KD", "SD")
df.result1 <- as.data.frame(matrix(ncol = 8, nrow = 1))
df.result2 <- as.data.frame(matrix(ncol = 8, nrow = 1))
colnames(df.result1) <- c("party", "treatment", "mean", "lower_95", "upper_95", "lower_90", "upper_90", "low/high")
colnames(df.result2) <- c("party", "treatment", "mean", "lower_95", "upper_95", "lower_90", "upper_90", "low/high")

# Loop
i <- 0
# Group 1: Rating of treatment coalition low; Group 2: Rating of treatment coalition high
for (group in c(1,2)) {
  for (part in parties) {
    ptv <- paste("ptv", part, sep="_")
    for (treat in treatments) {
      i <- i+1
      c <- match(treat,treatments)
      cw <- setdiff(c(1:4),c)
      # Restrict dta to subjects with low rating of treatment coalition
      if (group == 1) {
        dta_restr <- dta[dta[, evaluations[c]] < dta[, evaluations[cw[1]]] & 
                           dta[, evaluations[c]] < dta[, evaluations[cw[2]]] & 
                           dta[, evaluations[c]] < dta[,evaluations[cw[3]]],]
      }
      # Restrict dta to subjects with high rating of treatment coalition
      else {
        dta_restr <- dta[dta[, evaluations[c]] > dta[, evaluations[cw[1]]] & 
                           dta[, evaluations[c]] > dta[, evaluations[cw[2]]] & 
                           dta[, evaluations[c]] > dta[,evaluations[cw[3]]],]
      }
      
      # Extract treatment effects 
      formula <- as.formula(paste(ptv, paste("t", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP", "rat_V", 
                                             "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD", sep= " + "), sep=" ~ "))
      reg <- summary(lm(formula, data = dta_restr[which(dta_restr$group %in% c(treat, "Control")),]))
      effect <- reg$coefficients[2,1]
      lower_bound_95 <- effect-qnorm(0.975)*reg$coefficients[2,2]
      upper_bound_95 <- effect+qnorm(0.975)*reg$coefficients[2,2]
      lower_bound_90 <- effect-qnorm(0.95)*reg$coefficients[2,2]
      upper_bound_90 <- effect+qnorm(0.95)*reg$coefficients[2,2]
      if (group ==1) {
        df.result1[i,] <- c(part, treat, effect, lower_bound_95, 
                            upper_bound_95, lower_bound_90, upper_bound_90, "low")
      }
      else {
        df.result2[i-32,] <- c(part, treat, effect, lower_bound_95, 
                               upper_bound_95, lower_bound_90, upper_bound_90, "high")
      }
    }
  }
}

# Combine result from both groups
df.result <- rbind(df.result1,df.result2)
df.result$treatment_high_low <- paste(df.result$treatment, df.result$`low/high`, sep="_")

# Remove effects without uncertainty
df.result$mean[df.result$lower_95=="NaN"] <- NA

# Plot effects
e <- list()
i <- 0
for (part in parties) {
  i <- i+1
  if (i%%2==1) {lab <- c("", "Treatment M-C-L-KD", "", "",  "Treatment M-SD", "", "", 
                         "Treatment SAP-MP", "", "", "Treatment SAP-MP-L-C")} 
  else {lab <- c("", "", "", "", "", "", "", "", "", "", "")}
  e[[i]]<- qplot(x    = treatment_high_low,
                 y    = as.numeric(mean),
                 color = `low/high`,
                 xlab = "", 
                 ylab = part,
                 size = I(5),
                 ylim = c(-1, 1.2),
                 data = df.result[which(df.result$party==part),]) +
    
    scale_colour_manual("Rating of Treatment Coalition", values = c("black", "gray")) +
    
    scale_x_discrete(limits=c("Coalition ALLIANCE_low", "Coalition ALLIANCE_high", "Coalition ALLIANCE_high", 
                              "Coalition M-SD_low", "Coalition M-SD_high", "Coalition ALLIANCE_high", 
                              "Coalition SAP-MP_low", "Coalition SAP-MP_high", "Coalition ALLIANCE_high", 
                              "Coalition SAP-MP-L-C_low", "Coalition SAP-MP-L-C_high"), labels = lab) +
    geom_errorbar(aes(
      ymin  = as.numeric(lower_90),
      ymax  = as.numeric(upper_90),
      size = I(2),
      width = 0), ) +
    geom_errorbar(aes(
      ymin  = as.numeric(lower_95),
      ymax  = as.numeric(upper_95),
      size = I(0.75),
      width = 0)) +
    theme_minimal() +
    {if(i!=2) {theme(legend.position="none", axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0)))}} +
    geom_hline(yintercept = 0, linetype = 2) + 
    coord_flip() +
    {if(i%in%c(3:6)) {theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())}} +
    {if(i==2) {theme(legend.position="bottom", axis.title.y = element_text(margin = margin(t = 0, r = 10, b = -10, l = 0)))}}
}

# Define function for plotting legend
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
mylegend <- g_legend(e[[2]])

# Figure 11
p1 <- arrangeGrob(e[[1]], e[[2]] + theme(legend.position="none"), nrow=1, widths=c(3, 2.2))
grid.arrange(p1, mylegend, ncol=1, top=textGrob("", gp=gpar(fontsize=12, font = 2)), heights=c(1.15,0.2))

# Figure 12
p2 <- arrangeGrob(e[[3]], e[[4]], nrow=1, widths=c(3, 2.2))
p3 <- arrangeGrob(e[[5]], e[[6]], nrow=1, widths=c(3, 2.2))
p4 <- arrangeGrob(e[[7]], e[[8]], nrow=1, widths=c(3, 2.2))
grid.arrange(p2, p3, p4, mylegend, ncol=1, top=textGrob("", gp=gpar(fontsize=12, font = 2)), heights=c(1,1,1.15, 0.2))
