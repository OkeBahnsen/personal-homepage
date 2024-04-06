library(cowplot)

# Preparations for loop
treatments <- c("Coalition ALLIANCE", "Coalition M-SD", "Coalition SAP-MP", "Coalition SAP-MP-L-C")
liks <- c("coallik_M_ALLIANCE", "coallik_M_SD", "coallik_SAP_MP", "coallik_SAP_MPLC")
df.result <- as.data.frame(matrix(ncol = 7, nrow = 1))
colnames(df.result) <- c("lik", "treatment", "mean", "lower_95", "upper_95", "lower_90", "upper_90")

# Loop
i <- 0
for (l in liks) {
  for (treat in treatments) {
    i <- i+1
    # Extract treatment effects 
    formula <- as.formula(paste(l, paste("t", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP", "rat_V", 
                                         "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD", sep= " + "), sep=" ~ "))
    reg <- summary(lm(formula, data = dta[which(dta$group %in% c(treat, "Control")),]))
    effect <- reg$coefficients[2,1]
    lower_bound_95 <- effect-qnorm(0.975)*reg$coefficients[2,2]
    upper_bound_95 <- effect+qnorm(0.975)*reg$coefficients[2,2]
    lower_bound_90 <- effect-qnorm(0.95)*reg$coefficients[2,2]
    upper_bound_90 <- effect+qnorm(0.95)*reg$coefficients[2,2]
    df.result[i,] <- c(l, treat, effect, lower_bound_95, upper_bound_95, lower_bound_90, upper_bound_90)
  }
}

# Plot effects
e <- list()
i <- 0
for (l in liks[1:4]) {
  interval <- if(l %in% liks[1:2]) c(1:2) else c(3:4)
  i <- i+1
  ylabel <- c("M entering M-C-L-KD coalition", "M entering M-SD coalition", "SAP entering SAP-MP coalition", 
              "SAP entering SAP-MP-C-L coalition")[i]
  if (i%%2==1) {lab <- c("Treatment M-C-L-KD", "Treatment M-SD", "Treatment SAP-MP",  "Treatment SAP-MP-L-C")} 
  else {lab <- c("", "", "", "")}
  e[[i]]<- qplot(x    = treatment,
                 y    = as.numeric(mean), 
                 xlab = "", 
                 ylab = ylabel,
                 size = 20,
                 ylim = c(-0.6, 0.6),
                 data = df.result[which(df.result$lik==l),][interval,]) +
    scale_x_discrete(limits= {if(l %in% liks[1:2]) c("Coalition ALLIANCE", "Coalition M-SD") 
      else c("Coalition SAP-MP", "Coalition SAP-MP-L-C")}, labels = lab[interval]) +
    geom_errorbar(aes(
      ymin  = as.numeric(lower_90),
      ymax  = as.numeric(upper_90),
      size = 1,
      width = 0)) +
    geom_errorbar(aes(
      ymin  = as.numeric(lower_95),
      ymax  = as.numeric(upper_95),
      size = 0.5,
      width = 0)) +
    theme_minimal() +
    theme(legend.position="none", axis.title.y = element_text(margin = margin(t = 0, r = 10, b = 0, l = 0))) +
    geom_hline(yintercept = 0, linetype = 2) + 
    coord_flip() +
    {if(i%in%c(1:2)) {theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())}} 
}

# Plot Figure 4
grid.arrange(e[[1]], e[[2]], e[[3]], e[[4]], ncol=2, top=textGrob("", gp=gpar(fontsize=12, font = 2)), widths=c(3, 2.2), heights=c(1, 1.15))