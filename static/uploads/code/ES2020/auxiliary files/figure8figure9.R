library(cowplot)

# Preparations for loop
treatments <- c("Coalition ALLIANCE", "Coalition M-SD", "Coalition SAP-MP", "Coalition SAP-MP-L-C")
parties <- c("M", "SAP", "V", "MP", "C", "L", "KD", "SD")
df.result <- as.data.frame(matrix(ncol = 7, nrow = 1))
colnames(df.result) <- c("party", "treatment", "mean", "lower_95", "upper_95", "lower_90", "upper_90")

# Loop
i <- 0
for (part in parties) {
  ptv <- paste("ptv", part, sep="_")
  for (treat in treatments) {
    i <- i+1
    # Extract treatment effects
    formula <- as.formula(paste(ptv, paste("t", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP", "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD", sep= " + "), sep=" ~ "))
    reg <- summary(lm(formula, data = dta[which(dta$group %in% c(treat, "Control")),]))
    effect <- reg$coefficients[2,1]
    lower_bound_95 <- effect-qnorm(0.975)*reg$coefficients[2,2]
    upper_bound_95 <- effect+qnorm(0.975)*reg$coefficients[2,2]
    lower_bound_90 <- effect-qnorm(0.95)*reg$coefficients[2,2]
    upper_bound_90 <- effect+qnorm(0.95)*reg$coefficients[2,2]
    df.result[i,] <- c(part, treat, effect, lower_bound_95, upper_bound_95, lower_bound_90, upper_bound_90)
  }
}

# Plot effects
e <- list()
i <- 0
for (part in parties) {
  i <- i+1
  if (i%%2==1) {lab <- c("Treatment M-C-L-KD", "Treatment M-SD", "Treatment SAP-MP",  "Treatment SAP-MP-L-C")} else {lab <- c("", "", "", "")}
  e[[i]]<- qplot(x    = treatment,
                 y    = as.numeric(mean), 
                 xlab = "", 
                 ylab = part,
                 size = 20,
                 ylim = c(-0.4, 0.4),
                 data = df.result[which(df.result$party==part),]) +
    scale_x_discrete(limits=c("Coalition ALLIANCE", "Coalition M-SD", "Coalition SAP-MP", "Coalition SAP-MP-L-C"), labels = lab) +
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
    {if(i%in%c(3:6)) {theme(axis.text.x=element_blank(), axis.ticks.x=element_blank())}} 
}

# Plot Figure 8
grid.arrange(e[[1]], e[[2]], ncol=2, top=textGrob("", gp=gpar(fontsize=12, font = 2)), widths=c(3, 2.2), heights=c(1.15))
# Plot Figure 9
grid.arrange(e[[3]], e[[4]], e[[5]], e[[6]], e[[7]], e[[8]], ncol=2, top=textGrob("", gp=gpar(fontsize=12, font = 2)), widths=c(3, 2.2), heights=c(1,1,1.15))