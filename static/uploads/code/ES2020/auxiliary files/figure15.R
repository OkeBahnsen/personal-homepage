library(mediation)
out_sapmp_sap <- list(NA)
out_sapmp_m <- list(NA)

## SAP-MP: PTV SAP
dta_sapmp_sap <- dta[which(dta$lr_self > mean(dta$lr_SAP[dta$group=="Control"], na.rm=T) & 
                             dta$lr_self < mean(dta$lr_M[dta$group=="Control"], na.rm=T)),]
dta_sapmp_sap <- dta_sapmp_sap[which(dta_sapmp_sap$group %in% c("Control","Coalition SAP-MP")),]
dta_sapmp_sap <- dta_sapmp_sap[, c("ptv_SAP", "t", "coallik_SAP_MP", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP",
                                   "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_sapmp_sap <- na.omit(dta_sapmp_sap)

model.m <- lm(coallik_SAP_MP  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + 
                rat_KD + rat_SD, data=dta_sapmp_sap)
model.y <- lm(ptv_SAP ~ t + coallik_SAP_MP  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + 
                rat_L + rat_KD + rat_SD, data=dta_sapmp_sap)

for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_sapmp_sap[[i]] <- mediate(model.m, model.y, sims = 1000, boot = TRUE,
                                treat = "t", mediator = "coallik_SAP_MP", conf.level = ci)
}

## SAP-MP: PTV M
dta_sapmp_m <- dta[which(dta$lr_self > mean(dta$lr_SAP[dta$group=="Control"], na.rm=T) & 
                           dta$lr_self < mean(dta$lr_M[dta$group=="Control"], na.rm=T)),]
dta_sapmp_m <- dta_sapmp_m[which(dta_sapmp_m$group %in% c("Control","Coalition SAP-MP")),]
dta_sapmp_m <- dta_sapmp_m[, c("ptv_M", "t", "coallik_SAP_MP", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP", 
                               "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_sapmp_m <- na.omit(dta_sapmp_m)

model.m <- lm(coallik_SAP_MP  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + 
                rat_KD + rat_SD, data=dta_sapmp_m)
model.y <- lm(ptv_M ~ t + coallik_SAP_MP  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + 
                rat_L + rat_KD + rat_SD, data=dta_sapmp_m)

for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_sapmp_m[[i]] <- mediate(model.m, model.y, sims = 1000, boot = TRUE,
                              treat = "t", mediator = "coallik_SAP_MP", conf.level = ci)
}

med.ana <- data.frame(
  c(1,2),
  c("Coalition Likelihood Mechanism", "Coalition Likelihood Mechanism"),
  c("PTV for SAP", "PTV for M"),
  c("Coalition Signal M-SD", "Coalition Signal M-SD"),
  c(out_sapmp_sap[[1]]$d0, out_sapmp_m[[1]]$d0),
  c(out_sapmp_sap[[1]]$d0.ci[1], out_sapmp_m[[1]]$d0.ci[1]),
  c(out_sapmp_sap[[1]]$d0.ci[2], out_sapmp_m[[1]]$d0.ci[2]),
  c(out_sapmp_sap[[2]]$d0.ci[1], out_sapmp_m[[2]]$d0.ci[1]),
  c(out_sapmp_sap[[2]]$d0.ci[2], out_sapmp_m[[2]]$d0.ci[2]))
colnames(med.ana) <- c("number", "mechanism", "ptv", "treatment", "mean", "lower_95", "upper_95", "lower_90", "upper_90")

figure15 <- qplot(x= number,
      y    = mean,
      xlab = "", 
      ylab="",
      size = 22,
      ylim = c(-0.04, 0.09),
      data = med.ana) +
  scale_x_discrete(limits=c(1, 2), 
                   labels = c("PTV for SAP", "PTV for M")) +
  geom_errorbar(aes(
    ymin  = lower_90,
    ymax  = upper_90,
    size = 2.5,
    width = 0)) +
  geom_errorbar(aes(
    ymin  = lower_95,
    ymax  = upper_95,
    size = 1,
    width = 0)) +
  theme_minimal() +
  theme(legend.position="none") +
  geom_hline(yintercept = 0, linetype = 2) +
  coord_flip()