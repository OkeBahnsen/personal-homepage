library(mediation)

out_mds_sap <- list(NA)
out_mds_m <- list(NA)

## M-SD: PTV SAP
dta_mds_sap <- dta[which(dta$lr_self > mean(dta$lr_SAP[dta$group=="Control"], na.rm=T) & 
                           dta$lr_self < mean(dta$lr_M[dta$group=="Control"], na.rm=T)),]
dta_mds_sap <- dta_mds_sap[which(dta_mds_sap$group %in% c("Control","Coalition M-SD")),]
dta_mds_sap <- dta_mds_sap[, c("ptv_SAP", "t", "coallik_M_SD", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP",
                               "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_mds_sap <- na.omit(dta_mds_sap)

model.m <- lm(coallik_M_SD  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD + rat_SD, data=dta_mds_sap)
model.y <- lm(ptv_SAP ~ t + coallik_M_SD  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD + rat_SD, data=dta_mds_sap)
for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_mds_sap[[i]] <- mediate(model.m, model.y, sims = 1000, boot  = TRUE,
                              treat = "t", mediator = "coallik_M_SD", conf.level = ci)
}

## M-SD: PTV M
dta_mds_m <- dta[which(dta$lr_self > mean(dta$lr_SAP[dta$group=="Control"], na.rm=T) & 
                         dta$lr_self < mean(dta$lr_M[dta$group=="Control"], na.rm=T)),]
dta_mds_m <- dta_mds_m[which(dta_mds_m$group %in% c("Control","Coalition M-SD")),]
dta_mds_m <- dta_mds_m[, c("ptv_M", "t", "coallik_M_SD", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP", 
                           "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_mds_m <- na.omit(dta_mds_m)

model.m <- lm(coallik_M_SD  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD + rat_SD, data=dta_mds_m)
model.y <- lm(ptv_M ~ t + coallik_M_SD  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD + rat_SD, data=dta_mds_m)
for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_mds_m[[i]] <- mediate(model.m, model.y, sims = 1000, boot = TRUE,
                            treat = "t", mediator = "coallik_M_SD", conf.level = ci)
}

med.ana <- data.frame(
  c(1,2),
  c("Coalition Likelihood Mechanism", "Coalition Likelihood Mechanism"),
  c("PTV for SAP", "PTV for M"),
  c("Coalition Signal M-SD", "Coalition Signal M-SD"),
  c(out_mds_sap[[1]]$d0, out_mds_m[[1]]$d0),
  c(out_mds_sap[[1]]$d0.ci[1], out_mds_m[[1]]$d0.ci[1]),
  c(out_mds_sap[[1]]$d0.ci[2], out_mds_m[[1]]$d0.ci[2]),
  c(out_mds_sap[[2]]$d0.ci[1], out_mds_m[[2]]$d0.ci[1]),
  c(out_mds_sap[[2]]$d0.ci[2], out_mds_m[[2]]$d0.ci[2]))
colnames(med.ana) <- c("number", "mechanism", "ptv", "treatment", "mean", "lower_95", "upper_95", "lower_90", "upper_90")

figure14 <- qplot(x= number,
      y    = mean,
      xlab = "", 
      ylab="",
      size = 22,
      ylim = c(-0.05, 0.1),
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

# Plot Figure 16(b): Sensitivity Analysis
set.seed(28092018)
sens.out <- medsens(out_mds_sap[[2]], rho.by = 0.01, effect.type = "indirect", sims = 1000)
summary(sens.out)
figure16b <- plot(sens.out, ylim = c(-0.5, 0.5))