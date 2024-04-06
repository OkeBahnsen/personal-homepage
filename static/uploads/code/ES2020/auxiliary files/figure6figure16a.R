library(mediation)

out_msd_sap_low <- list(NA)
out_msd_sap_high <- list(NA)
out_msd_m_low <- list(NA)
out_msd_m_high <- list(NA)
out_sapmp_sap_low <- list(NA)
out_sapmp_sap_high <- list(NA)
out_sapmp_m_low <- list(NA)
out_sapmp_m_high <- list(NA)

# 1. Treatment M-SD

# 1.1 M-SD: Propensity to vote for SAP

# 1.1.1 M-SD: Propensity to vote for SAP: Low rating
dta_msd_low <- dta[dta$rat_coal_MSD < dta$rat_coal_ALLIANCE & dta$rat_coal_MSD < dta$rat_coal_SAPMPLC & 
                     dta$rat_coal_MSD < dta$rat_coal_SAPMP,]
dta_msd_low <- dta_msd_low[which(dta_msd_low$group %in% c("Control","Coalition M-SD")),]
dta_msd_sap_low <- dta_msd_low[, c("ptv_SAP", "t", "coallik_M_SD", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP",
                                   "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_msd_sap_low <- na.omit(dta_msd_sap_low)

model.m <- lm(coallik_M_SD  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD +
                rat_SD, data=dta_msd_sap_low)
model.y <- lm(ptv_SAP ~ t + coallik_M_SD  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L +
                rat_KD + rat_SD, data=dta_msd_sap_low)
for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_msd_sap_low[[i]] <- mediate(model.m, model.y, sims = 1000, boot = TRUE,
                                  treat = "t", mediator = "coallik_M_SD", conf.level = ci)
}

# 1.1.2 M-SD: Propensity to vote for SAP: High rating
dta_msd_high <- dta[dta$rat_coal_MSD > dta$rat_coal_ALLIANCE & dta$rat_coal_MSD > dta$rat_coal_SAPMPLC & 
                      dta$rat_coal_MSD > dta$rat_coal_SAPMP,]
dta_msd_high <- dta_msd_high[which(dta_msd_high$group %in% c("Control","Coalition M-SD")),]
dta_msd_sap_high <- dta_msd_high[, c("ptv_SAP", "t", "coallik_M_SD", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP",
                                     "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_msd_sap_high <- na.omit(dta_msd_sap_high)

model.m <- lm(coallik_M_SD  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD +
                rat_SD, data=dta_msd_sap_high)
model.y <- lm(ptv_SAP ~ t + coallik_M_SD  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L +
                rat_KD + rat_SD, data=dta_msd_sap_high)
for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_msd_sap_high[[i]] <- mediate(model.m, model.y, sims = 1000, boot = TRUE,
                                   treat = "t", mediator = "coallik_M_SD", conf.level = ci)
}


# 1.2 M-SD: Propensity to vote for M

# 1.2.1 M-SD: Propensity to vote for M: Low rating
dta_msd_m_low <- dta_msd_low[, c("ptv_M", "t", "coallik_M_SD", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP",
                                 "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_msd_m_low <- na.omit(dta_msd_m_low)

model.m <- lm(coallik_M_SD  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD +
                rat_SD, data=dta_msd_m_low)
model.y <- lm(ptv_M ~ t + coallik_M_SD  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L +
                rat_KD + rat_SD, data=dta_msd_m_low)
for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_msd_m_low[[i]] <- mediate(model.m, model.y, sims = 1000, boot = TRUE,
                                treat = "t", mediator = "coallik_M_SD", conf.level = ci)
}

# 1.2.1 M-SD: Propensity to vote for M: High rating
dta_msd_m_high <- dta_msd_high[, c("ptv_M", "t", "coallik_M_SD", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP",
                                   "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_msd_m_high <- na.omit(dta_msd_m_high)

model.m <- lm(coallik_M_SD  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD +
                rat_SD, data=dta_msd_m_high)
model.y <- lm(ptv_M ~ t + coallik_M_SD  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L +
                rat_KD + rat_SD, data=dta_msd_m_high)
for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_msd_m_high[[i]] <- mediate(model.m, model.y, sims = 1000, boot = TRUE,
                                 treat = "t", mediator = "coallik_M_SD", conf.level = ci)
}

# 2. Treatment SAP-MP

# 2.1 SAP-MP: Propensity to vote for SAP

# 2.1.1 SAP-MP: Propensity to vote for SAP: Low rating
dta_sapmp_low <- dta[dta$rat_coal_SAPMP < dta$rat_coal_MSD & dta$rat_coal_SAPMP < dta$rat_coal_ALLIANCE &
                       dta$rat_coal_SAPMP < dta$rat_coal_SAPMPLC,]
dta_sapmp_low <- dta_sapmp_low[which(dta_sapmp_low$group %in% c("Control","Coalition SAP-MP")),]
dta_sapmp_sap_low <- dta_sapmp_low[, c("ptv_SAP", "t", "coallik_SAP_MP", "age", "age_sqrt", "sex", "edu", "rat_M",
                                       "rat_SAP", "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_sapmp_sap_low <- na.omit(dta_sapmp_sap_low)

model.m <- lm(coallik_SAP_MP  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD
              + rat_SD, data=dta_sapmp_sap_low)
model.y <- lm(ptv_SAP ~ t + coallik_SAP_MP  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L
              + rat_KD + rat_SD, data=dta_sapmp_sap_low)
for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_sapmp_sap_low[[i]] <- mediate(model.m, model.y, sims = 1000, boot = TRUE,
                                    treat = "t", mediator = "coallik_SAP_MP", conf.level = ci)
}

# 2.1.2 SAP-MP: Propensity to vote for SAP: High rating
dta_sapmp_high <- dta[dta$rat_coal_SAPMP > dta$rat_coal_MSD & dta$rat_coal_SAPMP > dta$rat_coal_ALLIANCE &
                        dta$rat_coal_SAPMP > dta$rat_coal_SAPMPLC,]
dta_sapmp_high <- dta_sapmp_high[which(dta_sapmp_high$group %in% c("Control","Coalition SAP-MP")),]
dta_sapmp_sap_high <- dta_sapmp_high[, c("ptv_SAP", "t", "coallik_SAP_MP", "age", "age_sqrt", "sex", "edu", "rat_M",
                                         "rat_SAP", "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_sapmp_sap_high <- na.omit(dta_sapmp_sap_high)

model.m <- lm(coallik_SAP_MP  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD
              + rat_SD, data=dta_sapmp_sap_high)
model.y <- lm(ptv_SAP ~ t + coallik_SAP_MP  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L
              + rat_KD + rat_SD, data=dta_sapmp_sap_high)
for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_sapmp_sap_high[[i]] <- mediate(model.m, model.y, sims = 1000, boot = TRUE,
                                     treat = "t", mediator = "coallik_SAP_MP", conf.level = ci)
}

# 2.2 SAP-MP: Propensity to vote for M

# 2.2.1 SAP-MP: Propensity to vote for M: Low rating
dta_sapmp_m_low <- dta_sapmp_low[, c("ptv_M", "t", "coallik_SAP_MP", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP",
                                     "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_sapmp_m_low <- na.omit(dta_sapmp_m_low)

model.m <- lm(coallik_SAP_MP  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD
              + rat_SD, data=dta_sapmp_m_low)
model.y <- lm(ptv_M ~ t + coallik_SAP_MP  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L 
              + rat_KD + rat_SD, data=dta_sapmp_m_low)
for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_sapmp_m_low[[i]] <- mediate(model.m, model.y, sims = 1000, boot = TRUE,
                                  treat = "t", mediator = "coallik_SAP_MP", conf.level = ci)
}

# 2.2.2 SAP-MP: Propensity to vote for M: High rating
dta_sapmp_m_high <- dta_sapmp_high[, c("ptv_M", "t", "coallik_SAP_MP", "age", "age_sqrt", "sex", "edu", "rat_M", "rat_SAP",
                                       "rat_V", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_SD")]
dta_sapmp_m_high <- na.omit(dta_sapmp_m_high)

model.m <- lm(coallik_SAP_MP  ~ t + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L + rat_KD
              + rat_SD, data=dta_sapmp_m_high)
model.y <- lm(ptv_M ~ t + coallik_SAP_MP  + age + age_sqrt + sex + edu + rat_M + rat_SAP + rat_V + rat_MP + rat_C + rat_L 
              + rat_KD + rat_SD, data=dta_sapmp_m_high)
for (i in 1:2){
  set.seed(28092018)
  ci <- c(.95, .90)[i]
  out_sapmp_m_high[[i]] <- mediate(model.m, model.y, sims = 1000, boot = TRUE,
                                   treat = "t", mediator = "coallik_SAP_MP", conf.level = ci)
}

# Create df with ACMEs
med.ana <- data.frame(
  # Variable indicating whether low or high rating is considered
  c("high", "high", "low", "low", "high", "high", "low", "low"),
  # Variable indicating treatment and whether low or high rating is considered
  c("Coalition M-SD_high", "Coalition M-SD_high", "Coalition M-SD_low", "Coalition M-SD_low", 
    "Coalition SAP-MP_high", "Coalition SAP-MP_high", "Coalition SAP-MP_low", "Coalition SAP-MP_low"),
  # Variable indicating party that is considered
  c("SAP", "M", "SAP", "M", "SAP", "M", "SAP", "M"),
  # Variable indicating treatment
  c("Coalition Signal M-SD", "Coalition Signal M-SD", "Coalition Signal M-SD", "Coalition Signal M-SD", 
    "Coalition Signal SAP-MP", "Coalition Signal SAP-MP", "Coalition Signal SAP-MP", "Coalition Signal SAP-MP"),
  # Point estimate
  c(out_msd_sap_high[[1]]$d0, out_msd_m_high[[1]]$d0, out_msd_sap_low[[1]]$d0, out_msd_m_low[[1]]$d0,
    out_sapmp_sap_high[[1]]$d0, out_sapmp_m_high[[1]]$d0, out_sapmp_sap_low[[1]]$d0, out_sapmp_m_low[[1]]$d0),
  # Lower 95% CI 
  c(out_msd_sap_high[[1]]$d0.ci[1], out_msd_m_high[[1]]$d0.ci[1], out_msd_sap_low[[1]]$d0.ci[1],
    out_msd_m_low[[1]]$d0.ci[1], out_sapmp_sap_high[[1]]$d0.ci[1], out_sapmp_m_high[[1]]$d0.ci[1],
    out_sapmp_sap_low[[1]]$d0.ci[1], out_sapmp_m_low[[1]]$d0.ci[1]),
  # Upper 95% CI
  c(out_msd_sap_high[[1]]$d0.ci[2], out_msd_m_high[[1]]$d0.ci[2], out_msd_sap_low[[1]]$d0.ci[2],
    out_msd_m_low[[1]]$d0.ci[2], out_sapmp_sap_high[[1]]$d0.ci[2], out_sapmp_m_high[[1]]$d0.ci[2],
    out_sapmp_sap_low[[1]]$d0.ci[2], out_sapmp_m_low[[1]]$d0.ci[2]),
  # Lower 90% CI
  c(out_msd_sap_high[[2]]$d0.ci[1], out_msd_m_high[[2]]$d0.ci[1], out_msd_sap_low[[2]]$d0.ci[1],
    out_msd_m_low[[2]]$d0.ci[1], out_sapmp_sap_high[[2]]$d0.ci[1], out_sapmp_m_high[[2]]$d0.ci[1],
    out_sapmp_sap_low[[2]]$d0.ci[1], out_sapmp_m_low[[2]]$d0.ci[1]),
  # Upper 90% CI
  c(out_msd_sap_high[[2]]$d0.ci[2], out_msd_m_high[[2]]$d0.ci[2], out_msd_sap_low[[2]]$d0.ci[2],
    out_msd_m_low[[2]]$d0.ci[2], out_sapmp_sap_high[[2]]$d0.ci[2], out_sapmp_m_high[[2]]$d0.ci[2],
    out_sapmp_sap_low[[2]]$d0.ci[2], out_sapmp_m_low[[2]]$d0.ci[2]))

colnames(med.ana) <- c("low/high", "mechanism", "ptv", "treatment", "mean", "lower_95", 
                       "upper_95", "lower_90", "upper_90")

# Plot ACMEs
e <- list()
i <- 0
for (part in c("M", "SAP")) {
  i <- i+1
  if (i%%2==1) {lab <- c("", "Treatment M-SD", "", "",  "Treatment SAP-MP")} else {lab <- c("", "", "", "", "")}
  e[[i]]<- qplot(x    = mechanism,
                 y    = as.numeric(mean),
                 color = `low/high`,
                 xlab = "", 
                 ylab = part,
                 size = I(4),
                 ylim = {if(i==1) c(-0.21, 0.23) else c(-0.125, 0.15)},
                 data = med.ana[which(med.ana$ptv==part),]) +
    scale_colour_manual("Rating of Treatment Coalition", values = c("black", "gray")) +
    scale_x_discrete(limits=c("Coalition M-SD_low", "Coalition M-SD_high", "Coalition M-SD_low", 
                              "Coalition SAP-MP_low", "Coalition SAP-MP_high"), labels = lab) +
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
    {if(i==2) {theme(legend.position="bottom", 
                     axis.title.y = element_text(margin = margin(t = 0, r = 10, b = -10, l = 0)))}}
}

# Define function for plotting common legend
g_legend <- function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
mylegend <- g_legend(e[[2]])

# Plot Figure 6
p1 <- arrangeGrob(e[[1]], e[[2]] + theme(legend.position="none"), nrow=1, widths=c(3, 2.2))
grid.arrange(p1, mylegend, ncol=1, top=textGrob("", gp=gpar(fontsize=12, font = 2)), heights=c(1.15,0.2))

# Plot Figure 16 (a): Sensitivity Analysis
set.seed(28092018)
sens.out <- medsens(out_msd_sap_low[[2]], rho.by = 0.01, effect.type = "indirect", sims = 1000)
summary(sens.out)
plot(sens.out, ylim = c(-0.5, 0.5))