# Select variables
vars <- c(
  "ptv_V",
  "ptv_MP",
  "ptv_SAP",
  "ptv_C",
  "ptv_L",
  "ptv_KD",
  "ptv_M",
  "ptv_SD",
  "rat_V",
  "rat_MP",
  "rat_SAP",
  "rat_C",
  "rat_L",
  "rat_KD",
  "rat_M",
  "rat_SD",
  "rat_coal_ALLIANCE",
  "rat_coal_MSD",
  "rat_coal_SAPMP",
  "rat_coal_SAPMPLC",
  "coallik_M_ALLIANCE",
  "coallik_M_SD",
  "coallik_SAP_MP",
  "coallik_SAP_MPLC"
)

# Show descriptive statistics
dta_descriptives <- dta[vars][which(dta$t==0),]
stargazer(dta_descriptives)