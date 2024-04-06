# Load packages
library(readstata13)
library(tidyverse)
library(mlogit)
library(gtools)
library(stargazer)
library(Zelig)
library(grid)
library(gridExtra)
library(fastDummies)

# Load & Data Cleaning
dta <- read.dta13("auxiliary files/data/mp30_s7_gschwend_20180808_noid.dta",convert.factors = F) %>% 
  # Combine the ptv variables of the control group and of the treatment groups
  mutate(
    q155_1 = ifelse(s7gr==0,q156_1,q155_1),
    q155_2 = ifelse(s7gr==0,q156_2,q155_2),
    q155_3 = ifelse(s7gr==0,q156_3,q155_3), 
    q155_4 = ifelse(s7gr==0,q156_4,q155_4),
    q155_5 = ifelse(s7gr==0,q156_5,q155_5), 
    q155_6 = ifelse(s7gr==0,q156_6,q155_6),
    q155_7 = ifelse(s7gr==0,q156_7,q155_7),
    q155_8 = ifelse(s7gr==0,q156_8,q155_8)
  ) %>% 
  # Select the variables of interest
  dplyr::select(
    # Experimental group
    group = s7gr,
    
    # Party Ratings
    rat_V = q147_1, 
    rat_SAP = q147_2, 
    rat_MP = q147_3, 
    rat_C = q147_4, 
    rat_L = q147_5, 
    rat_KD = q147_6, 
    rat_M = q147_7, 
    rat_SD = q147_8,
    
    # Coalition Ratings
    rat_coal_SAPMP = q149_1,
    rat_coal_ALLIANCE = q149_2,
    rat_coal_SAPMPLC = q149_3,
    rat_coal_MSD = q149_4,
    
    # Propensity to Vote Parties
    ptv_V = q155_1, 
    ptv_SAP = q155_2, 
    ptv_MP = q155_3,
    ptv_C = q155_4, 
    ptv_L = q155_5, 
    ptv_KD = q155_6, 
    ptv_M = q155_7, 
    ptv_SD = q155_8, 
    
    # Coalition Likelihoods

    # Given that SAP is in government after the election. How likely is that the following party is the coalition partner?
    coallik_SAP_MP = q166_1, 
    coallik_SAP_MPLC = q166_2, 
    
    # Given that M is in government after the election. How likely is that the following party is the coalition partner?
    coallik_M_ALLIANCE = q168_1, 
    coallik_M_SD = q168_2, 
    
    # Left-Right Placements Parties 
    lr_V = q170_1, 
    lr_SAP = q170_2, 
    lr_MP = q170_3, 
    lr_C = q170_4, 
    lr_L = q170_5, 
    lr_KD = q170_6, 
    lr_M = q170_7, 
    lr_SD = q170_8, 
    
    # Left-Right Placements Coalitions
    lr_SAPMP = q172_1, 
    lr_ALLIANCE = q172_2, 
    lr_SAPMPLC  = q172_3, 
    lr_MSD  = q172_4, 
    
    # Left-Right Placement Self
    lr_self = q174,
    
    # Covariates
    sex = sex,
    edu = edu,
    age = age6,
    
    # Reaction Times Coalition Vigniette
    col_vign_reac = t_q157_3
    
  ) %>% 
  mutate(
    #coal_SAPMP = ifelse(coal_SAPMP-1==0,1,coal_SAPMP-1),
    group =   factor(group, 
                     labels = c("Control",
                                "Coalition ALLIANCE",
                                "Coalition SAP-MP", 
                                "Coalition M-SD", 
                                "Coalition SAP-MP-L-C")),
    gr =   factor(group, 
                  labels = c("Control",
                             "ALLIANCE",
                             "SAP_MP", 
                             "M_SD", 
                             "SAP_MP_L_C"
                  )),
    t = ifelse(gr=="Control",0,1),
    age_sqrt = age^2
  )