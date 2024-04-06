# Shape df for passing it to mlogit
dta.mlogit <- mlogit.data(dta, shape = "wide", choice = "group")

# Select coveriates (which are "chooser characteristics")
dta.mlogit <- dta.mlogit[which(!is.na(dta.mlogit$group) &
                                 !is.na(dta.mlogit$sex) &
                                 !is.na(dta.mlogit$edu) &
                                 !is.na(dta.mlogit$age) &
                                 !is.na(dta.mlogit$age_sqrt) &
                                 !is.na(dta.mlogit$rat_M) &
                                 !is.na(dta.mlogit$rat_SAP) &
                                 !is.na(dta.mlogit$rat_V) &
                                 !is.na(dta.mlogit$rat_MP) &
                                 !is.na(dta.mlogit$rat_C) &
                                 !is.na(dta.mlogit$rat_L) &
                                 !is.na(dta.mlogit$rat_KD) &
                                 !is.na(dta.mlogit$rat_SD)),]

# Run multinomial logistic regression
rand_test <- mlogit(group ~ 1 | 
                      sex +
                      edu +
                      age +
                      age_sqrt +
                      rat_M +
                      rat_SAP +
                      rat_V +
                      rat_MP +
                      rat_C +
                      rat_L +
                      rat_KD +
                      rat_SD, data = dta.mlogit, reflevel = "Control")

summary(rand_test)