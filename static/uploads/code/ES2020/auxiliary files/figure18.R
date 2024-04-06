library(MASS)

# Preparations
dta_stacked <- dta
dta_stacked$id <- 1:nrow(dta_stacked)
dta_stacked$treat <- NA
dta_stacked$treat[dta_stacked$group == "Control"] <- 0
dta_stacked$treat[dta_stacked$group %in% c("Coalition ALLIANCE",
                                           "Coalition SAP-MP", 
                                           "Coalition M-SD", 
                                           "Coalition SAP-MP-L-C")] <- 1

# Select variables 
vars <- c("ptv_V", "ptv_SAP", "ptv_MP", "ptv_C", "ptv_L", "ptv_KD", "ptv_M", "ptv_SD", "rat_V", "rat_SAP", "rat_MP", "rat_C", "rat_L", "rat_KD", "rat_M", "rat_SD", "rat_coal_SAPMP", "rat_coal_ALLIANCE", "rat_coal_SAPMPLC", "rat_coal_MSD", "sex", "edu", "age", "age_sqrt", "treat", "group", "id")
dta_stacked <- dta_stacked[vars]

# Stack
dta_stacked2 <- data.frame(dta_stacked[17:27], stack(dta_stacked[1:8]), stack(dta_stacked[9:16]))
colnames(dta_stacked2) <- c(names(dta_stacked[17:27]), "ptv", "ptv_ind", "rat", "rat_ind")

par(mfrow=c(4,4),
    oma = c(1,2,2,0) + 0.1,
    mar = c(2.5,2,0,0) + 0.1,
    xpd=NA)


for (r in c(1:4)) {
  
  coal <- c("rat_coal_ALLIANCE", "rat_coal_MSD", "rat_coal_SAPMP", "rat_coal_SAPMPLC")
  parties <- data.frame(c("M", "C", "L", "KD"),
                        c("M", "SD", "", ""),
                        c("SAP", "MP", "", ""),
                        c("SAP", "MP", "C", "L"), stringsAsFactors=FALSE)
  coal_members <- data.frame(c("ptv_M", "ptv_C", "ptv_L", "ptv_KD"),
                             c("ptv_M", "ptv_SD"),
                             c("ptv_SAP", "ptv_MP"),
                             c("ptv_SAP", "ptv_MP", "ptv_C", "ptv_L"), stringsAsFactors=FALSE)
  
  
  formula_with <- as.formula(paste("ptv", paste("treat", coal[r], "rat", paste(coal[r], ":treat", sep = ""), "rat:treat", "age", "age_sqrt", "sex", "edu", sep= " + "), sep=" ~ "))
  
  formula_without <- as.formula(paste("ptv", paste("treat", coal[r], "rat", "rat:treat", "age", "age_sqrt", "sex", "edu", sep= " + "), sep=" ~ "))
  
  
  k <- 0
  
  for (treatment in c("Coalition ALLIANCE", "Coalition M-SD", "Coalition SAP-MP", "Coalition SAP-MP-L-C")) {
    dta_stacked3 <- dta_stacked2[which(dta_stacked2$group %in% c("Control", treatment) & 
                                         dta_stacked2$ptv_ind %in% coal_members[,r]),]
    k <- k+1
    
    # Compute Loglihood Ratio Tests
    With_CI <- lm(formula_with, data = dta_stacked3)
    
    
    ## Simulate Estimation Uncertainty
    
    nsim <- 10000
    
    # Set up the sampling distribution
    
    set.seed(1)    
    
    S <- mvrnorm(nsim, With_CI$coefficients, vcov(summary(With_CI)))
    
    ## Apply observed value approach
    
    val <- list() 
    
    for (treatment in c(0,1)) {
      
      X <-      as.matrix(cbind(
        1,
        treatment,
        dta_stacked3[[coal[r]]],
        dta_stacked3$rat,
        dta_stacked3$age,
        dta_stacked3$age_sqrt,
        dta_stacked3$sex,
        dta_stacked3$edu,
        dta_stacked3[[coal[r]]]*treatment,
        dta_stacked3$rat*treatment
      ))
      
      X <- na.omit(X)
      
      dim(X)
      
      colnames(X) <- c(
        "integer",
        "t",
        coal[r],
        "rat",
        "age",
        "age_sqrt",
        "sex",
        "edu",
        paste(coal[r], "xt", sep=""),
        "ratxt"
      )
      
      rating <- seq(1, 7, length.out = 100)
      
      cases <- array(NA, c(dim(X), length(rating)))
      
      cases[ , , ] <- X
      
      col_pec_rat <- which(colnames(X) == coal[r])
      col_pec_ratint <- which(colnames(X) == paste(coal[r], "xt", sep =""))
      
      for (i in 1:length(rating)) {
        cases[, col_pec_rat, i] <- rating[i]
        cases[, col_pec_ratint, i] <- rating[i]*treatment
      }
      
      val[[treatment+1]] <- matrix(NA, nrow = nsim, ncol = length(rating))
      
      for (scenario in 1:length(rating)){
        val[[treatment+1]][, scenario] <- apply(S, 1, function(s)  mean(cases[, , scenario] %*% s))
      }
    }
    # Means and Quantiles
    mean_control <- apply(val[[1]], 2, mean)
    qu_control <- t(apply(val[[1]], 2, quantile, prob = c(0.025, 0.975)))
    mean_treat <- apply(val[[2]], 2, mean)
    qu_treat <- t(apply(val[[2]], 2, quantile, prob = c(0.025, 0.975)))
    
    ylims <- data.frame(
      c(1, c(1.8,3.2)),
      c(2, c(2.2,3.5)),
      c(3, c(2.2,3.2)),
      c(4, c(2.2,3.0))
    )
    
    ylims <- as.data.frame(t(ylims), stringsAsFactors=FALSE)
    
    plot(rating, mean_control, type="n",
         ylim = c(as.numeric(ylims$V2[ylims$V1==r]), as.numeric(ylims$V3[ylims$V1==r])),
         ylab = ifelse(k %in% c(1,5,9,13), paste("PTV for ", paste(parties[1:ifelse(r %in% c(1,4),4,2),r], collapse=", "), sep=""), ""),
         xlab = "", 
         bty = "n",
         las = 1, xaxt=ifelse(r!=4, 'n','s'), cex.lab = 1.5
    )
    
    polygon(c(rev(rating), rating), c(rev(qu_control[,2]), qu_control[,1]),
            col = adjustcolor("gray77", alpha = 0.5),
            border = NA)
    
    lines(rating, mean_control, lty = 3 , lwd = 3)
    lines(rating, qu_control[,1], lty = 3, lwd = 2)
    lines(rating, qu_control[,2], lty = 3, lwd = 2)
    
    polygon(c(rev(rating), rating), c(rev(qu_treat[,2]), qu_treat[,1]),
            col = adjustcolor("gray50", alpha = 0.5), 
            border = NA)
    
    lines(rating, mean_treat, lty = 1, lwd = 2)
    lines(rating, qu_treat[,1], lty = 1, lwd = 1)
    lines(rating, qu_treat[,2], lty = 1, lwd = 1)
    
    if (k ==4 & r==4) {
      legend(1,3, legend = c("Control", "Treatment"), fill = c("gray77","gray50"), cex = 1.2)
    }
  }
}

mtext( 'Treatment M-C-L-KD', side=3, line=0, at=grconvertX(-3.0,'npc','nic'), outer=TRUE, cex=1)
mtext( 'Treatment M-SD', side=3, line=0, at=grconvertX(-1.8,'npc','nic'), outer=TRUE, cex=1)
mtext( 'Treatment SAP-MP', side=3, line=0, at=grconvertX(-0.7,'npc','nic'), outer=TRUE, cex=1)
mtext( 'Treatment SAP-MP-C-L', side=3, line=0, at=grconvertX(0.5,'npc','nic'), outer=TRUE, cex=1)

mtext( 'Rating of M-C-L-KD Coalition', side=3, line=-12.8, at=grconvertX(-1.2,'npc','nic'), outer=TRUE, cex=1)
mtext( 'Rating of M-SD Coalition', side=3, line=-26.4, at=grconvertX(-1.2,'npc','nic'), outer=TRUE, cex=1)
mtext( 'Rating of SAP-MP Coalition', side=3, line=-40.2, at=grconvertX(-1.2,'npc','nic'), outer=TRUE, cex=1)
mtext( 'Rating of SAP-MP-C-L Coalition', side=3, line=-55.7, at=grconvertX(-1.2,'npc','nic'), outer=TRUE, cex=1)
