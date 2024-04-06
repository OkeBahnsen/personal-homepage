library(MASS)

# Preparations for looping over parties
party_loop <- data.frame(
  c("L", "rat_coal_ALLIANCE","rat_coal_SAPMPLC", "Rating of M-C-L-KD Coalition", 
    "Rating of SAP-MP-L-C   Coalition"),
  c("MP", "rat_coal_SAPMP", "rat_coal_SAPMPLC", "Rating of SAP-MP Coalition", 
    "Rating of SAP-MP-L-C Coalition"),
  c("SAP","rat_coal_SAPMP", "rat_coal_SAPMPLC", "Rating of SAP-MP Coalition", 
    "Rating of SAP-MP-L-C Coalition"),
  c("C", "rat_coal_ALLIANCE", "rat_coal_SAPMPLC", "Rating of M-C-L-KD Coalition", 
    "Rating of SAP-MP-L-C Coalition"),
  c("KD", "rat_coal_ALLIANCE", NA, "Rating of M-C-L-KD Coalition", NA),
  c("M", "rat_coal_ALLIANCE", "rat_coal_MSD", "Rating of M-C-L-KD Coalition", "Rating of M-SD Coalition"),
  c("SD", "rat_coal_MSD", NA, "Rating of M-SD Coalition", NA)
)
party_loop <- as.data.frame(t(party_loop), stringsAsFactors=FALSE)
colnames(party_loop) <- c("party", "c_p_rating1", "c_p_rating2", "xaxis_label1", "xaxis_label2")

# Loop
for (party in party_loop$party) {
  
  c_p_rating <- c(party_loop$c_p_rating1[party_loop$party==party], party_loop$c_p_rating2[party_loop$party==party], paste("rat", party, sep ="_"))
  xaxis_label <- c(party_loop$xaxis_label1[party_loop$party==party],
                   party_loop$xaxis_label2[party_loop$party==party],
                   paste("Rating of Party ", party, sep =""))
  
  if (party!="KD" & party!="SD") {number_plots <- 2} else {number_plots <- 1}
  for (m in c(1:number_plots)) {
    
    op <- par(mfrow = c(1,4),
              oma = c(3.2,3.2,1.2,0),
              mar = c(0.5,1.1,0,1.1))
    
    if (party!="KD" & party!="SD") {
      formula_with <- as.formula(paste(paste("ptv", party, sep ="_"), paste("t", c_p_rating[1], c_p_rating[2], c_p_rating[3], 
                                                                            paste(c_p_rating[1], ":t", sep = ""), paste(c_p_rating[2], ":t", sep = ""),
                                                                            paste(c_p_rating[3], ":t", sep = ""), "age", "age_sqrt", "sex", "edu", 
                                                                            sep= " + "), sep=" ~ "))
    }
    
    else {
      formula_with <- as.formula(paste(paste("ptv", party, sep ="_"), paste("t", c_p_rating[1], c_p_rating[3], 
                                                                            paste(c_p_rating[1], ":t", sep = ""), 
                                                                            paste(c_p_rating[3], ":t", sep = ""), "age", "age_sqrt", "sex", "edu", 
                                                                            sep= " + "), sep=" ~ "))
    }
    
    k <- 1
    
    for (treatment in c("Coalition ALLIANCE", "Coalition M-SD", "Coalition SAP-MP", "Coalition SAP-MP-L-C")) {
      dta_sub <- dta[which(dta$group %in% c("Control", paste(treatment))),]
      
      With_CI <- lm(formula_with, data = dta_sub)
      
      # Save regression output for Table 6
      if (party=="L" & treatment=="Coalition ALLIANCE") {
        Table6_model1 <- With_CI
      }
      else if (party=="L" & treatment=="Coalition M-SD") {
        Table6_model2 <- With_CI
      }
      else if (party=="L" & treatment=="Coalition SAP-MP") {
        Table6_model3 <- With_CI
      }
      else if (party=="L" & treatment=="Coalition SAP-MP-L-C") {
        Table6_model4 <- With_CI
      }
      
      ## Simulate Estimation Uncertainty
      
      nsim <- 10000
      
      # Set up the sampling distribution
      
      set.seed(1)
      
      S <- mvrnorm(nsim, With_CI$coefficients, vcov(summary(With_CI)))
      
      ## Apply observed value approach
      
      X <- dta_sub
      val <- list() 
      
      for (treatment in c(0,1)) {
        
        if (party!="KD" & party!="SD") {
          X <-      as.matrix(cbind(
            1,
            treatment,
            dta_sub[[c_p_rating[1]]],
            dta_sub[[c_p_rating[2]]],
            dta_sub[[c_p_rating[3]]],
            dta_sub$age,
            dta_sub$age_sqrt,
            dta_sub$sex,
            dta_sub$edu,
            dta_sub[[c_p_rating[1]]]*treatment,
            dta_sub[[c_p_rating[2]]]*treatment,
            dta_sub[[c_p_rating[3]]]*treatment
          ))
          
          X <- na.omit(X)
          
          dim(X)
          
          colnames(X) <- c(
            "integer",
            "t",
            c_p_rating[1],
            c_p_rating[2],
            c_p_rating[3],
            "age",
            "age_sqrt",
            "sex",
            "edu",
            paste(c_p_rating[1], "xt", sep=""),
            paste(c_p_rating[2], "xt", sep=""),
            paste(c_p_rating[3], "xt", sep="")
          )
        }
        
        else {
          X <-      as.matrix(cbind(
            1,
            treatment,
            dta_sub[[c_p_rating[1]]],
            dta_sub[[c_p_rating[3]]],
            dta_sub$age,
            dta_sub$age_sqrt,
            dta_sub$sex,
            dta_sub$edu,
            dta_sub[[c_p_rating[1]]]*treatment,
            dta_sub[[c_p_rating[3]]]*treatment
          ))
          
          X <- na.omit(X)
          
          dim(X)
          
          colnames(X) <- c(
            "integer",
            "t",
            c_p_rating[1],
            c_p_rating[3],
            "age",
            "age_sqrt",
            "sex",
            "edu",
            paste(c_p_rating[1], "xt", sep=""),
            paste(c_p_rating[3], "xt", sep="")
          )
        }
        
        rating <- seq(1, 7, length.out = 100)
        
        cases <- array(NA, c(dim(X), length(rating)))
        
        cases[ , , ] <- X
        
        col_pec_rat <- which(colnames(X) == c_p_rating[m])
        col_pec_ratint <- which(colnames(X) == paste(c_p_rating[m], "xt", sep =""))
        
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
      
      
      #Plot
      ylims <- data.frame(
        c("L", 1, c(1.5,3.5)),
        c("L", 2, c(1.5,3.5)),
        c("L", 3, c(0,6)),
        c("MP", 1, c(1.5,3)),
        c("MP", 2, c(1.5,2.5)),
        c("MP", 3, c(0,6)),
        c("SAP", 1, c(2.5,4)),
        c("SAP", 2, c(2.5,4)),
        c("SAP", 3, c(0,7)),
        c("C", 1, c(1.5,3.5)),
        c("C", 2, c(2,3)),
        c("C", 3, c(0,6)),
        c("KD", 1, c(1.5,2.5)),
        c("KD", 2, c(1.5,2.5)),
        c("KD", 3, c(0,6)),
        c("M", 1, c(1.5,4.5)),
        c("M", 2, c(2,3.5)),
        c("M", 3, c(0,6)),
        c("SD", 1, c(2,4.2)),
        c("SD", 2, c(2,4.2)),
        c("SD", 3, c(0,7))
      )
      
      ylims <- as.data.frame(t(ylims), stringsAsFactors=FALSE)
      
      plot(rating, mean_control, type="n",
           ylim = c(as.numeric(ylims$V3[ylims$V1==party & ylims$V2==m]), as.numeric(ylims$V4[ylims$V1==party & ylims$V2==m])),
           ylab = "",
           xlab = "",
           bty = "n",
           las = 1, xaxt=ifelse(party %in% c("MP", "SAP", "C", "KD", "M") | party =="L" & m==1, 'n','s')
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
      
      if (k ==4 & m==2 & party=="L") {
        legend(2,3.4, legend = c("Control", "Treatment"), fill = c("gray77","gray50"),  
               cex = 1.2)
      }
      
      if (k ==4 & m==3 & party=="L") {
        legend(1.5,5, legend = c("Control", "Treatment"), fill = c("gray77","gray50"),  
               cex = 1.2)
      }
      
      if (k ==4 & m==2 & party=="SD") {
        legend(1,4, legend = c("Control", "Treatment"), fill = c("gray77","gray50"),  
               cex = 1.2)
      }
      
      if (k ==4 & m==3 & party=="SD") {
        legend(1.5,6, legend = c("Control", "Treatment"), fill = c("gray77","gray50"),  
               cex = 1.2)
      }
      
      k <- k+1 
    }
    
    title(xlab = xaxis_label[m],
          ylab = paste("Expected PTV for ", party, sep=""),
          outer = TRUE, line = 2, cex.lab = 1.5)
    
    if (party %in% c("L", "MP")) {
      mtext( 'Treatment M-C-L-KD', side=3, line=0, at=grconvertX(-3,'npc','nic'), outer=TRUE, cex=1)
      mtext( 'Treatment M-SD', side=3, line=0, at=grconvertX(-1.8,'npc','nic'), outer=TRUE, cex=1)
      mtext( 'Treatment SAP-MP', side=3, line=0, at=grconvertX(-0.65,'npc','nic'), outer=TRUE, cex=1)
      mtext( 'Treatment SAP-MP-L-C', side=3, line=0, at=grconvertX(0.5,'npc','nic'), outer=TRUE, cex=1)
    }
    
    par(op)
  }
}

# Table 6 (Appendix)
stargazer(Table6_model1, Table6_model2, Table6_model3, Table6_model4)
