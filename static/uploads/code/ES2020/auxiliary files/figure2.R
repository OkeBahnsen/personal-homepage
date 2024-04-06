# Create df with information on party system 
party_system <- data.frame(# Party names
  c("V","MP","SAP","C","L","KD","M","SD"), 
  # Party positions according to subjects in control group 
  c(mean(dta$lr_V[dta$group=="Control"], na.rm=T), 
    mean(dta$lr_MP[dta$group=="Control"], na.rm=T),
    mean(dta$lr_SAP[dta$group=="Control"], na.rm=T),
    mean(dta$lr_C[dta$group=="Control"], na.rm=T),
    mean(dta$lr_L[dta$group=="Control"], na.rm=T),
    mean(dta$lr_KD[dta$group=="Control"], na.rm=T),
    mean(dta$lr_M[dta$group=="Control"], na.rm=T),
    mean(dta$lr_SD[dta$group=="Control"], na.rm=T)),
  # 2018 election results
  c(8.00,4.41,28.26,8.61,5.49,6.32,19.84,17.53))
colnames(party_system) <- c("Party", "Party_Position", "Election_Result_2018")

figure2 <- ggplot(party_system, aes(x=Party_Position, y=Election_Result_2018, label=Party)) + geom_point(size=party_system$Election_Result_2018/2, stroke = 2, shape=1, color="black") + geom_text(aes(label=Party),hjust=c(-1,-0.5,-1,-1,-1,-0.5,-2,-1), vjust=c(0,0,0,0,0,0,0,0.4)) + scale_y_continuous(limits=c(0, 31), expand = c(0, 0)) + scale_x_continuous(limits=c(1, 7), expand = c(0, 0)) + theme_bw() + xlab("Left-Right Placement") + ylab("2018 Election Result (in %)") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
