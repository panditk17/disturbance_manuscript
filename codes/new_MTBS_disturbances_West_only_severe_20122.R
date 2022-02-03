## codes to create chronsequence analysis based on MTBS and FIA data
## compare post-fire recovery following severe fire
## AGB and seedling density compared between conifer and hardwood
## -------------- Karun Pandit, 2022 -------------------------------
rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
memory.limit(size=100000000)

# read data with disturbance
mtbs_plots<-readRDS("../data/nnn2_all_plots_with_disturb_MTBS.RDS")
all_plots<-readRDS("../data/all_plots_with_disturb_FIA.RDS")

tab11<-count(all_plots,DISTURB)

fire0<-read.csv("../../MTBS_fire_FIA_plots.csv") 
fire01<-fire0[c("nplotid","fire_id_mtbs")]

#merge fire one time to temporal data
mtbs_plots2<-merge(mtbs_plots,fire01,by.x="NUNIDS.2",by.y="nplotid")

# edit ecoregion data by splitting
mtbs_plots3<-separate(mtbs_plots2,fire_id_mtbs, into = c("id_1", "fire_yr",
                                             "fire_mn","fire_day"), 
                sep =c(13,17,19,21), remove = FALSE)

#read seedling data from two measurements
seedling1<-readRDS("../data/all_seedlings_first.RDS")
seedling2<-readRDS("../data/all_seedlings_second.RDS")

# rename
ssdam<-mtbs_plots3

# read plot data for all FIA pllts
plots<-read.csv("../../data/PLOT.CSV")

library(tidyverse)

plots_all<-plots[c("INVYR","STATECD","UNITCD","COUNTYCD",
                            "PLOT","MEASMON","MEASDAY")]

plots_all$pplotid<-paste0(plots_all$STATECD,"-",plots_all$UNITCD,"-",
                          plots_all$COUNTYCD,"-",plots_all$PLOT)
plots_all$pplotidyr<-paste0(plots_all$STATECD,"-",plots_all$UNITCD,"-",
                            plots_all$COUNTYCD,"-",plots_all$PLOT,"-",plots_all$INVYR)


sdam11a<-merge(ssdam,plots_all,by.x="NUNID.1",by.y="pplotidyr",all.x=TRUE)
# sdam11<-merge(sdam11a,cond_sel2,by.x="NUNID.1",by.y="pplotidyr",all.x=TRUE)

rm(plots)


conds<-read.csv("../../data/COND.CSV")

library(tidyverse)


conds_all<-conds[c("INVYR","STATECD","UNITCD","COUNTYCD",
                            "PLOT","FLDSZCD","CONDPROP_UNADJ","STDAGE","FLDAGE")]
# 
# conds_all<-conds %>% select(INVYR,STATECD,UNITCD,COUNTYCD,
#                             PLOT,FLDSZCD,CONDPROP_UNADJ,STDAGE,FLDAGE)
conds_all$pplotid<-paste0(conds_all$STATECD,"-",conds_all$UNITCD,"-",
                          conds_all$COUNTYCD,"-",conds_all$PLOT)
conds_all$pplotidyr<-paste0(conds_all$STATECD,"-",conds_all$UNITCD,"-",
                            conds_all$COUNTYCD,"-",conds_all$PLOT,"-",conds_all$INVYR)
conds_sort <- conds_all[order(conds_all$pplotidyr,conds_all$CONDPROP_UNADJ),]

#
# remove.packages("tidyverse")
# install.packages("tidyverse")
library(tidyverse)
cond_sel<-conds_all %>% group_by(pplotidyr) %>%
  filter(abs(CONDPROP_UNADJ) == min(CONDPROP_UNADJ))
#
cond_sel2<-cond_sel[c("pplotidyr","FLDSZCD")]
colnames(cond_sel2)<-c("pplotidyr","FLDSZCD")

cond_sel3<-cond_sel %>% group_by(pplotidyr) %>%  slice(1) 


sdam11b<-merge(sdam11a,cond_sel3,by.x="NUNID.1",by.y="pplotidyr")
sdam11<-merge(sdam11b,cond_sel3,by.x="NUNID.2",by.y="pplotidyr")

jkjk<-count(sdam11,STDAGE.1,STDAGE.2,STDAGE.x,STDAGE.y)

rm(conds)

sdam11$date<-paste0(sdam11$MEASYEAR.2,"-",sdam11$MEASMON,"-",
                    sdam11$MEASDAY)
sdam11$date_ym<-paste0(sdam11$MEASYEAR.2,"-",sdam11$MEASMON)
sdam11$diff_yr<-sdam11$MEASYEAR.2-as.numeric(sdam11$fire_yr)
sdam11$diff_mn<-sdam11$MEASMON-as.numeric(sdam11$fire_mn)
sdam11$diff_dy<-sdam11$MEASDAY-as.numeric(sdam11$fire_day)
sdam11$diff_days<-round(sdam11$diff_yr*365 + sdam11$diff_mn*30.41 +sdam11$diff_dy,digits=0)
sdam11$diff_months<-round(sdam11$diff_days/30.41,digits=2)
sdam11$diff_years<-round(sdam11$diff_months/12,digits=2)


# merge MTBS data with seedling data
dam111<-merge(sdam11,seedling1,by.x="NUNID.1",by.y="NUNID",all.x=TRUE)
dam112<-merge(dam111,seedling2,by.x="NUNID.2",by.y="NUNID",all.x=TRUE)

sdam1<-dam112

sdam1$seed_count.1[is.na(sdam1$seed_count.1)] <- 0
sdam1$seed_count.2[is.na(sdam1$seed_count.2)] <- 0
sdam1$sd_spp_rich.1[is.na(sdam1$sd_spp_rich.1)] <- 0
sdam1$sd_spp_rich.2[is.na(sdam1$sd_spp_rich.2)] <- 0

sdam1$seed_count_con.1[is.na(sdam1$seed_count_con.1)] <- 0
sdam1$seed_count_con.2[is.na(sdam1$seed_count_con.2)] <- 0
sdam1$sd_spp_rich_con.1[is.na(sdam1$sd_spp_rich_con.1)] <- 0
sdam1$sd_spp_rich_con.2[is.na(sdam1$sd_spp_rich_con.2)] <- 0

sdam1$seed_count.1<-sdam1$seed_count.1*74.96*2.471
sdam1$seed_count.2<-sdam1$seed_count.2*74.96*2.471

sdam1$seed_count_con.1<-sdam1$seed_count_con.1*74.96*2.471
sdam1$seed_count_con.2<-sdam1$seed_count_con.2*74.96*2.471

sdam1$seed_ct_ch<-sdam1$seed_count.2-sdam1$seed_count.1
sdam1$sdsp_rh_ch<-sdam1$sd_spp_rich.2-sdam1$sd_spp_rich.1

sdam1$seed_ct_ch_con<-sdam1$seed_count_con.2-sdam1$seed_count_con.1
sdam1$sdsp_rh_ch_con<-sdam1$sd_spp_rich_con.2-sdam1$sd_spp_rich_con.1


dam1<-sdam1

write.csv(dam1,"outputs/plots_disturbance_MTBS1.csv")

fff<-count(dam1,rep_std,severity)

# all the plots plotted both east and west
dam1<-dam1[which(dam1$diff_days>0),]
dam1<-dam1[which(dam1$diff_days<5000),]

ggplot(data=dam1) +
  geom_jitter(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
    # scale_y_continuous(limits=c(0,500))+
  xlab("time since fire (days)")+
  ylab("FIA stand age (years)")+
  
    theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))


ggplot(data=dam1) +
  geom_jitter(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
  scale_y_continuous(limits=c(0,15))+
  xlab("time since fire (years)")+
  ylab("FIA stand age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))


# edit ecoregion data by splitting
dam3<-separate(dam1, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)

# use function to convert forest type to groups
source("codes/FTGC.R")
dam3$FOR_GRP<-FTGC(dam3$FORTYPCD.1)


dam3$AGB.1<-(dam3$AGB.1*453.6*2.471)/1000000
dam3$AGB.2<-(dam3$AGB.2*453.6*2.471)/1000000


dam3$AGB_CHP<-(dam3$AGB.2-dam3$AGB.1)

# read file with ecoregion code from the west
ecosel<-read.csv("../data/eco_select.csv")

dam3$ecocode <- trimws(dam3$Spl_1, which = c("left"))

library(operators)
dam33a<-dam3[(dam3$ecocode %in% ecosel$econew),]

dam33b<-dam3[(dam3$ecocode %!in% ecosel$econew),]

east1<-count(dam33b,severity,rep_std)


dam33c<-separate(dam33b, NUNIDS.2, 
                 into = c("st","cty","unt","pl"), remove = FALSE)
dam33d<-separate(dam33a, NUNIDS.2, 
                into = c("st","cty","unt","pl"), remove = FALSE)

# analysis for Western only
dam_west<-dam33a

ggplot(data=dam_west) +
  geom_jitter(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
  # scale_y_continuous(limits=c(0,500))+
  xlab("time since fire (days)")+
  ylab("FIA stand age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))

ggplot(data=dam_west) +
  geom_jitter(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
  scale_y_continuous(limits=c(0,15))+
  xlab("time since fire (years)")+
  ylab("FIA stand age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))



# analysis for Eastern only
dam_east<-dam33b

ggplot(data=dam_east) +
  geom_jitter(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
  # scale_y_continuous(limits=c(0,500))+
  xlab("time since fire (days)")+
  ylab("FIA stand age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))



ggplot(data=dam_east) +
  geom_jitter(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
  scale_y_continuous(limits=c(0,15))+
  xlab("time since fire (years)")+
  ylab("FIA stand age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))


# analysis for Western severe forest


dam_westsev<-dam_west[which(dam_west$severity==1),]

dam_westmild<-dam_west[which(dam_west$severity==0),]


ggplot(data=dam_westsev) +
  geom_jitter(aes(x=diff_days, y=STDAGE.2,color=as.factor(Spl_1))) +
  # scale_color_manual(name="fire severity",labels=c(" 231"," 232","M242","M261",
  #                   "M332","M333"), values=c(" 231"="yellow"," 232"="orange",
  #                   "M242"="brown", "M261"="red","M332"="green","M333"="blue")) + 
  scale_y_continuous(limits=c(0,100))+
  xlab("time since fire (years)")+
  ylab("FIA stand age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))

ggplot(data=dam_westsev) +
  geom_jitter(aes(x=diff_days, y=STDAGE.2,color=as.factor(Spl_1))) +
  # scale_color_manual(name="fire severity",labels=c(" 231"," 232","M242","M261",
  #                   "M332","M333"), values=c(" 231"="yellow"," 232"="orange",
  #                   "M242"="brown", "M261"="red","M332"="green","M333"="blue")) + 
  scale_y_continuous(limits=c(0,15))+
  xlab("time since fire (years)")+
  ylab("FIA stand age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))
# dam31<-dam2[(dam2$FOR_GRP %in% table21$FOR_GRP),]



sdam4<-dam_west

sdam4$conifer<-ifelse(sdam4$FOR_GRP<400,"Conifer","Hardwood")


sdam4$for_type<-ifelse(sdam4$FOR_GRP<400,"Conifer",
                       ifelse(sdam4$FOR_GRP>=400 & sdam4$FOR_GRP<999,"Hardwood",
                              ifelse(sdam4$FOR_GRP==999,"Non-stocked","None"
                              )))

sdam_sev<-sdam4[which(sdam4$severity==1),]

sdam_mild<-sdam4[which(sdam4$severity==0),]

plot(sdam_sev$seed_count.2)

sdam_sev<-sdam_sev[which(sdam_sev$AGB.2<50),]
sdam_sev<-sdam_sev[which(sdam_sev$seed_count.2<50000),]


sdam44<-rbind(sdam_sev,sdam_mild)

sdam44$age<-ifelse(sdam44$STDAGE.2>0,1,0)

sdam_con<-sdam44[which(sdam44$for_type=="Conifer"),]
sdam_hard<-sdam44[which(sdam44$for_type=="Hardwood"),]

sdam_both<-rbind(sdam_con,sdam_hard)
sdam_both_sev<-sdam_both[which(sdam_both$severity==1),]
sdam_both_mild<-sdam_both[which(sdam_both$severity==0),]

# sdam_both_sev$age<-ifelse(sdam_both_sev$STDAGE.2>0,1,0)

sdam_consev<-sdam_con[which(sdam_con$severity==1),]
sdam_hardsev<-sdam_hard[which(sdam_hard$severity==1),]

sdam_consev$seed_class1<-ifelse(sdam_consev$seed_count.1>0,1,0)
sdam_consev$seed_class2<-ifelse(sdam_consev$seed_count.2>0,1,0)

ppp<-count(sdam_consev,seed_class1,seed_class2)

sdam_hardsev$seed_class1<-ifelse(sdam_hardsev$seed_count.1>0,1,0)
sdam_hardsev$seed_class2<-ifelse(sdam_hardsev$seed_count.2>0,1,0)

pppp<-count(sdam_hardsev,seed_class1,seed_class2)


ggplot(data=sdam_con) +
  geom_point(aes(x=diff_days, y=AGB.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
  # scale_y_continuous(limits=c(0,500))+
  xlab("time since fire (days)")+
  ylab("FIA stand age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))


ggplot(data=sdam_hard) +
  geom_point(aes(x=diff_days, y=AGB_CHP,color=as.factor(severity))) +
  geom_smooth(aes(x=diff_days, y=AGB_CHP,color=as.factor(severity)),methods=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
  scale_y_continuous(limits=c(-300,100))+
  xlab("time since fire (years)")+
  ylab("FIA stand age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))



hardtab1<-data.frame(table(sdam_hardsev$FOR_GRP,sdam_hardsev$age))
hardtab2<-data.frame(table(sdam_hardsev$FOR_GRP))
hardtab3<-merge(hardtab1,hardtab2,by="Var1")
hardtab3$per<-hardtab3$Freq.x/hardtab3$Freq.y*100
hardtab4<-hardtab3[which(hardtab3$Var2==1),]
hardtab4b<-hardtab3[which(hardtab3$Var2==0),]
hardtab5<-merge(hardtab4,hardtab4b,by="Var1")
hardtab4<-hardtab3[which(hardtab3$Var2==1),]
hardtab6<-hardtab5[,c(1,8,3,5,7,9)]
colnames(hardtab6)<-c("Forest type","total plots","plots increasing in age",
                      "% plots increasing in age","plots stuck at 0","percent stuck at 0")
ch_hardsev<-chisq.test(sdam_hardsev$FOR_GRP,sdam_hardsev$age)


contab1<-data.frame(table(sdam_consev$FOR_GRP,sdam_consev$age))
contab2<-data.frame(table(sdam_consev$FOR_GRP))
contab3<-merge(contab1,contab2,by="Var1")
contab3$per<-contab3$Freq.x/contab3$Freq.y*100
contab4<-contab3[which(contab3$Var2==1),]
contab4b<-contab3[which(contab3$Var2==0),]
contab5<-merge(contab4,contab4b,by="Var1")
contab4<-contab3[which(contab3$Var2==1),]
contab6<-contab5[,c(1,8,3,5,7,9)]
colnames(contab6)<-c("Forest type","total plots","plots increasing in age",
                     "% plots increasing in age","plots stuck at 0","percent stuck at 0")
ch_consev<-chisq.test(sdam_consev$FOR_GRP,sdam_consev$age)

ch_both_sev<-chisq.test(sdam_both_sev$conifer,sdam_both_sev$age)

print(ch_both_sev)
print(ch_hardsev)
print(ch_consev)


write.csv(contab6,"outputs/summary_conifer.csv")
write.csv(hardtab6,"outputs/summary_hardwood.csv")

# plot comparing severe vs mild fire


png("figures/mtbs_figures_severe_mild.jpeg", width = 1000, height = 1400)
par(xpd = F, mar = c(4,4,4,0.5))
par(mfrow=c(7,2))

# plots standage vs time since fire for conifer and hardwood
p1<-ggplot(data=sdam_consev) +
  geom_point(aes(x=diff_years, y=STDAGE.2,color=as.factor(severity))) +
  # geom_smooth(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity)),method=loess) +

  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  scale_y_continuous(limits=c(-1,10))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  # scale_x_continuous(seq(1,3500,365))+
  
  xlab("time since fire (year)")+
  ylab("FIA stand age (year)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13)) +
  geom_text(x=3, y=10, label="Stand age increasing: 36 % of plots",size=4.7)+
  
  geom_text(x=3, y=9, label="Stand age 0 persistent: 64 % of plots",size=4.7)

p2<-ggplot(data=sdam_hardsev) +
  geom_point(aes(x=diff_years, y=STDAGE.2,color=as.factor(severity))) +
  # geom_smooth(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity)),method=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  scale_y_continuous(limits=c(-1,10))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  xlab("time since fire (year)")+
  ylab("FIA stand age (year)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))+
  geom_text(x=3, y=10, label="Stand age increasing: 82 % of plots",size=4.7)+
  
  geom_text(x=3, y=9, label="Stand age 0 persistent: 18 % of plots",size=4.7)
# sdam_consev$age<-ifelse(sdam_consev$STDAGE.2>0,1,0)
# sdam_hardsev$age<-ifelse(sdam_hardsev$STDAGE.2>0,1,0)


p3<-  ggplot(data=sdam_con) +
  geom_point(aes(x=diff_years, y=AGB.2,color=as.factor(severity))) +
  geom_smooth(aes(x=diff_years, y=AGB.2,color=as.factor(severity)),method=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  
  scale_y_continuous(limits=c(-30,400))+
    scale_x_continuous(breaks = c(seq(1,10,1)))+
    
  xlab("time since fire (year)")+
  ylab("Post-fire AGB (Mg/ha)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))

p4<-ggplot(data=sdam_hard) +
  geom_point(aes(x=diff_years, y=AGB.2,color=as.factor(severity))) +
  geom_smooth(aes(x=diff_years, y=AGB.2,color=as.factor(severity)),method=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  scale_y_continuous(limits=c(-30,400))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("time since fire (year)")+
  ylab("Post-fire AGB (Mg/ha)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))

p5<-ggplot(data=sdam_con) +
  geom_point(aes(x=diff_years, y=AGB_CHP,color=as.factor(severity))) +
  geom_smooth(aes(x=diff_years, y=AGB_CHP,color=as.factor(severity)),method=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  scale_y_continuous(limits=c(-300,100))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("time since fire (year)")+
  ylab("Change in AGB (Mg/ha)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))

p6<-ggplot(data=sdam_hard) +
  geom_point(aes(x=diff_years, y=AGB_CHP,color=as.factor(severity))) +
  geom_smooth(aes(x=diff_years, y=AGB_CHP,color=as.factor(severity)),method=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  scale_y_continuous(limits=c(-300,100))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("time since fire (year)")+
  ylab("Change in AGB (Mg/ha)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))


p7<-ggplot(data=sdam_con) +
  geom_point(aes(x=diff_years, y=seed_count.2,color=as.factor(severity))) +
  geom_smooth(aes(x=diff_years, y=seed_count.2,color=as.factor(severity)),method=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  scale_y_continuous(limits=c(-1000,8000))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("time since fire (days)")+
  ylab("post-fire seed density (ha)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))

p8<-ggplot(data=sdam_hard) +
  geom_point(aes(x=diff_years, y=seed_count.2,color=as.factor(severity))) +
  geom_smooth(aes(x=diff_years, y=seed_count.2,color=as.factor(severity)),method=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  # scale_y_continuous(limits=c(-300,100))+
  scale_y_continuous(limits=c(-1000,8000))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  
  xlab("time since fire (year)")+
  ylab("post-fire seed density (ha)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))

p9<-ggplot(data=sdam_con) +
  geom_point(aes(x=diff_years, y=seed_ct_ch,color=as.factor(severity))) +
  geom_smooth(aes(x=diff_years, y=seed_ct_ch,color=as.factor(severity)),method=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  scale_y_continuous(limits=c(-8000,10000))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("time since fire (year)")+
  ylab("change in seed density (ha)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))

p10<-ggplot(data=sdam_hard) +
  geom_point(aes(x=diff_years, y=seed_ct_ch,color=as.factor(severity))) +
  geom_smooth(aes(x=diff_years, y=seed_ct_ch,color=as.factor(severity)),method=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  # scale_y_continuous(limits=c(-300,100))+
  scale_y_continuous(limits=c(-8000,10000))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  
  xlab("time since fire (year)")+
  ylab("change in seed density (ha)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))

library(grid)

library(gtable)
library(gridExtra)
legend1 = gtable_filter(ggplot_gtable(ggplot_build(p3 + theme(legend.position="bottom"))), "guide-box")
legend2 = gtable_filter(ggplot_gtable(ggplot_build(p8 + theme(legend.position="bottom"))), "guide-box")
tg1 <- textGrob('Conifer', gp = gpar(fontsize = 14, fontface = 'bold'))
tg2 <- textGrob('Hardwood', gp = gpar(fontsize = 14, fontface = 'bold'))

# png("all_age_sev_mtbsnew7.jpeg", width = 1000, height = 1500)
# par(xpd = F, mar = c(4,4,4,0.5))
# par(mfrow=c(5,2))

grid.arrange(tg1,tg2,p1 + theme(legend.position="none"), 
             p2 + theme(legend.position="none"),
             p3 + theme(legend.position="none"), 
             p4 + theme(legend.position="none"),
             p5+  theme(legend.position="none"),
             p6+  theme(legend.position="none"),
             p7 + theme(legend.position="none"),
             p8+  theme(legend.position="none"),
             p9 + theme(legend.position="none"),
             p10+  theme(legend.position="none"),
             legend1,
             legend2,
             heights=c(0.1,1.1, 1.1, 1.1, 1.1, 1.1,0.1),
             nrow = 7)
dev.off()




png("figures/mtbs_fig_by_foresttype45.jpeg", width = 1000, height = 1100)
par(xpd = F, mar = c(4,4,6,0.5))
par(mfrow=c(4,2))

# plots standage vs time since fire for conifer and hardwood
p1<-ggplot(data=sdam_consev) +
  geom_point(aes(x=diff_years, y=STDAGE.2,color=as.factor(severity))) +
  # geom_smooth(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity)),method=loess) +
  
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  scale_y_continuous(limits=c(-1,10))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  # scale_x_continuous(seq(1,3500,365))+
  
  xlab("time since fire (year)")+
  ylab("post-fire stand age (year)")+
  ggtitle("Stand age - conifer")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13)) +
  geom_text(x=3, y=10, label="Stand age increasing: 36 % of plots",size=4.7)+
  
  geom_text(x=3, y=9, label="Stand age 0 persistent: 64 % of plots",size=4.7)

p2<-ggplot(data=sdam_hardsev) +
  geom_point(aes(x=diff_years, y=STDAGE.2,color=as.factor(severity))) +
  # geom_smooth(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity)),method=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  scale_y_continuous(limits=c(-1,10))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  xlab("time since fire (year)")+
  ylab("post-fire stand age (year)")+
  ggtitle("Stand age - hardwood")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))+
  geom_text(x=3, y=10, label="Stand age increasing: 82 % of plots",size=4.7)+
  
  geom_text(x=3, y=9, label="Stand age 0 persistent: 18 % of plots",size=4.7)
# sdam_consev$age<-ifelse(sdam_consev$STDAGE.2>0,1,0)
# sdam_hardsev$age<-ifelse(sdam_hardsev$STDAGE.2>0,1,0)



p13<-ggplot(data=sdam_both_sev) +
  geom_point(aes(x=diff_years, y=AGB.2,color=as.factor(conifer))) +
  geom_smooth(aes(x=diff_years, y=AGB.2,color=as.factor(conifer)),method=loess) +
  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+
  
  scale_y_continuous(limits=c(-5,30))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("time since fire (year)")+
  ylab("Post-fire AGB (Mg/ha)")+
  ggtitle("Pre-fire AGB")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=19),
        legend.text = element_text(size=18))


# sdam_hardsev$age<-ifelse(sdam_hardsev$STDAGE.2>0,1,0)


p15<-ggplot(data=sdam_both_sev) +
  geom_point(aes(x=diff_years, y=AGB_CHP,color=as.factor(conifer))) +
  geom_smooth(aes(x=diff_years, y=AGB_CHP,color=as.factor(conifer)),method=loess) +
  
  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+ 
  scale_y_continuous(limits=c(-300,90))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("time since fire (year)")+
  ylab("Change in AGB (Mg/ha)")+
  ggtitle("AGB change")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))


p17<-ggplot(data=sdam_both_sev) +
  geom_point(aes(x=diff_years, y=seed_count.2,color=as.factor(conifer))) +
  geom_smooth(aes(x=diff_years, y=seed_count.2,color=as.factor(conifer)),method=loess) +
  
  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+ 
  scale_y_continuous(limits=c(-800,7800))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("time since fire (year)")+
  ylab("post-fire seed density (ha)")+
  ggtitle("Pre-fire seedling density")+
  
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))


p19<-ggplot(data=sdam_both_sev) +
  geom_point(aes(x=diff_years, y=seed_ct_ch,color=as.factor(conifer))) +
  geom_smooth(aes(x=diff_years, y=seed_ct_ch,color=as.factor(conifer)),method=loess) +
  
  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+ 
  scale_y_continuous(limits=c(-7000,10000))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("time since fire (year)")+
  ylab("change in seed density (ha)")+
  ggtitle("seedling density change")+
  
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))





library(gtable)
legend1 = gtable_filter(ggplot_gtable(ggplot_build(p13 + theme(legend.position="bottom"))), "guide-box")
# legend1 = gtable_filter(ggplot_gtable(ggplot_build(p13 + theme(legend.position="bottom"))), "guide-box")

legend2 = gtable_filter(ggplot_gtable(ggplot_build(p19 + theme(legend.position="none"))), "guide-box")

# png("all_age_sev_mtbsnew7.jpeg", width = 1000, height = 1500)
# par(xpd = F, mar = c(4,4,4,0.5))
# par(mfrow=c(5,2))
# tg1 <- textGrob('Severe', gp = gpar(fontsize = 14, fontface = 'bold'))
# tg2 <- textGrob('Mild', gp = gpar(fontsize = 14, fontface = 'bold'))


library(grid)
grid.arrange(p1 + theme(legend.position="none"), 
             p2 + theme(legend.position="none"),
             p13 + theme(legend.position="none"), 
             p15+  theme(legend.position="none"),
             p17 + theme(legend.position="none"),
             p19 + theme(legend.position="none"),
             legend1,
             legend2,
             heights=c(1.1, 1.1, 1.1, 0.1),
             nrow = 4)
             
             # top= textGrob(
             #   "Severe Mild",
             #   gp = gpar(fontface = 3, fontsize = 18),
             #   hjust = 0.5,
             #   x = 10


dev.off()




sdam_hardsev$age_cat.1<-ifelse(sdam_hardsev$STDAGE.1==0,"Zero","Non-zero")
sdam_hardsev$age_cat.2<-ifelse(sdam_hardsev$STDAGE.2==0,"Zero","Non-zero")
sdam_hardsev$stocked.1<-ifelse(sdam_hardsev$FLDSZCD.x>0,"Stocked","Non-stocked")
sdam_hardsev$stocked.2<-ifelse(sdam_hardsev$FLDSZCD.y>0,"Stocked","Non-stocked")
hardsev0<-count(sdam_hardsev,age_cat.1,age_cat.2,stocked.1,stocked.2)

sdam_consev$age_cat.1<-ifelse(sdam_consev$STDAGE.1==0,"Zero","Non-zero")
sdam_consev$age_cat.2<-ifelse(sdam_consev$STDAGE.2==0,"Zero","Non-zero")
sdam_consev$stocked.1<-ifelse(sdam_consev$FLDSZCD.x>0,"Stocked","Non-stocked")
sdam_consev$stocked.2<-ifelse(sdam_consev$FLDSZCD.y>0,"Stocked","Non-stocked")
consev0<-count(sdam_consev,age_cat.1,age_cat.2,stocked.1,stocked.2)




sdam_hardsev$age_cat.1b<-ifelse(sdam_hardsev$STDAGE.x==0,"Zero","Non-zero")
sdam_hardsev$age_cat.2b<-ifelse(sdam_hardsev$STDAGE.y==0,"Zero","Non-zero")
sdam_hardsev$stocked.1b<-ifelse(sdam_hardsev$FLDSZCD.x>0,"Stocked","Non-stocked")
sdam_hardsev$stocked.2b<-ifelse(sdam_hardsev$FLDSZCD.y>0,"Stocked","Non-stocked")
hardsev0b<-count(sdam_hardsev,age_cat.1b,age_cat.2b,stocked.1b,stocked.2b)

sdam_consev$age_cat.1b<-ifelse(sdam_consev$STDAGE.x==0,"Zero","Non-zero")
sdam_consev$age_cat.2b<-ifelse(sdam_consev$STDAGE.y==0,"Zero","Non-zero")
sdam_consev$stocked.1b<-ifelse(sdam_consev$FLDSZCD.x>0,"Stocked","Non-stocked")
sdam_consev$stocked.2b<-ifelse(sdam_consev$FLDSZCD.y>0,"Stocked","Non-stocked")
consev0b<-count(sdam_consev,age_cat.1b,age_cat.2b,stocked.1b,stocked.2b)




sdam_hardsev$age_cat.1c<-ifelse(sdam_hardsev$FLDAGE.x==0,"Zero","Non-zero")
sdam_hardsev$age_cat.2c<-ifelse(sdam_hardsev$FLDAGE.y==0,"Zero","Non-zero")
sdam_hardsev$stocked.1c<-ifelse(sdam_hardsev$FLDSZCD.x>0,"Stocked","Non-stocked")
sdam_hardsev$stocked.2c<-ifelse(sdam_hardsev$FLDSZCD.y>0,"Stocked","Non-stocked")
hardsev0c<-count(sdam_hardsev,age_cat.1c,age_cat.2c,stocked.1c,stocked.2c)

sdam_consev$age_cat.1c<-ifelse(sdam_consev$FLDAGE.x==0,"Zero","Non-zero")
sdam_consev$age_cat.2c<-ifelse(sdam_consev$FLDAGE.y==0,"Zero","Non-zero")
sdam_consev$stocked.1c<-ifelse(sdam_consev$FLDSZCD.x>0,"Stocked","Non-stocked")
sdam_consev$stocked.2c<-ifelse(sdam_consev$FLDSZCD.y>0,"Stocked","Non-stocked")
consev0c<-count(sdam_consev,age_cat.1c,age_cat.2c,stocked.1c,stocked.2c)







# 
# sdam_consev<-sdam_consev[which(sdam_consev$stocked.2=="Stocked"),]
# sdam_hardsev<-sdam_consev[which(sdam_hardsev$stocked.2=="Stocked"),]


# plots standage vs time since fire for conifer and hardwood
p31<-ggplot(data=sdam_consev) +
  geom_point(aes(x=diff_years, y=STDAGE.2)) +
  # geom_smooth(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity)),method=loess) +
  
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  scale_y_continuous(limits=c(-1,10))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  geom_hline(yintercept=0,size=0.7,color="black",alpha=0.6)+
  
  
  # scale_x_continuous(seq(1,3500,365))+
  
  xlab("")+
  ylab("post-fire stand age (year)")+
  # ggtitle("Stand age - conifer")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 17),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13)) +
  geom_text(x=3, y=10, label="Stand age increasing: 36 % of plots",size=4.7)+
  
  geom_text(x=3, y=9, label="Stand age 0 persistent: 64 % of plots",size=4.7)

p32<-ggplot(data=sdam_hardsev) +
  geom_point(aes(x=diff_years, y=STDAGE.2)) +
  
  # geom_point(aes(x=diff_years, y=STDAGE.2,color=as.factor(severity))) +
  # geom_smooth(aes(x=diff_days, y=STDAGE.2,color=as.factor(severity)),method=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="#009E73","1"="#D55E00"))+
  scale_y_continuous(limits=c(-1,10))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  geom_hline(yintercept=0,size=0.7,color="black",alpha=0.6)+
  
  xlab("")+
  ylab("")+
  # ggtitle("Stand age - hardwood")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))+
  geom_text(x=3, y=10, label="Stand age increasing: 82 % of plots",size=4.7)+
  
  geom_text(x=3, y=9, label="Stand age 0 persistent: 18 % of plots",size=4.7)
# sdam_consev$age<-ifelse(sdam_consev$STDAGE.2>0,1,0)
# sdam_hardsev$age<-ifelse(sdam_hardsev$STDAGE.2>0,1,0)



p33<-ggplot(data=sdam_consev) +
  geom_point(aes(x=diff_years, y=AGB_CHP)) +
  geom_smooth(aes(x=diff_years, y=AGB_CHP),color="black",method=loess) +
  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+
  
  scale_y_continuous(limits=c(-300,90))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("")+
  ylab("change in AGB (Mg/ha)")+
  # ggtitle("Pre-fire AGB")+
  geom_hline(yintercept=0,size=0.7,color="black",alpha=0.6)+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 17),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=19),
        legend.text = element_text(size=18))


# sdam_hardsev$age<-ifelse(sdam_hardsev$STDAGE.2>0,1,0)


p34<-ggplot(data=sdam_hardsev) +
  geom_point(aes(x=diff_years, y=AGB_CHP)) +
  geom_smooth(aes(x=diff_years, y=AGB_CHP,color="black"),method=loess) +
  
  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+ 
  scale_y_continuous(limits=c(-300,90))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("")+
  ylab("")+
  geom_hline(yintercept=0,size=0.7,color="black",alpha=0.6)+
  
  # ggtitle("AGB change")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))


sdam_consev1<-sdam_consev[which(sdam_consev$seed_class1==1),]
sdam_hardsev1<-sdam_hardsev[which(sdam_hardsev$seed_class1==1),]

p35<-ggplot(data=sdam_consev) +
  geom_point(aes(x=diff_years, y=seed_ct_ch)) +
  geom_smooth(aes(x=diff_years, y=seed_ct_ch,color="black"),method=loess) +
  
  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+ 
  scale_y_continuous(limits=c(-7000,9000))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("time since fire (year)")+
  ylab("change in seedling density (ha)")+
  geom_hline(yintercept=0,size=0.7,color="black",alpha=0.6)+
  
  # ggtitle("Pre-fire seedling density")+
  
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 17),
        axis.title.x=element_text(size=17),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))


p36<-ggplot(data=sdam_hardsev) +
  geom_point(aes(x=diff_years, y=seed_ct_ch)) +
  geom_smooth(aes(x=diff_years, y=seed_ct_ch,color="black"),method=loess) +
  
  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+ 
  scale_y_continuous(limits=c(-7000,9000))+
  scale_x_continuous(breaks = c(seq(1,10,1)))+
  
  xlab("time since fire (year)")+
  geom_hline(yintercept=0,size=0.7,color="black",alpha=0.6)+
  
  ylab("")+
  # ggtitle("seedling density change")+
  
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=17),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))





sdam_consevage0<-sdam_consev[which(sdam_consev$STDAGE.2==0),]
table(sdam_consevage0$FLDSZCD.y,sdam_consevage0$STDAGE.2)


sdam_hardsevage0<-sdam_hardsev[which(sdam_hardsev$STDAGE.2==0),]
table(sdam_hardsevage0$FLDSZCD.y,sdam_hardsevage0$STDAGE.2)



conifer_age0pre<-count(sdam_consevage0,FLDSZCD.x,STDAGE.1)
conifer_age0post<-count(sdam_consevage0,FLDSZCD.y,STDAGE.2)
conifer_post<-count(sdam_consev,FLDSZCD.y,STDAGE.2)
conifer_pre<-count(sdam_consev,FLDSZCD.x,STDAGE.1)

hardwood_age0pre<-count(sdam_hardsevage0,FLDSZCD.x,STDAGE.1)
hardwood_age0post<-count(sdam_hardsevage0,FLDSZCD.y,STDAGE.2)
hardwood_post<-count(sdam_hardsev,FLDSZCD.y,STDAGE.2)
hardwood_pre<-count(sdam_hardsev,FLDSZCD.x,STDAGE.1)

sdam_hardsev$age_cat.1<-ifelse(sdam_hardsev$STDAGE.1==0,"Zero","Non-zero")
sdam_hardsev$age_cat.2<-ifelse(sdam_hardsev$STDAGE.2==0,"Zero","Non-zero")
sdam_hardsev$stocked.1<-ifelse(sdam_hardsev$FLDSZCD.x>0,"Stocked","Non-stocked")
sdam_hardsev$stocked.2<-ifelse(sdam_hardsev$FLDSZCD.y>0,"Stocked","Non-stocked")
hardsev0<-count(sdam_hardsev,age_cat.1,age_cat.2,stocked.1,stocked.2)

sdam_consev$age_cat.1<-ifelse(sdam_consev$STDAGE.1==0,"Zero","Non-zero")
sdam_consev$age_cat.2<-ifelse(sdam_consev$STDAGE.2==0,"Zero","Non-zero")
sdam_consev$stocked.1<-ifelse(sdam_consev$FLDSZCD.x>0,"Stocked","Non-stocked")
sdam_consev$stocked.2<-ifelse(sdam_consev$FLDSZCD.y>0,"Stocked","Non-stocked")
consev0<-count(sdam_consev,age_cat.1,age_cat.2,stocked.1,stocked.2)

write.csv(hardsev0,"hardsev_table_age_stocking.csv")
write.csv(consev0,"consev_table_age_stocking.csv")



conifer_age0_prepost<-count(sdam_consevage0,FLDSZCD.x,FLDSZCD.y,STDAGE.1,STDAGE.2,)
hardwood_age0_prepost<-count(sdam_hardsevage0,FLDSZCD.x,FLDSZCD.y,STDAGE.1,STDAGE.2,)

write.csv(conifer_age0_prepost,"conifer_age0_prepost.csv")
write.csv(hardwood_age0_prepost,"hardwood_age0_prepost.csv")



write.csv(conifer_age0pre,"conifer_age0pre.csv")
write.csv(conifer_age0post,"conifer_age0post.csv")
write.csv(conifer_post,"conifer_post.csv")
write.csv(conifer_pre,"conifer_pre.csv")


write.csv(hardwood_age0pre,"hardwood_age0pre.csv")
write.csv(hardwood_age0post,"hardwood_age0post.csv")
write.csv(hardwood_post,"hardwood_post.csv")
write.csv(hardwood_pre,"hardwood_pre.csv")



sdam_consev$diff_yearsn<-round(sdam_consev$diff_months/12,digits=0)
sdam_hardsev$diff_yearsn<-round(sdam_hardsev$diff_months/12,digits=0)

sdam_consev$diff_bin<-ifelse(sdam_consev$diff_yearsn<=2,2,
                             ifelse(sdam_consev$diff_yearsn<=4,4,
                                    ifelse(sdam_consev$diff_yearsn<=6,6,
                                           ifelse(sdam_consev$diff_yearsn<=8,8,
                                                  ifelse(sdam_consev$diff_yearsn<=10,10,0)
                                                  )
                                    )))

sdam_hardsev$diff_bin<-ifelse(sdam_hardsev$diff_yearsn<=2,2,
                             ifelse(sdam_hardsev$diff_yearsn<=4,4,
                                    ifelse(sdam_hardsev$diff_yearsn<=6,6,
                                           ifelse(sdam_hardsev$diff_yearsn<=8,8,
                                                  ifelse(sdam_hardsev$diff_yearsn<=10,10,0)
                                           )
                                    )))

con101<-data.frame(count(sdam_consev, diff_bin, age_cat.2))

con102<-data.frame(count(sdam_consev, diff_bin))

con103<-merge(con101,con102,by="diff_bin")

con103$per<-con103$n.x/con103$n.y *100

con103a<-con103[which(con103$age_cat.2=="Non-zero"),]
con103b<-con103[which(con103$age_cat.2=="Zero"),]

con104<-merge(con103a,con103b,by="diff_bin",all=TRUE)

con104$per.x[is.na(con104$per.x)]<-0
con104$per.y[is.na(con104$per.y)]<-0

con104$age_cat.2.y[is.na(con104$age_cat.2.y)]<-"Zero"
con104$age_cat.2.x[is.na(con104$age_cat.2.x)]<-"Non-zero"


write.csv(con104,"percent_by_year_con.csv")



bin_count1 <-count(sdam_consev,diff_bin)

p37<-ggplot(sdam_consev,aes(x=diff_bin,group=age_cat.2,fill=age_cat.2))+
geom_histogram(position="dodge",binwidth=1) +
scale_x_continuous(breaks = c(seq(2,10,2)),labels=c(" {0 - 2}","{2 - 4}","{4 - 6}",
                                                  "{6 - 8}","{8 - 10}"))+
scale_fill_manual(labels=c(" plots with non-zero age"," plots with zero age"),
                    values=c("gray60","black"))+
scale_y_continuous(limit=c(0,50),breaks=c(seq(0,60,10)),name = "number of plots") +
theme_bw()+
theme(legend.position = c(0.7,0.85))+
xlab("")+
ylab("% of plots")+
ggtitle("")+
theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 17),
        axis.title.x=element_text(size=17),
        legend.title=element_text(size=1),
        legend.text = element_text(size=13),
        # legend.margin=unit(0,"cm"),
        legend.background = element_rect(fill = NULL))




# p95<-ggplot()+
#   geom_line(data=con104,aes(x=diff_bin, y=per.y,color="black"),linetype="solid",lwd=0.8)+
#   scale_x_continuous(breaks = c(seq(2,10,2)),labels=c(" {0 - 2}","{2 - 4}","{4 - 6}",
#                                                     "{6 - 8}","{8 - 10}"))+
#   geom_histogram(data=sdam_consev,aes(diff_bin,fill="gray60"),binwidth=1,alpha=0.55)+
#   geom_text(data=bin_count1, aes(diff_bin, y=n+2,label=n), color="black", check_overlap = TRUE)+
#   
#   scale_color_manual(values = "black",label="percentage of plots with zero age")+ 
#   scale_fill_manual(labels=c(" total number of plots"),
#                     values=c("gray60","black"))+
#   
#   scale_y_continuous(limit=c(0,83),name = "percentage of plots with zero age",
#   ) +
# 
#   theme_bw()+
#   theme(legend.position = c(0.7,0.88))+
#   xlab("")+
#   ylab("% of plots")+
#   ggtitle("")+
#   
#   theme(axis.text.x = element_text(size=14),
#         axis.text.y = element_text(size=14),
#         plot.title = element_text(size = 16, hjust = 0.5),
#         axis.title.y = element_text(size = 15),
#         axis.title.x=element_text(size=17),
#         legend.title=element_text(size=1),
#         legend.text = element_text(size=13),
#         legend.margin=unit(0,"cm"),
#         legend.background = element_rect(fill = NULL))  
# 


# 
# 
# p97<-ggplot()+
#   geom_line(data=con104,aes(x=diff_bin, y=per.y),linetype="solid",lwd=0.8)+
# 
#   scale_x_continuous(breaks = c(seq(2,10,2)),labels=c(" {0 - 2}","{2 - 4}","{4 - 6}",
#                                                       "{6 - 8}","{8 - 10}"))+
# 
#   geom_histogram(data=sdam_consev,aes(diff_bin,fill=age_cat.2),binwidth=1,alpha=0.55)+
#   scale_fill_manual(name=NULL,labels=c("plots with non-zero age",
#                                                        "plots with zero age"),
#                     values=c("gray60","black"))+
#   scale_y_continuous(limit=c(0,83),name = "frequency of plots", 
#                      sec.axis = sec_axis(~.*1, name = "percent of age zero plots" 
#                      )) + 
#   theme_bw()+
#   theme(legend.position = c(0.75,0.88))+
# 
#   xlab("time since fire (year)")+
#   ylab("% of plots")+
#   ggtitle("")+
# 
# theme(axis.text.x = element_text(size=14),
#       axis.text.y = element_text(size=14),
#       plot.title = element_text(size = 16, hjust = 0.5),
#       axis.title.y = element_text(size = 15),
#       axis.title.x=element_text(size=17),
#       legend.title=element_text(size=1),
#       legend.text = element_text(size=13),
#       legend.background = element_rect(fill = NULL))  
# 



#   
# ggplot() +
#   geom_line(data=con104,aes(x=diff_yearsn, y=per.x,color=age_cat.2.x),linetype="solid",lwd=0.8)+
# geom_line(data=con104,aes(x=diff_yearsn, y=per.y,color=age_cat.2.y),linetype="dashed",lwd=0.8)+
#    # scale_color_manual(name="",values=c("Non-zero"="black","Zero"="black"),
#    #                    labels=c("Non-zero"="Stand age > 0","Zero"="Stand age = 0"))+
#   scale_linetype_manual("Variabler",values=c("Non-zero"=1,"Zero"=2))+
# 
#   scale_x_continuous(breaks = c(seq(1,10,1)))+
#   theme_bw()+
#   theme(legend.position = c(0.65, 0.87))+
#   xlab("time since fire (year)")+
#   ylab("percentage of plots with severe fire")+
#   ggtitle("")

 
  
hard101<-data.frame(count(sdam_hardsev, diff_bin, age_cat.2))
hard102<-data.frame(count(sdam_hardsev, diff_bin))
hard103<-merge(hard101,hard102,by="diff_bin")
hard103$per<-hard103$n.x/hard103$n.y *100
hard103a<-hard103[which(hard103$age_cat.2=="Non-zero"),]
hard103b<-hard103[which(hard103$age_cat.2=="Zero"),]
hard104<-merge(hard103a,hard103b,by="diff_bin",all=TRUE)

write.csv(hard104,"percent_by_year_hard.csv")

hard104$per.x[is.na(hard104$per.x)]<-0
hard104$per.y[is.na(hard104$per.y)]<-0
hard104$age_cat.2.y[is.na(hard104$age_cat.2.y)]<-"Zero"
hard104$age_cat.2.x[is.na(hard104$age_cat.2.x)]<-"Non-zero"

write.csv(hard104,"percent_by_year_hard.csv")

bin_count <-count(sdam_hardsev,diff_bin)

p38<-ggplot(sdam_hardsev,aes(x=diff_bin,group=age_cat.2,fill=age_cat.2))+
  geom_histogram(position="dodge",binwidth=1) +
  scale_x_continuous(breaks = c(seq(2,10,2)),labels=c(" {0 - 2}","{2 - 4}","{4 - 6}",
                                                      "{6 - 8}","{8 - 10}"))+
  scale_fill_manual(labels=c(" plots with non-zero age"," plots with zero age"),
                    values=c("gray60","black"))+
  scale_y_continuous(limit=c(0,50),breaks=c(seq(0,60,10)),name = "") +
  theme_bw()+
  theme(legend.position = c(0.7,0.85))+
  xlab("")+
  ylab("% of plots")+
  ggtitle("")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 17),
        axis.title.x=element_text(size=17),
        legend.title=element_text(size=1),
        legend.text = element_text(size=13),
        # legend.margin=unit(0,"cm"),
        legend.background = element_rect(fill = NULL))  


# p93<-ggplot()+
#   geom_line(data=hard104,aes(x=diff_bin, y=per.y,color="black"),linetype="solid",lwd=0.8)+
#   scale_x_continuous(breaks = c(seq(2,10,2)),labels=c(" {0 - 2}","{2 - 4}","{4 - 6}",
#                                                       "{6 - 8}","{8 - 10}"))+
#   geom_histogram(data=sdam_hardsev,aes(diff_bin,fill="gray60"),binwidth=1,alpha=0.55)+
#   geom_text(data=bin_count, aes(diff_bin, y=n+2,label=n), color="black", check_overlap = TRUE)+
#   scale_color_manual(values = "black",label="percentage of plots with zero age")+ 
#   scale_fill_manual(labels=c(" total number of plots"),
#                     values=c("gray60","black"))+
#   scale_y_continuous(limit=c(0,83),name = "",theme_bw())+
#   theme(legend.position = c(0.7,0.87))+
#   xlab("")+
#   ylab("")+
#   ggtitle("")+
# theme(axis.text.x = element_text(size=14),
#       axis.text.y = element_text(size=14),
#       plot.title = element_text(size = 16, hjust = 0.5),
#       axis.title.y = element_text(size = 15),
#       axis.title.x=element_text(size=17),
#       legend.title=element_text(size=1),
#       legend.text = element_text(size=13),
#       legend.margin=unit(0,"cm"),
#       legend.background = element_rect(fill = NULL))  
# 
# 
# p88<-ggplot() +
#   geom_line(data=hard104,aes(x=diff_yearsn, y=per.y,color=age_cat.2.y),linetype="solid",lwd=0.8)+
#   scale_color_manual(name="",values=c("Non-zero"="black","Zero"="black"),
#                      labels=c("Non-zero"="Stand age > 0","Zero"="Stand age = 0"))+
#   scale_x_continuous(breaks = c(seq(1,10,1)))+
#   scale_y_continuous(limit = c(0,100))+
#   
#   theme_bw()+
#   # theme(legend.position = c(0.65, 0.89))+
#   
#   
#   xlab("time since fire (year)")+
#   ylab("")+
#   ggtitle("")


# ggplot() +
#   geom_line(data=hard104,aes(x=diff_yearsn, y=per.x,color=age_cat.2.x),linetype="solid",lwd=0.8)+
#   geom_line(data=hard104,aes(x=diff_yearsn, y=per.y,color=age_cat.2.y),linetype="dashed",lwd=0.8)+
#   scale_color_manual(name="",values=c("Non-zero"="black","Zero"="black"),
#                      labels=c("Non-zero"="Stand age > 0","Zero"="Stand age = 0"))+
#   scale_x_continuous(breaks = c(seq(1,10,1)))+
#   theme_bw()+
#   theme(legend.position = c(0.65, 0.88))+
#   
#   
#   xlab("time since fire (year)")+
#   ylab("percentage of plots with severe fire")+
#   ggtitle("")



## create panel of figures

current_date<-Sys.Date()
png(paste0("mtbs_fig_by_foresttype_",current_date,".jpeg"), width = 1000, height = 1100)
par(xpd = F, mar = c(8,8,6,0.5))
par(mfrow=c(5,2))

library(gtable)

tg1 <- textGrob('Conifer', gp = gpar(fontsize = 15, fontface = 'bold'))
tg2 <- textGrob('Hardwood', gp = gpar(fontsize = 15, fontface = 'bold'))


library(grid)
grid.arrange(tg1,
             tg2,
             p31 + theme(legend.position="none"), 
             p32 + theme(legend.position="none"),
             p37,
             p38,
             p33 + theme(legend.position="none"), 
             p34+  theme(legend.position="none"),
             p35 + theme(legend.position="none"),
             p36 + theme(legend.position="none"),
             heights=c(0.1,1.1, 1.1, 1.1,1.1),
             nrow = 5)

dev.off()



pqpq<-cbind(ppp,pppp)

write.csv(pqpq,"zero_seedling_plots.csv")

