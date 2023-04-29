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

#read data with disturbance
mtbs_plots<-read.csv("plots_with_MTBS.csv")
  
mtbs_plots<-read.csv("plots_data_with_only_mtbs_seedling_f_7_14.csv")
  
  
library(operators)
library(tidyr)
 mtbs_plots3<-separate(mtbs_plots,fire_id_mtbs, into = c("id_1", "fire_yr",
                                                         "fire_mn","fire_day"),
                       sep =c(13,17,19,21), remove = FALSE)

plots<-read.csv("../../data/PLOT.CSV")

 library(tidyverse)

 plots_all<-plots[c("INVYR","STATECD","UNITCD","COUNTYCD",
                    "PLOT","MEASMON","MEASDAY")]

 plots_all$pplotid<-paste0(plots_all$STATECD,"-",plots_all$UNITCD,"-",
                           plots_all$COUNTYCD,"-",plots_all$PLOT)
 plots_all$pplotidyr<-paste0(plots_all$STATECD,"-",plots_all$UNITCD,"-",
                             plots_all$COUNTYCD,"-",plots_all$PLOT,"-",plots_all$INVYR)


 sdam11<-merge(mtbs_plots3,plots_all,by.x="NUNID.2",by.y="pplotidyr",all.x=TRUE)

#  write.csv(sdam11,"mtbs_plots_FIA_9_26.csv")
# sdam11<-read.csv("mtbs_plots_FIA_9_26.csv")
 # sdam11<-mtbs_plots3

sdam11$diff_yr<-sdam11$MEASYEAR.2.x-as.numeric(sdam11$fire_yr)
sdam11$diff_mn<-sdam11$MEASMON.2.x-as.numeric(sdam11$fire_mn)
# sdam11$diff_dy<-sdam11$MEASDAY-as.numeric(sdam11$fire_day)
sdam11$diff_months<-round(sdam11$diff_yr*12 + sdam11$diff_mn,digits=0)
# sdam11$diff_months<-round(sdam11$diff_days/30.41,digits=2)
sdam11$diff_years<-round(sdam11$diff_months/12,digits=2)

sdam12<-sdam11[which(sdam11$diff_years>0),]
sdam12<-sdam12[which(sdam12$diff_years<11),]

# sdam12$AGB.11<-(sdam12$AGB.1*453.6*2.471)/1000000
# sdam12$AGB.22<-(sdam12$AGB.2*453.6*2.471)/1000000
# sdam12$BALIVE.11<-(sdam12$BALIVE.1*30.48*30.48*2.471)/10000
# sdam12$BALIVE.22<-(sdam12$BALIVE.2*30.48*30.48*2.471)/10000

sdam12$AGB_CHP<-sdam12$AGB.22.x-sdam12$AGB.11.x


# seedling_data<-readRDS("all_seedlings_pre_post.RDS")
# 
# seedling_all<-read.csv("../data/SEEDLING.CSV")
# 
# 
# seedling_all$nunid<-paste0(seedling_all$STATECD,"-",seedling_all$UNITCD,"-",
#                            seedling_all$COUNTYCD,"-", seedling_all$PLOT,
#                            "-",seedling_all$INVYR)
# 
# 
# 
# fplot1<-aggregate(TREECOUNT~nunid, seedling_all, FUN=sum)
# fplot3<-count(seedling_all,nunid,SPCD)
# fplot4<-count(fplot3,nunid)
# 
# fdam1<-merge(fplot1,fplot4,by="nunid")
# 
# seedling_all$con<-ifelse(seedling_all$SPGRPCD<25|seedling_all$SPGRPCD==51|
#                            seedling_all$SPGRPCD==52,1,0)
# 
# seedling12b<-seedling_all[which(seedling_all$con==1),]
# 
# fplot7<-aggregate(TREECOUNT~nunid, seedling12b, FUN=sum)
# 
# fplot9b<-count(seedling12b,nunid,SPCD)
# fplot9<-count(fplot9b,nunid)
# 
# 
# fdam4<-merge(fdam1,fplot7,by="nunid",all.x=TRUE)
# 
# fdam6<-merge(fdam4,fplot9,by="nunid",all.x=TRUE)
# 
# 
# colnames(fdam6)<-c("nunid","seed_count","sd_spp_rich",
#                    "seed_count_con","sd_spp_rich_con")
# 
# fdam6$nunid_cp<-fdam6$nunid
# fdam6$seed_count_con[is.na(fdam6$seed_count_con)] <- 0
# fdam6$sd_spp_rich_con[is.na(fdam6$sd_spp_rich_con)] <- 0
# 
# 
# #
# # seedling_data$nunid.1<-paste0(seedling_data$NUNIDS,"-",seedling_data$INVYR.x)
# # seedling_data$nunid.2<-paste0(seedling_data$NUNIDS,"-",seedling_data$INVYR.y)
# 
# 
# mtbs_seed_n<-merge(sdam12,fdam6,by.x="NUNID.1",by.y="nunid",all.x=TRUE)
# mtbs_seed_n2<-merge(mtbs_seed_n,fdam6,by.x="NUNID.2",by.y="nunid",all.x=TRUE)
# 
# mtbs_seed_n2<-mtbs_seed_n2[-1]
# 
# colnames(mtbs_seed_n2)[59]<-"seed_count.1"
# colnames(mtbs_seed_n2)[60]<-"sd_spp_rich.1"
# colnames(mtbs_seed_n2)[61]<-"seed_count_con.1"
# colnames(mtbs_seed_n2)[62]<-"sd_spp_rich_con.1"
# colnames(mtbs_seed_n2)[64]<-"seed_count.2"
# colnames(mtbs_seed_n2)[65]<-"sd_spp_rich.2"
# colnames(mtbs_seed_n2)[66]<-"seed_count_con.2"
# colnames(mtbs_seed_n2)[67]<-"sd_spp_rich_con.2"
# 
# 
# 
# # na_first<-mtbs_seed_n2[is.na(mtbs_seed_n2$seed_count.1),]
# # na_second<-mtbs_seed_n2[is.na(mtbs_seed_n2$seed_count.2),]
# mtbs_seed<-mtbs_seed_n2[!is.na(mtbs_seed_n2$seed_count.2) |
#                                       !is.na (mtbs_seed_n2$seed_count.1),]
# 
# # mtbs_seed<-merge(sdam12,seedling_data,by.x="NUNID.1",by.y="nunid.1",all.x=TRUE)
# 
# mtbs_seed$seed_count.1[is.na(mtbs_seed$seed_count.1)] <- 0
# mtbs_seed$seed_count.2[is.na(mtbs_seed$seed_count.2)] <- 0
# mtbs_seed$sd_spp_rich.1[is.na(mtbs_seed$sd_spp_rich.1)] <- 0
# mtbs_seed$sd_spp_rich.2[is.na(mtbs_seed$sd_spp_rich.2)] <- 0
# 
# mtbs_seed$seed_count_con.1[is.na(mtbs_seed$seed_count_con.1)] <- 0
# mtbs_seed$seed_count_con.2[is.na(mtbs_seed$seed_count_con.2)] <- 0
# mtbs_seed$sd_spp_rich_con.1[is.na(mtbs_seed$sd_spp_rich_con.1)] <- 0
# mtbs_seed$sd_spp_rich_con.2[is.na(mtbs_seed$sd_spp_rich_con.2)] <- 0
# 
# mtbs_seed$seed_count.1<-mtbs_seed$seed_count.1*74.96*2.471
# mtbs_seed$seed_count.2<-mtbs_seed$seed_count.2*74.96*2.471
# 
# mtbs_seed$seed_count_con.1<-mtbs_seed$seed_count_con.1*74.96*2.471
# mtbs_seed$seed_count_con.2<-mtbs_seed$seed_count_con.2*74.96*2.471
# 
# 
# mtbs_seed$seed_ct_ch<-mtbs_seed$seed_count.2-mtbs_seed$seed_count.1
# mtbs_seed$sdsp_rh_ch<-mtbs_seed$sd_spp_rich.2-mtbs_seed$sd_spp_rich.1
# 
# mtbs_seed$seed_ct_ch_con<-mtbs_seed$seed_count_con.2-mtbs_seed$seed_count_con.1
# mtbs_seed$sdsp_rh_ch_con<-mtbs_seed$sd_spp_rich_con.2-mtbs_seed$sd_spp_rich_con.1
# 
# # mtbs_seed$AGB_CHP<-mtbs_seed$AGB.2-mtbs_seed$AGB_CHP
# 
# 
# mtbs_seed<-mtbs_seed[which(mtbs_seed$seed_count.2<50000),]
# 
# 
# write.csv(mtbs_seed,"mtbs_data_with_seedling.csv")

# mtbs_seed<-read.csv("mtbs_data_with_seedling.csv")
library(ggplot2)
ggplot(data=sdam12) +
  geom_jitter(aes(x=diff_years, y=STDAGE.2.x,color=as.factor(mtbs_sev))) +
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



ggplot(data=sdam12) +
  geom_jitter(aes(x=diff_years, y=AGB.22.x,color=as.factor(mtbs_sev))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
  # scale_y_continuous(limits=c(0,15))+
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



ggplot(data=sdam12) +
  geom_jitter(aes(x=diff_years, y=seed_count.2.x,color=as.factor(mtbs_sev))) +
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


ggplot(data=sdam12) +
  geom_jitter(aes(x=diff_months, y=STDAGE.2.x,color=as.factor(mtbs_sev))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
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

dam1<-sdam12

# edit ecoregion data by splitting
dam3<-separate(dam1, ECOSUBCD.1.x, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)

# use function to convert forest type to groups
source("../data/FTGC.R")
dam3$FOR_GRP<-FTGC(dam3$FORTYPCD.1.x)

# read file with ecoregion code from the west
ecosel<-read.csv("../data/eco_select.csv")

dam3$ecocode <- trimws(dam3$Spl_1, which = c("left"))

library(operators)
dam33a<-dam3[(dam3$ecocode %in% ecosel$econew),]

dam33b<-dam3[(dam3$ecocode %!in% ecosel$econew),]

library(tidyverse)
east1<-count(dam33b,mtbs_sev,fia_sev.x)

# dam33c<-separate(dam33b, NUNIDS.2, 
#                  into = c("st","cty","unt","pl"), remove = FALSE)
# dam33d<-separate(dam33a, NUNIDS.2, 
#                 into = c("st","cty","unt","pl"), remove = FALSE)

# analysis for Western only
dam_west<-dam33a

ggplot(data=dam_west) +
  geom_jitter(aes(x=diff_years, y=STDAGE.2.x,color=as.factor(mtbs_sev))) +
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
  geom_jitter(aes(x=diff_years, y=STDAGE.2.x,color=as.factor(mtbs_sev))) +
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
  geom_jitter(aes(x=diff_years, y=STDAGE.2.x,color=as.factor(mtbs_sev))) +
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

table_east<-count(dam_east,mtbs_sev,fia_sev.x)

ggplot(data=dam_east) +
  geom_jitter(aes(x=diff_years, y=STDAGE.2.x,color=as.factor(mtbs_sev))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
  scale_y_continuous(limits=c(0,150))+
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


dam_westsev<-dam_west[which(dam_west$mtbs_sev==1),]

dam_westmild<-dam_west[which(dam_west$mtbs_sev==0),]


ggplot(data=dam_westsev) +
  geom_jitter(aes(x=diff_months, y=STDAGE.2.x,color=as.factor(Spl_1))) +
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
  geom_jitter(aes(x=diff_months, y=STDAGE.2.x,color=as.factor(Spl_1))) +
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

sdam_sev<-sdam4[which(sdam4$mtbs_sev1==1),]

sdam_mild<-sdam4[which(sdam4$mtbs_sev1==0),]

plot(sdam_sev$AGB.22.y)

sdam_sev<-sdam_sev[which(sdam_sev$AGB.22.x<50),]
# sdam_sev<-sdam_sev[which(sdam_sev$seed_count.2<50000),]


sdam44<-rbind(sdam_sev,sdam_mild)

sdam44$age<-ifelse(sdam44$STDAGE.2.x>0,1,0)

sdam_con<-sdam44[which(sdam44$for_type=="Conifer"),]
sdam_hard<-sdam44[which(sdam44$for_type=="Hardwood"),]

sdam_both<-rbind(sdam_con,sdam_hard)
sdam_both_sev<-sdam_both[which(sdam_both$mtbs_sev1==1),]
sdam_both_mild<-sdam_both[which(sdam_both$mtbs_sev1==0),]

# sdam_both_sev$age<-ifelse(sdam_both_sev$STDAGE.2>0,1,0)

sdam_consev<-sdam_con[which(sdam_con$mtbs_sev1==1),]
sdam_hardsev<-sdam_hard[which(sdam_hard$mtbs_sev1==1),]


ggplot(data=sdam_con) +
  geom_point(aes(x=diff_months, y=AGB.22.x,color=as.factor(mtbs_sev))) +
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
  geom_point(aes(x=diff_months, y=AGB_CHP,color=as.factor(mtbs_sev))) +
  geom_smooth(aes(x=diff_months, y=AGB_CHP,color=as.factor(mtbs_sev)),methods=loess) +
  
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
  # scale_y_continuous(limits=c(-300,100))+
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


write.csv(contab6,"./outputs/summary_conifer_716.csv")
write.csv(hardtab6,"./outputs/summary_hardwood_716.csv")

# plot comparing severe vs mild fire


# plots standage vs time since fire for conifer and hardwood
ggplot(data=sdam_consev) +
  geom_point(aes(x=diff_years, y=STDAGE.2.x,color=as.factor(mtbs_sev))) +
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

ggplot(data=sdam_hardsev) +
  geom_point(aes(x=diff_years, y=STDAGE.2.x,color=as.factor(mtbs_sev))) +
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


ggplot(data=sdam_con) +
  geom_point(aes(x=diff_years, y=AGB.2,color=as.factor(mtbs_sev))) +
  geom_smooth(aes(x=diff_years, y=AGB.2,color=as.factor(mtbs_sev)),method=loess) +
  
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

ggplot(data=sdam_hard) +
  geom_point(aes(x=diff_years, y=AGB.22.x,color=as.factor(mtbs_sev))) +
  geom_smooth(aes(x=diff_years, y=AGB.22.x,color=as.factor(mtbs_sev)),method=loess) +
  
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

ggplot(data=sdam_con) +
  geom_point(aes(x=diff_years, y=AGB_CHP,color=as.factor(mtbs_sev))) +
  geom_smooth(aes(x=diff_years, y=AGB_CHP,color=as.factor(mtbs_sev)),method=loess) +
  
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

ggplot(data=sdam_hard) +
  geom_point(aes(x=diff_years, y=AGB_CHP,color=as.factor(mtbs_sev))) +
  geom_smooth(aes(x=diff_years, y=AGB_CHP,color=as.factor(mtbs_sev)),method=loess) +
  
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


# 


# plots standage vs time since fire for conifer and hardwood
ggplot(data=sdam_consev) +
  geom_point(aes(x=diff_years, y=STDAGE.2.x,color=as.factor(mtbs_sev))) +
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

ggplot(data=sdam_hardsev) +
  geom_point(aes(x=diff_years, y=STDAGE.2.x,color=as.factor(mtbs_sev))) +
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



ggplot(data=sdam_both_sev) +
  geom_point(aes(x=diff_years, y=AGB.22.x,color=as.factor(conifer))) +
  geom_smooth(aes(x=diff_years, y=AGB.22.x,color=as.factor(conifer)),method=loess) +
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


ggplot(data=sdam_both_sev) +
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
  geom_point(aes(x=diff_years, y=seed_count.2.x,color=as.factor(conifer))) +
  geom_smooth(aes(x=diff_years, y=seed_count.2.x,color=as.factor(conifer)),method=loess) +
  
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

# 
# p19<-ggplot(data=sdam_both_sev) +
#   geom_point(aes(x=diff_years, y=seed_ct_ch,color=as.factor(conifer))) +
#   geom_smooth(aes(x=diff_years, y=seed_ct_ch,color=as.factor(conifer)),method=loess) +
#   
#   scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
#                      values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+ 
#   scale_y_continuous(limits=c(-7000,10000))+
#   scale_x_continuous(breaks = c(seq(1,10,1)))+
#   
#   xlab("time since fire (year)")+
#   ylab("change in seed density (ha)")+
#   ggtitle("seedling density change")+
#   
#   
#   theme_bw()+
#   theme(axis.text.x = element_text(size=14),
#         axis.text.y = element_text(size=14),
#         plot.title = element_text(size = 16, hjust = 0.5),
#         axis.title.y = element_text(size = 15),
#         axis.title.x=element_text(size=15),
#         legend.title=element_text(size=14),
#         legend.text = element_text(size=13))
# 
# 
# 



library(tidyverse)

sdam_hardsev$age_cat.1<-ifelse(sdam_hardsev$STDAGE.1.x==0,"Zero","Non-zero")
sdam_hardsev$age_cat.2<-ifelse(sdam_hardsev$STDAGE.2.x==0,"Zero","Non-zero")
sdam_hardsev$stocked.1<-ifelse(sdam_hardsev$FLDSZCD.1.x>0,"Stocked","Non-stocked")
sdam_hardsev$stocked.2<-ifelse(sdam_hardsev$FLDSZCD.2>0,"Stocked","Non-stocked")
hardsev0<-count(sdam_hardsev,age_cat.1,age_cat.2,stocked.1,stocked.2)

sdam_consev$age_cat.1<-ifelse(sdam_consev$STDAGE.1.x==0,"Zero","Non-zero")
sdam_consev$age_cat.2<-ifelse(sdam_consev$STDAGE.2.x==0,"Zero","Non-zero")
sdam_consev$stocked.1<-ifelse(sdam_consev$FLDSZCD.1.x>0,"Stocked","Non-stocked")
sdam_consev$stocked.2<-ifelse(sdam_consev$FLDSZCD.2>0,"Stocked","Non-stocked")
consev0<-count(sdam_consev,age_cat.1,age_cat.2,stocked.1,stocked.2)


write.csv(consev0,"con_sev_change_all_data.csv")
write.csv(hardsev0,"hard_sev_change_all_data.csv")


### figures used in the manuscript

# plots standage vs time since fire for conifer and hardwood
p31<-ggplot(data=sdam_consev) +
  geom_point(aes(x=diff_years, y=STDAGE.2.x)) +
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
  geom_point(aes(x=diff_years, y=STDAGE.2.x)) +
  
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
  geom_text(x=3, y=10, label="Stand age increasing: 85 % of plots",size=4.7)+
  
  geom_text(x=3, y=9, label="Stand age 0 persistent: 15 % of plots",size=4.7)
# sdam_consev$age<-ifelse(sdam_consev$STDAGE.2>0,1,0)
# sdam_hardsev$age<-ifelse(sdam_hardsev$STDAGE.2>0,1,0)



p33<-ggplot(data=sdam_consev) +
  geom_point(aes(x=diff_years, y=AGB_CHP)) +
  geom_smooth(aes(x=diff_years, y=AGB_CHP),color="black",method=loess) +
  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+
  
  scale_y_continuous(limits=c(-400,90))+
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
  scale_y_continuous(limits=c(-400,90))+
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
  geom_point(aes(x=diff_years, y=seed_ct_ch.x)) +
  geom_smooth(aes(x=diff_years, y=seed_ct_ch.x,color="black"),method=loess) +

  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"),
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+
  scale_y_continuous(limits=c(-15000,15000))+
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
  geom_point(aes(x=diff_years, y=seed_ct_ch.x)) +
  geom_smooth(aes(x=diff_years, y=seed_ct_ch.x,color="black"),method=loess) +

  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"),
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+
  scale_y_continuous(limits=c(-15000,15000))+
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





sdam_consevage0<-sdam_consev[which(sdam_consev$STDAGE.2.x==0),]
table(sdam_consevage0$FLDSZCD.2,sdam_consevage0$STDAGE.2.x)


sdam_hardsevage0<-sdam_hardsev[which(sdam_hardsev$STDAGE.2.x==0),]
table(sdam_hardsevage0$FLDSZCD.2,sdam_hardsevage0$STDAGE.2.x)



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
scale_y_continuous(limit=c(0,37),breaks=c(seq(0,60,10)),name = "number of plots") +
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
  scale_y_continuous(limit=c(0,37),breaks=c(seq(0,60,10)),name = "") +
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

## create panel of figures

current_date<-Sys.Date()
png(paste0("mtbs_fig_all_data_",current_date,".jpeg"), width = 1000, height = 1100)
par(xpd = F, mar = c(8,8,6,0.5))
par(mfrow=c(5,2))

library(gtable)

tg1 <- textGrob('Conifer', gp = gpar(fontsize = 15, fontface = 'bold'))
tg2 <- textGrob('Hardwood', gp = gpar(fontsize = 15, fontface = 'bold'))


library(grid)
library(gridExtra)
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
# 
# 
# 
# pqpq<-cbind(ppp,pppp)
# 
# write.csv(pqpq,"zero_seedling_plots.csv")

##############################################
### repeat the codes for seedling data (smaller data where seedling 
### data is available)

rm(list = setdiff(ls(), c('p31','p32','p33','p34','p37','p38')))

dam1<-read.csv("mtbs_data_with_seedling.csv")

# edit ecoregion data by splitting
dam3<-separate(dam1, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)

# use function to convert forest type to groups
source("../data/FTGC.R")
dam3$FOR_GRP<-FTGC(dam3$FORTYPCD.1)

# read file with ecoregion code from the west
ecosel<-read.csv("../data/eco_select.csv")

dam3$ecocode <- trimws(dam3$Spl_1, which = c("left"))

library(operators)
dam33a<-dam3[(dam3$ecocode %in% ecosel$econew),]

dam33b<-dam3[(dam3$ecocode %!in% ecosel$econew),]

east1<-count(dam33b,mtbs_sev,fia_sev)

dam_west<-dam33a

# analysis for Eastern only
dam_east<-dam33b


table_east<-count(dam_east,mtbs_sev,fia_sev)


dam_westsev<-dam_west[which(dam_west$mtbs_sev==1),]

dam_westmild<-dam_west[which(dam_west$mtbs_sev==0),]


sdam4<-dam_west

sdam4$conifer<-ifelse(sdam4$FOR_GRP<400,"Conifer","Hardwood")


sdam4$for_type<-ifelse(sdam4$FOR_GRP<400,"Conifer",
                       ifelse(sdam4$FOR_GRP>=400 & sdam4$FOR_GRP<999,"Hardwood",
                              ifelse(sdam4$FOR_GRP==999,"Non-stocked","None"
                              )))

sdam_sev<-sdam4[which(sdam4$mtbs_sev==1),]

sdam_mild<-sdam4[which(sdam4$mtbs_sev==0),]

plot(sdam_sev$AGB.2)
plot(sdam_sev$seed_count.1)

sdam_sev<-sdam_sev[which(sdam_sev$AGB.2<50),]
sdam_sev<-sdam_sev[which(sdam_sev$seed_count.2<50000),]


sdam44<-rbind(sdam_sev,sdam_mild)

sdam44$age<-ifelse(sdam44$STDAGE.2>0,1,0)

sdam_con<-sdam44[which(sdam44$for_type=="Conifer"),]
sdam_hard<-sdam44[which(sdam44$for_type=="Hardwood"),]

sdam_both<-rbind(sdam_con,sdam_hard)
sdam_both_sev<-sdam_both[which(sdam_both$mtbs_sev==1),]
sdam_both_mild<-sdam_both[which(sdam_both$mtbs_sev==0),]

# sdam_both_sev$age<-ifelse(sdam_both_sev$STDAGE.2>0,1,0)

sdam_consev<-sdam_con[which(sdam_con$mtbs_sev==1),]
sdam_hardsev<-sdam_hard[which(sdam_hard$mtbs_sev==1),]


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


write.csv(contab6,"outputs/summary_conifer_with_seedling_data.csv")
write.csv(hardtab6,"outputs/summary_hardwood_with_seedling_data.csv")

# plot comparing severe vs mild fire

# sdam_hardsev$age<-ifelse(sdam_hardsev$STDAGE.2>0,1,0)


sdam_hardsev$age_cat.1<-ifelse(sdam_hardsev$STDAGE.1==0,"Zero","Non-zero")
sdam_hardsev$age_cat.2<-ifelse(sdam_hardsev$STDAGE.2==0,"Zero","Non-zero")
sdam_hardsev$stocked.1<-ifelse(sdam_hardsev$FLDSZCD.1>0,"Stocked","Non-stocked")
sdam_hardsev$stocked.2<-ifelse(sdam_hardsev$FLDSZCD.2>0,"Stocked","Non-stocked")
hardsev0<-count(sdam_hardsev,age_cat.1,age_cat.2,stocked.1,stocked.2)

sdam_consev$age_cat.1<-ifelse(sdam_consev$STDAGE.1==0,"Zero","Non-zero")
sdam_consev$age_cat.2<-ifelse(sdam_consev$STDAGE.2==0,"Zero","Non-zero")
sdam_consev$stocked.1<-ifelse(sdam_consev$FLDSZCD.1>0,"Stocked","Non-stocked")
sdam_consev$stocked.2<-ifelse(sdam_consev$FLDSZCD.2>0,"Stocked","Non-stocked")
consev0<-count(sdam_consev,age_cat.1,age_cat.2,stocked.1,stocked.2)


write.csv(consev0,"conifer_stocking_with_seedling.csv")
write.csv(hardsev0,"hardwood_stocking_with_seedling.csv")


# plots standage vs time since fire for conifer and hardwood
p41<-ggplot(data=sdam_consev) +
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

p42<-ggplot(data=sdam_hardsev) +
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



p43<-ggplot(data=sdam_consev) +
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


p44<-ggplot(data=sdam_hardsev) +
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

p45<-ggplot(data=sdam_consev) +
  geom_point(aes(x=diff_years, y=seed_ct_ch)) +
  geom_smooth(aes(x=diff_years, y=seed_ct_ch,color="black"),method=loess) +
  
  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+ 
  scale_y_continuous(limits=c(-20000,8000))+
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


p46<-ggplot(data=sdam_hardsev) +
  geom_point(aes(x=diff_years, y=seed_ct_ch)) +
  geom_smooth(aes(x=diff_years, y=seed_ct_ch,color="black"),method=loess) +
  
  scale_color_manual(name="forest type",labels=c("Conifer","Hardwood"), 
                     values=c("Conifer"="#009E73","Hardwood"="#D55E00"))+ 
  scale_y_continuous(limits=c(-20000,8000))+
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


write.csv(con104,"percent_by_year_con_seedlingdata.csv")



bin_count1 <-count(sdam_consev,diff_bin)

p47<-ggplot(sdam_consev,aes(x=diff_bin,group=age_cat.2,fill=age_cat.2))+
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




hard101<-data.frame(count(sdam_hardsev, diff_bin, age_cat.2))
hard102<-data.frame(count(sdam_hardsev, diff_bin))
hard103<-merge(hard101,hard102,by="diff_bin")
hard103$per<-hard103$n.x/hard103$n.y *100
hard103a<-hard103[which(hard103$age_cat.2=="Non-zero"),]
hard103b<-hard103[which(hard103$age_cat.2=="Zero"),]
hard104<-merge(hard103a,hard103b,by="diff_bin",all=TRUE)

write.csv(hard104,"percent_by_year_hard_seedlingdata.csv")

hard104$per.x[is.na(hard104$per.x)]<-0
hard104$per.y[is.na(hard104$per.y)]<-0
hard104$age_cat.2.y[is.na(hard104$age_cat.2.y)]<-"Zero"
hard104$age_cat.2.x[is.na(hard104$age_cat.2.x)]<-"Non-zero"

write.csv(hard104,"percent_by_year_hard_seedlingdata.csv")

bin_count <-count(sdam_hardsev,diff_bin)

p48<-ggplot(sdam_hardsev,aes(x=diff_bin,group=age_cat.2,fill=age_cat.2))+
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

## create panel of figures

current_date<-Sys.Date()
png(paste0("mtbs_fig_with_all_and_seedling_data_",current_date,".jpeg"), width = 1000, height = 1100)
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
             p45 + theme(legend.position="none"),
             p46 + theme(legend.position="none"),
             heights=c(0.1,1.1, 1.1, 1.1,1.1),
             nrow = 5)

dev.off()




current_date<-Sys.Date()
png(paste0("mtbs_all_figs_with_seedling_data_",current_date,".jpeg"), width = 1000, height = 1100)
par(xpd = F, mar = c(8,8,6,0.5))
par(mfrow=c(5,2))

library(gtable)

tg1 <- textGrob('Conifer', gp = gpar(fontsize = 15, fontface = 'bold'))
tg2 <- textGrob('Hardwood', gp = gpar(fontsize = 15, fontface = 'bold'))


library(grid)
grid.arrange(tg1,
             tg2,
             p41 + theme(legend.position="none"), 
             p42 + theme(legend.position="none"),
             p47,
             p48,
             p43 + theme(legend.position="none"), 
             p44+  theme(legend.position="none"),
             p45 + theme(legend.position="none"),
             p46 + theme(legend.position="none"),
             heights=c(0.1,1.1, 1.1, 1.1,1.1),
             nrow = 5)

dev.off()


