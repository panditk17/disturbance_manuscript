### plots based on disturbances from FIA data

# setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000)

plots_all2<-read.csv("all_plots_data_with_seedling_8_27.csv")
# remove.packages("tidyverse")
# install.packages("tidyverse")
library(tidyverse)
ddd<-count(plots_all2,dist_type)


plots_all3<-plots_all2[which(plots_all2$dist_type=="FIRE"|
                               plots_all2$dist_type=="CUT" |
                               plots_all2$dist_type=="INSDIS"),]

ddd2<-count(plots_all3,dist_type)

# plots_all4<-plots_all3[which(plots_all3$rep_std_fia2==1),]
sdam3<-plots_all3


sdam3<-separate(sdam3, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)


sdam2<-separate(sdam3, NUNIDS.2, 
                 into = c("st","cty","unt","pl"), remove = FALSE)

ecosel<-read.csv("../disturbance/eco_select.csv")

sdam2$ecocode <- trimws(sdam2$Spl_1, which = c("left"))

source("../data/FTGC.R")
sdam2$FOR_GRP<-FTGC(sdam2$FORTYPCD.1)

plot(sdam2$AGB.22)


# sdam2$AGB.1<-(sdam2$AGB.1*453.6*2.471)/1000000
# sdam2$AGB.2<-(sdam2$AGB.2*453.6*2.471)/1000000

plot(sdam2$AGB.11)



sdam5b<-sdam2[(sdam2$ecocode %in% ecosel$econew),]


# 
# sev_std<-sdam2[sdam2$rep_std==1,]
# sev_std2<-sev_std[sev_std$AGB.2<50,]
# sev_std3<-sev_std2[sev_std2$seed_count.2<50000 ,]
# 
# 
# 
# mild_std3<-sdam2[sdam2$rep_std==0,]
# 
# both_sev_data<-rbind(sev_std3,mild_std3)
# 
# write.csv(both_sev_data,"dist_data_both.csv")




# sdam4<-sdam3[which(sdam3$FOR_GRP<400),]


sdam5b$rep_std=sdam5b$rep_std_fia2

pdam16<-sdam5b

## plots with no disturbance

trall_1a_ds<-pdam16[which(pdam16$dist_type=="FIRE" & pdam16$fia_sev1==0),]
trall_1b_ds<-pdam16[which(pdam16$dist_type=="FIRE" & pdam16$fia_sev1==1),]

plot(trall_1b_ds$AGB.22)

aaa2a<-data.frame(trall_1a_ds$AGB.11)
aaa2s<-data.frame(trall_1a_ds$seed_count.1)
aaa2r<-data.frame(trall_1a_ds$sd_spp_rich.1)

aaa2<-cbind(aaa2a,aaa2s,aaa2r)

aaa2$type<-"MF"
aaa2$time<-"first"
colnames(aaa2)<-c("AGBN","seed","seedrich","type","time")


aaa3a<-data.frame(trall_1b_ds$AGB.11)
aaa3s<-data.frame(trall_1b_ds$seed_count.1)
aaa3r<-data.frame(trall_1b_ds$sd_spp_rich.1)


aaa3<-cbind(aaa3a,aaa3s,aaa3r)


aaa3$type<-"SF"
aaa3$time<-"first"

colnames(aaa3)<-c("AGBN","seed","seedrich","type","time")


bbb2a<-data.frame(trall_1a_ds$AGB.22)
bbb2b<-data.frame(trall_1a_ds$seed_count.2)
bbb2c<-data.frame(trall_1a_ds$sd_spp_rich.2)


bbb2<-cbind(bbb2a,bbb2b,bbb2c)

bbb2$type<-"MF"
bbb2$time<-"second"

colnames(bbb2)<-c("AGBN","seed","seedrich","type","time")

bbb3a<-data.frame(trall_1b_ds$AGB.22)
bbb3b<-data.frame(trall_1b_ds$seed_count.2)
bbb3c<-data.frame(trall_1b_ds$sd_spp_rich.2)

bbb3<-cbind(bbb3a,bbb3b,bbb3c)

bbb3$type<-"SF"
bbb3$time<-"second"

colnames(bbb3)<-c("AGBN","seed","seedrich","type","time")

fire_dist<-rbind(aaa2,bbb2,aaa3,bbb3)




trall_1a_dsc<-pdam16[which(pdam16$dist_type=="CUT" & pdam16$fia_sev1==0),]
trall_1b_dsc<-pdam16[which(pdam16$dist_type=="CUT" & pdam16$fia_sev1==1),]


caaa2a<-data.frame(trall_1a_dsc$AGB.11)
caaa2b<-data.frame(trall_1a_dsc$seed_count.1)
caaa2c<-data.frame(trall_1a_dsc$sd_spp_rich.1)

caaa2<-cbind(caaa2a,caaa2b,caaa2c)

caaa2$type<-"MC"
caaa2$time<-"first"
colnames(caaa2)<-c("AGBN","seed","seedrich","type","time")

caaa3a<-data.frame(trall_1b_dsc$AGB.11)
caaa3b<-data.frame(trall_1b_dsc$seed_count.1)
caaa3c<-data.frame(trall_1b_dsc$sd_spp_rich.1)

caaa3<-cbind(caaa3a,caaa3b,caaa3c)
caaa3$type<-"SC"
caaa3$time<-"first"

colnames(caaa3)<-c("AGBN","seed","seedrich","type","time")


cbbb2a<-data.frame(trall_1a_dsc$AGB.22)
cbbb2b<-data.frame(trall_1a_dsc$seed_count.2)
cbbb2c<-data.frame(trall_1a_dsc$sd_spp_rich.2)


cbbb2<-cbind(cbbb2a,cbbb2b,cbbb2c)
cbbb2$type<-"MC"
cbbb2$time<-"second"

colnames(cbbb2)<-c("AGBN","seed","seedrich","type","time")

cbbb3a<-data.frame(trall_1b_dsc$AGB.22)
cbbb3b<-data.frame(trall_1b_dsc$seed_count.2)
cbbb3c<-data.frame(trall_1b_dsc$sd_spp_rich.2)


cbbb3<-cbind(cbbb3a,cbbb3b,cbbb3c)

cbbb3$type<-"SC"
cbbb3$time<-"second"

colnames(cbbb3)<-c("AGBN","seed","seedrich","type","time")





trall_1a_dsi<-pdam16[which(pdam16$dist_type=="INSDIS" & pdam16$fia_sev1==0),]
trall_1b_dsi<-pdam16[which(pdam16$dist_type=="INSDIS" & pdam16$fia_sev1==1),]


iaaa2a<-data.frame(trall_1a_dsi$AGB.11)
iaaa2b<-data.frame(trall_1a_dsi$seed_count.1)
iaaa2c<-data.frame(trall_1a_dsi$sd_spp_rich.1)


iaaa2<-cbind(iaaa2a,iaaa2b,iaaa2c)
iaaa2$type<-"MI"
iaaa2$time<-"first"
colnames(iaaa2)<-c("AGBN","seed","seedrich","type","time")

iaaa3a<-data.frame(trall_1b_dsi$AGB.11)
iaaa3b<-data.frame(trall_1b_dsi$seed_count.1)
iaaa3c<-data.frame(trall_1b_dsi$sd_spp_rich.1)


iaaa3<-cbind(iaaa3a,iaaa3b,iaaa3c)

iaaa3$type<-"SI"
iaaa3$time<-"first"

colnames(iaaa3)<-c("AGBN","seed","seedrich","type","time")


ibbb2a<-data.frame(trall_1a_dsi$AGB.22)
ibbb2b<-data.frame(trall_1a_dsi$seed_count.2)
ibbb2c<-data.frame(trall_1a_dsi$sd_spp_rich.2)


ibbb2<-cbind(ibbb2a,ibbb2b,ibbb2c)

ibbb2$type<-"MI"
ibbb2$time<-"second"

colnames(ibbb2)<-c("AGBN","seed","seedrich","type","time")

ibbb3a<-data.frame(trall_1b_dsi$AGB.22)
ibbb3b<-data.frame(trall_1b_dsi$seed_count.2)
ibbb3c<-data.frame(trall_1b_dsi$sd_spp_rich.2)


ibbb3<-cbind(ibbb3a,ibbb3b,ibbb3c)

ibbb3$type<-"SI"
ibbb3$time<-"second"

colnames(ibbb3)<-c("AGBN","seed","seedrich","type","time")



all_sev_dist<-rbind(aaa3,bbb3,caaa3,cbbb3,iaaa3,ibbb3)

all_mild_dist<-rbind(aaa2,bbb2,caaa2,cbbb2,iaaa2,ibbb2)

write.csv(all_sev_dist,"all_sev_dist_west_827.csv")
write.csv(all_mild_dist,"all_mild_dist_west_827.csv")

all_sev_dist$inter<-interaction(all_sev_dist$type,all_sev_dist$time)

agb_med<-aggregate(AGBN~inter,all_sev_dist,FUN=median)
agb_mean<-aggregate(AGBN~inter,all_sev_dist,FUN=mean)
agb_count<-count(all_sev_dist,inter)


seed_med<-aggregate(seed~inter,all_sev_dist,FUN=median)
seed_mean<-aggregate(seed~inter,all_sev_dist,FUN=mean)


seedrich_med<-aggregate(seedrich~inter,all_sev_dist,FUN=median)
seedrich_mean<-aggregate(seedrich~inter,all_sev_dist,FUN=mean)


sevstat1<-merge(agb_count,agb_med,by="inter")
sevstat2<-merge(sevstat1,agb_mean,by="inter")
sevstat3<-merge(sevstat2,seed_med,by="inter")
sevstat4<-merge(sevstat3,seed_mean,by="inter")
sevstat5<-merge(sevstat4,seedrich_med,by="inter")
sevstat6<-merge(sevstat5,seedrich_mean,by="inter")

colnames(sevstat6)<-c("class","n_west","AGB_med_west","AGB_mean_west",
                      "seed_med_west","seed_mean_west","seedrich_med_west",
                      "seedrich_mean_west")


table_west<-sevstat6


all_mild_dist$inter<-interaction(all_mild_dist$type,all_mild_dist$time)

agb_med_m<-aggregate(AGBN~inter,all_mild_dist,FUN=median)
agb_mean_m<-aggregate(AGBN~inter,all_mild_dist,FUN=mean)
agb_count_m<-count(all_mild_dist,inter)


seed_med_m<-aggregate(seed~inter,all_mild_dist,FUN=median)
seed_mean_m<-aggregate(seed~inter,all_mild_dist,FUN=mean)


seedrich_med_m<-aggregate(seedrich~inter,all_mild_dist,FUN=median)
seedrich_mean_m<-aggregate(seedrich~inter,all_mild_dist,FUN=mean)


sevstat1m<-merge(agb_count_m,agb_med_m,by="inter")
sevstat2m<-merge(sevstat1m,agb_mean_m,by="inter")
sevstat3m<-merge(sevstat2m,seed_med_m,by="inter")
sevstat4m<-merge(sevstat3m,seed_mean_m,by="inter")
sevstat5m<-merge(sevstat4m,seedrich_med_m,by="inter")
sevstat6m<-merge(sevstat5m,seedrich_mean_m,by="inter")

colnames(sevstat6m)<-c("class","n_west","AGB_med_west","AGB_mean_west",
                      "seed_med_west","seed_mean_west","seedrich_med_west",
                      "seedrich_mean_west")


table_west_m<-sevstat6m



library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(tidyverse)
### ###

xlow<-7.5
xhigh<-13.5

ylow2<-0
yhigh2<-19500

ylow1<-0
yhigh1<-375

ylow3<-0
yhigh3<-13
### ###

# 


# Calculates mean, sd, se and IC
mild_agb1 <- all_mild_dist %>%
  group_by(inter) %>%
  summarise( 
    n=n(),
    mean=mean(AGBN),
    sd=sd(AGBN)
  ) %>%
  mutate( se=2*(sd/sqrt(n))) 

mild_agb1$type<-ifelse(mild_agb1$inter=="MF.first","MF",
                     ifelse(mild_agb1$inter=="MF.second","MF",
                            ifelse(mild_agb1$inter=="MC.first","MC",
                                   ifelse(mild_agb1$inter=="MC.second","MC",
                                          ifelse(mild_agb1$inter=="MI.first","MI",
                                                 ifelse(mild_agb1$inter=="MI.second","MI",
                                                        0))))))
mild_agb1$time<-ifelse(mild_agb1$inter=="MF.first","first",
                     ifelse(mild_agb1$inter=="MF.second","second",
                            ifelse(mild_agb1$inter=="MC.first","first",
                                   ifelse(mild_agb1$inter=="MC.second","second",
                                          ifelse(mild_agb1$inter=="MI.first","first",
                                                 ifelse(mild_agb1$inter=="MI.second","second",
                                                        0))))))

# mild_agb1$type<-fct_relevel("MF","MC","MI")

p1<-mild_agb1 %>%
  mutate(type = fct_relevel(type,
                            "MF","MC","MI"))%>%

ggplot(aes(x=as.factor(type), y=mean, fill=time),stat="identity") +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean, ymax=mean+2*se), width=.2,position=position_dodge(.9))+
  scale_fill_manual(values=c("#1B9E77", "#D95F02")) +
  ggtitle("West") + 
  # scale_x_discrete(level=c("MC","MF","MI"))+
  scale_x_discrete(labels=c("MC" = "Harvest", "MF" = "Fire",
                            "MI" = "Insect/Disease"))+
  
  xlab("")+
  ylab("AGB (Mg/ha)")+
  # geom_text(x=0.8, y=123, label="a",size=4.5)+
  # geom_text(x=1.2, y=120, label="a",size=4.5)+
  # geom_text(x=1.8, y=130, label="a",size=4.5)+
  # geom_text(x=2.2, y=130, label="b",size=4.5)+
  # geom_text(x=2.8, y=130, label="a",size=4.5)+
  # geom_text(x=3.2, y=130, label="b",size=4.5)+
  geom_text(x=0.5, y=165, label="(a)",size=6.5)+
  
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        plot.title = element_text(size = 22, hjust = 0.5),
        axis.title.y = element_text(size = 20)) 

  
  

sev_agb1 <- all_sev_dist %>%
  group_by(inter) %>%
  summarise( 
    n=n(),
    mean=mean(AGBN),
    sd=sd(AGBN)
  ) %>%
  mutate( se=2*(sd/sqrt(n))) 

sev_agb1$type<-ifelse(sev_agb1$inter=="SF.first","SF",
                       ifelse(sev_agb1$inter=="SF.second","SF",
                              ifelse(sev_agb1$inter=="SC.first","SC",
                                     ifelse(sev_agb1$inter=="SC.second","SC",
                                            ifelse(sev_agb1$inter=="SI.first","SI",
                                                   ifelse(sev_agb1$inter=="SI.second","SI",
                                                          0))))))
sev_agb1$time<-ifelse(sev_agb1$inter=="SF.first","first",
                       ifelse(sev_agb1$inter=="SF.second","second",
                              ifelse(sev_agb1$inter=="SC.first","first",
                                     ifelse(sev_agb1$inter=="SC.second","second",
                                            ifelse(sev_agb1$inter=="SI.first","first",
                                                   ifelse(sev_agb1$inter=="SI.second","second",
                                                          0))))))

# sev_agb1$type<-fct_relevel("MF","MC","MI")

p2<-sev_agb1 %>%
  mutate(type = fct_relevel(type,
                            "SF","SC","SI"))%>%
  
  ggplot(aes(x=as.factor(type), y=mean, fill=time),stat="identity") +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean, ymax=mean+2*se), width=.2,position=position_dodge(.9))+
  scale_fill_manual(values=c("#1B9E77", "#D95F02")) +
  ggtitle("West") + 
  # scale_x_discrete(level=c("MC","MF","MI"))+
  scale_x_discrete(labels=c("SC" = "Harvest", "SF" = "Fire",
                            "SI" = "Insect/Disease"))+
  
  xlab("")+
  ylab("AGB (Mg/ha)")+
  # geom_text(x=0.8, y=90, label="a",size=4.5)+
  # geom_text(x=1.2, y=20, label="a",size=4.5)+
  # geom_text(x=1.8, y=175, label="b",size=4.5)+
  # geom_text(x=2.2, y=30, label="b",size=4.5)+
  # geom_text(x=2.8, y=80, label="a",size=4.5)+
  # geom_text(x=3.2, y=35, label="b",size=4.5)+
  geom_text(x=0.5, y=305, label="(a)",size=6.5)+
  
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        plot.title = element_text(size = 22, hjust = 0.5),
        axis.title.y = element_text(size = 20)) 


mild_seed1 <- all_mild_dist %>%
  group_by(inter) %>%
  summarise( 
    n=n(),
    mean=mean(seed),
    sd=sd(seed)
  ) %>%
  mutate( se=2*(sd/sqrt(n))) 

mild_seed1$type<-ifelse(mild_seed1$inter=="MF.first","MF",
                       ifelse(mild_seed1$inter=="MF.second","MF",
                              ifelse(mild_seed1$inter=="MC.first","MC",
                                     ifelse(mild_seed1$inter=="MC.second","MC",
                                            ifelse(mild_seed1$inter=="MI.first","MI",
                                                   ifelse(mild_seed1$inter=="MI.second","MI",
                                                          0))))))
mild_seed1$time<-ifelse(mild_seed1$inter=="MF.first","first",
                       ifelse(mild_seed1$inter=="MF.second","second",
                              ifelse(mild_seed1$inter=="MC.first","first",
                                     ifelse(mild_seed1$inter=="MC.second","second",
                                            ifelse(mild_seed1$inter=="MI.first","first",
                                                   ifelse(mild_seed1$inter=="MI.second","second",
                                                          0))))))

# mild_seed1$type<-fct_relevel("MF","MC","MI")

p3<-mild_seed1 %>%
  mutate(type = fct_relevel(type,
                            "MF","MC","MI"))%>%
  
  ggplot(aes(x=as.factor(type), y=mean, fill=time),stat="identity") +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean, ymax=mean+2*se), width=.2,position=position_dodge(.9))+
  scale_fill_manual(values=c("#1B9E77", "#D95F02")) +
  ggtitle("") + 
  # scale_x_discrete(level=c("MC","MF","MI"))+
  scale_x_discrete(labels=c("MC" = "Harvest", "MF" = "Fire",
                            "MI" = "Insect/Disease"))+
  
  xlab("")+
  ylab("seedling density (ha)")+
  # geom_text(x=0.8, y=2230, label="a",size=4.5)+
  # geom_text(x=1.2, y=2200, label="a",size=4.5)+
  # geom_text(x=1.8, y=2600, label="a",size=4.5)+
  # geom_text(x=2.2, y=2600, label="a",size=4.5)+
  # geom_text(x=2.8, y=3050, label="b",size=4.5)+
  # geom_text(x=3.2, y=3150, label="b",size=4.5)+
  geom_text(x=0.5, y=3900, label="(c)",size=6.5)+
  
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        plot.title = element_text(size = 22, hjust = 0.5),
        axis.title.y = element_text(size = 20)) 




sev_seed1 <- all_sev_dist %>%
  group_by(inter) %>%
  summarise( 
    n=n(),
    mean=mean(seed),
    sd=sd(seed)
  ) %>%
  mutate( se=2*(sd/sqrt(n))) 

sev_seed1$type<-ifelse(sev_seed1$inter=="SF.first","SF",
                      ifelse(sev_seed1$inter=="SF.second","SF",
                             ifelse(sev_seed1$inter=="SC.first","SC",
                                    ifelse(sev_seed1$inter=="SC.second","SC",
                                           ifelse(sev_seed1$inter=="SI.first","SI",
                                                  ifelse(sev_seed1$inter=="SI.second","SI",
                                                         0))))))
sev_seed1$time<-ifelse(sev_seed1$inter=="SF.first","first",
                      ifelse(sev_seed1$inter=="SF.second","second",
                             ifelse(sev_seed1$inter=="SC.first","first",
                                    ifelse(sev_seed1$inter=="SC.second","second",
                                           ifelse(sev_seed1$inter=="SI.first","first",
                                                  ifelse(sev_seed1$inter=="SI.second","second",
                                                         0))))))

# sev_seed1$type<-fct_relevel("MF","MC","MI")

p4<-sev_seed1 %>%
  mutate(type = fct_relevel(type,
                            "SF","SC","SI"))%>%
  
  ggplot(aes(x=as.factor(type), y=mean, fill=time),stat="identity") +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean, ymax=mean+2*se), width=.2,position=position_dodge(.9))+
  scale_fill_manual(values=c("#1B9E77", "#D95F02")) +
  ggtitle("") + 
  # scale_x_discrete(level=c("MC","MF","MI"))+
  scale_x_discrete(labels=c("SC" = "Harvest", "SF" = "Fire",
                            "SI" = "Insect/Disease"))+
  
  xlab("")+
  ylab("seedling density (ha)")+
  # geom_text(x=0.8, y=2760, label="a",size=4.5)+
  # geom_text(x=1.2, y=3100, label="a",size=4.5)+
  # geom_text(x=1.8, y=2340, label="a",size=4.5)+
  # geom_text(x=2.2, y=3600, label="a",size=4.5)+
  # geom_text(x=2.8, y=4700, label="a",size=4.5)+
  # geom_text(x=3.2, y=6600, label="b",size=4.5)+
  geom_text(x=0.5, y=12000, label="(c)",size=6.5)+
  
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        plot.title = element_text(size = 22, hjust = 0.5),
        axis.title.y = element_text(size = 20)) 


rm(list = setdiff(ls(), c('p1','p2','p3','p4','sdam2','table_west','table_west_m')))


# 
# library(dplyr)
# library(plyr)
# library(tidyverse)

ecosel<-read.csv("../disturbance/eco_select.csv")

sdam2$ecocode <- trimws(sdam2$Spl_1, which = c("left"))

source("../data/FTGC.R")

# library(tidyr)
# require(dplyr)

sdam5b<-sdam2[(!sdam2$ecocode %in% ecosel$econew),]


sdam5b$rep_std=sdam5b$rep_std_fia2

pdam16<-sdam5b


## plots with no disturbance


trall_1a_ds<-pdam16[which(pdam16$dist_type=="FIRE" & pdam16$fia_sev1==0),]
trall_1b_ds<-pdam16[which(pdam16$dist_type=="FIRE" & pdam16$fia_sev1==1),]

plot(trall_1b_ds$AGB.22)

aaa2a<-data.frame(trall_1a_ds$AGB.11)
aaa2s<-data.frame(trall_1a_ds$seed_count.1)
aaa2r<-data.frame(trall_1a_ds$sd_spp_rich.1)

aaa2<-cbind(aaa2a,aaa2s,aaa2r)

aaa2$type<-"MF"
aaa2$time<-"first"
colnames(aaa2)<-c("AGBN","seed","seedrich","type","time")


aaa3a<-data.frame(trall_1b_ds$AGB.11)
aaa3s<-data.frame(trall_1b_ds$seed_count.1)
aaa3r<-data.frame(trall_1b_ds$sd_spp_rich.1)


aaa3<-cbind(aaa3a,aaa3s,aaa3r)


aaa3$type<-"SF"
aaa3$time<-"first"

colnames(aaa3)<-c("AGBN","seed","seedrich","type","time")


bbb2a<-data.frame(trall_1a_ds$AGB.22)
bbb2b<-data.frame(trall_1a_ds$seed_count.2)
bbb2c<-data.frame(trall_1a_ds$sd_spp_rich.2)


bbb2<-cbind(bbb2a,bbb2b,bbb2c)

bbb2$type<-"MF"
bbb2$time<-"second"

colnames(bbb2)<-c("AGBN","seed","seedrich","type","time")

bbb3a<-data.frame(trall_1b_ds$AGB.22)
bbb3b<-data.frame(trall_1b_ds$seed_count.2)
bbb3c<-data.frame(trall_1b_ds$sd_spp_rich.2)

bbb3<-cbind(bbb3a,bbb3b,bbb3c)

bbb3$type<-"SF"
bbb3$time<-"second"

colnames(bbb3)<-c("AGBN","seed","seedrich","type","time")

fire_dist<-rbind(aaa2,bbb2,aaa3,bbb3)




trall_1a_dsc<-pdam16[which(pdam16$dist_type=="CUT" & pdam16$fia_sev1==0),]
trall_1b_dsc<-pdam16[which(pdam16$dist_type=="CUT" & pdam16$fia_sev1==1),]


caaa2a<-data.frame(trall_1a_dsc$AGB.11)
caaa2b<-data.frame(trall_1a_dsc$seed_count.1)
caaa2c<-data.frame(trall_1a_dsc$sd_spp_rich.1)

caaa2<-cbind(caaa2a,caaa2b,caaa2c)

caaa2$type<-"MC"
caaa2$time<-"first"
colnames(caaa2)<-c("AGBN","seed","seedrich","type","time")

caaa3a<-data.frame(trall_1b_dsc$AGB.11)
caaa3b<-data.frame(trall_1b_dsc$seed_count.1)
caaa3c<-data.frame(trall_1b_dsc$sd_spp_rich.1)

caaa3<-cbind(caaa3a,caaa3b,caaa3c)
caaa3$type<-"SC"
caaa3$time<-"first"

colnames(caaa3)<-c("AGBN","seed","seedrich","type","time")


cbbb2a<-data.frame(trall_1a_dsc$AGB.22)
cbbb2b<-data.frame(trall_1a_dsc$seed_count.2)
cbbb2c<-data.frame(trall_1a_dsc$sd_spp_rich.2)


cbbb2<-cbind(cbbb2a,cbbb2b,cbbb2c)
cbbb2$type<-"MC"
cbbb2$time<-"second"

colnames(cbbb2)<-c("AGBN","seed","seedrich","type","time")

cbbb3a<-data.frame(trall_1b_dsc$AGB.22)
cbbb3b<-data.frame(trall_1b_dsc$seed_count.2)
cbbb3c<-data.frame(trall_1b_dsc$sd_spp_rich.2)


cbbb3<-cbind(cbbb3a,cbbb3b,cbbb3c)

cbbb3$type<-"SC"
cbbb3$time<-"second"

colnames(cbbb3)<-c("AGBN","seed","seedrich","type","time")





trall_1a_dsi<-pdam16[which(pdam16$dist_type=="INSDIS" & pdam16$fia_sev1==0),]
trall_1b_dsi<-pdam16[which(pdam16$dist_type=="INSDIS" & pdam16$fia_sev1==1),]


iaaa2a<-data.frame(trall_1a_dsi$AGB.11)
iaaa2b<-data.frame(trall_1a_dsi$seed_count.1)
iaaa2c<-data.frame(trall_1a_dsi$sd_spp_rich.1)


iaaa2<-cbind(iaaa2a,iaaa2b,iaaa2c)
iaaa2$type<-"MI"
iaaa2$time<-"first"
colnames(iaaa2)<-c("AGBN","seed","seedrich","type","time")

iaaa3a<-data.frame(trall_1b_dsi$AGB.11)
iaaa3b<-data.frame(trall_1b_dsi$seed_count.1)
iaaa3c<-data.frame(trall_1b_dsi$sd_spp_rich.1)


iaaa3<-cbind(iaaa3a,iaaa3b,iaaa3c)

iaaa3$type<-"SI"
iaaa3$time<-"first"

colnames(iaaa3)<-c("AGBN","seed","seedrich","type","time")


ibbb2a<-data.frame(trall_1a_dsi$AGB.22)
ibbb2b<-data.frame(trall_1a_dsi$seed_count.2)
ibbb2c<-data.frame(trall_1a_dsi$sd_spp_rich.2)


ibbb2<-cbind(ibbb2a,ibbb2b,ibbb2c)

ibbb2$type<-"MI"
ibbb2$time<-"second"

colnames(ibbb2)<-c("AGBN","seed","seedrich","type","time")

ibbb3a<-data.frame(trall_1b_dsi$AGB.22)
ibbb3b<-data.frame(trall_1b_dsi$seed_count.2)
ibbb3c<-data.frame(trall_1b_dsi$sd_spp_rich.2)


ibbb3<-cbind(ibbb3a,ibbb3b,ibbb3c)

ibbb3$type<-"SI"
ibbb3$time<-"second"

colnames(ibbb3)<-c("AGBN","seed","seedrich","type","time")



all_sev_dist<-rbind(aaa3,bbb3,caaa3,cbbb3,iaaa3,ibbb3)

all_mild_dist<-rbind(aaa2,bbb2,caaa2,cbbb2,iaaa2,ibbb2)

write.csv(all_sev_dist,"all_sev_dist_east_827.csv")
write.csv(all_mild_dist,"all_mild_dist_east_827.csv")

all_sev_dist$inter<-interaction(all_sev_dist$type,all_sev_dist$time)

agb_med<-aggregate(AGBN~inter,all_sev_dist,FUN=median)
agb_mean<-aggregate(AGBN~inter,all_sev_dist,FUN=mean)

# remove.packages("tidyverse")
# install.packages("tidyverse")

# library(tidyverse)
# library(dplyr)
agb_count<-count(all_sev_dist,inter)

seed_med<-aggregate(seed~inter,all_sev_dist,FUN=median)
seed_mean<-aggregate(seed~inter,all_sev_dist,FUN=mean)


seedrich_med<-aggregate(seedrich~inter,all_sev_dist,FUN=median)
seedrich_mean<-aggregate(seedrich~inter,all_sev_dist,FUN=mean)


sevstat1<-merge(agb_count,agb_med,by="inter")
sevstat2<-merge(sevstat1,agb_mean,by="inter")
sevstat3<-merge(sevstat2,seed_med,by="inter")
sevstat4<-merge(sevstat3,seed_mean,by="inter")
sevstat5<-merge(sevstat4,seedrich_med,by="inter")
sevstat6<-merge(sevstat5,seedrich_mean,by="inter")

colnames(sevstat6)<-c("class","east","AGB_med_east","AGB_mean_east",
                      "seed_med_east","seed_mean_east","seedrich_med_east",
                      "seedrich_mean_east")


table_east<-sevstat6


all_mild_dist$inter<-interaction(all_mild_dist$type,all_mild_dist$time)

agb_med_m<-aggregate(AGBN~inter,all_mild_dist,FUN=median)
agb_mean_m<-aggregate(AGBN~inter,all_mild_dist,FUN=mean)
agb_count_m<-count(all_mild_dist,inter)


seed_med_m<-aggregate(seed~inter,all_mild_dist,FUN=median)
seed_mean_m<-aggregate(seed~inter,all_mild_dist,FUN=mean)


seedrich_med_m<-aggregate(seedrich~inter,all_mild_dist,FUN=median)
seedrich_mean_m<-aggregate(seedrich~inter,all_mild_dist,FUN=mean)


sevstat1m<-merge(agb_count_m,agb_med_m,by="inter")
sevstat2m<-merge(sevstat1m,agb_mean_m,by="inter")
sevstat3m<-merge(sevstat2m,seed_med_m,by="inter")
sevstat4m<-merge(sevstat3m,seed_mean_m,by="inter")
sevstat5m<-merge(sevstat4m,seedrich_med_m,by="inter")
sevstat6m<-merge(sevstat5m,seedrich_mean_m,by="inter")

colnames(sevstat6m)<-c("class","n_east","AGB_med_east","AGB_mean_east",
                       "seed_med_east","seed_mean_east","seedrich_med_east",
                       "seedrich_mean_east")


table_east_m<-sevstat6m


table_all<- merge(table_west,table_east,by="class")

write.csv(table_all,"table_summary_all_severe_827.csv")


table_all2<- merge(table_west_m,table_east_m,by="class")

write.csv(table_all2,"table_summary_all_mild_827.csv")





library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)

### ###

xlow<-7.5
xhigh<-13.5

ylow2<-0
yhigh2<-19500

ylow1<-0
yhigh1<-375

ylow3<-0
yhigh3<-13
### ###



# Calculates mean, sd, se and IC
mild_agb1 <- all_mild_dist %>%
  group_by(inter) %>%
  summarise( 
    n=n(),
    mean=mean(AGBN),
    sd=sd(AGBN)
  ) %>%
  mutate( se=2*(sd/sqrt(n))) 

mild_agb1$type<-ifelse(mild_agb1$inter=="MF.first","MF",
                       ifelse(mild_agb1$inter=="MF.second","MF",
                              ifelse(mild_agb1$inter=="MC.first","MC",
                                     ifelse(mild_agb1$inter=="MC.second","MC",
                                            ifelse(mild_agb1$inter=="MI.first","MI",
                                                   ifelse(mild_agb1$inter=="MI.second","MI",
                                                          0))))))
mild_agb1$time<-ifelse(mild_agb1$inter=="MF.first","first",
                       ifelse(mild_agb1$inter=="MF.second","second",
                              ifelse(mild_agb1$inter=="MC.first","first",
                                     ifelse(mild_agb1$inter=="MC.second","second",
                                            ifelse(mild_agb1$inter=="MI.first","first",
                                                   ifelse(mild_agb1$inter=="MI.second","second",
                                                          0))))))

# mild_agb1$type<-fct_relevel("MF","MC","MI")


p15<-mild_agb1 %>%
  mutate(type = fct_relevel(type,
                            "MF","MC","MI"))%>%
  
  ggplot(aes(x=as.factor(type), y=mean, fill=time),stat="identity") +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean, ymax=mean+2*se), width=.2,position=position_dodge(.9))+
  scale_fill_manual(values=c("#1B9E77", "#D95F02")) +
  ggtitle("East") + 
  # scale_x_discrete(level=c("MC","MF","MI"))+
  scale_x_discrete(labels=c("MC" = "Harvest", "MF" = "Fire",
                            "MI" = "Insect/Disease"))+
  
  xlab("")+
  ylab("AGB (Mg/ha)")+
  # geom_text(x=0.8, y=100, label="a",size=4.5)+
  # geom_text(x=1.2, y=110, label="a",size=4.5)+
  # geom_text(x=1.8, y=115, label="b",size=4.5)+
  # geom_text(x=2.2, y=90, label="b",size=4.5)+
  # geom_text(x=2.8, y=135, label="c",size=4.5)+
  # geom_text(x=3.2, y=135, label="c",size=4.5)+
  geom_text(x=0.5, y=138, label="(b)",size=6.5)+
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        plot.title = element_text(size = 22, hjust = 0.5),
        axis.title.y = element_text(size = 20)) 




sev_agb1 <- all_sev_dist %>%
  group_by(inter) %>%
  summarise( 
    n=n(),
    mean=mean(AGBN),
    sd=sd(AGBN)
  ) %>%
  mutate( se=2*(sd/sqrt(n))) 

sev_agb1$type<-ifelse(sev_agb1$inter=="SF.first","SF",
                      ifelse(sev_agb1$inter=="SF.second","SF",
                             ifelse(sev_agb1$inter=="SC.first","SC",
                                    ifelse(sev_agb1$inter=="SC.second","SC",
                                           ifelse(sev_agb1$inter=="SI.first","SI",
                                                  ifelse(sev_agb1$inter=="SI.second","SI",
                                                         0))))))
sev_agb1$time<-ifelse(sev_agb1$inter=="SF.first","first",
                      ifelse(sev_agb1$inter=="SF.second","second",
                             ifelse(sev_agb1$inter=="SC.first","first",
                                    ifelse(sev_agb1$inter=="SC.second","second",
                                           ifelse(sev_agb1$inter=="SI.first","first",
                                                  ifelse(sev_agb1$inter=="SI.second","second",
                                                         0))))))

# sev_agb1$type<-fct_relevel("MF","MC","MI")

p16<-sev_agb1 %>%
  mutate(type = fct_relevel(type,
                            "SF","SC","SI"))%>%
  
  ggplot(aes(x=as.factor(type), y=mean, fill=time),stat="identity") +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean, ymax=mean+2*se), width=.2,position=position_dodge(.9))+
  scale_fill_manual(values=c("#1B9E77", "#D95F02")) +
  ggtitle("East") + 
  # scale_x_discrete(level=c("MC","MF","MI"))+
  scale_x_discrete(labels=c("SC" = "Harvest", "SF" = "Fire",
                            "SI" = "Insect/Disease"))+
  
  xlab("")+
  ylab("AGB (Mg/ha)")+
  # geom_text(x=0.8, y=60, label="a",size=4.5)+
  # geom_text(x=1.2, y=20, label="a",size=4.5)+
  # geom_text(x=1.8, y=105, label="b",size=4.5)+
  # geom_text(x=2.2, y=20, label="a",size=4.5)+
  # geom_text(x=2.8, y=102, label="a",size=4.5)+
  # geom_text(x=3.2, y=33, label="a",size=4.5)+
  geom_text(x=0.5, y=120, label="(b)",size=6.5)+
  
  theme_bw()+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        plot.title = element_text(size = 22, hjust = 0.5),
        axis.title.y = element_text(size = 20)) 


mild_seed1 <- all_mild_dist %>%
  group_by(inter) %>%
  summarise( 
    n=n(),
    mean=mean(seed),
    sd=sd(seed)
  ) %>%
  mutate( se=2*(sd/sqrt(n))) 

mild_seed1$type<-ifelse(mild_seed1$inter=="MF.first","MF",
                        ifelse(mild_seed1$inter=="MF.second","MF",
                               ifelse(mild_seed1$inter=="MC.first","MC",
                                      ifelse(mild_seed1$inter=="MC.second","MC",
                                             ifelse(mild_seed1$inter=="MI.first","MI",
                                                    ifelse(mild_seed1$inter=="MI.second","MI",
                                                           0))))))
mild_seed1$time<-ifelse(mild_seed1$inter=="MF.first","first",
                        ifelse(mild_seed1$inter=="MF.second","second",
                               ifelse(mild_seed1$inter=="MC.first","first",
                                      ifelse(mild_seed1$inter=="MC.second","second",
                                             ifelse(mild_seed1$inter=="MI.first","first",
                                                    ifelse(mild_seed1$inter=="MI.second","second",
                                                           0))))))

# mild_seed1$type<-fct_relevel("MF","MC","MI")

p17<- mild_seed1 %>%
  mutate(type = fct_relevel(type,
                            "MF","MC","MI"))%>%
  
  ggplot(aes(x=as.factor(type), y=mean, fill=time),stat="identity") +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean, ymax=mean+2*se), width=.2,position=position_dodge(.9))+
  # scale_fill_manual(values=c("#1B9E77", "#D95F02")) +
  scale_fill_manual(name="",values=c("#1B9E77", "#D95F02"),
                    labels=c("first" = "pre-disturbance",
                             "second" = "post-disturbance")) +
  ggtitle("") + 
  # scale_x_discrete(level=c("MC","MF","MI"))+
  scale_x_discrete(labels=c("MC" = "Harvest", "MF" = "Fire",
                            "MI" = "Insect/Disease"))+
  
  xlab("")+
  ylab("seedling density (ha)")+
  # geom_text(x=0.8, y=4330, label="a",size=4.5)+
  # geom_text(x=1.2, y=4550, label="a",size=4.5)+
  # geom_text(x=1.8, y=4400, label="a",size=4.5)+
  # geom_text(x=2.2, y=5050, label="b",size=4.5)+
  # geom_text(x=2.8, y=5930, label="b",size=4.5)+
  # geom_text(x=3.2, y=5850, label="c",size=4.5)+
  geom_text(x=0.5, y=8700, label="(d)",size=6.5)+
  
  theme_bw()+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        plot.title = element_text(size = 22, hjust = 0.5),
        axis.title.y = element_text(size = 20),
        legend.text=element_text(size=20) ) 




sev_seed1 <- all_sev_dist %>%
  group_by(inter) %>%
  summarise( 
    n=n(),
    mean=mean(seed),
    sd=sd(seed)
  ) %>%
  mutate( se=2*(sd/sqrt(n))) 

sev_seed1$type<-ifelse(sev_seed1$inter=="SF.first","SF",
                       ifelse(sev_seed1$inter=="SF.second","SF",
                              ifelse(sev_seed1$inter=="SC.first","SC",
                                     ifelse(sev_seed1$inter=="SC.second","SC",
                                            ifelse(sev_seed1$inter=="SI.first","SI",
                                                   ifelse(sev_seed1$inter=="SI.second","SI",
                                                          0))))))
sev_seed1$time<-ifelse(sev_seed1$inter=="SF.first","first",
                       ifelse(sev_seed1$inter=="SF.second","second",
                              ifelse(sev_seed1$inter=="SC.first","first",
                                     ifelse(sev_seed1$inter=="SC.second","second",
                                            ifelse(sev_seed1$inter=="SI.first","first",
                                                   ifelse(sev_seed1$inter=="SI.second","second",
                                                          0))))))

# sev_seed1$type<-fct_relevel("MF","MC","MI")

p18<-sev_seed1 %>%
  mutate(type = fct_relevel(type,
                            "SF","SC","SI"))%>%
  
  ggplot(aes(x=as.factor(type), y=mean, fill=time),stat="identity") +
  geom_bar(position=position_dodge(), stat="identity", colour='black') +
  geom_errorbar(aes(ymin=mean, ymax=mean+2*se), width=.2,position=position_dodge(.9))+
  # scale_fill_manual(values=c("#1B9E77", "#D95F02")) +
  scale_fill_manual(name="",values=c("#1B9E77", "#D95F02"),
                    labels=c("first" = "pre-disturbance",
                             "second" = "post-disturbance")) +
  ggtitle("") + 
  # scale_x_discrete(level=c("MC","MF","MI"))+
  scale_x_discrete(labels=c("SC" = "Harvest", "SF" = "Fire",
                            "SI" = "Insect/Disease"))+
  
  xlab("")+
  ylab("seedling density (ha)")+
  # geom_text(x=0.8, y=6210, label="a",size=4.5)+
  # geom_text(x=1.2, y=7200, label="a",size=4.5)+
  # geom_text(x=1.8, y=5540, label="a",size=4.5)+
  # geom_text(x=2.2, y=8000, label="a",size=4.5)+
  # geom_text(x=2.8, y=12000, label="a",size=4.5)+
  # geom_text(x=3.2, y=12000, label="a",size=4.5)+
  geom_text(x=0.5, y=15150, label="(d)",size=6.5)+
  
  theme_bw()+
  theme(legend.position="bottom")+
  theme(axis.text.x = element_text(size=17),
        axis.text.y = element_text(size=17),
        plot.title = element_text(size = 22, hjust = 0.5),
        axis.title.y = element_text(size = 20),
        legend.text=element_text(size=20)) 





### ###

library(cowplot)
legend1<-get_legend(p17)
legend2<-get_legend(p18)
current_date<-Sys.Date()
### ###
png(paste0("mild_pre_post_age_stock_",current_date,".jpg"), width = 1300, height = 1000,res=100)
par(xpd = F, mar = c(10,5,3,7))
par(oma=c(10,5,0,0))

par(mfrow=c(3,2))
# p22<-grid.arrange(p1,p8,p15,p3,p10,p17,p5,p12,p19,ncol=3)

# p22<-grid.arrange(p1,p15,p3,p17,ncol=2)
# dev.off()


grid.arrange(p1 + theme(legend.position="none"), 
             p15 + theme(legend.position="none"),
             p3 + theme(legend.position="none"),
             p17 + theme(legend.position="none"),
             legend1,
             heights=c(1.1, 1.1, 0.1),
             nrow = 3)

dev.off()




png(paste0("sev_pre_post_age_stock_",current_date,".jpeg"), width = 1300, height = 1000,res=100)
par(xpd = F, mar = c(10,5,3,7))
par(oma=c(10,5,0,0))

par(mfrow=c(3,2))
# p22<-grid.arrange(p1,p8,p15,p3,p10,p17,p5,p12,p19,ncol=3)

# p22<-grid.arrange(p1,p15,p3,p17,ncol=2)
# dev.off()


grid.arrange(p2 + theme(legend.position="none"), 
             p16 + theme(legend.position="none"),
             p4 + theme(legend.position="none"),
             p18 + theme(legend.position="none"),
             legend2,
             heights=c(1.1, 1.1, 0.1),
             nrow = 3)

dev.off()


