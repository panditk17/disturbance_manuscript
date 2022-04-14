# setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000)

tr<-readRDS("../../data/all_four_repeated_tree1.RDS")

trfr$rep_std.2<-ifelse(trfr$STDAGE.2 <= trfr$REMPER.2,1,0)
trfr$rep_std.3<-ifelse(trfr$STDAGE <= trfr$REMPER,1,0)
trfr$severity<-ifelse(trfr$sev_mean>2,1,0)
trfr$disturb1<-ifelse(trfr$DSTRBCD1.1|trfr$DSTRBCD2.1|trfr$DSTRBCD3.1>0,1,0)
trfr$disturb2<-ifelse(trfr$DSTRBCD1.2|trfr$DSTRBCD2.2|trfr$DSTRBCD3.2>0,1,0)
trfr$disturb3<-ifelse(trfr$DSTRBCD1|trfr$DSTRBCD2|trfr$DSTRBCD3>0,1,0)

hhh<-count(trfr,disturb1)

jjj<-count(trfr,NUNIDS.1,severity,rep_std.2,rep_std.3)
kkk<-count(jjj,NUNIDS.1,severity,rep_std.2,rep_std.3)


plot1<-aggregate(rep_std.2~NUNIDS.2,trfr,FUN=max)
plot2<-aggregate(rep_std.3~NUNIDS,trfr,FUN=max)


names(tr)
# MTBS fire data
fire<-read.csv("../../MTBS_fire_FIa_plots.csv")  
names(fire)

fire$r_fire_yr<-round(fire$MTBS_fireyear) # rounding fire year

# create extra columns for sanity check later
tr$NUNID_CC<-tr$NUNIDS.1
fire$nplotid_cp<-fire$nplotid

# merge tree table with MTBS fire data
trfr<-merge(tr,fire,by.x="NUNIDS.2",by.y="nplotid",all=TRUE)

 jjj<-aggregate(STDAGE.1~NUNIDS.1,trfr,FUN=max)
 jjj2<-aggregate(STDAGE.2~NUNIDS.2,trfr,FUN=max)
 jjj3<-aggregate(STDAGE~NUNIDS,trfr,FUN=max)
 
# jjj2<-trfr[!is.na(trfr$NUNIDS.2),]
# jjj3<-trfr[!is.na(trfr$NUNIDS),]

rm(tr)

trfr$severity<-ifelse(trfr$sev_mean>2,1,0)

table11<-count(trfr,nplotid)

trees_fire_22<-trfr[trfr$NUNIDS.2 %in% fire$nplotid,]

#data with first measurements less than fire year
trees_fire_33<-trees_fire_22[which(trees_fire_22$MEASYEAR.1<trees_fire_22$MTBS_fireyear),]

#data with first measurements less and second measurements greater than fire year
trees_fire_44<-trees_fire_33[which(trees_fire_33$MEASYEAR.2>trees_fire_33$MTBS_fireyear),]

tablepp<-count(trees_fire_44,NUNIDS.2)

#data with first &second measureemtns less and third measurements greater than fire year
trees_fire_55<-trees_fire_33[which(trees_fire_33$MEASYEAR.2<trees_fire_33$MTBS_fireyear
                               & trees_fire_33$MEASYEAR>trees_fire_33$MTBS_fireyear),]

tableppp<-count(trees_fire_55,NUNIDS.2)

# select columns from first subset of data
table_1a<-cbind(trees_fire_44[,1:139],trees_fire_44[,211:222])

table_1aa<-cbind(table_1a[,1:69],table_1a[71:151])

tableqq<-count(table_1aa,NUNIDS.2)

# select columns from second subset of data
table_1b<-cbind(trees_fire_55[,1:2],trees_fire_55[,72:139],
                trees_fire_55[,141:209],trees_fire_55[,211:222])

table_1bb<-cbind(table_1b[,1:137],table_1b[,139:151])

# make consistent column names
colnames(table_1bb)<-colnames(table_1aa)

# combine two tables

fire_11<-rbind(table_1aa,table_1bb)

tablerr<-count(fire_11,NUNIDS.2)


#data with first measurements less than fire year
trees_fire_333<-trfr[which(trfr$MEASYEAR.1<trfr$MTBS_fireyear),]

#data with first measurements less and second measurements greater than fire year
trees_fire_44<-trees_fire_33[which(trees_fire_33$MEASYEAR.2>trees_fire_33$MTBS_fireyear),]

tablepp<-count(trees_fire_44,NUNIDS.2)



qqqq<-count(trfr,NUNIDS.1)

#data with first &second measureemtns less and third measurements greater than fire year
trees_fire_55<-trees_fire_33[which(trees_fire_33$MEASYEAR.2<trees_fire_33$MTBS_fireyear
                                   & trees_fire_33$MEASYEAR>trees_fire_33$MTBS_fireyear),]

tableppp<-count(trees_fire_55,NUNIDS.2)








trees_fire_11<-fire_11

# calculate since fire 
trees_fire_11$since_fire<-trees_fire_11$MEASYEAR.2 - trees_fire_11$r_fire_yr

# table compare tree status for different severity levels
table_11<-data.frame(count(trees_fire_11,severity,STATUSCD.1,STATUSCD.2))
colnames(table_11)<- c("MTBS severity","Pre-fire status","Post-fire status","n")

df2 <- trees_fire_11 %>% 
  group_by(severity,STATUSCD.1, STATUSCD.2) %>% 
  tally() %>% 
  complete(STATUSCD.2, fill = list(n = 0)) %>% 
  mutate(percentage = n / sum(n) * 100)

current_date<-Sys.Date()

write.csv(df2,paste0("nnnall_states_MTBS_sev_tree_status1_",current_date,".csv"))

table_222<-data.frame(count(trees_fire_11,NUNIDS.2,MEASYEAR.2,r_fire_yr))


plot1<-aggregate(since_fire~NUNIDS.2, trees_fire_11, FUN=min)

trees_fire33n<-trees_fire_11[!is.na(trees_fire_11$STDAGE.1),]
tablerrr<-count(trees_fire33n,NUNIDS.2)


plot2<-aggregate(STDAGE.1~NUNIDS.2, trees_fire_11, FUN=mean)
plot22<-aggregate(STDAGE.2~NUNIDS.2, trees_fire_11, FUN=mean)
plot3<-aggregate(severity~NUNIDS.2, trees_fire_11, FUN=mean)


dam11<-merge(plot1,plot2,by="NUNIDS.2")
dam12<-merge(dam11,plot22,by="NUNIDS.2")
dam1<-merge(dam12,plot3,by="NUNIDS.2")


dam1<-dam1[which(dam1$since_fire>0),]

dam1$rep_stand<-ifelse(dam1$STDAGE.2<=dam1$since_fire,1,0)

write.csv(trees_fire_11,"nndata_with_MTBS_only1.csv")
write.csv(dam1,"nnplots_with_MTBS_only1.csv")

# trees_fire_11<-read.csv("nndata_with_MTBS_only1.csv")


library(ggplot2)
library(gridExtra)

dev.new()
png("nall_states_sincefire_stdage111.jpeg", width = 600, height = 600)
par(xpd = T, mar = c(12,6,8,6))
par(mfrow=c(2,1))



p1<-ggplot(data=dam1) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("mild","severe"), 
                     values=c("0"="gold1","1"="red")) + 
    # scale_y_continuous(limits=c(0,500))+
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



p2<-ggplot(data=dam1) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(severity))) +
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


 q1<-grid.arrange(p1,p2)

# ggsave(q1,"all_states_yrs_stdage.png")
dev.off()

table_sev_rep<-data.frame(count(dam1, severity,rep_stand))
colnames(table_sev_rep)<-c("MTBS severity","stand replaced","n")
write.csv(table_sev_rep,"all_states_MTBS_severity_stand_replaced1.csv")

dam22<-dam1[which(dam1$rep_stand==1),]

dev.new()
png("all_states_sincefire_stdage_std_rep11.jpeg", width = 600, height = 400)
par(xpd = T, mar = c(6,6,6,6))
par(mfrow=c(2,1))



p3<- ggplot(data=dam22) +
  geom_jitter(aes(x=since_fire, y=STDAGE.1,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("low","high"), 
                     values=c("0"="gold1","1"="red")) + theme_bw()



p4<- ggplot(data=dam22) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(severity))) +
  scale_color_manual(name="fire severity",labels=c("low","high"), 
                     values=c("0"="gold1","1"="red")) + theme_bw()


q2<-grid.arrange(p3,p4)

dev.off()

hum.names <- as_labeller(c('0' = "Low fire intensity", '1' = "High fire intensity"))

dev.new()
png("all_states_stacked_numbers_status1.jpeg", width = 400, height = 400)
par(xpd = T, mar = c(6,6,6,6))

trees_fire_11 %>%
  ggplot() +
  geom_bar(aes(x = factor(STATUSCD.1), fill = factor(STATUSCD.2))) +
  facet_wrap(~severity,nrow=1,labeller = hum.names)+
  ggtitle("Shift in tree status") +
  xlab("First inventory") +
  labs(fill = "Second inventory")+
  scale_fill_manual(name="Second inventory",labels=c("No data","Live","Dead","Removed","NA"), 
                    values=c("0"="gray50","1"="green4","2"="red","3"= "chocolate3"),na.value="black") +
  scale_x_discrete(labels=c("1" = "Live", "2" = "Dead"))+
  theme_bw()


dev.off()


# trees_fire_22<-trees_fire_11
# 
# 
# trees_fire_22$rep_stand_all<-ifelse(trees_fire_22$STDAGE.2<=trees_fire_22$REMPER.2,1,0)


trfr$rep_stand_all<-ifelse(trfr$STDAGE.2<=trfr$REMPER.2,1,0)

## subset data by stand age and remper for chronological analysis
## other analysis would be done with main trfr data


age_tr2<-trfr[!is.na(trfr$STDAGE.2),]
age_tr2<-age_tr2[!is.na(age_tr2$REMPER.2),]

age_tr2$rep_stand_all<-ifelse(age_tr2$STDAGE.2<=age_tr2$REMPER.2,1,0)



tr<-age_tr2

rm(age_tr2)
rm(trfr)

tr1<-tr

tr2<-trees_fire_11[which(trees_fire_11$MEASYEAR.1<= trees_fire_11$r_fire_yr & 
                           trees_fire_11$MEASYEAR.2>=trees_fire_11$r_fire_yr),]

# tr<-tr[which(tr$MEASYEAR.2>2001),]

cut_agnt <- c(10)

tr$CUT <- ifelse(tr$TRTCD1.2 %in% cut_agnt | tr$TRTCD2.2 %in% cut_agnt 
                 | tr$TRTCD3.2 %in% cut_agnt, 1,  0)

trees_fire_11$CUT <- ifelse(trees_fire_11$TRTCD1.2 %in% cut_agnt | 
                              trees_fire_11$TRTCD2.2 %in% cut_agnt 
                 | trees_fire_11$TRTCD3.2 %in% cut_agnt, 1,  0)


fire_agnt <- c(30,31,32)

tr$FIRE_ALL <- ifelse(tr$DSTRBCD1.2 %in% fire_agnt | tr$DSTRBCD2.2 %in% fire_agnt 
                      | tr$DSTRBCD3.2 %in% fire_agnt, 1,  0)

trees_fire_11$FIRE_ALL <- ifelse(trees_fire_11$DSTRBCD1.2 %in% fire_agnt | 
                                   trees_fire_11$DSTRBCD2.2 %in% fire_agnt 
                      | trees_fire_11$DSTRBCD3.2 %in% fire_agnt, 1,  0)

gfire_agnt <- c(31)

tr$G_FIRE <- ifelse(tr$DSTRBCD1.2 %in% gfire_agnt | tr$DSTRBCD2.2 %in% gfire_agnt 
                    | tr$DSTRBCD3.2 %in% gfire_agnt, 1,  0)
trees_fire_11$G_FIRE <- ifelse(trees_fire_11$DSTRBCD1.2 %in% gfire_agnt | 
                                 trees_fire_11$DSTRBCD2.2 %in% gfire_agnt 
                    | trees_fire_11$DSTRBCD3.2 %in% gfire_agnt, 1,  0)


cfire_agnt <- c(32)
tr$C_FIRE <- ifelse(tr$DSTRBCD1.2 %in% cfire_agnt | tr$DSTRBCD2.2 %in% cfire_agnt 
                    | tr$DSTRBCD3.2 %in% cfire_agnt, 1,  0)
trees_fire_11$C_FIRE <- ifelse(trees_fire_11$DSTRBCD1.2 %in% cfire_agnt | 
                                 trees_fire_11$DSTRBCD2.2 %in% cfire_agnt 
                    | trees_fire_11$DSTRBCD3.2 %in% cfire_agnt, 1,  0)

nfire_agnt <- c(30)
tr$N_FIRE <- ifelse(tr$DSTRBCD1.2 %in% nfire_agnt | tr$DSTRBCD2.2 %in% nfire_agnt 
                    | tr$DSTRBCD3.2 %in% nfire_agnt, 1,  0)

insdis_agnt <- c(10,11,12,20,21,22)
tr$INSDIS <- ifelse(tr$DSTRBCD1.2 %in% insdis_agnt | tr$DSTRBCD2.2 %in% insdis_agnt 
                    | tr$DSTRBCD3.2 %in% insdis_agnt, 1,  0)
trees_fire_11$INSDIS <- ifelse(trees_fire_11$DSTRBCD1.2 %in% insdis_agnt | 
                                 trees_fire_11$DSTRBCD2.2 %in% insdis_agnt 
                    | trees_fire_11$DSTRBCD3.2 %in% insdis_agnt, 1,  0)

ins_agnt <- c(10,11,12)
tr$INSECT <- ifelse(tr$DSTRBCD1.2 %in% ins_agnt | tr$DSTRBCD2.2 %in% ins_agnt 
                    | tr$DSTRBCD3.2 %in% ins_agnt, 1,  0)

disease_agnt <- c(20,21,22)
tr$DISEASE <- ifelse(tr$DSTRBCD1.2 %in% disease_agnt | tr$DSTRBCD2.2 %in% disease_agnt 
                     | tr$DSTRBCD3.2 %in% disease_agnt, 1,  0)

tr$DISTURB <- ifelse(tr$DSTRBCD1.2 !=0 | tr$DSTRBCD2.2 !=0 | tr$DSTRBCD3.2 !=0, 1,  0)

trees_fire_11$DISTURB <- ifelse(trees_fire_11$DSTRBCD1.2 !=0 | 
                                  trees_fire_11$DSTRBCD2.2 !=0 | 
                                  trees_fire_11$DSTRBCD3.2 !=0, 1,  0)

tr$DISTURB1 <- ifelse(tr$DSTRBCD1.2 !=0,  1,  0)
tr$DISTURB2 <- ifelse(tr$DSTRBCD2.2 !=0,  1,  0)
tr$DISTURB3 <- ifelse(tr$DSTRBCD3.2 !=0,  1,  0)

# ntr<-tr[is.na(tr$DRYBIO_BG.1),]

## calculate basal area and AGB
tr$AGB.2<-ifelse((tr$STATUSCD.2==1),tr$DRYBIO_AG.2*tr$TPA_UNADJ.2,0)
tr$AGB.1<-ifelse((tr$STATUSCD.1==1),tr$DRYBIO_AG.1*tr$TPA_UNADJ.1,0)

trees_fire_11$AGB.2<-ifelse((trees_fire_11$STATUSCD.2==1),
                            trees_fire_11$DRYBIO_AG.2*trees_fire_11$TPA_UNADJ.2,0)
trees_fire_11$AGB.1<-ifelse((trees_fire_11$STATUSCD.1==1),
                            trees_fire_11$DRYBIO_AG.1*trees_fire_11$TPA_UNADJ.1,0)


# tr$AGB_CH<-tr$AGB.2 -tr$AGB.1

tr$since_fire<-tr$MEASYEAR.2 - tr$r_fire_yr

trees_fire_11$since_fire<-trees_fire_11$MEASYEAR.2 - trees_fire_11$r_fire_yr
# sub_tr<-tr[which(tr$rep_stand_all==1),]


table_fia_mtbs<-count(tr,severity,FIRE_ALL)

table_fia_mtbs2<-count(tr,severity,C_FIRE)
  

table_fia_mtbs3<-count(trees_fire_11,severity,FIRE_ALL)

table_fia_mtbs4<-count(trees_fire_11,severity,C_FIRE)

uuu<-count(tr,NUNIDS.2)

## calculate plot level variables and disturbances

pplot1<-aggregate(AGB.2~NUNIDS.2, tr, FUN=sum)
pplot2<-aggregate(AGB.1~NUNIDS.2, tr, FUN=sum)
pplot3<-aggregate(BALIVE.2~NUNIDS.2, tr, FUN=mean)
pplot4<-aggregate(BALIVE.1~NUNIDS.2, tr, FUN=mean)
pplot5<-data.frame(count(tr,NUNIDS.2,TREEID))
pplot55<-aggregate(n~NUNIDS.2, pplot5, FUN=sum)
pplot6<-data.frame(count(tr,NUNIDS.2,STATUSCD.2))
pplot7<-data.frame(count(tr,NUNIDS.2,STATUSCD.1))
pplot8<-aggregate(since_fire~NUNIDS.2, tr, FUN=mean)
pplot9<-aggregate(CUT~NUNIDS.2, tr, FUN=mean)
pplot10<-aggregate(FIRE_ALL~NUNIDS.2, tr, FUN=mean)
pplot11<-aggregate(INSDIS~NUNIDS.2, tr, FUN=mean)
pplot12<-aggregate(G_FIRE~NUNIDS.2, tr, FUN=mean)
pplot13<-aggregate(C_FIRE~NUNIDS.2, tr, FUN=mean)
pplot13a<-aggregate(N_FIRE~NUNIDS.2, tr, FUN=mean)

pplot14<-aggregate(LAT.2~NUNIDS.2, tr, FUN=mean)
pplot15<-aggregate(LON.2~NUNIDS.2, tr, FUN=mean)

pplot16<-aggregate(severity~NUNIDS.2, tr, FUN=mean)

pplot17<-aggregate(REMPER.2~NUNIDS.2, tr, FUN=mean)
pplot18<-aggregate(rep_stand_all~NUNIDS.2, tr, FUN=max)
pplot19<-aggregate(MEASYEAR.2~NUNIDS.2, tr, FUN=max)
pplot19b<-aggregate(MEASYEAR.1~NUNIDS.2, tr, FUN=max)
pplot20<-aggregate(STDAGE.2~NUNIDS.2, tr, FUN=max)
pplot20b<-aggregate(STDAGE.1~NUNIDS.2, tr, FUN=max)

pplot21<-aggregate(DISTURB~NUNIDS.2, tr, FUN=mean)

pplot22<-aggregate(STDORGCD.1~NUNIDS.2, tr, FUN=max)
pplot23<-aggregate(STDORGCD.2~NUNIDS.2, tr, FUN=max)

pplot24<-aggregate(FORTYPCD.1~NUNIDS.2, tr, FUN=max)
pplot25<-aggregate(FORTYPCD.2~NUNIDS.2, tr, FUN=max)
pplot26<-aggregate(ECOSUBCD.1~NUNIDS.2, tr, FUN=max)


pplot27<-aggregate(NUNID.1~NUNIDS.2, tr, FUN=max)
pplot28<-aggregate(NUNID.2~NUNIDS.2, tr, FUN=max)



pdam1<-merge(pplot1,pplot2,by="NUNIDS.2")
pdam2<-merge(pdam1,pplot3,by="NUNIDS.2")
pdam3<-merge(pdam2,pplot4,by="NUNIDS.2")
pdam4<-merge(pdam3,pplot9,by="NUNIDS.2")
pdam5<-merge(pdam4,pplot10,by="NUNIDS.2")
pdam6<-merge(pdam5,pplot11,by="NUNIDS.2")
pdam7<-merge(pdam6,pplot12,by="NUNIDS.2")
pdam8<-merge(pdam7,pplot13,by="NUNIDS.2")
pdam9<-merge(pdam8,pplot14,by="NUNIDS.2")
pdam10<-merge(pdam9,pplot15,by="NUNIDS.2")
pdam11<-merge(pdam10,pplot17,by="NUNIDS.2")
pdam12<-merge(pdam11,pplot13a,by="NUNIDS.2")
pdam13<-merge(pdam12,pplot18,by="NUNIDS.2")
pdam14<-merge(pdam13,pplot19,by="NUNIDS.2")
pdam14b<-merge(pdam14,pplot19b,by="NUNIDS.2")
pdam15<-merge(pdam14b,pplot20,by="NUNIDS.2")
pdam15b<-merge(pdam15,pplot20b,by="NUNIDS.2")

pdam151<-merge(pdam15b,pplot22,by="NUNIDS.2")
pdam152<-merge(pdam151,pplot23,by="NUNIDS.2")
pdam153<-merge(pdam152,pplot24,by="NUNIDS.2")
pdam154<-merge(pdam153,pplot25,by="NUNIDS.2")
pdam155<-merge(pdam154,pplot26,by="NUNIDS.2")
pdam156<-merge(pdam155,pplot27,by="NUNIDS.2")
pdam157<-merge(pdam156,pplot28,by="NUNIDS.2")


pdam16<-merge(pdam157,pplot21,by="NUNIDS.2")



pdam16$rep_std<-ifelse(pdam16$STDAGE.2<=pdam16$REMPER.2,1,0)


#small table for MTBS only plots
pdam_MTBS<-merge(pdam16,pplot8,by="NUNIDS.2")
pdam_MTBS2<-merge(pdam_MTBS,pplot16,by="NUNIDS.2")

# tr<-tr[which(tr$STATUSCD.2==1),]

saveRDS(pdam_MTBS2,"nall_plots_with_disturb_MTBS.RDS")
saveRDS(pdam16,"nall_plots_with_disturb_FIA.RDS")
saveRDS(tr,"nall_trees_with_disturb_FIA.RDS")

# pdam_MTBS2<-readRDS("nall_plots_with_disturb_MTBS.RDS")
# 
# pdam16<-readRDS("nall_plots_with_disturb_FIA.RDS")


write.csv(pdam_MTBS2,"nall_plots_with_disturb_MTBS.csv")
write.csv(pdam16,"nall_plots_with_disturb_FIA.csv")


pdam17<-separate(pdam16, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)

# write.csv(pdam17,"all_plots_with_disturb_FIA_ECO1.csv")


## calculate plot level variables and disturbances

splot1<-aggregate(AGB.2~NUNIDS.2, trees_fire_11, FUN=sum)
splot2<-aggregate(AGB.1~NUNIDS.2, trees_fire_11, FUN=sum)
splot3<-aggregate(BALIVE.2~NUNIDS.2, trees_fire_11, FUN=mean)
splot4<-aggregate(BALIVE.1~NUNIDS.2, trees_fire_11, FUN=mean)
splot5<-data.frame(count(trees_fire_11,NUNIDS.2,TREEID))
splot55<-aggregate(n~NUNIDS.2, splot5, FUN=sum)
splot6<-data.frame(count(trees_fire_11,NUNIDS.2,STATUSCD.2))
splot7<-data.frame(count(trees_fire_11,NUNIDS.2,STATUSCD.1))
splot8<-aggregate(since_fire~NUNIDS.2, trees_fire_11, FUN=mean)
splot9<-aggregate(CUT~NUNIDS.2, trees_fire_11, FUN=mean)
splot10<-aggregate(FIRE_ALL~NUNIDS.2, trees_fire_11, FUN=mean)
splot11<-aggregate(INSDIS~NUNIDS.2, trees_fire_11, FUN=mean)
splot12<-aggregate(G_FIRE~NUNIDS.2, trees_fire_11, FUN=mean)
splot13<-aggregate(C_FIRE~NUNIDS.2, trees_fire_11, FUN=mean)

splot14<-aggregate(LAT.2~NUNIDS.2, trees_fire_11, FUN=mean)
splot15<-aggregate(LON.2~NUNIDS.2, trees_fire_11, FUN=mean)

splot16<-aggregate(severity~NUNIDS.2, trees_fire_11, FUN=mean)
splot161<-aggregate(sev_mean~NUNIDS.2, trees_fire_11, FUN=mean)

splot17<-aggregate(REMPER.2~NUNIDS.2, trees_fire_11, FUN=mean)
splot19<-aggregate(MEASYEAR.2~NUNIDS.2, trees_fire_11, FUN=max)
splot19b<-aggregate(MEASYEAR.1~NUNIDS.2, trees_fire_11, FUN=max)
splot20<-aggregate(STDAGE.2~NUNIDS.2, trees_fire_11, FUN=max)
splot20b<-aggregate(STDAGE.1~NUNIDS.2, trees_fire_11, FUN=max)

splot21<-aggregate(DISTURB~NUNIDS.2, trees_fire_11, FUN=mean)

splot22<-aggregate(STDORGCD.1~NUNIDS.2, trees_fire_11, FUN=max)
splot23<-aggregate(STDORGCD.2~NUNIDS.2, trees_fire_11, FUN=max)

splot24<-aggregate(FORTYPCD.1~NUNIDS.2, trees_fire_11, FUN=max)
splot25<-aggregate(FORTYPCD.2~NUNIDS.2, trees_fire_11, FUN=max)
splot26<-aggregate(ECOSUBCD.1~NUNIDS.2, trees_fire_11, FUN=max)


splot27<-aggregate(NUNID.1~NUNIDS.2, trees_fire_11, FUN=max)
splot28<-aggregate(NUNID.2~NUNIDS.2, trees_fire_11, FUN=max)



sdam1<-merge(splot1,splot2,by="NUNIDS.2")
sdam2<-merge(sdam1,splot3,by="NUNIDS.2")
sdam3<-merge(sdam2,splot4,by="NUNIDS.2")
sdam4<-merge(sdam3,splot9,by="NUNIDS.2")
sdam5<-merge(sdam4,splot10,by="NUNIDS.2")
sdam6<-merge(sdam5,splot11,by="NUNIDS.2")
sdam7<-merge(sdam6,splot12,by="NUNIDS.2")
sdam8<-merge(sdam7,splot13,by="NUNIDS.2")
sdam9<-merge(sdam8,splot14,by="NUNIDS.2")
sdam10<-merge(sdam9,splot15,by="NUNIDS.2")
sdam11<-merge(sdam10,splot17,by="NUNIDS.2")

sdam14<-merge(sdam11,splot19,by="NUNIDS.2")
sdam14b<-merge(sdam14,splot19b,by="NUNIDS.2")
sdam15<-merge(sdam14b,splot20,by="NUNIDS.2")
sdam15b<-merge(sdam15,splot20b,by="NUNIDS.2")

sdam151<-merge(sdam15b,splot22,by="NUNIDS.2")
sdam152<-merge(sdam151,splot23,by="NUNIDS.2")
sdam153<-merge(sdam152,splot24,by="NUNIDS.2")
sdam154<-merge(sdam153,splot25,by="NUNIDS.2")
sdam155<-merge(sdam154,splot26,by="NUNIDS.2")
sdam156<-merge(sdam155,splot27,by="NUNIDS.2")
sdam157<-merge(sdam156,splot28,by="NUNIDS.2")


sdam16<-merge(sdam157,splot21,by="NUNIDS.2")



sdam16$rep_std<-ifelse(sdam16$STDAGE.2<=sdam16$REMPER.2,1,0)


#small table for MTBS only plots
sdam_MTBS<-merge(sdam16,splot8,by="NUNIDS.2")
sdam_MTBS2<-merge(sdam_MTBS,splot16,by="NUNIDS.2")
sdam_MTBS3<-merge(sdam_MTBS2,splot161,by="NUNIDS.2")

saveRDS(sdam_MTBS3,"nnn2_all_plots_with_disturb_MTBS11022.RDS")

# mtbs_all<-readRDS("nall_plots_with_disturb_MTBS.RDS")
# 
# fia_all<-readRDS("nall_plots_with_disturb_FIA.RDS")

mtbs_all<-sdam_MTBS3
fia_all<-pdam16

both<-merge(mtbs_all,fia_all,by="NUNIDS.2",all=TRUE)

both2<-both[which(both$MEASYEAR.1.y>1998),]

tab222<-count(both2,FIRE_ALL.y,severity)

write.csv(tab222,"table_fire_nofire.csv")
tab333<-count(both2,G_FIRE.y,C_FIRE.y,severity)

write.csv(sdam_MTBS2,"nn2all_plots_with_disturb_MTBS1.csv")





# mtbs_plots<-readRDS("all_plots_with_disturb_MTBS.RDS")
# all_plots<-readRDS("all_plots_with_disturb_FIA.RDS")


tablea<-count(mtbs_plot,FIRE_ALL,severity)
tableb<-count(mtbs_plot,C_FIRE,G_FIRE,severity)

write.csv(tableb,"fire_MTBS_FIA2.csv")

##################

pdam_MTBS2$AGB.1<-(pdam_MTBS2$AGB.1*453.6*2.471)/1000000
pdam_MTBS2$AGB.2<-(pdam_MTBS2$AGB.2*453.6*2.471)/1000000
pdam_MTBS2$BALIVE.1<-(pdam_MTBS2$BALIVE.1*30.48*30.48*2.471)/10000
pdam_MTBS2$BALIVE.2<-(pdam_MTBS2$BALIVE.2*30.48*30.48*2.471)/10000

pdam_MTBS2$BALIVE_CH<-(pdam_MTBS2$BALIVE.2 - pdam_MTBS2$BALIVE.1)
pdam_MTBS2$AGB_CH<-(pdam_MTBS2$AGB.2 - pdam_MTBS2$AGB.1)


# seed<-read.csv("repeated_seedling_table.csv")
# seed$seedling.1<-(seed$seedling.1* 74.96 * 2.471)
# seed$seedling.2<-(seed$seedling.2* 74.96 * 2.471)




## merge with seedling

# trfrsd<-merge(trfr,seed,by.x="NUNIDS.2",by.y="NUNIDS",all=TRUE)
# tr_seed<-trfrsd[!is.na(trfrsd$seedling.1),]
# 
# pplot_ss<-aggregate(seedling.1~NUNIDS.2, tr_seed, FUN=sum)
# pplot_tt<-aggregate(seedling.2~NUNIDS.2, tr_seed, FUN=sum)
# 
# 
# pdam_MTBS3<-merge(pdam_MTBS2,pplot_ss,by="NUNIDS.2")
# pdam_MTBS4<-merge(pdam_MTBS3,pplot_tt,by="NUNIDS.2")
# 
# pdam_MTBS4$seedling_ch<-pdam_MTBS4$seedling.2-pdam_MTBS4$seedling.1


tr_0<-pdam_MTBS2[which(pdam_MTBS2$severity==0),]
tr_1<-pdam_MTBS2[which(pdam_MTBS2$severity==1),]




splot3<-aggregate(BALIVE.1~since_fire, tr_0, FUN=mean)
splot4<-aggregate(BALIVE.1~since_fire, tr_1, FUN=mean)
splot3b<-aggregate(BALIVE.2~since_fire, tr_0, FUN=mean)
splot4b<-aggregate(BALIVE.2~since_fire, tr_1, FUN=mean)
DBALIVE<-merge(splot3,splot4,by="since_fire")
DBALIVE1<-merge(DBALIVE,splot3b,by="since_fire")
DBALIVE2<-merge(DBALIVE1,splot4b,by="since_fire")

splot33<-aggregate(BALIVE_CH~since_fire, tr_0, FUN=mean)
splot34<-aggregate(BALIVE_CH~since_fire, tr_1, FUN=mean)
splot53<-aggregate(AGB_CH~since_fire, tr_0, FUN=mean)
splot54<-aggregate(AGB_CH~since_fire, tr_1, FUN=mean)

# splot73<-aggregate(seedling.1~since_fire, tr_0, FUN=mean)
# splot74<-aggregate(seedling.2~since_fire, tr_0, FUN=mean)
# splot75<-aggregate(seedling_ch~since_fire, tr_0, FUN=mean)
# 
# 
# splot83<-aggregate(seedling.1~since_fire, tr_1, FUN=mean)
# splot84<-aggregate(seedling.2~since_fire, tr_1, FUN=mean)
# splot85<-aggregate(seedling_ch~since_fire, tr_1, FUN=mean)




splot5<-aggregate(AGB.1~since_fire, tr_0, FUN=mean)
splot6<-aggregate(AGB.1~since_fire, tr_1, FUN=mean)
splot5b<-aggregate(AGB.2~since_fire, tr_0, FUN=mean)
splot6b<-aggregate(AGB.2~since_fire, tr_1, FUN=mean)
DAGB<-merge(splot5,splot6,by="since_fire")
DAGB1<-merge(DAGB,splot5b,by="since_fire")
DAGB2<-merge(DAGB1,splot6b,by="since_fire")



tr_0$type="0"
tr_1$type="1"

tr_both<-rbind(tr_0,tr_1)


ylow=0
yhigh=500

ylow1=-200
yhigh1=100
png("agb_all_plots_mtbs3.jpeg", width = 1200, height = 400)
par(xpd = T, mar = c(6,6,6,6))
par(mfrow=c(1,3))
p1<-ggplot(tr_both,aes(x=type,y=AGB.1,fill=type))+
  geom_boxplot(outlier.shape=NA)+
  scale_y_continuous(limits=c(ylow,yhigh))+
  geom_jitter(color="black", size=0.15, alpha=0.5)+ 
  geom_boxplot(aes(type, AGB.1, fill = type, colour = type), alpha = 0.2,lwd=0.8,outlier.shape=NA)+
  scale_fill_manual(values=c("gold2","red2")) +
  ggtitle("Pre-fire") +  
  scale_x_discrete(labels=c("0" = "Mild fire", "1" = "Severe fire"))+
  xlab("")+
  ylab("AGB (Mg/ha)")+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=13),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15)) 

p2<-ggplot(tr_both,aes(x=type,y=AGB.2,fill=type))+
  geom_boxplot(outlier.shape=NA)+
  scale_y_continuous(limits=c(ylow,yhigh))+
  geom_jitter(color="black", size=0.15, alpha=0.5)+ 
  geom_boxplot(aes(type, AGB.2, fill = type, colour = type), alpha = 0.2,lwd=0.8,outlier.shape=NA)+
  scale_fill_manual(values=c("gold2","red2")) +
  ggtitle("Post-fire") +  
  scale_x_discrete(labels=c("0" = "Mild fire", "1" = "Severe fire"))+
  xlab("")+
  ylab("AGB (Mg/ha)")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank())+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=13),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15)) 
p3<-ggplot(tr_both,aes(x=type,y=AGB_CH,fill=type))+
  geom_boxplot(outlier.shape=NA)+
  scale_y_continuous(limits=c(ylow1,yhigh1))+
  geom_jitter(color="black", size=0.15, alpha=0.5)+ 
  geom_boxplot(aes(type, AGB_CH, fill = type, colour = type), alpha = 0.2,lwd=0.8,outlier.shape=NA)+
  scale_fill_manual(values=c("gold2","red2")) +
  ggtitle("Change") +  
  scale_x_discrete(labels=c("0" = "Mild fire", "1" = "Severe fire"))+
  xlab("")+
  ylab("AGB (Mg/ha)")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank())+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=13),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15)) 


grid.arrange(p1,p2,p3,ncol=3)
dev.off()



png("agb_only_all_change_scatter_mtbs.jpeg", width = 600, height = 500)
par(xpd = F, mar = c(4,4,4,0.5))



plot(tr_0$since_fire,tr_0$AGB_CH,pch=19,cex=0.9,col="gold",
     ylim=c(ylow1,yhigh1),xlim=c(0,10),xlab = "Year since fire", cex.lab=1.4,ylab = "AGB (Mg/ha)")
par(new=TRUE)
plot(tr_1$since_fire,tr_1$AGB_CH,pch=19,cex=0.9,col="red2",
     ylim=c(ylow1,yhigh1),xlim=c(0,10),xlab = "", ylab = "")
par(new=TRUE)
plot(splot53[,1],splot53[,2],type="l",col="gold",ylim=c(ylow1,yhigh1),xlim=c(0,10),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(splot54[,1],splot54[,2],type="l",lwd=2,col="red2",ylim=c(ylow1,yhigh1),xlim=c(0,10),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("low severity","high severity"),
       cex = 1.2,
       pch = c(19,19),col=c("gold","red2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean low", "mean high"),
       col = c("gold", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")

dev.off()




ylow=0
yhigh=80

ylow1=-60
yhigh1=20
png("ba_all_mtbs3.jpeg", width = 1200, height = 400)
par(xpd = T, mar = c(6,6,6,6))
par(mfrow=c(1,3))

p4<-ggplot(tr_both,aes(x=type,y=BALIVE.1,fill=type))+
  geom_boxplot(outlier.shape=NA)+
  scale_y_continuous(limits=c(ylow,yhigh))+
  geom_jitter(color="black", size=0.15, alpha=0.5)+ 
  geom_boxplot(aes(type, BALIVE.1, fill = type, colour = type), alpha = 0.2,lwd=0.8,outlier.shape=NA)+
  scale_fill_manual(values=c("gold2","red2")) +
  ggtitle("Pre-fire") +  
  scale_x_discrete(labels=c("0" = "Mild fire", "1" = "Severe fire"))+
  xlab("")+
  ylab("Basal area (M2/ha)")+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=13),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15)) 

p5<-ggplot(tr_both,aes(x=type,y=BALIVE.2,fill=type))+
  geom_boxplot(outlier.shape=NA)+
  scale_y_continuous(limits=c(ylow,yhigh))+
  geom_jitter(color="black", size=0.15, alpha=0.5)+ 
  geom_boxplot(aes(type, BALIVE.2, fill = type, colour = type), alpha = 0.2,lwd=0.8,outlier.shape=NA)+
  scale_fill_manual(values=c("gold2","red2")) +
  ggtitle("Post-fire") +  
  scale_x_discrete(labels=c("0" = "Mild fire", "1" = "Severe fire"))+
  xlab("")+
  ylab("Basal Area (M2/ha)")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank())+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=13),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15)) 
p6<-ggplot(tr_both,aes(x=type,y=BALIVE_CH,fill=type))+
  geom_boxplot(outlier.shape=NA)+
  scale_y_continuous(limits=c(ylow1,yhigh1))+
  geom_jitter(color="black", size=0.15, alpha=0.5)+ 
  geom_boxplot(aes(type, BALIVE_CH, fill = type, colour = type), alpha = 0.2,lwd=0.8,outlier.shape=NA)+
  scale_fill_manual(values=c("gold2","red2")) +
  ggtitle("Change") +  
  scale_x_discrete(labels=c("0" = "Mild fire", "1" = "Severe fire"))+
  xlab("")+
  ylab("Basal area (M2/ha)")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank())+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=13),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15)) 


grid.arrange(p4,p5,p6,ncol=3)
dev.off()


png("ba_only_change_scatter_mtbs.jpeg", width = 600, height = 500)
par(xpd = F, mar = c(4,4,4,0.5))



plot(tr_0$since_fire,tr_0$BALIVE_CH,pch=19,cex=0.9,col="gold",
     ylim=c(ylow1,yhigh1),xlim=c(0,10),xlab = "Year since fire", cex.lab=1.4,
     ylab = "Basal Area (M2/ha)")
par(new=TRUE)
plot(tr_1$since_fire,tr_1$BALIVE_CH,pch=19,cex=0.9,col="red2",
     ylim=c(ylow1,yhigh1),xlim=c(0,10),xlab = "", ylab = "")
par(new=TRUE)
plot(splot33[,1],splot33[,2],type="l",col="gold",ylim=c(ylow1,yhigh1),xlim=c(0,10),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(splot34[,1],splot34[,2],type="l",lwd=2,col="red2",ylim=c(ylow1,yhigh1),xlim=c(0,10),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("low severity","high severity"),
       cex = 1.2,
       pch = c(19,19),col=c("gold","red2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean low", "mean high"),
       col = c("gold", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")



dev.off()




ylow=500
yhigh=50000

ylow1=-200
yhigh1=200
png("agb_all_mtbs3.jpeg", width = 1200, height = 400)
par(xpd = T, mar = c(6,6,6,6))
par(mfrow=c(1,3))

ggplot(tr_both,aes(x=type,y=seedling.1,fill=type))+
  geom_boxplot(outlier.shape=NA)+
  scale_y_continuous(limits=c(ylow,yhigh))+
  geom_jitter(color="black", size=0.15, alpha=0.5)+ 
  geom_boxplot(aes(type, seedling.1, fill = type, colour = type), alpha = 0.2,lwd=0.8,outlier.shape=NA)+
  scale_fill_manual(values=c("gold2","red2")) +
  ggtitle("Pre-fire") +  
  scale_x_discrete(labels=c("0" = "Mild fire", "1" = "Severe fire"))+
  xlab("")+
  ylab("AGB (Mg/ha)")+
  theme(legend.position="none")+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=13),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15)) 

ggplot(tr_both,aes(x=type,y=seedling.2,fill=type))+
  geom_boxplot(outlier.shape=NA)+
  scale_y_continuous(limits=c(ylow,yhigh))+
  geom_jitter(color="black", size=0.15, alpha=0.5)+ 
  geom_boxplot(aes(type, seedling.2, fill = type, colour = type), alpha = 0.2,lwd=0.8,outlier.shape=NA)+
  scale_fill_manual(values=c("gold2","red2")) +
  ggtitle("Post-fire") +  
  scale_x_discrete(labels=c("0" = "Mild fire", "1" = "Severe fire"))+
  xlab("")+
  ylab("AGB (Mg/ha)")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank())+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=13),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15)) 
p3<-ggplot(tr_both,aes(x=type,y=seedling_ch,fill=type))+
  geom_boxplot(outlier.shape=NA)+
  scale_y_continuous(limits=c(ylow1,yhigh1))+
  geom_jitter(color="black", size=0.15, alpha=0.5)+ 
  geom_boxplot(aes(type, seedling_ch, fill = type, colour = type), alpha = 0.2,lwd=0.8,outlier.shape=NA)+
  scale_fill_manual(values=c("gold2","red2")) +
  ggtitle("Change") +  
  scale_x_discrete(labels=c("0" = "Mild fire", "1" = "Severe fire"))+
  xlab("")+
  ylab("AGB (Mg/ha)")+
  theme(legend.position="none")+
  theme(axis.title.y=element_blank())+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=13),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15)) 


grid.arrange(p1,p2,p3,ncol=3)
dev.off()



png("agb_only_change_scatter_mtbs.jpeg", width = 600, height = 500)
par(xpd = F, mar = c(4,4,4,0.5))



plot(tr_0$since_fire,tr_0$AGB_CH,pch=19,cex=0.9,col="gold",
     ylim=c(ylow1,yhigh1),xlim=c(0,10),xlab = "Year since fire", cex.lab=1.4,ylab = "AGB (Mg/ha)")
par(new=TRUE)
plot(tr_1$since_fire,tr_1$AGB_CH,pch=19,cex=0.9,col="red2",
     ylim=c(ylow1,yhigh1),xlim=c(0,10),xlab = "", ylab = "")
par(new=TRUE)
plot(splot53[,1],splot53[,2],type="l",col="gold",ylim=c(ylow1,yhigh1),xlim=c(0,10),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(splot54[,1],splot54[,2],type="l",lwd=2,col="red2",ylim=c(ylow1,yhigh1),xlim=c(0,10),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("low severity","high severity"),
       cex = 1.2,
       pch = c(19,19),col=c("gold","red2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean low", "mean high"),
       col = c("gold", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")



dev.off()











seed<-read.csv("repeated_seedling_table.csv")

pdam15$AGB.1<-(pdam15$AGB.1*453.6*2.471)/1000000
pdam15$AGB.2<-(pdam15$AGB.2*453.6*2.471)/1000000
pdam15$BALIVE.1<-(pdam15$BALIVE.1*30.48*30.48*2.471)/10000
pdam15$BALIVE.2<-(pdam15$BALIVE.2*30.48*30.48*2.471)/10000


## merge with seedling
pdam16<-merge(seed,pdam15,by.x="NUNIDS",by.y="NUNIDS.2",all.x=TRUE)

pdam16<-pdam16[!is.na(pdam16$DISTURB),]

pdam16$seedling.1<-(pdam16$seedling.1* 74.96 * 2.471)
pdam16$seedling.2<-(pdam16$seedling.2* 74.96 * 2.471)



library(cowplot)
library(lattice)






xaxis1<-c(1:10)
dev.off()
dev.new()
ggplot(DAGB2,aes(DAGB2[,1])) +
  geom_line(aes(y=DAGB2[,2],colour="yellow"),size=1) +
  geom_line(aes(y=DAGB2[,3], colour="red2"),size=1) +
  scale_color_manual(name="Fire",values=c("yellow"="gold", "red2"="red2"),
                     labels=c("severe","mild"))+
  scale_y_continuous(name="AGB (lb/ac)", limits=c(0, 5000))+
  scale_x_continuous(name="Year since fire", breaks=xaxis1)+
  ggtitle("First inventory")+
  theme_bw()

b5<-ggplot(DAGB2,aes(DAGB2[,1])) +
  geom_line(aes(y=DAGB2[,4],colour="yellow"),size=1) +
  geom_line(aes(y=DAGB2[,5], colour="red2"),size=1) +
  scale_color_manual(name="Fire",values=c("yellow"="gold", "red2"="red2"),
                     labels=c("severe","mild"))+
  scale_y_continuous(name="AGB (lb/ac)", limits=c(0, 5000))+
  scale_x_continuous(name="Year since fire", breaks=xaxis1)+
  ggtitle("Second inventory")+
  
  theme_bw()

b6<-grid.arrange(b4,b5,nrow=1)





trall_0<-pdam15[which(pdam15$INSDIS==0),]
trall_1<-pdam15[which(pdam15$INSDIS==1),]


trall_1a<-pdam15[which(pdam15$INSDIS==1 & pdam15$rep_stand_all==0),]
trall_1b<-pdam15[which(pdam15$INSDIS==1 & pdam15$rep_stand_all==1),]


wplot3<-aggregate(AGB.1~REMPER.2, trall_0, FUN=mean)
wplot4<-aggregate(AGB.1~REMPER.2, trall_1, FUN=mean)
wplot5a<-aggregate(AGB.1~REMPER.2, trall_1a, FUN=mean)
wplot5b<-aggregate(AGB.1~REMPER.2, trall_1b, FUN=mean)


wplot6<-aggregate(AGB.2~REMPER.2, trall_0, FUN=mean)
wplot7<-aggregate(AGB.2~REMPER.2, trall_1, FUN=mean)
wplot7a<-aggregate(AGB.2~REMPER.2, trall_1a, FUN=mean)
wplot7b<-aggregate(AGB.2~REMPER.2, trall_1b, FUN=mean)




png("agb_second_inventory_insdis.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(trall_0$REMPER.2,trall_0$AGB.2,pch=19,cex=0.9,col="green2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "Year since fire", ylab = "AGB (lb/ac)")
par(new=TRUE)
plot(trall_1$REMPER.2,trall_1$AGB.2,pch=19,cex=0.9,col="orange2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(wplot6[,1],wplot6[,2],type="l",col="forestgreen",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(wplot7[,1],wplot7[,2],type="l",lwd=2,col="red2",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no insect/disease","insect/disease"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","orange2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no insect/disease", "mean insect/disease"),
       col = c("forestgreen", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1)

dev.off()


png("agb_first_inventory_disins_stands1.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))


plot(trall_0$REMPER.2,trall_0$AGB.1,pch=19,cex=0.9,col="green2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "Year since fire", ylab = "AGB (lb/ac)")
par(new=TRUE)
plot(trall_1a$REMPER.2,trall_1a$AGB.1,pch=19,cex=0.9,col="yellow1",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(trall_1b$REMPER.2,trall_1b$AGB.1,pch=19,cex=0.9,col="chocolate1",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(wplot3[,1],wplot3[,2],type="l",col="forestgreen",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2.5)
par(new=TRUE)
plot(wplot5a[,1],wplot5a[,2],type="l",lwd=2.5,col="gold3",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")
par(new=TRUE)
plot(wplot5b[,1],wplot5b[,2],type="l",lwd=2.5,col="red2",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no fire","fire (low disturbance)","fire (stand replacing)"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","yellow1","chocolate1"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no fire", "mean low fire","mean stand replacing fire"),
       col = c("forestgreen", "gold3","red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("First Inventory", line = -1)

dev.off()



kplot3<-aggregate(BALIVE.1~REMPER.2, trall_0, FUN=mean)
kplot4<-aggregate(BALIVE.1~REMPER.2, trall_1, FUN=mean)
kplot5a<-aggregate(BALIVE.1~REMPER.2, trall_1a, FUN=mean)
kplot5b<-aggregate(BALIVE.1~REMPER.2, trall_1b, FUN=mean)


kplot6<-aggregate(BALIVE.2~REMPER.2, trall_0, FUN=mean)
kplot7<-aggregate(BALIVE.2~REMPER.2, trall_1, FUN=mean)
kplot7a<-aggregate(BALIVE.2~REMPER.2, trall_1a, FUN=mean)
kplot7b<-aggregate(BALIVE.2~REMPER.2, trall_1b, FUN=mean)




png("balive_first_inventory_insdis1.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(trall_0$REMPER.2,trall_0$BALIVE.1,pch=19,cex=0.9,col="green2",
     ylim=c(0,1000),xlim=c(6,15),xlab = "Year since fire", ylab = "Basal Area (sq. ft/ac)")
par(new=TRUE)
plot(trall_1$REMPER.2,trall_1$BALIVE.1,pch=19,cex=0.9,col="orange2",
     ylim=c(0,1000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(kplot3[,1],kplot3[,2],type="l",col="forestgreen",ylim=c(0,1000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(kplot4[,1],kplot4[,2],type="l",lwd=2,col="red2",ylim=c(0,1000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no insect/disease","insect/disease"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","orange2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no insect/disease", "mean insect/disease"),
       col = c("forestgreen", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("First Inventory", line = -1)

dev.off()


trall_0<-pdam15[which(pdam15$FIRE_ALL==0),]
trall_1<-pdam15[which(pdam15$FIRE_ALL==1),]


trall_1a<-pdam15[which(pdam15$FIRE_ALL==1 & pdam15$rep_stand_all==0),]
trall_1b<-pdam15[which(pdam15$FIRE_ALL==1 & pdam15$rep_stand_all==1),]




fplot3<-aggregate(BALIVE.1~REMPER.2, trall_0, FUN=mean)
fplot4<-aggregate(BALIVE.1~REMPER.2, trall_1, FUN=mean)
fplot5a<-aggregate(BALIVE.1~REMPER.2, trall_1a, FUN=mean)
fplot5b<-aggregate(BALIVE.1~REMPER.2, trall_1b, FUN=mean)


fplot6<-aggregate(BALIVE.2~REMPER.2, trall_0, FUN=mean)
fplot7<-aggregate(BALIVE.2~REMPER.2, trall_1, FUN=mean)
fplot7a<-aggregate(BALIVE.2~REMPER.2, trall_1a, FUN=mean)
fplot7b<-aggregate(BALIVE.2~REMPER.2, trall_1b, FUN=mean)


gplot3<-aggregate(AGB.1~REMPER.2, trall_0, FUN=mean)
gplot4<-aggregate(AGB.1~REMPER.2, trall_1, FUN=mean)
gplot5a<-aggregate(AGB.1~REMPER.2, trall_1a, FUN=mean)
gplot5b<-aggregate(AGB.1~REMPER.2, trall_1b, FUN=mean)


gplot6<-aggregate(AGB.2~REMPER.2, trall_0, FUN=mean)
gplot7<-aggregate(AGB.2~REMPER.2, trall_1, FUN=mean)
gplot7a<-aggregate(AGB.2~REMPER.2, trall_1a, FUN=mean)
gplot7b<-aggregate(AGB.2~REMPER.2, trall_1b, FUN=mean)






png("agb_first_inventory_fire1.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(trall_0$REMPER.2,trall_0$AGB.1,pch=19,cex=0.9,col="green2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "Year since fire", ylab = "AGB (lb/ac)")
par(new=TRUE)
plot(trall_1$REMPER.2,trall_1$AGB.1,pch=19,cex=0.9,col="orange2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(gplot3[,1],gplot3[,2],type="l",col="forestgreen",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(gplot4[,1],gplot4[,2],type="l",lwd=2,col="red2",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no fire","fire"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","orange2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no fire", "mean fire"),
       col = c("forestgreen", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("First Inventory", line = -1)

dev.off()

## fire for stand replacing


png("balive_first_inventory_fire_stands1.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))


plot(trall_0$REMPER.2,trall_0$BALIVE.1,pch=19,cex=0.9,col="green2",
     ylim=c(0,1000),xlim=c(6,15),xlab = "Year since fire", ylab = "Basal area (sq. ft/ac)")
par(new=TRUE)
plot(trall_1a$REMPER.2,trall_1a$BALIVE.1,pch=19,cex=0.9,col="yellow1",
     ylim=c(0,1000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(trall_1b$REMPER.2,trall_1b$BALIVE.1,pch=19,cex=0.9,col="chocolate1",
     ylim=c(0,1000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(fplot3[,1],fplot3[,2],type="l",col="forestgreen",ylim=c(0,1000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2.5)
par(new=TRUE)
plot(fplot5a[,1],fplot5a[,2],type="l",lwd=2.5,col="gold3",ylim=c(0,1000),xlim=c(6,15),
     xlab = "", ylab = "")
par(new=TRUE)
plot(fplot5b[,1],fplot5b[,2],type="l",lwd=2.5,col="red2",ylim=c(0,1000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no fire","fire (low disturbance)","fire (stand replacing)"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","yellow1","chocolate1"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no fire", "mean low fire","mean stand replacing fire"),
       col = c("forestgreen", "gold3","red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("First Inventory", line = -1)

dev.off()




### codes for harvest/cut

trall_0<-pdam15[which(pdam15$CUT==0),]
trall_1<-pdam15[which(pdam15$CUT==1),]


trall_1a<-pdam15[which(pdam15$CUT==1 & pdam15$rep_stand_all==0),]
trall_1b<-pdam15[which(pdam15$CUT==1 & pdam15$rep_stand_all==1),]






hplot3<-aggregate(BALIVE.1~REMPER.2, trall_0, FUN=mean)
hplot4<-aggregate(BALIVE.1~REMPER.2, trall_1, FUN=mean)
hplot5a<-aggregate(BALIVE.1~REMPER.2, trall_1a, FUN=mean)
hplot5b<-aggregate(BALIVE.1~REMPER.2, trall_1b, FUN=mean)


hplot6<-aggregate(BALIVE.2~REMPER.2, trall_0, FUN=mean)
hplot7<-aggregate(BALIVE.2~REMPER.2, trall_1, FUN=mean)
hplot7a<-aggregate(BALIVE.2~REMPER.2, trall_1a, FUN=mean)
hplot7b<-aggregate(BALIVE.2~REMPER.2, trall_1b, FUN=mean)


iplot3<-aggregate(AGB.1~REMPER.2, trall_0, FUN=mean)
iplot4<-aggregate(AGB.1~REMPER.2, trall_1, FUN=mean)
iplot5a<-aggregate(AGB.1~REMPER.2, trall_1a, FUN=mean)
iplot5b<-aggregate(AGB.1~REMPER.2, trall_1b, FUN=mean)


iplot6<-aggregate(AGB.2~REMPER.2, trall_0, FUN=mean)
iplot7<-aggregate(AGB.2~REMPER.2, trall_1, FUN=mean)
iplot7a<-aggregate(AGB.2~REMPER.2, trall_1a, FUN=mean)
iplot7b<-aggregate(AGB.2~REMPER.2, trall_1b, FUN=mean)





png("agb_second_inventory_cut.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))

plot(trall_0$REMPER.2,trall_0$AGB.2,pch=19,cex=0.9,col="green2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "Year since fire", ylab = "AGB (lb/ac)")
par(new=TRUE)
plot(trall_1$REMPER.2,trall_1$AGB.2,pch=19,cex=0.9,col="orange2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(iplot6[,1],iplot6[,2],type="l",col="forestgreen",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2)
par(new=TRUE)
plot(iplot7[,1],iplot7[,2],type="l",lwd=2,col="red2",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no fire","fire"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","orange2"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no fire", "mean fire"),
       col = c("forestgreen", "red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1)

dev.off()

## fire for stand replacing


png("agb_second_inventory_cut_stands1.jpeg", width = 700, height = 600)
par(xpd = T, mar = c(6,6,6,6))


plot(trall_0$REMPER.2,trall_0$AGB.2,pch=19,cex=0.9,col="green2",
     ylim=c(0,30000),xlim=c(6,15),xlab = "Year since fire", ylab = "AGB (lb/ac)")
par(new=TRUE)
plot(trall_1a$REMPER.2,trall_1a$AGB.2,pch=19,cex=0.9,col="yellow1",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(trall_1b$REMPER.2,trall_1b$AGB.2,pch=19,cex=0.9,col="chocolate1",
     ylim=c(0,30000),xlim=c(6,15),xlab = "", ylab = "")
par(new=TRUE)
plot(iplot6[,1],iplot6[,2],type="l",col="forestgreen",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "",lwd=2.5)
par(new=TRUE)
plot(iplot7a[,1],iplot7a[,2],type="l",lwd=2.5,col="gold3",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")
par(new=TRUE)
plot(iplot7b[,1],iplot7b[,2],type="l",lwd=2.5,col="red2",ylim=c(0,30000),xlim=c(6,15),
     xlab = "", ylab = "")

legend("topleft",inset=c(0.01,-0.15),
       c("no fire","fire (low disturbance)","fire (stand replacing)"),
       cex = 1.2,
       pch = c(19,19),col=c("green2","yellow1","chocolate1"),
       xpd=TRUE,bty="n")

legend("topleft",inset=c(0.4,-0.15),
       c("mean no fire", "mean low fire","mean stand replacing fire"),
       col = c("forestgreen", "gold3","red2"),
       cex = 1.2,
       lwd = 1, lty = 1,xpd=TRUE,bty="n")
title("Second Inventory", line = -1)

dev.off()






xaxis1<-c(8:20)

b1<-ggplot(WDBALIVE2,aes(WDBALIVE2[,1])) +
  geom_line(aes(y=WDBALIVE2[,2],colour="yellow"),size=1) +
  geom_line(aes(y=WDBALIVE2[,3], colour="red2"),size=1) +
  scale_color_manual(name="Fire",values=c("yellow"="green", "red2"="red2"),
                     labels=c("cut","no cut"))+
  scale_y_continuous(name="AGB (lb/ac)", limits=c(0, 8000))+
  scale_x_continuous(name="Time between measurements (REMPER)", breaks=xaxis1)+
  ggtitle("First inventory")+
  theme_bw()

b2<-ggplot(WDBALIVE2,aes(WDBALIVE2[,1])) +
  geom_line(aes(y=WDBALIVE2[,4],colour="yellow"),size=1) +
  geom_line(aes(y=WDBALIVE2[,5], colour="red2"),size=1) +
  scale_color_manual(name="Fire",values=c("yellow"="green", "red2"="red2"),
                     labels=c("cut","no cut"))+
  scale_y_continuous(name="AGB (lb/ac)", limits=c(0, 8000))+
  scale_x_continuous(name="Time between measurements (REMPER)", breaks=xaxis1)+
  ggtitle("Second inventory")+
  
  theme_bw()

b3<-grid.arrange(b1,b2,nrow=1)





splot5<-aggregate(BALIVE_CH~since_fire, tr_0, FUN=mean)
splot6<-aggregate(BALIVE_CH~since_fire, tr_1, FUN=mean)
plot(splot3[,2])

BALIVE_CH<-merge(splot5,splot6,by="since_fire")
ggplot(BALIVE_CH, aes(BALIVE_CH[,1])) +
  geom_line(aes(y=BALIVE_CH[,2]), colour="orange") +
  geom_line(aes(y=BALIVE_CH[,3]), colour="red2") + theme_bw()



tr_fire_0<-tr[which(tr$FIRE==0),]
tr_fire_1<-tr[which(tr$FIRE==1),]
splot7<-aggregate(AGBCH_ACR~since_fire, tr_fire_0, FUN=mean)
splot8<-aggregate(AGBCH_ACR~since_fire, tr_fire_1, FUN=mean)

AGBch1<-merge(splot7,splot8,by="since_fire")
ggplot(AGBch1, aes(AGBch1[,1])) +
  geom_line(aes(y=AGBch1[,2]), colour="orange") +
  geom_line(aes(y=AGBch1[,3]), colour="red2") + theme_bw()


splot7b<-aggregate(BALIVE_CH~since_fire, tr_fire_0, FUN=mean)
splot8b<-aggregate(BALIVE_CH~since_fire, tr_fire_1, FUN=mean)

Balivech1<-merge(splot7b,splot8b,by="since_fire")
ggplot(Balivech1, aes(Balivech1[,1])) +
  geom_line(aes(y=Balivech1[,2]), colour="orange") +
  geom_line(aes(y=Balivech1[,3]), colour="red2") + theme_bw()


tr_cut_0<-tr[which(tr$CUT==0),]
tr_cut_1<-tr[which(tr$CUT==1),]
splot9<-aggregate(AGBCH_ACR~since_fire, tr_cut_0, FUN=mean)
splot10<-aggregate(AGBCH_ACR~since_fire, tr_cut_1, FUN=mean)

AGBch2<-merge(splot9,splot10,by="since_fire")
ggplot(AGBch2, aes(AGBch2[,1])) +
  geom_line(aes(y=AGBch2[,2]), colour="orange") +
  geom_line(aes(y=AGBch2[,3]), colour="red2") + theme_bw()

splot9b<-aggregate(BALIVE_CH~since_fire, tr_cut_0, FUN=mean)
splot10b<-aggregate(BALIVE_CH~since_fire, tr_cut_1, FUN=mean)

Balivech2<-merge(splot9b,splot10b,by="since_fire")
ggplot(Balivech2, aes(Balivech2[,1])) +
  geom_line(aes(y=Balivech2[,2]), colour="orange") +
  geom_line(aes(y=Balivech2[,3]), colour="red2") + theme_bw()


tr_dis_0<-tr[which(tr$DISEASE==0),]
tr_dis_1<-tr[which(tr$DISEASE==1),]
splot11<-aggregate(AGBCH_ACR~since_fire, tr_dis_0, FUN=mean)
splot12<-aggregate(AGBCH_ACR~since_fire, tr_dis_1, FUN=mean)

AGBch3<-merge(splot11,splot12,by="since_fire")
ggplot(AGBch3, aes(AGBch3[,1])) +
  geom_line(aes(y=AGBch3[,2]), colour="orange") +
  geom_line(aes(y=AGBch3[,3]), colour="red2") + theme_bw()

