### plots based on disturbances from FIA data

setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000)


dam1<- read.csv("../gis_data/plots_with_climate_variables.csv")


sr<-read.csv("../../data/COND.csv")

srp<-read.csv("../../data/PLOT.csv")


sr$NUNID <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                   sr$PLOT,"-",sr$INVYR)

sr$NUNIDS <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",sr$PLOT)

sss<-count(sr,NUNIDS)

firstcond<-sr %>% group_by(NUNID) %>% 
  filter(abs(CONDPROP_UNADJ) == max(CONDPROP_UNADJ)) 

sr<-firstcond

rm(firstcond)

sr$dist1<-ifelse(sr$DSTRBCD1>0 | sr$DSTRBCD2>0 | sr$DSTRBCD3>0,1,0)

sr$cd1<-ifelse(sr$DSTRBCD1==30|sr$DSTRBCD1==31 | sr$DSTRBCD1==32,2,
               ifelse(sr$DSTRBCD1<23 & sr$DSTRBCD1>9,3,
              ifelse(sr$DSTRBCD1==54,4,ifelse(sr$DSTRBCD1>0,5,0))))
sr$cd1[is.na(sr$cd1)]<-0

sr$cd2<-ifelse(sr$DSTRBCD2==30|sr$DSTRBCD2==31 | sr$DSTRBCD2==32,2,
               ifelse(sr$DSTRBCD2<23 & sr$DSTRBCD2>9,3,
              ifelse(sr$DSTRBCD2==54,4,ifelse(sr$DSTRBCD2>0,5,0))))
sr$cd2[is.na(sr$cd2)]<-0

sr$cd3<-ifelse(sr$DSTRBCD3==30|sr$DSTRBCD3==31 | sr$DSTRBCD3==32,2,
               ifelse(sr$DSTRBCD3<23 & sr$DSTRBCD3>9,3,
               ifelse(sr$DSTRBCD3==54,4,ifelse(sr$DSTRBCD3>0,5,0))))
sr$cd3[is.na(sr$cd3)]<-0

sr$tcd1<-ifelse(sr$TRTCD1==10,6,ifelse(sr$TRTCD1>10,7,0))
sr$tcd1[is.na(sr$tcd1)]<-0

sr$tcd2<-ifelse(sr$TRTCD2==10,6,ifelse(sr$TRTCD2>10,7,0))
sr$tcd2[is.na(sr$tcd2)]<-0

sr$tcd3<-ifelse(sr$TRTCD3==10,6,ifelse(sr$TRTCD3>10,7,0))
sr$tcd3[is.na(sr$tcd3)]<-0

sr$distrt<-interaction(sr$cd1,sr$cd2,sr$cd3,sr$tcd1,sr$tcd2,sr$tcd3)

srp$NUNID <- paste0(srp$STATECD,"-",srp$UNITCD,"-",srp$COUNTYCD,"-",
                   srp$PLOT,"-",srp$INVYR)

srp$NUNIDS <- paste0(srp$STATECD,"-",srp$UNITCD,"-",srp$COUNTYCD,"-",srp$PLOT)

cn0<-count(sr,NUNID)
cn1<-aggregate(PHYSCLCD~NUNID,data=sr,FUN=max)
cn2<-aggregate(SLOPE~NUNID,data=sr,FUN=max)
cn3<-aggregate(ASPECT~NUNID,data=sr,FUN=max)
cn4<-aggregate(dist1~NUNID,data=sr,FUN=max)
cn5<-count(sr,NUNID,distrt)

saveRDS(cn5,"disturbance_plots_all_years11.RDS")

cn_all1<-merge(cn1,cn2,by="NUNID",all=TRUE)
cn_all2<-merge(cn_all1,cn3,by="NUNID",all=TRUE)
cn_all3<-merge(cn_all2,cn4,by="NUNID",all=TRUE)
cn_all4<-merge(cn_all3,cn5,by="NUNID",all=TRUE)


cn_all5<-cn_all4[,1:6]
dam21<-merge(dam1,cn_all5,by.x="NUNID_1",by.y="NUNID",all.x=TRUE)
dam22<-merge(dam21,cn_all5,by.x="NUNID_2",by.y="NUNID",all.x=TRUE)


pl1<-aggregate(ECOSUBCD~NUNIDS,data=srp,FUN=max)
pl2<-aggregate(ELEV~NUNIDS,data=srp,FUN=max)
pl_all<-merge(pl1,pl2,by="NUNIDS",all=TRUE)

dam23<-merge(dam22,pl_all,by.x="NUNIDS_2",by.y="NUNIDS",all.x=TRUE)

dam23$distrtc.y <- dam23$distrt.y
dam23$distrtc.x <- dam23$distrt.x

dam24<-separate(dam23,"distrtc.y", c("cd1.2", "cd2.2","cd3.2","tcd1.2","tcd2.2","tcd3.2"), sep = "\\.")

dam25<-separate(dam24,"distrtc.x", c("cd1.1", "cd2.1","cd3.1","tcd1.1","tcd2.1","tcd3.1"), sep = "\\.")


dam25$dist_codes<-
  
  ifelse(dam25$distrt.y=="0.0.0.0.0.0","ND",
  ifelse(dam25$distrt.y=="2.0.0.0.0.0"|dam25$distrt.y=="2.2.0.0.0.0","F",
  ifelse(dam25$distrt.y=="3.2.0.0.0.0"|dam25$distrt.y=="2.3.0.0.0.0","IF",
  ifelse(dam25$distrt.y=="5.2.0.0.0.0" | dam25$distrt.y=="2.5.0.0.0.0","OF",
  ifelse(dam25$distrt.y=="2.0.0.6.0.0","FC",
  ifelse(dam25$distrt.y=="2.0.0.6.7.0"|dam25$distrt.y=="2.0.0.6.7.7","FCT",
  ifelse(dam25$distrt.y=="2.0.0.7.0.0","FT",
  ifelse(dam25$distrt.y=="3.0.0.0.0.0"|dam25$distrt.y=="3.3.0.0.0.0"|dam25$distrt.y=="3.3.3.0.0.0","I",
  ifelse(dam25$distrt.y=="3.4.0.0.0.0"|dam25$distrt.y=="4.3.0.0.0.0","ID",
  ifelse(dam25$distrt.y=="3.3.5.0.0.0"|dam25$distrt.y=="5.3.0.0.0.0"|dam25$distrt.y=="3.5.0.0.0.0"|
           dam25$distrt.y=="5.3.3.0.0.0","IO",
  ifelse(dam25$distrt.y=="3.0.0.6.0.0"|dam25$distrt.y=="3.3.0.6.0.0","IC", 
  ifelse(dam25$distrt.y=="3.0.0.6.7.0","ICT",
  ifelse(dam25$distrt.y=="3.5.0.6.0.0","IOC",
  ifelse(dam25$distrt.y=="3.0.0.7.0.0","IT",
  ifelse(dam25$distrt.y=="0.0.0.6.0.0"| dam25$distrt.y=="0.0.0.6.6.0","C",
  ifelse(dam25$distrt.y=="0.0.0.6.7.0"|dam25$distrt.y=="0.0.0.7.6.0"|dam25$distrt.y=="0.0.0.6.7.7","CT",
  ifelse(dam25$distrt.y=="5.0.0.6.0.0"|dam25$distrt.y=="5.5.0.6.0.0","OC",
  ifelse(dam25$distrt.y=="5.0.0.6.7.0"|dam25$distrt.y=="5.0.0.6.7.7","OCT", 
  ifelse(dam25$distrt.y=="4.0.0.0.0.0","D",
                "OOO")))))))))))))))))))



dam25$dist_codes_prev<-
  
  ifelse(dam25$distrt.x=="0.0.0.0.0.0","ND",
  ifelse(dam25$distrt.x=="2.0.0.0.0.0"|dam25$distrt.x=="2.2.0.0.0.0","F",
  ifelse(dam25$distrt.x=="3.2.0.0.0.0"|dam25$distrt.x=="2.3.0.0.0.0","IF",
  ifelse(dam25$distrt.x=="5.2.0.0.0.0" | dam25$distrt.x=="2.5.0.0.0.0","OF",
  ifelse(dam25$distrt.x=="2.0.0.6.0.0","FC",
  ifelse(dam25$distrt.x=="2.0.0.6.7.0"|dam25$distrt.x=="2.0.0.6.7.7","FCT",
  ifelse(dam25$distrt.x=="2.0.0.7.0.0","FT",
  ifelse(dam25$distrt.x=="3.0.0.0.0.0"|dam25$distrt.x=="3.3.0.0.0.0"|dam25$distrt.x=="3.3.3.0.0.0","I",
  ifelse(dam25$distrt.x=="3.4.0.0.0.0"|dam25$distrt.x=="4.3.0.0.0.0","ID",
  ifelse(dam25$distrt.x=="3.3.5.0.0.0"|dam25$distrt.x=="5.3.0.0.0.0"|dam25$distrt.x=="3.5.0.0.0.0"|
      dam25$distrt.x=="5.3.3.0.0.0","IO",
  ifelse(dam25$distrt.x=="3.0.0.6.0.0"|dam25$distrt.x=="3.3.0.6.0.0","IC", 
  ifelse(dam25$distrt.x=="3.0.0.6.7.0","ICT",
  ifelse(dam25$distrt.x=="3.5.0.6.0.0","IOC",
  ifelse(dam25$distrt.x=="3.0.0.7.0.0","IT",
  ifelse(dam25$distrt.x=="0.0.0.6.0.0"| dam25$distrt.x=="0.0.0.6.6.0","C",
  ifelse(dam25$distrt.x=="0.0.0.6.7.0"|dam25$distrt.x=="0.0.0.7.6.0"|dam25$distrt.x=="0.0.0.6.7.7","CT",
  ifelse(dam25$distrt.x=="5.0.0.6.0.0"|dam25$distrt.x=="5.5.0.6.0.0","OC",
  ifelse(dam25$distrt.x=="5.0.0.6.7.0"|dam25$distrt.x=="5.0.0.6.7.7","OCT", 
  ifelse(dam25$distrt.x=="4.0.0.0.0.0","D",
     "OOO")))))))))))))))))))

dam25$dist_code_n1<-interaction(dam25$cd1.1,dam25$tcd1.1)
dam25$dist_code_n2<-interaction(dam25$cd1.2,dam25$tcd1.2)
dam25$dist_code_n12<-interaction(dam25$dist_code_n1,dam25$dist_code_n2)

mmm<-count(dam25,dist_code_n2)


dam25$damtrt1<-ifelse(dam25$dist_code_n1=="0.0","ND",
              ifelse(dam25$dist_code_n1=="2.0","F",
                ifelse(dam25$dist_code_n1=="3.0","I",       
                  ifelse(dam25$dist_code_n1=="4.0"|dam25$dist_code_n1=="5.0","O",
                      ifelse(dam25$dist_code_n1=="0.6","C",
                        ifelse(dam25$dist_code_n1=="2.6","FC",
                          ifelse(dam25$dist_code_n1=="3.6","IC",
                            ifelse(dam25$dist_code_n1=="4.6"|dam25$dist_code_n1=="5.6","OC",
                              ifelse(dam25$dist_code_n1=="0.7","T",
                                ifelse(dam25$dist_code_n1=="2.7","FT",
                                  ifelse(dam25$dist_code_n1=="3.7","IT",
                                    ifelse(dam25$dist_code_n1=="5.7","OT","other"
                                        ))))))))))))

dam25$damtrt2<-ifelse(dam25$dist_code_n2=="0.0","ND",
                ifelse(dam25$dist_code_n2=="2.0","F",
                 ifelse(dam25$dist_code_n2=="3.0","I",       
                  ifelse(dam25$dist_code_n2=="4.0"|dam25$dist_code_n2=="5.0","O",
                   ifelse(dam25$dist_code_n2=="0.6","C",
                    ifelse(dam25$dist_code_n2=="2.6","FC",
                     ifelse(dam25$dist_code_n2=="3.6","IC",
                      ifelse(dam25$dist_code_n2=="4.6"|dam25$dist_code_n2=="5.6","OC",
                       ifelse(dam25$dist_code_n2=="0.7","T",
                        ifelse(dam25$dist_code_n2=="2.7","FT",
                         ifelse(dam25$dist_code_n2=="3.7","IT",
                          ifelse(dam25$dist_code_n2=="5.7","OT","other"
                                    ))))))))))))


nnn<-count(dam26,damtrt2)
dam26<-dam25[which(dam25$rep_std==1 & dam25$dist_codes!="ND"),]
nnn<-count(dam26,damtrt2)



dam_ND<-dam25[which(dam25$dist_codes=="ND"),]
dam_D<-dam25[which(dam25$dist_codes!="ND"),]
dam_FCI <-dam_D[which(dam_D$dist_codes=="F"|dam_D$dist_codes=="I"|dam_D$dist_codes=="C" ),]

dam_FCI_sev<-dam_FCI[which(dam_FCI$rep_std==1),]
dam_FCI_mild<-dam_FCI[which(dam_FCI$rep_std==0),]

write.csv(dam_ND,"plots_not_disturbed1.csv")
write.csv(dam_FCI_sev,"plots_severe_dist_FCI1.csv")
write.csv(dam_FCI_mild,"plots_mild_dist_FCI1.csv")


dam261<-dam25[which(dam25$DISTURB==1),]

dam262<-dam261[which(dam261$rep_std==1),]


dam27<-dam26[which(dam26$dist_codes=="F"|dam26$dist_codes=="I"|dam26$dist_codes=="C" ),]

dam28<-dam27[!is.na(dam27$dist_codes_prev),]

dam28$dist_shift<-interaction(dam28$dist_codes_prev,dam28$dist_codes)

dam28$dist_shift_new<-interaction(dam28$damtrt1,dam28$damtrt2)


table66<-count(dam28, dist_shift_new)
table77<-count(dam28, damtrt1,damtrt2)



# df2 <- dam28 %>% 
#   group_by(dist_codes_prev,dist_codes) %>% 
#   tally() %>% 
#   complete(dist_codes, fill = list(n = 0)) %>% 
#   mutate(percentage = n / sum(n) * 100)


dam2<-dam28

dam2$phys_typ<-ifelse(dam2$PHYSCLCD.y > 10 & dam2$PHYSCLCD.y<20,"Xeric",
                    ifelse(dam2$PHYSCLCD.y>20 & dam2$PHYSCLCD.y<30,"Mesic",
                           ifelse(dam2$PHYSCLCD.y>30 & dam2$PHYSCLCD.y<40,"Hydric",0)))


dam2$phy_fac<-as.factor(dam2$phys_typ)

dam2$phy_fac <- factor(dam2$phy_fac, 
                     levels = c("Mesic", "Xeric", "Hydric"))


seedling1<-readRDS("../data/all_seedlings_first.RDS")
seedling2<-readRDS("../data/all_seedlings_second.RDS")


dam111<-merge(dam2,seedling1,by.x="NUNID_1",by.y="NUNID",all.x=TRUE)
sdam1<-merge(dam111,seedling2,by.x="NUNID_2",by.y="NUNID",all.x=TRUE)


sdam1$seed_count.1[is.na(sdam1$seed_count.1)] <- 0
sdam1$seed_count.2[is.na(sdam1$seed_count.2)] <- 0
sdam1$sd_spp_rich.1[is.na(sdam1$sd_spp_rich.1)] <- 0
sdam1$sd_spp_rich.2[is.na(sdam1$sd_spp_rich.2)] <- 0


sdam1$seed_count_con.1[is.na(sdam1$seed_count_con.1)] <- 0
sdam1$seed_count_con.2[is.na(sdam1$seed_count_con.2)] <- 0
sdam1$sd_spp_rich_con.1[is.na(sdam1$sd_spp_rich_con.1)] <- 0
sdam1$sd_spp_rich_con.2[is.na(sdam1$sd_spp_rich_con.2)] <- 0


sdam1$AGB.1<-(sdam1$AGB_1*453.6*2.471)/1000000
sdam1$AGB.2<-(sdam1$AGB_2*453.6*2.471)/1000000
sdam1$BALIVE.1<-(sdam1$BALIVE_1*30.48*30.48*2.471)/10000
sdam1$BALIVE.2<-(sdam1$BALIVE_2*30.48*30.48*2.471)/10000

# pdam1577<-pdam15[which(pdam15$AGB.1>0),]

sdam1$AGB_CHP<-(sdam1$AGB.2-sdam1$AGB.1)
sdam1$BALIVE_CHP<-(sdam1$BALIVE.2 - sdam1$BALIVE.1)



sdam1$seed_count.1<-sdam1$seed_count.1*74.96*2.471
sdam1$seed_count.2<-sdam1$seed_count.2*74.96*2.471
sdam1$seed_ct_ch<-sdam1$seed_count.2-sdam1$seed_count.1
sdam1$sdsp_rh_ch<-sdam1$sd_spp_rich.2-sdam1$sd_spp_rich.1



sdam1$seed_count_con.1<-sdam1$seed_count_con.1*74.96*2.471
sdam1$seed_count_con.2<-sdam1$seed_count_con.2*74.96*2.471
sdam1$seed_ct_ch_con<-sdam1$seed_count_con.2-sdam1$seed_count_con.1
sdam1$sdsp_rh_ch_con<-sdam1$sd_spp_rich_con.2-sdam1$sd_spp_rich_con.1

# 
# plot(sdam15$REMPER_2,sdam15$STDAGE_2)

sdam2<-separate(sdam1, ECOSUBCD, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)


stable11<-count(sdam2,Spl_1)
# stable12<-stable11[which(stable11$n>9),]
# 
# 
# 
# sdam3<-sdam2[(sdam2$Spl_1 %in% stable11$Spl_1),]

source("FTGC.R")
sdam2$FOR_GRP<-FTGC(sdam2$FORTYPCD_1)
# stable31<-count(sdam3,FOR_GRP)
# 
# stable33<-stable31[which(stable31$n>9),]
# stable33<-stable32[which(stable32$FOR_GRP<999),]
# 
# sdam41<-sdam3[(sdam3$FOR_GRP %in% stable31$FOR_GRP),]

sdam3<-sdam2[which(sdam2$STDAGE_1<800),]

write.csv(sdam2,"new_plot_data.csv")

sdam2<-read.csv("data_for_RF.csv")

plot(sdam3$STDAGE_1,sdam3$STDAGE_2)



library(ggplot2)

ggplot(data=sdam3) +
  geom_jitter(aes(x=REMPER_2, y=STDAGE_2,color=as.factor(FOR_GRP))) +
  # scale_color_manual(name="Ecoregion",
  #                    labels=c("Outer coastal plain mixed forest",
  #                             "Cascade mixed forest conifer forest",
  #                             "Sierran Steppe mixed forest conifer forest",
  #                             "Arizona NM semidesert woodland coniferous",
  #                             "Middle rocky steppe coniferous",
  #                             "Northern rocky forest steppe"),
  #                    values=c(" 232"="gray",
  #                             "M242"="red", "M261"="chocolate","M313" ="yellow2",
  #                             "M332"="blue","M333"="green2")) +
  scale_y_continuous(limits=c(-1,12))+
  xlab("Interval between two measurements (REMPER, years)")+
  ggtitle("Stand age after disturbance")+
  ylab("Stage age (years)")+
  
  theme_bw()+
  theme(axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.y = element_text(size = 15),
        axis.title.x=element_text(size=15),
        legend.title=element_text(size=14),
        legend.text = element_text(size=13))



sdam3$ECOREG<-as.factor(sdam3$Spl_1)
sdam3$FORGRP<-as.factor(sdam3$FOR_GRP)
sdam3$STDORG<-as.factor(sdam3$STDORGCD_1)
sdam3$DISCOD<-as.factor(sdam3$dist_shift)

sdam3$DISCOD<-droplevels(sdam3$DISCOD)
sdam31<-sdam3[which(sdam3$AGB.2<200),]

# 
# stable81<-count(sdam3,DISCOD)
# 
# stable33<-stable32[which(stable32$FOR_GRP<999),]
# 
# sdam41<-sdam3[(sdam3$FOR_GRP %in% stable31$FOR_GRP),]
# 
# 
# sdam4<-sdam3[which(sdam3$DISCOD)]

conifer<-sdam4[which(sdam4$FOR_GRP< 401),]


data_con_agb<-sdam31[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                      "ECOREG","DISCOD","ELEV","ASPECT.y","SLOPE.y","phy_fac")]

data_con_agb<-na.omit(data_con_agb)

colnames(data_con_agb)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                          "Pre_dist_stand_age","Post_dist_stand_age",
                          "Ecoregion","Disturbance_types","elevation","aspect","slope",
                          "physiography")


data_con_seed<-sdam31[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","DISCOD","ELEV","ASPECT.y","SLOPE.y","phy_fac")]

jkjkl<-na.omit(data_con_seed)

colnames(data_con_seed)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","Disturbance_types","elevation","aspect","slope",
                      "physiography")


jkjkl<-na.omit(data_con_seed)

library(randomForest)
### severe
set.seed(101)

train_con_agb<-sample(1:nrow(data_con_agb),1200)
valid_con_agb<-data_con_agb[-train_con_agb,]


rf_con_agb=randomForest(AGB.2 ~ . , data = data_con_agb, subset = train_con_agb,mtry=6,ntree=10000) 

print(rf_con_agb)
importance(rf_con_agb)
varImpPlot(rf_con_agb)
pred_con_agb<-predict(rf_con_agb,data_con_agb[-train_con_agb,]) #Predictions on Test Set for each Tree
with(data_con_agb[-train_con_agb,], mean( (AGB.2 - pred_con_agb)^2)) #Mean Squared Test Error
plot(pred_con_agb,valid_con_agb$AGB.2,xlab="predicted",ylab="observed",main="AGB (Mg/ha)")




## for all seedling




set.seed(101)

train_con_seed<-sample(1:nrow(data_con_seed),600)
valid_con_seed<-data_con_seed[-train_con_seed,]


rf_con_seed=randomForest(seed_count.2 ~ . , data = data_con_seed, subset = train_con_seed,mtry=6,ntree=10000) 

print(rf_con_seed)
importance(rf_con_seed)
varImpPlot(rf_con_seed)
pred_con_seed<-predict(rf_con_seed,data_con_seed[-train_con_seed,]) #Predictions on Test Set for each Tree
with(data_con_seed[-train_con_seed,], mean( (seed_count.2 - pred_con_seed)^2)) #Mean Squared Test Error
plot(pred_con_seed,valid_con_seed$seed_count.2,xlab="predicted",ylab="observed",main="seed count (ha)")



## for only fire agb

confire<-conifer[which(conifer$DISCOD=="ND.F"),]


con_fire_agb<-confire[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                      "ECOREG","ELEV","ASPECT.y","SLOPE.y","phy_fac")]



colnames(con_fire_agb)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                          "Pre_dist_stand_age","Post_dist_stand_age",
                          "Ecoregion","elevation","aspect","slope",
                          "physiography")

set.seed(101)

train_con_fire_agb<-sample(1:nrow(con_fire_agb),160)
valid_con_fire_agb<-con_fire_agb[-train_con_fire_agb,]


rf_con_fire_agb=randomForest(AGB.2 ~ . , data = con_fire_agb, subset = train_con_fire_agb,mtry=4,ntree=50000) 

print(rf_con_fire_agb)
importance(rf_con_fire_agb)
varImpPlot(rf_con_fire_agb)

pred_fire_agb<-predict(rf_fire_agb,data_fire_agb[-train_fire_agb,]) #Predictions on Test Set for each Tree
with(data_fire_agb[-train_fire_agb,], mean( (AGB.2 - pred_fire_agb)^2)) #Mean Squared Test Error
plot(pred_fire_agb,valid_fire_agb$AGB.2,xlab="predicted",ylab="observed",main="AGB (Mg/ha)")



## only fire agb

con_fire_seed<-conifire[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","ELEV","ASPECT.y","SLOPE.y","phy_fac")]



colnames(data_fire_seed)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","elevation","aspect","slope",
                           "physiography")

set.seed(101)

train_fire_seed<-sample(1:nrow(data_fire_seed),200)
valid_fire_seed<-data_fire_seed[-train_fire_seed,]


rf_fire_seed=randomForest(seed_count.2 ~ . , data = data_fire_seed, subset = train_fire_seed,mtry=6,ntree=10000) 

print(rf_fire_seed)
importance(rf_fire_seed)
varImpPlot(rf_fire_seed)
pred_fire_seed<-predict(rf_fire_seed,data_fire_seed[-train_fire_seed,]) #Predictions on Test Set for each Tree
with(data_fire_seed[-train_fire_seed,], mean( (seed_count.2 - pred_fire_seed)^2)) #Mean Squared Test Error
plot(pred_fire_seed,valid_fire_seed$seed_count.2,xlab="predicted",ylab="observed",main="seed count (ha)")




## for only fire agb

sdam6<-sdam4[which(sdam4$DISCOD=="ND.C"),]


data_cut_agb<-sdam6[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","ELEV","ASPECT.y","SLOPE.y","phy_fac")]



colnames(data_cut_agb)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","elevation","aspect","slope",
                           "physiography")

set.seed(101)

train_cut_agb<-sample(1:nrow(data_cut_agb),450)
valid_cut_agb<-data_cut_agb[-train_cut_agb,]


rf_cut_agb=randomForest(AGB.2 ~ . , data = data_cut_agb, subset = train_cut_agb,mtry=6,ntree=10000) 

print(rf_cut_agb)
importance(rf_cut_agb)
varImpPlot(rf_cut_agb)
pred_cut_agb<-predict(rf_cut_agb,data_cut_agb[-train_cut_agb,]) #Predictions on Test Set for each Tree
with(data_cut_agb[-train_cut_agb,], mean( (AGB.2 - pred_cut_agb)^2)) #Mean Squared Test Error
plot(pred_cut_agb,valid_cut_agb$AGB.2,xlab="predicted",ylab="observed",main="AGB (Mg/ha)")



## only fire agb

data_cut_seed<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","ELEV","ASPECT.y","SLOPE.y","phy_fac")]



colnames(data_cut_seed)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","elevation","aspect","slope",
                            "physiography")

set.seed(101)

train_cut_seed<-sample(1:nrow(data_cut_seed),200)
valid_cut_seed<-data_cut_seed[-train_cut_seed,]


rf_cut_seed=randomForest(seed_count.2 ~ . , data = data_cut_seed, subset = train_cut_seed,mtry=6,ntree=10000) 

print(rf_cut_seed)
importance(rf_cut_seed)
varImpPlot(rf_cut_seed)
pred_cut_seed<-predict(rf_cut_seed,data_cut_seed[-train_cut_seed,]) #Predictions on Test Set for each Tree
with(data_cut_seed[-train_cut_seed,], mean( (seed_count.2 - pred_cut_seed)^2)) #Mean Squared Test Error
plot(pred_cut_seed,valid_cut_seed$seed_count.2,xlab="predicted",ylab="observed",main="seed count (ha)")






## for only fire agb

sdam7<-sdam4[which(sdam4$DISCOD=="ND.I"),]


data_cut_agb<-sdam6[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                      "ECOREG","ELEV","ASPECT.y","SLOPE.y","phy_fac")]



colnames(data_cut_agb)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                          "Pre_dist_stand_age","Post_dist_stand_age",
                          "Ecoregion","elevation","aspect","slope",
                          "physiography")

set.seed(101)

train_cut_agb<-sample(1:nrow(data_cut_agb),450)
valid_cut_agb<-data_cut_agb[-train_cut_agb,]


rf_cut_agb=randomForest(AGB.2 ~ . , data = data_cut_agb, subset = train_cut_agb,mtry=6,ntree=10000) 

print(rf_cut_agb)
importance(rf_cut_agb)
varImpPlot(rf_cut_agb)
pred_cut_agb<-predict(rf_cut_agb,data_cut_agb[-train_cut_agb,]) #Predictions on Test Set for each Tree
with(data_cut_agb[-train_cut_agb,], mean( (AGB.2 - pred_cut_agb)^2)) #Mean Squared Test Error
plot(pred_cut_agb,valid_cut_agb$AGB.2,xlab="predicted",ylab="observed",main="AGB (Mg/ha)")



## only fire agb

data_cut_seed<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","ELEV","ASPECT.y","SLOPE.y","phy_fac")]



colnames(data_cut_seed)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","elevation","aspect","slope",
                           "physiography")

set.seed(101)

train_cut_seed<-sample(1:nrow(data_cut_seed),200)
valid_cut_seed<-data_cut_seed[-train_cut_seed,]


rf_cut_seed=randomForest(seed_count.2 ~ . , data = data_cut_seed, subset = train_cut_seed,mtry=6,ntree=10000) 

print(rf_cut_seed)
importance(rf_cut_seed)
varImpPlot(rf_cut_seed)
pred_cut_seed<-predict(rf_cut_seed,data_cut_seed[-train_cut_seed,]) #Predictions on Test Set for each Tree
with(data_cut_seed[-train_cut_seed,], mean( (seed_count.2 - pred_cut_seed)^2)) #Mean Squared Test Error
plot(pred_cut_seed,valid_cut_seed$seed_count.2,xlab="predicted",ylab="observed",main="seed count (ha)")




























## agb fire only

set.seed(101)

train_all_seed<-sample(1:nrow(data_all_seed),900)
valid_all_seed<-data_all_seed[-train_all_seed,]


rf_all_seed=randomForest(seed_count.2 ~ . , data = data_all_seed, subset = train_all_seed,mtry=6,ntree=10000) 

print(rf_all_seed)
importance(rf_all_seed)
varImpPlot(rf_all_seed)
pred_all_seed<-predict(rf_all_seed,data_all_seed[-train_all_seed,]) #Predictions on Test Set for each Tree
with(data_all_seed[-train_all_seed,], mean( (seed_count.2 - pred_all_seed)^2)) #Mean Squared Test Error
plot(pred_all_seed,valid_all_seed$seed_count.2,xlab="predicted",ylab="observed",main="seed count (ha)")








fire_all<-pdam19_all2[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","PRISM_ppt_","PRISM_tmea","PRISM_vpdm",
                       "PRISM_tdme","projdem_al","aspect_alb","slope_alb")]


cut_all<-pdam19_all2[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","PRISM_ppt_","PRISM_tmea","PRISM_vpdm",
                        "PRISM_tdme","projdem_al","aspect_alb","slope_alb")]

ins_all<-pdam19_all2[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","PRISM_ppt_","PRISM_tmea","PRISM_vpdm",
                       "PRISM_tdme","projdem_al","aspect_alb","slope_alb")]

colnames(fire_all)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","ppt","tmean","vpd",
                      "tdmean","elevation","aspect","slope")
colnames(cut_all)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","ppt","tmean","vpd",
                      "tdmean","elevation","aspect","slope")
colnames(ins_all)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                     "Pre_dist_stand_age","Post_dist_stand_age",
                     "Ecoregion","ppt","tmean","vpd",
                     "tdmean","elevation","aspect","slope")


data_all2<-pdam19_all2[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                       "ECOREG","disttype")]


colnames(data_all2)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","Disturbance_types")



data_seed<-pdam19_all[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                       "ECOREG","dist_type")]
# 
colnames(data_seed)<-c("seedcount.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","Disturbance_types")

data_seed<-pdam19_all[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","dist_type1","PRISM_ppt_","PRISM_tmea","PRISM_vpdm",
                       "PRISM_tdme","projdem_al","aspect_alb","slope_alb")]

colnames(data_seed)<-c("seed_count.2","seedling_count_first","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","Disturbance_types","ppt","tmean","vpd",
                      "tdmean","elevation","aspect","slope")

seed_fire<-pdam19_all2[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","PRISM_ppt_","PRISM_tmea","PRISM_vpdm",
                       "PRISM_tdme","projdem_al","aspect_alb","slope_alb")]

colnames(seed_fire)<-c("seed_count.2","seedling_count_first","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","ppt","tmean","vpd",
                      "tdmean","elevation","aspect","slope")


seed_cut<-pdam19_all2[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                         "ECOREG","PRISM_ppt_","PRISM_tmea","PRISM_vpdm",
                         "PRISM_tdme","projdem_al","aspect_alb","slope_alb")]

colnames(seed_cut)<-c("seed_count.2","seedling_count_first","Stand_origin","Forest_Group",
                       "Pre_dist_stand_age","Post_dist_stand_age",
                       "Ecoregion","ppt","tmean","vpd",
                       "tdmean","elevation","aspect","slope")

seed_ins<-pdam19_all2[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","PRISM_ppt_","PRISM_tmea","PRISM_vpdm",
                        "PRISM_tdme","projdem_al","aspect_alb","slope_alb")]

colnames(seed_ins)<-c("seed_count.2","seedling_count_first","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","ppt","tmean","vpd",
                      "tdmean","elevation","aspect","slope")

library(randomForest)




### severe
set.seed(51)

train_all<-sample(1:nrow(data_all),900)
valid_all<-data_all[-train_all,]


rf_agb=randomForest(AGB.2 ~ . , data = data_all, subset = train_all,mtry=6,ntree=10000) 
print(rf_agb)
plot.new()
importance(rf_agb)
varImpPlot(rf_agb)


rf_agb$mse[5000] #Error of all Trees fitted

pred_agb<-predict(rf_agb,data_all[-train_all,]) #Predictions on Test Set for each Tree
with(data_all[-train_all,], mean( (AGB.2 - pred_agb)^2)) #Mean Squared Test Error

plot(pred_agb,valid_all$AGB.2,xlab="predicted",ylab="observed",main="AGB (Mg/ha)")




set.seed(100)

train_seed<-sample(1:nrow(data_seed),900)
valid_seed<-data_seed[-train_seed,]


rf_seed=randomForest(seed_count.2 ~ . , data = data_seed, subset = train_seed,mtry=6,ntree=10000) 
print(rf_seed)

importance(rf_seed)
varImpPlot(rf_seed)


rf_seed$mse[10000] #Error of all Trees fitted

pred_seed<-predict(rf_seed,data_seed[-train_seed,]) #Predictions on Test Set for each Tree
with(data_seed[-train_seed,], mean( (seed_count.2 - pred_seed)^2)) #Mean Squared Test Error

plot(pred_seed,valid_seed$seed_count.2,xlab="predicted",ylab="observed",main="seedling count (ha)")







set.seed(51)


train_fire<-sample(1:nrow(fire_all),550)
valid_fire<-fire_all[-train_fire,]


rf_fire_agb=randomForest(AGB.2 ~ . , data = fire_all, subset = train_fire,mtry=6,ntree=10000) 
print(rf_fire_agb)

importance(rf_fire_agb)
varImpPlot(rf_fire_agb)


rf_fire_agb$mse[5000] #Error of all Trees fitted

pred_fire_agb<-predict(rf_fire_agb,fire_all[-train_fire,]) #Predictions on Test Set for each Tree
with(fire_all[-train_fire,], mean( (AGB.2 - pred_fire_agb)^2)) #Mean Squared Test Error
plot(pred_fire_agb,valid_fire$AGB.2)







set.seed(500)


train_fire_seed<-sample(1:nrow(seed_fire),550)
valid_fire_seed<-seed_fire[-train_fire_seed,]


rf_fire_seed=randomForest(seed_count.2 ~ . , data = seed_fire, subset = train_fire_seed,mtry=6,ntree=10000) 
print(rf_fire_seed)

importance(rf_fire_seed)
varImpPlot(rf_fire_seed)


rf_fire_seed$mse[10000] #Error of all Trees fitted

pred_fire_seed<-predict(rf_fire_seed,seed_fire[-train_fire_seed,]) #Predictions on Test Set for each Tree
with(seed_fire[-train_fire_seed,], mean( (seed_count.2 - pred_fire_seed)^2)) #Mean Squared Test Error
plot(pred_fire_seed,valid_fire_seed$seed_count.2)




set.seed(51)


train_cut_seed<-sample(1:nrow(seed_cut),100)
valid_cut_seed<-seed_cut[-train_cut_seed,]


rf_cut_seed=randomForest(seed_count.2 ~ . , data = seed_cut, subset = train_cut_seed,mtry=6,ntree=40000) 
print(rf_cut_seed)

importance(rf_cut_seed)
varImpPlot(rf_cut_seed)


rf_cut_seed$mse[5000] #Error of all Trees fitted

pred_cut_seed<-predict(rf_cut_seed,seed_cut[-train_cut_seed,]) #Predictions on Test Set for each Tree
with(seed_cut[-train_cut_seed,], mean( (seed_count.2 - pred_cut_seed)^2)) #Mean Squared Test Error
plot(pred_cut_seed,valid_cut_seed$seed_count.2)



set.seed(51)


train_ins_seed<-sample(1:nrow(seed_ins),90)
valid_ins_seed<-seed_ins[-train_ins_seed,]


rf_ins_seed=randomForest(seed_count.2 ~ . , data = seed_ins, subset = train_ins_seed,mtry=6,ntree=10000) 
print(rf_ins_seed)

importance(rf_ins_seed)
varImpPlot(rf_ins_seed)


rf_ins_seed$mse[5000] #Error of all Trees fitted

pred_ins_seed<-predict(rf_ins_seed,seed_ins[-train_ins_seed,]) #Predictions on Test Set for each Tree
with(seed_ins[-train_ins_seed,], mean( (seed_count.2 - pred_ins_seed)^2)) #Mean Squared Test Error
plot(pred_ins_seed,valid_ins_seed$seed_count.2)










set.seed(51)


train_cut<-sample(1:nrow(cut_all),100)
valid_cut<-cut_all[-train_cut,]


rf_cut_agb=randomForest(AGB.2 ~ . , data = cut_all, subset = train_cut,mtry=6,ntree=10000) 
print(rf_cut_agb)

importance(rf_cut_agb)
varImpPlot(rf_cut_agb)


rf_cut_agb$mse[5000] #Error of all Trees fitted

pred_cut_agb<-predict(rf_cut_agb,cut_all[-train_cut,]) #Predictions on Test Set for each Tree
with(cut_all[-train_cut,], mean( (AGB.2 - pred_cut_agb)^2)) #Mean Squared Test Error
plot(pred_cut_agb,valid_cut$AGB.2)



set.seed(51)


train_ins<-sample(1:nrow(ins_all),90)
valid_ins<-ins_all[-train_ins,]


rf_ins_agb=randomForest(AGB.2 ~ . , data = ins_all, subset = train_ins,mtry=6,ntree=10000) 
print(rf_ins_agb)

importance(rf_ins_agb)
varImpPlot(rf_ins_agb)


rf_ins_agb$mse[5000] #Error of all Trees fitted

pred_ins_agb<-predict(rf_ins_agb,ins_all[-train_ins,]) #Predictions on Test Set for each Tree
with(ins_all[-train_ins,], mean( (AGB.2 - pred_ins_agb)^2)) #Mean Squared Test Error
plot(pred_ins_agb,valid_ins$AGB.2)



png(filename="varimp_agb2.png", res=150, width = 1500, height = 1000)
par(mfrow=c(2,2))
varImpPlot(rf_agb,main="")
title(main="all disturbances")

varImpPlot(rf_fire_agb,main="")
title(main="fire disturbance")


varImpPlot(rf_cut_agb,main="")
title(main="harvest disturbance")


varImpPlot(rf_ins_agb,main="")
title(main="insect/disease disturbance")


dev.off()




png(filename="prediction_agb3.png", res=150, width = 1500, height = 1000)
par(mfrow=c(2,2))

plot(valid_all$AGB.2,pred_agb,main="",xlab="",ylab="")

title(main="all disturbances",xlab="observed",ylab="predicted")
text(190,80, "train n = 900\nvalid n = 300\nvar explained = 15.1%")

plot(valid_fire$AGB.2,pred_fire_agb,main="",xlab="",ylab="")
title(main="fire disturbance",xlab="observed",ylab="predicted")
text(140,100, "train n = 550\nvalid n = 224\nvar explained = 17.5%")


plot(valid_cut$AGB.2,pred_cut_agb,main="",xlab="",ylab="")
title(main="harvest disturbance",xlab="observed",ylab="predicted")
text(60,40, "train n = 100\nvalid n = 23\nvar explained = -12.1%")

plot(valid_ins$AGB.2,pred_ins_agb,main="",xlab="",ylab="")
title(main="insect/disease disturbance",xlab="observed",ylab="predicted")
text(160,60, "train n = 90\nvalid n = 25\nvar explained = 19.0%")

dev.off()






png(filename="varimp_seed.png", res=150, width = 1500, height = 1000)
par(mfrow=c(2,2))
varImpPlot(rf_seed,main="")
title(main="all disturbances")

varImpPlot(rf_fire_seed,main="")
title(main="fire disturbance")

varImpPlot(rf_cut_seed,main="")
title(main="harvest disturbance")


varImpPlot(rf_ins_seed,main="")
title(main="insect/disease disturbance")


dev.off()




png(filename="prediction_seedling.png", res=150, width = 1500, height = 1000)
par(mfrow=c(2,2))

plot(valid_seed$seed_count.2,pred_seed,main="",xlab="",ylab="")

title(main="all disturbances",xlab="observed",ylab="predicted")
text(25000,32000, "train n = 900\nvalid n = 300\nvar explained = 28.4%")

plot(valid_fire_seed$seed_count.2,pred_fire_seed,main="",xlab="",ylab="")
title(main="fire disturbance",xlab="observed",ylab="predicted")
text(70000,25000, "train n = 550\nvalid n = 224\nvar explained = 17.8%")


plot(valid_cut_seed$seed_count.2,pred_cut_seed,main="",xlab="",ylab="")
title(main="harvest disturbance",xlab="observed",ylab="predicted")
text(25000,12000, "train n = 100\nvalid n = 23\nvar explained = 37.0%")

plot(valid_ins_seed$seed_count.2,pred_ins_seed,main="",xlab="",ylab="")
title(main="insect/disease disturbance",xlab="observed",ylab="predicted")
text(20000,12000, "train n = 90\nvalid n = 25\nvar explained = 63.8%")

dev.off()



























print(rf_ins_seed)

par.new()

plot(rf_agb$IncNodePurity)


pdam19_sub<-pdam18[which(pdam18$dist_type=="1.1.0.0"|pdam18$dist_type=="1.0.1.0"|   
                           pdam18$dist_type=="1.0.0.1"),]


pdam19_sub$dist_type1 <- droplevels(pdam19_sub$dist_type)

pdam19_sub$disttype<-as.factor(pdam19_sub$dist_type1)

data_sub<-pdam19_sub[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                       "ECOREG","disttype")]


colnames(data_sub)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","Disturbance_types")

data_seed<-pdam19_all[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                        "ECOREG","disttype")]

data_seed1<-data_seed[!is.na(data_seed$seed_count.1),]
data_seed2<-data_seed1[!is.na(data_seed1$seed_count.2),]


colnames(data_seed2)<-c("seedcount.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                        "Pre_dist_stand_age","Post_dist_stand_age",
                        "Ecoregion","Disturbance_types")


library(randomForest)




### severe
set.seed(101)
data_all<-data_sub


train_all<-sample(1:nrow(data_all),700)
valid_all<-data_all[-train_all,]


rf1=randomForest(AGB.2 ~ . , data = data_all, subset = train_all,mtry=6,ntree=5000) 
print(rf1)

importance(rf1)
varImpPlot(rf1)


rf1$mse[5000] #Error of all Trees fitted

pred1<-predict(rf1,data_all[-train_all,]) #Predictions on Test Set for each Tree
with(data_all[-train_all,], mean( (AGB.2 - pred1)^2)) #Mean Squared Test Error

plot(pred1,valid_all$AGB.2,xlab="predicted",ylab="observed",main="AGB (Mg/ha)")

