
## codes to identify mild and severe disturbances including no disturbances
## excluding any disturbances in the initial measurements
## produces limited number of severe disturbances

## codes to combine FIA data and MTBS data
## updated on April 20222

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000000)

tr<-readRDS("../../data/all_four_repeated_tree1.RDS")

jkjk<-count(tr,NUNIDS.1)

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
# trfra<-merge(tr,fire,by.x="NUNIDS.2",by.y="nplotid")

rm(tr)

trfr$dist1<-ifelse(trfr$DSTRBCD1.1>0 & !is.na (trfr$DSTRBCD1.1),1,0)
trfr$dist2<-ifelse(trfr$DSTRBCD1.2>0 & !is.na (trfr$DSTRBCD1.2),1,0)
trfr$dist3<-ifelse(trfr$DSTRBCD1.x>0 & !is.na (trfr$DSTRBCD1.x),1,0)
trfr$dist4<-ifelse(trfr$DSTRBCD1.y>0 & !is.na (trfr$DSTRBCD1.y),1,0)
trfr$trt1<-ifelse(trfr$TRTCD1.1>0 & !is.na (trfr$TRTCD1.1),1,0)
trfr$trt2<-ifelse(trfr$TRTCD1.2>0 & !is.na (trfr$TRTCD1.2),1,0)
trfr$trt3<-ifelse(trfr$TRTCD1.x>0 & !is.na (trfr$TRTCD1.x),1,0)
trfr$trt4<-ifelse(trfr$TRTCD1.y>0 & !is.na (trfr$TRTCD1.y),1,0)

## remove disturbed and treated plots in first inventory
# 
# trfr$dist1234<-interaction(trfr$dist1,trfr$dist2,trfr$dist3,trfr$dist4)
# trfr$trt1234<-interaction(trfr$trt1,trfr$trt2,trfr$trt3,trfr$trt4)
# 
# trfr$D1<-ifelse(trfr$dist1234=="1.0.0.0","IDND",
#                 ifelse(trfr$dist1234=="1.1.0.0"|
#                          trfr$dist1234=="1.1.1.0"|trfr$dist1234=="1.1.0.1"|
#                          trfr$dist1234=="1.1.1.1","IDFD",
#                        ifelse(trfr$dist1234=="0.0.0.0","ND",
#                               ifelse(trfr$dist1234=="0.1.0.0"|
#                                        trfr$dist1234=="0.1.1.0"|trfr$dist1234=="0.1.0.1"|
#                                        trfr$dist1234=="0.1.1.1","NDFD",
#                                      ifelse(trfr$dist1234=="1.0.1.0"|
#                                               trfr$dist1234=="1.0.1.1","IDSD",
#                                             ifelse(trfr$dist1234=="0.0.1.0"|
#                                                      trfr$dist1234=="0.0.1.1","NDSD",
#                                                    ifelse(trfr$dist1234=="1.0.0.1","IDTD",
#                                                           ifelse(trfr$dist1234=="0.0.0.1","NDTD",
#                                                                  "NN"))))))))
# 
# kkk<-count(trfr,D1)
# 
# ## for severe plots only
# # trfr$D2<-ifelse(trfr$D1=="IDFD" & trfr$STDAGE.1>trfr$REMPER.1,"NDFD",
# #                 ifelse(trfr$D1=="IDSD" & trfr$STDAGE.1>trfr$REMPER.1,"NDSD",
# #                        ifelse(trfr$D1=="IDTD" & trfr$STDAGE.1>trfr$REMPER.1,"NDTD",
# #                               trfr$D1
# #                               )
# #                        ))
# trfr$D2<-ifelse(trfr$D1=="IDFD" & trfr$STDAGE.1>50,"NDFD",
#                 ifelse(trfr$D1=="IDSD" & trfr$STDAGE.1>50,"NDSD",
#                        ifelse(trfr$D1=="IDTD" & trfr$STDAGE.1>50,"NDTD",
#                               trfr$D1
#                        )
#                 ))
# 
# kkk3<-count(trfr,D1,D2)
# 
# trfr$T1<-ifelse(trfr$trt1234=="1.0.0.0","ITNT",
#                 ifelse(trfr$trt1234=="1.1.0.0"|
#                          trfr$trt1234=="1.1.1.0"|trfr$trt1234=="1.1.0.1"|
#                          trfr$trt1234=="1.1.1.1","ITFT",
#                        ifelse(trfr$trt1234=="0.0.0.0","NT",
#                               ifelse(trfr$trt1234=="0.1.0.0"|
#                                        trfr$trt1234=="0.1.1.0"|trfr$trt1234=="0.1.0.1"|
#                                        trfr$trt1234=="0.1.1.1","NTFT",
#                                      ifelse(trfr$trt1234=="1.0.1.0"|
#                                               trfr$trt1234=="1.0.1.1","ITST",
#                                             ifelse(trfr$trt1234=="0.0.1.0"|
#                                                      trfr$trt1234=="0.0.1.1","NTST",
#                                                    ifelse(trfr$trt1234=="1.0.0.1","ITTT",
#                                                           ifelse(trfr$trt1234=="0.0.0.1","NTTT",
#                                                                  "NN"))))))))
# 
# 
# trfr$T2<-ifelse(trfr$T1=="ITFT" & trfr$STDAGE.1>50,"NTFT",
#                 ifelse(trfr$T1=="ITST" & trfr$STDAGE.1>50,"NTST",
#                        ifelse(trfr$T1=="ITTT" & trfr$STDAGE.1>50,"NTTT",
#                               trfr$T1
#                        )
#                 ))
# 
# 
# ddd<-count(trfr,T1,T2)
# 
# # age_tr2<-trfr[!is.na(trfr$STDAGE.1),]
# # 
# # age_tr2<-age_tr2[!is.na(age_tr2$REMPER.1),]
# # 
# # age_tr2$rep_stand_all2<-ifelse(age_tr2$STDAGE.2<=age_tr2$REMPER.2
# #                                & age_tr2$STDAGE.1>20,1,0)
# # 
# # tr<-age_tr2
# # 
# # rm(age_tr2)
# 
# 
# trfr_a<-trfr[which(trfr$D2=="ND"|trfr$D2=="NDFD"|
#                      trfr$D2=="NDSD"|trfr$D2=="NDTD"),]
# trfr_b<-trfr_a[which(trfr_a$T2=="NT"|trfr_a$T2=="NTFT"|
#                        trfr_a$T2=="NTST"|trfr_a$T2=="NTTT"),]
# 
# gc()
# rm(trfr_a)
# trfr_b$D2T2<-interaction(trfr_b$D2,trfr_b$T2)
# 
# uvuv<-count(trfr_b,D2T2)
# 
# 
# data_dist1<-trfr_b[which(trfr_b$D2T2=="ND.NT"|trfr_b$D2T2=="NDFD.NT"|
#                            trfr_b$D2T2=="ND.NTFT" | trfr_b$D2T2=="NDSD.NTFT" |
#                            trfr_b$D2T2=="NDTD.NTFT" | trfr_b$D2T2=="NDFD.NTST"|
#                            trfr_b$D2T2=="NDFD.NTTT"),]
# 
# 
# data_dist1$time<-"1"
# 
# www11<-count(data_dist1,D2T2)
# 
# rm(trfr_b)
# names(data_dist1)

names(trfr)
trees_fia_first<-cbind(trfr[,1:157],trfr[,314:333])
trees_fia_first1<-select(trees_fia_first,-c("NUNIDS.1"))

rm(trees_fia_first)
gc()
# write.csv(trees_fia_first1,"trees_fia_first1.csv")

colnames<-colnames(trees_fia_first1)
write.csv(colnames,"colnames.csv")
ccc<-read.csv("colnames.csv")
ccc2<-ccc[,2]

 
tr<-trees_fia_first1
 
rm(trees_fia_first1)


cut_agnt <- c(10)

tr$CUT <- ifelse(tr$TRTCD1.2 %in% cut_agnt | tr$TRTCD2.2 %in% cut_agnt 
                 | tr$TRTCD3.2 %in% cut_agnt, 1,  0)

fire_agnt <- c(30,31,32)

tr$FIRE_ALL <- ifelse(tr$DSTRBCD1.2 %in% fire_agnt | tr$DSTRBCD2.2 %in% fire_agnt 
                      | tr$DSTRBCD3.2 %in% fire_agnt, 1,  0)

insdis_agnt <- c(10,11,12,20,21,22)
tr$INSDIS <- ifelse(tr$DSTRBCD1.2 %in% insdis_agnt | tr$DSTRBCD2.2 %in% insdis_agnt 
                    | tr$DSTRBCD3.2 %in% insdis_agnt, 1,  0)

tr$DISTURB <- ifelse(tr$DSTRBCD1.2 !=0 | tr$DSTRBCD2.2 !=0 | tr$DSTRBCD3.2 !=0, 1,  0)


tr$DISTURB1 <- ifelse(tr$DSTRBCD1.2 !=0,  1,  0)
tr$DISTURB2 <- ifelse(tr$DSTRBCD2.2 !=0,  1,  0)
tr$DISTURB3 <- ifelse(tr$DSTRBCD3.2 !=0,  1,  0)

# ntr<-tr[is.na(tr$DRYBIO_BG.1),]

## calculate basal area and AGB
tr$AGB.2<-ifelse((tr$STATUSCD.2==1),tr$DRYBIO_AG.2*tr$TPA_UNADJ.2,0)
tr$AGB.1<-ifelse((tr$STATUSCD.1==1),tr$DRYBIO_AG.1*tr$TPA_UNADJ.1,0)

tr$since_fire<-tr$MEASYEAR.2 - tr$r_fire_yr
tr$mtbs_sev<-ifelse(tr$sev_mean>2,1,0)

table_fia_mtbs<-count(tr,mtbs_sev,FIRE_ALL)


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

pplot16<-aggregate(mtbs_sev~NUNIDS.2, tr, FUN=mean)

pplot17<-aggregate(REMPER.2~NUNIDS.2, tr, FUN=mean)
# pplot18<-aggregate(rep_stand_all2~NUNIDS.2, tr, FUN=max)
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

pplot29<-aggregate(DSTRBCD1.2~NUNIDS.2,tr,FUN=max)

pplot30<-aggregate(DSTRBCD2.2~NUNIDS.2,tr,FUN=max)
pplot31<-aggregate(DSTRBCD3.2~NUNIDS.2,tr,FUN=max)

pplot32<-aggregate(TRTCD1.2~NUNIDS.2,tr,FUN=max)

pplot33<-aggregate(TRTCD2.2~NUNIDS.2,tr,FUN=max)
pplot34<-aggregate(TRTCD3.2~NUNIDS.2,tr,FUN=max)

pplot35<-aggregate(STDSZCD.1~NUNIDS.2,tr,FUN=max)
pplot36<-aggregate(FLDSZCD.1~NUNIDS.2,tr,FUN=min)

pplot37<-aggregate(MEASMON.1~NUNIDS.2,tr,FUN=max)
pplot38<-aggregate(MEASMON.2~NUNIDS.2,tr,FUN=max)


pplot39<-aggregate(DSTRBCD1.1~NUNIDS.2,tr,FUN=max)

pplot40<-aggregate(DSTRBCD2.1~NUNIDS.2,tr,FUN=max)
pplot41<-aggregate(DSTRBCD3.1~NUNIDS.2,tr,FUN=max)

pplot42<-aggregate(TRTCD1.1~NUNIDS.2,tr,FUN=max)

pplot43<-aggregate(TRTCD2.1~NUNIDS.2,tr,FUN=max)
pplot44<-aggregate(TRTCD3.1~NUNIDS.2,tr,FUN=max)


pdam1<-merge(pplot1,pplot2,by="NUNIDS.2")
pdam2<-merge(pdam1,pplot3,by="NUNIDS.2")
pdam3<-merge(pdam2,pplot4,by="NUNIDS.2")
pdam4<-merge(pdam3,pplot9,by="NUNIDS.2")
pdam5<-merge(pdam4,pplot10,by="NUNIDS.2")
pdam6<-merge(pdam5,pplot11,by="NUNIDS.2")
# pdam7<-merge(pdam6,pplot12,by="NUNIDS.2")
# pdam8<-merge(pdam7,pplot13,by="NUNIDS.2")
pdam9<-merge(pdam6,pplot14,by="NUNIDS.2")
pdam10<-merge(pdam9,pplot15,by="NUNIDS.2")
pdam11<-merge(pdam10,pplot17,by="NUNIDS.2")
# pdam12<-merge(pdam11,pplot13a,by="NUNIDS.2")
# pdam13<-merge(pdam11,pplot18,by="NUNIDS.2")
pdam14<-merge(pdam11,pplot19,by="NUNIDS.2")
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

pdam161<-merge(pdam157,pplot29,by="NUNIDS.2")
pdam162<-merge(pdam161,pplot30,by="NUNIDS.2")
pdam163<-merge(pdam162,pplot31,by="NUNIDS.2")
pdam164<-merge(pdam163,pplot32,by="NUNIDS.2")
pdam165<-merge(pdam164,pplot33,by="NUNIDS.2")
pdam166<-merge(pdam165,pplot34,by="NUNIDS.2")
pdam167<-merge(pdam166,pplot35,by="NUNIDS.2")
pdam168<-merge(pdam167,pplot36,by="NUNIDS.2")
pdam169<-merge(pdam168,pplot37,by="NUNIDS.2")
pdam170<-merge(pdam169,pplot38,by="NUNIDS.2")

pdam171<-merge(pdam170,pplot39,by="NUNIDS.2")
pdam172<-merge(pdam171,pplot40,by="NUNIDS.2")
pdam173<-merge(pdam172,pplot41,by="NUNIDS.2")
pdam174<-merge(pdam173,pplot42,by="NUNIDS.2")
pdam175<-merge(pdam174,pplot43,by="NUNIDS.2")
pdam176<-merge(pdam175,pplot44,by="NUNIDS.2")


pdam16<-merge(pdam176,pplot21,by="NUNIDS.2")

pdam16_first<-pdam16
write.csv(pdam16_first,"pdam16_first_all_n.csv")

rm(tr)
gc()
trees_fia_second<-cbind(trfr[,1:2],trfr[,81:235],trfr[,314:333])
trees_fia_second1<-select(trees_fia_second,-c("NUNIDS.x"))


rm(trees_fia_second)
write.csv(trees_fia_second1,"trees_fia_second1.csv")

gc()

col_name1<-colnames(read.csv("trees_fia_first1.csv"))

# first_fia<-read.csv("trees_fia_first1.csv",sep=",")

colnames<-col_name1[-1]
write.csv(colnames,"colnames.csv")

ss<-read.csv("colnames.csv")

jjj<-as.character(ss[,2])

colnames(trees_fia_second1)<-colnames



# age_tr2$rep_stand_all2<-ifelse(age_tr2$STDAGE.2<=age_tr2$REMPER.2
#                                & age_tr2$STDAGE.1>20,1,0)

tr<-trees_fia_second1

rm(trees_fia_second1)


cut_agnt <- c(10)

tr$CUT <- ifelse(tr$TRTCD1.2 %in% cut_agnt | tr$TRTCD2.2 %in% cut_agnt 
                 | tr$TRTCD3.2 %in% cut_agnt, 1,  0)

fire_agnt <- c(30,31,32)

tr$FIRE_ALL <- ifelse(tr$DSTRBCD1.2 %in% fire_agnt | tr$DSTRBCD2.2 %in% fire_agnt 
                      | tr$DSTRBCD3.2 %in% fire_agnt, 1,  0)

insdis_agnt <- c(10,11,12,20,21,22)
tr$INSDIS <- ifelse(tr$DSTRBCD1.2 %in% insdis_agnt | tr$DSTRBCD2.2 %in% insdis_agnt 
                    | tr$DSTRBCD3.2 %in% insdis_agnt, 1,  0)

tr$DISTURB <- ifelse(tr$DSTRBCD1.2 !=0 | tr$DSTRBCD2.2 !=0 | tr$DSTRBCD3.2 !=0, 1,  0)


tr$DISTURB1 <- ifelse(tr$DSTRBCD1.2 !=0,  1,  0)
tr$DISTURB2 <- ifelse(tr$DSTRBCD2.2 !=0,  1,  0)
tr$DISTURB3 <- ifelse(tr$DSTRBCD3.2 !=0,  1,  0)

# ntr<-tr[is.na(tr$DRYBIO_BG.1),]

## calculate basal area and AGB
tr$AGB.2<-ifelse((tr$STATUSCD.2==1),tr$DRYBIO_AG.2*tr$TPA_UNADJ.2,0)
tr$AGB.1<-ifelse((tr$STATUSCD.1==1),tr$DRYBIO_AG.1*tr$TPA_UNADJ.1,0)

tr$since_fire<-tr$MEASYEAR.2 - tr$r_fire_yr
tr$mtbs_sev<-ifelse(tr$sev_mean>2,1,0)

table_fia_mtbs<-count(tr,mtbs_sev,FIRE_ALL)


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
# pplot12<-aggregate(G_FIRE~NUNIDS.2, tr, FUN=mean)
# pplot13<-aggregate(C_FIRE~NUNIDS.2, tr, FUN=mean)
# pplot13a<-aggregate(N_FIRE~NUNIDS.2, tr, FUN=mean)

pplot14<-aggregate(LAT.2~NUNIDS.2, tr, FUN=mean)
pplot15<-aggregate(LON.2~NUNIDS.2, tr, FUN=mean)

pplot16<-aggregate(mtbs_sev~NUNIDS.2, tr, FUN=mean)

pplot17<-aggregate(REMPER.2~NUNIDS.2, tr, FUN=mean)
# pplot18<-aggregate(rep_stand_all2~NUNIDS.2, tr, FUN=max)
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

pplot29<-aggregate(DSTRBCD1.2~NUNIDS.2,tr,FUN=max)

pplot30<-aggregate(DSTRBCD2.2~NUNIDS.2,tr,FUN=max)
pplot31<-aggregate(DSTRBCD3.2~NUNIDS.2,tr,FUN=max)

pplot32<-aggregate(TRTCD1.2~NUNIDS.2,tr,FUN=max)

pplot33<-aggregate(TRTCD2.2~NUNIDS.2,tr,FUN=max)
pplot34<-aggregate(TRTCD3.2~NUNIDS.2,tr,FUN=max)

pplot35<-aggregate(STDSZCD.1~NUNIDS.2,tr,FUN=max)
pplot36<-aggregate(FLDSZCD.1~NUNIDS.2,tr,FUN=min)
pplot37<-aggregate(MEASMON.1~NUNIDS.2,tr,FUN=max)
pplot38<-aggregate(MEASMON.2~NUNIDS.2,tr,FUN=max)

pplot39<-aggregate(DSTRBCD1.1~NUNIDS.2,tr,FUN=max)

pplot40<-aggregate(DSTRBCD2.1~NUNIDS.2,tr,FUN=max)
pplot41<-aggregate(DSTRBCD3.1~NUNIDS.2,tr,FUN=max)

pplot42<-aggregate(TRTCD1.1~NUNIDS.2,tr,FUN=max)

pplot43<-aggregate(TRTCD2.1~NUNIDS.2,tr,FUN=max)
pplot44<-aggregate(TRTCD3.1~NUNIDS.2,tr,FUN=max)

pdam1<-merge(pplot1,pplot2,by="NUNIDS.2")
pdam2<-merge(pdam1,pplot3,by="NUNIDS.2")
pdam3<-merge(pdam2,pplot4,by="NUNIDS.2")
pdam4<-merge(pdam3,pplot9,by="NUNIDS.2")
pdam5<-merge(pdam4,pplot10,by="NUNIDS.2")
pdam6<-merge(pdam5,pplot11,by="NUNIDS.2")
# pdam7<-merge(pdam6,pplot12,by="NUNIDS.2")
# pdam8<-merge(pdam7,pplot13,by="NUNIDS.2")
pdam9<-merge(pdam6,pplot14,by="NUNIDS.2")
pdam10<-merge(pdam9,pplot15,by="NUNIDS.2")
pdam11<-merge(pdam10,pplot17,by="NUNIDS.2")
# pdam12<-merge(pdam11,pplot13a,by="NUNIDS.2")
# pdam13<-merge(pdam11,pplot18,by="NUNIDS.2")
pdam14<-merge(pdam11,pplot19,by="NUNIDS.2")
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

pdam161<-merge(pdam157,pplot29,by="NUNIDS.2")
pdam162<-merge(pdam161,pplot30,by="NUNIDS.2")
pdam163<-merge(pdam162,pplot31,by="NUNIDS.2")
pdam164<-merge(pdam163,pplot32,by="NUNIDS.2")
pdam165<-merge(pdam164,pplot33,by="NUNIDS.2")
pdam166<-merge(pdam165,pplot34,by="NUNIDS.2")
pdam167<-merge(pdam166,pplot35,by="NUNIDS.2")
pdam168<-merge(pdam167,pplot36,by="NUNIDS.2")
pdam169<-merge(pdam168,pplot37,by="NUNIDS.2")
pdam170<-merge(pdam169,pplot38,by="NUNIDS.2")

pdam171<-merge(pdam170,pplot39,by="NUNIDS.2")
pdam172<-merge(pdam171,pplot40,by="NUNIDS.2")
pdam173<-merge(pdam172,pplot41,by="NUNIDS.2")
pdam174<-merge(pdam173,pplot42,by="NUNIDS.2")
pdam175<-merge(pdam174,pplot43,by="NUNIDS.2")
pdam176<-merge(pdam175,pplot44,by="NUNIDS.2")


pdam16<-merge(pdam176,pplot21,by="NUNIDS.2")


pdam16_second<-pdam16
write.csv(pdam16_second,"pdam16_second_all_pre.csv")


rm(trees_fia_second1)
rm(tr)

trees_fia_third<-cbind(trfr[,1:2],trfr[,158:313],trfr[,314:333])
trees_fia_third1<-select(trees_fia_third,-c("NUNIDS.x","NUNIDS.y"))


rm(trees_fia_third)
write.csv(trees_fia_third1,"trees_fia_third1.csv")

# colnamesb<-read.table("pdam16_first.csv",
#                       head = TRUE,
#            nrows = 1,
#            sep = ";")[- 1, ]

colnamesb<-colnames(read.csv("trees_fia_first1.csv"))
# colnames<-colnames(first_fia)
colnames<-colnamesb[-1]

colnames(trees_fia_third1)<-colnames

# age_tr2<-trees_fia_third1[!is.na(trees_fia_third1$STDAGE.2),]
# 
# age_tr2<-age_tr2[!is.na(age_tr2$REMPER.2),]
# 
# age_tr2$rep_stand_all2<-ifelse(age_tr2$STDAGE.2<=age_tr2$REMPER.2
#                                & age_tr2$STDAGE.1>20,1,0)

tr<-trees_fia_third1

rm(trees_fia_third1)


cut_agnt <- c(10)

tr$CUT <- ifelse(tr$TRTCD1.2 %in% cut_agnt | tr$TRTCD2.2 %in% cut_agnt 
                 | tr$TRTCD3.2 %in% cut_agnt, 1,  0)

fire_agnt <- c(30,31,32)

tr$FIRE_ALL <- ifelse(tr$DSTRBCD1.2 %in% fire_agnt | tr$DSTRBCD2.2 %in% fire_agnt 
                      | tr$DSTRBCD3.2 %in% fire_agnt, 1,  0)

insdis_agnt <- c(10,11,12,20,21,22)
tr$INSDIS <- ifelse(tr$DSTRBCD1.2 %in% insdis_agnt | tr$DSTRBCD2.2 %in% insdis_agnt 
                    | tr$DSTRBCD3.2 %in% insdis_agnt, 1,  0)

tr$DISTURB <- ifelse(tr$DSTRBCD1.2 !=0 | tr$DSTRBCD2.2 !=0 | tr$DSTRBCD3.2 !=0, 1,  0)


tr$DISTURB1 <- ifelse(tr$DSTRBCD1.2 !=0,  1,  0)
tr$DISTURB2 <- ifelse(tr$DSTRBCD2.2 !=0,  1,  0)
tr$DISTURB3 <- ifelse(tr$DSTRBCD3.2 !=0,  1,  0)

# ntr<-tr[is.na(tr$DRYBIO_BG.1),]

## calculate basal area and AGB
tr$AGB.2<-ifelse((tr$STATUSCD.2==1),tr$DRYBIO_AG.2*tr$TPA_UNADJ.2,0)
tr$AGB.1<-ifelse((tr$STATUSCD.1==1),tr$DRYBIO_AG.1*tr$TPA_UNADJ.1,0)

tr$since_fire<-tr$MEASYEAR.2 - tr$r_fire_yr
tr$mtbs_sev<-ifelse(tr$sev_mean>2,1,0)

table_fia_mtbs<-count(tr,mtbs_sev,FIRE_ALL)


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
# pplot12<-aggregate(G_FIRE~NUNIDS.2, tr, FUN=mean)
# pplot13<-aggregate(C_FIRE~NUNIDS.2, tr, FUN=mean)
# pplot13a<-aggregate(N_FIRE~NUNIDS.2, tr, FUN=mean)

pplot14<-aggregate(LAT.2~NUNIDS.2, tr, FUN=mean)
pplot15<-aggregate(LON.2~NUNIDS.2, tr, FUN=mean)

pplot16<-aggregate(mtbs_sev~NUNIDS.2, tr, FUN=mean)

pplot17<-aggregate(REMPER.2~NUNIDS.2, tr, FUN=mean)
# pplot18<-aggregate(rep_stand_all2~NUNIDS.2, tr, FUN=max)
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

pplot29<-aggregate(DSTRBCD1.2~NUNIDS.2,tr,FUN=max)

pplot30<-aggregate(DSTRBCD2.2~NUNIDS.2,tr,FUN=max)
pplot31<-aggregate(DSTRBCD3.2~NUNIDS.2,tr,FUN=max)

pplot32<-aggregate(TRTCD1.2~NUNIDS.2,tr,FUN=max)

pplot33<-aggregate(TRTCD2.2~NUNIDS.2,tr,FUN=max)
pplot34<-aggregate(TRTCD3.2~NUNIDS.2,tr,FUN=max)

pplot35<-aggregate(STDSZCD.1~NUNIDS.2,tr,FUN=max)
pplot36<-aggregate(FLDSZCD.1~NUNIDS.2,tr,FUN=min)
pplot37<-aggregate(MEASMON.1~NUNIDS.2,tr,FUN=max)
pplot38<-aggregate(MEASMON.2~NUNIDS.2,tr,FUN=max)

pdam1<-merge(pplot1,pplot2,by="NUNIDS.2")
pdam2<-merge(pdam1,pplot3,by="NUNIDS.2")
pdam3<-merge(pdam2,pplot4,by="NUNIDS.2")
pdam4<-merge(pdam3,pplot9,by="NUNIDS.2")
pdam5<-merge(pdam4,pplot10,by="NUNIDS.2")
pdam6<-merge(pdam5,pplot11,by="NUNIDS.2")
# pdam7<-merge(pdam6,pplot12,by="NUNIDS.2")
# pdam8<-merge(pdam7,pplot13,by="NUNIDS.2")
pdam9<-merge(pdam6,pplot14,by="NUNIDS.2")
pdam10<-merge(pdam9,pplot15,by="NUNIDS.2")
pdam11<-merge(pdam10,pplot17,by="NUNIDS.2")
# pdam12<-merge(pdam11,pplot13a,by="NUNIDS.2")
# pdam13<-merge(pdam11,pplot18,by="NUNIDS.2")
pdam14<-merge(pdam11,pplot19,by="NUNIDS.2")
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

pdam161<-merge(pdam157,pplot29,by="NUNIDS.2")
pdam162<-merge(pdam161,pplot30,by="NUNIDS.2")
pdam163<-merge(pdam162,pplot31,by="NUNIDS.2")
pdam164<-merge(pdam163,pplot32,by="NUNIDS.2")
pdam165<-merge(pdam164,pplot33,by="NUNIDS.2")
pdam166<-merge(pdam165,pplot34,by="NUNIDS.2")
pdam167<-merge(pdam166,pplot35,by="NUNIDS.2")
pdam168<-merge(pdam167,pplot36,by="NUNIDS.2")
pdam169<-merge(pdam168,pplot37,by="NUNIDS.2")
pdam170<-merge(pdam169,pplot38,by="NUNIDS.2")


pdam16<-merge(pdam170,pplot21,by="NUNIDS.2")

pdam16_third<-pdam16
write.csv(pdam16_third,"pdam16_third_all_pre.csv")



### find out plots with different disturbances without seedling data

plots_all1<-read.csv("pdam16_first_all_pre.csv")
plots_first$time<-"1"
# plots_second<-read.csv("pdam16_second_all_pre.csv")
# plots_second$time<-"2"
# plots_third<-read.csv("pdam16_third_all_pre.csv")
# plots_third$time<-"3"
# plots_all<-rbind(plots_first,plots_second,plots_third)
# 



plots_all$measdate1<-plots_all$MEASYEAR.1 + (plots_all$MEASMON.1/12) 
plots_all$measdate2<-plots_all$MEASYEAR.2 + (plots_all$MEASMON.2/12) 
plots_all$diff_measdate<-plots_all$measdate2-plots_all$measdate1

  
plot(plots_all$diff_measdate,plots_all$REMPER.2)

plots_all$ppp<-ifelse(abs(plots_all$diff_measdate-plots_all$REMPER.2)<0.25,1,0)

plots_all$prepos<-ifelse(abs((plots_all$measdate2 - plots_all$measdate1 )- 
                               plots_all$REMPER.2) < 0.25,1,0)

plots_all2<-plots_all[which(plots_all$prepos==1),]
plot(plots_all2$diff_measdate,plots_all2$REMPER.2)


tr<-plots_all2

cut_agnt <- c(10)

tr$CUT <- ifelse(tr$TRTCD1.1 %in% cut_agnt, 1,  0)

fire_agnt <- c(30,31,32)

tr$FIRE <- ifelse(tr$DSTRBCD1.1 %in% fire_agnt, 1,  0)

insdis_agnt <- c(10,11,12,20,21,22)
tr$INSDIS <- ifelse(tr$DSTRBCD1.1 %in% insdis_agnt, 1,  0)

tr$DISTURB <- ifelse(tr$DSTRBCD1.1 !=0 | tr$DSTRBCD2.1 !=0 | tr$DSTRBCD3.1 !=0, 1,  0)
tr$TRT <- ifelse(tr$TRTCD1.1 !=0 | tr$TRTCD2.1 !=0 | tr$TRTCD3.1 !=0, 1,  0)


ddd<-count(tr,FIRE,CUT,INSDIS,DISTURB,TRT)

tr$DISTRT<-interaction(tr$FIRE,tr$CUT,tr$INSDIS,tr$DISTURB,tr$TRT)
# 
# plots_ndnt<-tr[which(tr$DISTRT=="0.0.0.0.0"),]
# plots_fire<-tr[which(tr$DISTRT=="1.0.0.1.0" | tr$DISTRT=="1.0.0.1.1"),]
# plots_insdis<-tr[which(tr$DISTRT=="0.0.1.1.0" | tr$DISTRT=="0.0.1.1.1"),]
# plots_cut<-tr[which(tr$DISTRT=="0.1.0.0.1"),]
# 
# xxx<-count(tr,DISTRT)
# 
# plots_ndnt$dist_type<-"NDNT"
# plots_fire$dist_type<-"FIRE"
# plots_insdis$dist_type<-"INSDIS"
# plots_cut$dist_type<-"CUT"

tr$dist_type<-ifelse(tr$DISTRT=="0.0.0.0.0","NDNT",
                       ifelse(tr$DISTRT=="1.0.0.1.0" | tr$DISTRT=="1.0.0.1.1","FIRE",
                              ifelse(tr$DISTRT=="0.0.1.1.0" | tr$DISTRT=="0.0.1.1.1","INSDIS",
                                     ifelse(tr$DISTRT=="0.1.0.0.1","CUT","OTHER"))))

# plots_dist<-rbind(plots_fire,plots_insdis,plots_cut)

disdis<-count(tr,dist_type,DISTRT)
plots_dist<-tr

write.csv(disdis,"plots_dist_type_all_nofilter_only1.csv")

plots_dist2<-plots_dist[which(plots_dist$STDAGE.1>=20),]

plots_dist3<-plots_dist2[which(plots_dist2$STDSZCD.1<5),]


plots_dist3$fia_sev<-ifelse(plots_dist3$STDAGE.2<=plots_dist3$REMPER.2,1,0)

disdis3<-count(plots_dist3,dist_type,fia_sev)

pdam18<-separate(plots_dist3, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)

sdam2b<-separate(pdam18, NUNIDS.2, 
                 into = c("st","cty","unt","pl"), remove = FALSE)
sdam2<-sdam2b[which(sdam2b$st!=2),]

sdam2<-sdam2[which(sdam2$st!=15),]



sdam2<-sdam2[which(sdam2$st<57 |sdam2$st>70 |sdam2$st==6),]



plot<-read.csv("../../data/PLOT.csv")

survey<-read.csv("../../data/SURVEY.csv")
survey_sel<-survey[which(survey$P3_OZONE_IND=="N"),]
survey_sel2<-survey_sel[which(survey_sel$ANN_INVENTORY=="Y"),]


# plots_all<-plots
library(dplyr)
library(tidyverse)
plot1<-plot %>% select(CN,SRV_CN,INVYR,STATECD,UNITCD,COUNTYCD,
                           PLOT,PLOT_STATUS_CD,MEASYEAR,REMPER,LAT,LON,ELEV,
                           ECOSUBCD,KINDCD,DESIGNCD)



plot2<-plot1[(plot1$SRV_CN %in% survey_sel2$CN),]

rm(plot)
rm(plot1)

plot2$pplotid<-paste0(plot2$STATECD,"-",plot2$UNITCD,"-",
                      plot2$COUNTYCD,"-",plot2$PLOT)
plot2$pplotidyr<-paste0(plot2$STATECD,"-",plot2$UNITCD,"-",
                        plot2$COUNTYCD,"-",plot2$PLOT,"-",plot2$INVYR)



sdam3<-sdam2[(sdam2$NUNIDS.2 %in% plot2$pplotid),]




seedling_all<-read.csv("../data/SEEDLING.CSV")


seedling_all$nunid<-paste0(seedling_all$STATECD,"-",seedling_all$UNITCD,"-",
                           seedling_all$COUNTYCD,"-", seedling_all$PLOT,
                           "-",seedling_all$INVYR)


fplot1<-aggregate(TREECOUNT~nunid, seedling_all, FUN=sum)
fplot3<-count(seedling_all,nunid,SPCD)
fplot4<-count(fplot3,nunid)

fdam1<-merge(fplot1,fplot4,by="nunid")

seedling_all$con<-ifelse(seedling_all$SPGRPCD<25|seedling_all$SPGRPCD==51|
                           seedling_all$SPGRPCD==52,1,0)

seedling12b<-seedling_all[which(seedling_all$con==1),]

fplot7<-aggregate(TREECOUNT~nunid, seedling12b, FUN=sum)

fplot9b<-count(seedling12b,nunid,SPCD)
fplot9<-count(fplot9b,nunid)


fdam4<-merge(fdam1,fplot7,by="nunid",all.x=TRUE)

fdam6<-merge(fdam4,fplot9,by="nunid",all.x=TRUE)


colnames(fdam6)<-c("nunid","seed_count","sd_spp_rich",
                   "seed_count_con","sd_spp_rich_con")

fdam6$nunid_cp<-fdam6$nunid
fdam6$seed_count_con[is.na(fdam6$seed_count_con)] <- 0
fdam6$sd_spp_rich_con[is.na(fdam6$sd_spp_rich_con)] <- 0


#
# seedling_data$nunid.1<-paste0(seedling_data$NUNIDS,"-",seedling_data$INVYR.x)
# seedling_data$nunid.2<-paste0(seedling_data$NUNIDS,"-",seedling_data$INVYR.y)


plots_seed_n<-merge(sdam3,fdam6,by.x="NUNID.1",by.y="nunid",all.x=TRUE)
plots_seed_n2<-merge(plots_seed_n,fdam6,by.x="NUNID.2",by.y="nunid",all.x=TRUE)


colnames(plots_seed_n2)[57]<-"seed_count.1"
colnames(plots_seed_n2)[58]<-"sd_spp_rich.1"
colnames(plots_seed_n2)[59]<-"seed_count_con.1"
colnames(plots_seed_n2)[60]<-"sd_spp_rich_con.1"
colnames(plots_seed_n2)[62]<-"seed_count.2"
colnames(plots_seed_n2)[63]<-"sd_spp_rich.2"
colnames(plots_seed_n2)[64]<-"seed_count_con.2"
colnames(plots_seed_n2)[65]<-"sd_spp_rich_con.2"









# 
# 
# 
# sdam2b<-separate(plots_seed_n2, NUNID.1, 
#                  into = c("st.1","county.1","unit.1","plot.1","invyr.1"), remove = FALSE)
# 
# hhh<-count(sdam2b,st.1,invyr.1,seed_count.1)
# sdam2b$na<-ifelse(sdam2b$seed_count.1>0,"data","nodata")
# iii<-count(sdam2b,st.1,invyr.1,na)
# 
# 
# iii$state_inv<-paste0(iii$st.1,"-",iii$invyr.1)
# 
# agg1<-iii[which(iii$na=="data"),]
# agg2<-iii[is.na(iii$na),]
# 
# agg3<-merge(agg1,agg2,by="state_inv",all=TRUE)
# 
# 
# agg3$tot<-agg3$n.x+agg3$n.y
# agg3$per_missing<-(agg3$n.y/agg3$tot)*100
# 
# agg3$per_missing2<-ifelse(is.na(agg3$n.x),100,
#                           ifelse(is.na(agg3$n.y),0,agg3$per_missing))
#                           
# 
# agg4<-agg3[,c(1,2,3,5,9,12)]
# colnames(agg4)<-c("STATE-INVYR","STATE","INVYR","Present","Missing",
#                   "Percentage of missing data")
# 
# 
# 
# sdam2c<-separate(plots_seed_n2, NUNID.2, 
#                  into = c("st.2","county.2","unit.2","plot.2","invyr.2"), remove = FALSE)
# 
# hhh2<-count(sdam2c,st.2,invyr.2,seed_count.2)
# sdam2c$na<-ifelse(sdam2c$seed_count.2>0,"data","nodata")
# iii2<-count(sdam2c,st.2,invyr.2,na)
# 
# 
# iii2$state_inv2<-paste0(iii2$st.2,"-",iii2$invyr.2)
# 
# agg11<-iii2[which(iii2$na=="data"),]
# agg21<-iii2[is.na(iii2$na),]
# 
# agg31<-merge(agg11,agg21,by="state_inv2",all=TRUE)
# 
# 
# agg31$tot<-agg31$n.x+agg31$n.y
# agg31$per_missing<-(agg31$n.y/agg31$tot)*100
# 
# agg31$per_missing2<-ifelse(is.na(agg31$n.x),100,
#                           ifelse(is.na(agg31$n.y),0,agg31$per_missing))
# 
# 
# agg41<-agg31[,c(1,2,3,5,9,12)]
# colnames(agg41)<-c("STATE-INVYR2","STATE2","INVYR2","Present2","Missing2",
#                   "Percentage of missing data2")
# 
# 
# agg_both<-merge(agg4,agg41,by.x="STATE",by.y="STATE2")
# 
# write.csv(agg4,"pre_dist_seedling_presence.csv")
# write.csv(agg41,"post_dist_seedling_presence.csv")


na_first<-plots_seed_n2[is.na(plots_seed_n2$seed_count.1),]
na_second<-plots_seed_n2[is.na(plots_seed_n2$seed_count.2),]
# plots_seed<-plots_seed_n2[!is.na(plots_seed_n2$seed_count.2) |
 #                                     !is.na (plots_seed_n2$seed_count.1),]

# plots_seed<-merge(sdam12,seedling_data,by.x="NUNID.1",by.y="nunid.1",all.x=TRUE)

plots_seed<-plots_seed_n2

plots_seed$seed_count.1[is.na(plots_seed$seed_count.1)] <- 0
plots_seed$seed_count.2[is.na(plots_seed$seed_count.2)] <- 0
plots_seed$sd_spp_rich.1[is.na(plots_seed$sd_spp_rich.1)] <- 0
plots_seed$sd_spp_rich.2[is.na(plots_seed$sd_spp_rich.2)] <- 0

plots_seed$seed_count_con.1[is.na(plots_seed$seed_count_con.1)] <- 0
plots_seed$seed_count_con.2[is.na(plots_seed$seed_count_con.2)] <- 0
plots_seed$sd_spp_rich_con.1[is.na(plots_seed$sd_spp_rich_con.1)] <- 0
plots_seed$sd_spp_rich_con.2[is.na(plots_seed$sd_spp_rich_con.2)] <- 0

plots_seed$seed_count.1<-plots_seed$seed_count.1*74.96*2.471
plots_seed$seed_count.2<-plots_seed$seed_count.2*74.96*2.471

plots_seed$seed_count_con.1<-plots_seed$seed_count_con.1*74.96*2.471
plots_seed$seed_count_con.2<-plots_seed$seed_count_con.2*74.96*2.471


plots_seed$seed_ct_ch<-plots_seed$seed_count.2-plots_seed$seed_count.1
plots_seed$sdsp_rh_ch<-plots_seed$sd_spp_rich.2-plots_seed$sd_spp_rich.1

plots_seed$seed_ct_ch_con<-plots_seed$seed_count_con.2-plots_seed$seed_count_con.1
plots_seed$sdsp_rh_ch_con<-plots_seed$sd_spp_rich_con.2-plots_seed$sd_spp_rich_con.1

# plots_seed$AGB_CHP<-plots_seed$AGB.2-plots_seed$AGB_CHP


sdam3<-plots_seed

sdam3$AGB.11<-(sdam3$AGB.1*453.6*2.471)/1000000
sdam3$AGB.22<-(sdam3$AGB.2*453.6*2.471)/1000000
sdam3$BALIVE.11<-(sdam3$BALIVE.1*30.48*30.48*2.471)/10000
sdam3$BALIVE.22<-(sdam3$BALIVE.2*30.48*30.48*2.471)/10000

sdam3_sev<-sdam3[which(sdam3$fia_sev==1),]
sdam3_mild<-sdam3[which(sdam3$fia_sev==0),]
plot(sdam3_sev$AGB.2)


sdam3$fia_sev2<-ifelse(sdam3$STDAGE.2<=sdam3$REMPER.2,1,0)
sdam3$fia_sev1<-ifelse(sdam3$STDAGE.2<=sdam3$REMPER.2 & sdam3$AGB.22<50,1,0)

sdam3_sev1<-sdam3[which(sdam3$fia_sev1==1),]
plot(sdam3_sev1$AGB.22)

ccc<-count(sdam3,fia_sev1)
ddd<-count(sdam3,dist_type,fia_sev1)

# sdam3_sev1<-sdam3_sev[which(sdam3_sev$AGB.22<50),]
# sdam3_sev2<-sdam3_sev1[which(sdam3_sev1$seed_count.2<50000),]
sdam3_sev2<-sdam3_sev1
plot(sdam3_sev$AGB.2)


sdam3f<-rbind(sdam3_sev2,sdam3_mild)


ecosel<-read.csv("../disturbance/eco_select.csv")

sdam3$ecocode <- trimws(sdam3$Spl_1, which = c("left"))

sdam3f$ecocode <- trimws(sdam3f$Spl_1, which = c("left"))

library(operators)
library(tidyverse)
sdam3b<-sdam3[(sdam3$ecocode %in% ecosel$econew),]
sdam3a<-sdam3[(sdam3$ecocode %!in% ecosel$econew),]

sdam3fb<-sdam3f[(sdam3f$ecocode %in% ecosel$econew),]
sdam3fa<-sdam3f[(sdam3f$ecocode %!in% ecosel$econew),]

table_dist_sev_west<-count(sdam3b,dist_type, fia_sev1)

table_dist_sev_east<-count(sdam3a,dist_type, fia_sev1)

tot_remp_east<-sum(sdam3a$REMPER.2)
tot_remp_west<-sum(sdam3b$REMPER.2)

mean_remp_east<-mean(sdam3a$REMPER.2)
mean_remp_west<-mean(sdam3b$REMPER.2)

table_dist_sev_east$per_ann<-(100*table_dist_sev_east$n)/tot_remp_east
table_dist_sev_west$per_ann<-(100*table_dist_sev_west$n)/tot_remp_west

hh<-sum(table_dist_sev_east$per_ann)
ii<-sum(table_dist_sev_west$per_ann)


table_dist_sev_west_f<-count(sdam3fb,dist_type, fia_sev)

table_dist_sev_east_f<-count(sdam3fa,dist_type, fia_sev)

write.csv(table_dist_sev_west,"tabl_dist_sev_west_only1.csv")
write.csv(table_dist_sev_east,"tabl_dist_sev_east_only1.csv")

write.csv(table_dist_sev_west_f,"tabl_dist_sev_west_f_only1.csv")
write.csv(table_dist_sev_east_f,"tabl_dist_sev_east_f_only1.csv")

write.csv(sdam3,"all_plots_data_with_seedling_8_27_only1.csv")
write.csv(sdam3f,"plots_data_with_seedling_f_6_4_only1.csv")




###################################
## find out plots for MTBS data

trees_fire_22<-trfr[trfr$NUNIDS.2 %in% fire$nplotid_cp,]

trees_fire_22b<-merge(trfr,fire,by.x="NUNIDS.2",by.y="nplotid")

write.csv(trees_fire_22,"trees_fire_22.csv")

trees_fire_22<-read.csv("trees_fire_22.csv")

#data with first measurements less than fire year
trees_fire_33<-trees_fire_22[which(trees_fire_22$MEASYEAR.1<trees_fire_22$MTBS_fireyear),]

jjj<-count(trees_fire_33,nplotid_cp)

#data with first measurements less and second measurements greater than fire year
trees_fire_44<-trees_fire_33[which(trees_fire_33$MEASYEAR.2>trees_fire_33$MTBS_fireyear),]

trees_fire_44$time<-1

tablepp<-count(trees_fire_44,NUNIDS.2)

#data with second measurements less than fire year
trees_fire_55<-trees_fire_22[which(trees_fire_22$MEASYEAR.2<=trees_fire_22$MTBS_fireyear),]

trees_fire_66<-trees_fire_55[which(trees_fire_55$MEASYEAR.x>=trees_fire_55$MTBS_fireyear),]

trees_fire_66$time<-2

tableqq<-count(trees_fire_66,NUNIDS.2)
#data with first measurements less than fire year
trees_fire_77<-trees_fire_22[which(trees_fire_22$MEASYEAR.x<=trees_fire_22$MTBS_fireyear),]

trees_fire_88<-trees_fire_77[which(trees_fire_77$MEASYEAR.y>=trees_fire_77$MTBS_fireyear),]

trees_fire_88$time<-3
tablerr<-count(trees_fire_88,NUNIDS.2)
#data with first &second measureemtns less and third measurements greater than fire year
# trees_fire_55<-trees_fire_33[which(trees_fire_33$MEASYEAR.2<trees_fire_33$MTBS_fireyear
#                                & trees_fire_33$MEASYEAR>trees_fire_33$MTBS_fireyear),]

all_selected<-rbind(trees_fire_88,trees_fire_66,trees_fire_44)

jjkk<-count(all_selected,NUNIDS.2)

trees_data_first<-cbind(trees_fire_44[,1:158],trees_fire_44[,315:327])
trees_data_first1<-select(trees_data_first,-c("NUNIDS.1"))


trees_data_second<-cbind(trees_fire_66[,1:3],trees_fire_66[,82:236],trees_fire_66[,315:327])
trees_data_second1<-select(trees_data_second,-c("NUNIDS.x"))

trees_data_third<-cbind(trees_fire_88[,1:3],trees_fire_88[,159:314],trees_fire_88[,315:327])
trees_data_third1<-select(trees_data_third,-c("NUNIDS.x","NUNIDS.y"))

colnames1<-colnames(trees_data_first1)

colnames(trees_data_second1)<-colnames1
colnames(trees_data_third1)<-colnames1



trees_data_all<-rbind(trees_data_first1,trees_data_second1,trees_data_third1)

trees_data_all$mtbs_sev1<-ifelse(trees_data_all$sev_mean>2,1,0)






























tr<-trees_data_all

cut_agnt <- c(10)

tr$CUT <- ifelse(tr$TRTCD1.2 %in% cut_agnt | tr$TRTCD2.2 %in% cut_agnt 
                 | tr$TRTCD3.2 %in% cut_agnt, 1,  0)

fire_agnt <- c(30,31,32)

tr$FIRE_ALL <- ifelse(tr$DSTRBCD1.2 %in% fire_agnt | tr$DSTRBCD2.2 %in% fire_agnt 
                      | tr$DSTRBCD3.2 %in% fire_agnt, 1,  0)

insdis_agnt <- c(10,11,12,20,21,22)
tr$INSDIS <- ifelse(tr$DSTRBCD1.2 %in% insdis_agnt | tr$DSTRBCD2.2 %in% insdis_agnt 
                    | tr$DSTRBCD3.2 %in% insdis_agnt, 1,  0)

tr$DISTURB <- ifelse(tr$DSTRBCD1.2 !=0 | tr$DSTRBCD2.2 !=0 | tr$DSTRBCD3.2 !=0, 1,  0)


tr$DISTURB1 <- ifelse(tr$DSTRBCD1.2 !=0,  1,  0)
tr$DISTURB2 <- ifelse(tr$DSTRBCD2.2 !=0,  1,  0)
tr$DISTURB3 <- ifelse(tr$DSTRBCD3.2 !=0,  1,  0)

# ntr<-tr[is.na(tr$DRYBIO_BG.1),]

## calculate basal area and AGB
tr$AGB.2<-ifelse((tr$STATUSCD.2==1),tr$DRYBIO_AG.2*tr$TPA_UNADJ.2,0)
tr$AGB.1<-ifelse((tr$STATUSCD.1==1),tr$DRYBIO_AG.1*tr$TPA_UNADJ.1,0)

tr$since_fire<-tr$MEASYEAR.2 - tr$r_fire_yr
tr$mtbs_sev<-ifelse(tr$sev_mean>2,1,0)

table_fia_mtbs<-count(tr,mtbs_sev,FIRE_ALL)

write.csv(tr,"trees_joined_MTBS_FIA_7_14.csv")


tr<-read.csv("trees_joined_MTBS_FIA_7_14.csv")

## calculate plot level variables and disturbances
library(tidyverse)

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

pplot16<-aggregate(mtbs_sev~NUNIDS.2, tr, FUN=mean)

pplot17<-aggregate(REMPER.2~NUNIDS.2, tr, FUN=mean)
pplot18<-aggregate(rep_stand_all2~NUNIDS.2, tr, FUN=max)
pplot19<-aggregate(MEASYEAR.2~NUNIDS.2, tr, FUN=max)
pplot19b<-aggregate(MEASYEAR.1~NUNIDS.2, tr, FUN=max)
pplot19c<-aggregate(MEASMON.2~NUNIDS.2, tr, FUN=max)
pplot19d<-aggregate(MEASMON.1~NUNIDS.2, tr, FUN=max)

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

pplot29<-aggregate(DSTRBCD1.2~NUNIDS.2,tr,FUN=max)

pplot30<-aggregate(DSTRBCD2.2~NUNIDS.2,tr,FUN=max)
pplot31<-aggregate(DSTRBCD3.2~NUNIDS.2,tr,FUN=max)

pplot32<-aggregate(TRTCD1.2~NUNIDS.2,tr,FUN=max)

pplot33<-aggregate(TRTCD2.2~NUNIDS.2,tr,FUN=max)
pplot34<-aggregate(TRTCD3.2~NUNIDS.2,tr,FUN=max)

pplot35<-aggregate(STDSZCD.1~NUNIDS.2,tr,FUN=max)
pplot36<-aggregate(FLDSZCD.1~NUNIDS.2,tr,FUN=min)
pplot36b<-aggregate(FLDSZCD.2~NUNIDS.2,tr,FUN=min)
pplot37<-aggregate(fire_id_mtbs~NUNIDS.2,tr,FUN=max)


pdam1<-merge(pplot1,pplot2,by="NUNIDS.2")
pdam2<-merge(pdam1,pplot3,by="NUNIDS.2")
pdam3<-merge(pdam2,pplot4,by="NUNIDS.2")
pdam4<-merge(pdam3,pplot9,by="NUNIDS.2")
pdam5<-merge(pdam4,pplot10,by="NUNIDS.2")
pdam6<-merge(pdam5,pplot11,by="NUNIDS.2")
# pdam7<-merge(pdam6,pplot12,by="NUNIDS.2")
# pdam8<-merge(pdam7,pplot13,by="NUNIDS.2")
pdam9<-merge(pdam6,pplot14,by="NUNIDS.2")
pdam10<-merge(pdam9,pplot15,by="NUNIDS.2")
pdam11<-merge(pdam10,pplot17,by="NUNIDS.2")
 pdam12<-merge(pdam11,pplot16,by="NUNIDS.2")
pdam13<-merge(pdam12,pplot8,by="NUNIDS.2")
pdam14<-merge(pdam13,pplot19,by="NUNIDS.2")
pdam14b<-merge(pdam14,pplot19b,by="NUNIDS.2")
pdam14c<-merge(pdam14b,pplot19c,by="NUNIDS.2")
pdam14d<-merge(pdam14c,pplot19d,by="NUNIDS.2")
pdam15<-merge(pdam14d,pplot20,by="NUNIDS.2")
pdam15b<-merge(pdam15,pplot20b,by="NUNIDS.2")

pdam151<-merge(pdam15b,pplot22,by="NUNIDS.2")
pdam152<-merge(pdam151,pplot23,by="NUNIDS.2")
pdam153<-merge(pdam152,pplot24,by="NUNIDS.2")
pdam154<-merge(pdam153,pplot25,by="NUNIDS.2")
pdam155<-merge(pdam154,pplot26,by="NUNIDS.2")
pdam156<-merge(pdam155,pplot27,by="NUNIDS.2")
pdam157<-merge(pdam156,pplot28,by="NUNIDS.2")

pdam161<-merge(pdam157,pplot29,by="NUNIDS.2")
pdam162<-merge(pdam161,pplot30,by="NUNIDS.2")
pdam163<-merge(pdam162,pplot31,by="NUNIDS.2")
pdam164<-merge(pdam163,pplot32,by="NUNIDS.2")
pdam165<-merge(pdam164,pplot33,by="NUNIDS.2")
pdam166<-merge(pdam165,pplot34,by="NUNIDS.2")
pdam167<-merge(pdam166,pplot35,by="NUNIDS.2")
pdam168<-merge(pdam167,pplot36,by="NUNIDS.2")
pdam168b<-merge(pdam168,pplot36b,by="NUNIDS.2")
pdam169<-merge(pdam168b,pplot37,by="NUNIDS.2")
pdam16<-merge(pdam169,pplot21,by="NUNIDS.2")

plots_all<-pdam16


plots_all$measdate1<-plots_all$MEASYEAR.1 + (plots_all$MEASMON.1/12) 
plots_all$measdate2<-plots_all$MEASYEAR.2 + (plots_all$MEASMON.2/12) 
plots_all$diff_measdate<-plots_all$measdate2-plots_all$measdate1


plot(plots_all$diff_measdate,plots_all$REMPER.2)

plots_all$prepos<-ifelse(abs(plots_all$measdate2 - plots_all$measdate1 - 
                               plots_all$REMPER.2) < 0.25,1,0)

plots_all2<-plots_all[which(plots_all$prepos==1),]
plot(plots_all2$diff_measdate,plots_all2$REMPER.2)


tr<-plots_all

cut_agnt <- c(10)

tr$CUT <- ifelse(tr$TRTCD1.2 %in% cut_agnt, 1,  0)

fire_agnt <- c(30,31,32)

tr$FIRE <- ifelse(tr$DSTRBCD1.2 %in% fire_agnt, 1,  0)

insdis_agnt <- c(10,11,12,20,21,22)
tr$INSDIS <- ifelse(tr$DSTRBCD1.2 %in% insdis_agnt, 1,  0)

tr$DISTURB <- ifelse(tr$DSTRBCD1.2 !=0 | tr$DSTRBCD2.2 !=0 | tr$DSTRBCD3.2 !=0, 1,  0)
tr$TRT <- ifelse(tr$TRTCD1.2 !=0 | tr$TRTCD2.2 !=0 | tr$TRTCD3.2 !=0, 1,  0)


ddd<-count(tr,FIRE,CUT,INSDIS,DISTURB,TRT)

tr$DISTRT<-interaction(tr$FIRE,tr$CUT,tr$INSDIS,tr$DISTURB,tr$TRT)



tr$dist_type<-ifelse(tr$DISTRT=="0.0.0.0.0","NDNT",
                     ifelse(tr$DISTRT=="1.0.0.1.0" | tr$DISTRT=="1.0.0.1.1","FIRE",
                            ifelse(tr$DISTRT=="0.0.1.1.0" | tr$DISTRT=="0.0.1.1.1","INSDIS",
                                   ifelse(tr$DISTRT=="0.1.0.0.1","CUT","OTHER"))))

# plots_dist<-rbind(plots_fire,plots_insdis,plots_cut)

disdis<-count(tr,dist_type,DISTRT)
plots_dist<-tr

# 
# 
# 
# plots_ndnt<-tr[which(tr$DISTRT=="0.0.0.0.0"),]
# plots_fire<-tr[which(tr$DISTRT=="1.0.0.1.0" | tr$DISTRT=="1.0.0.1.1"),]
# plots_insdis<-tr[which(tr$DISTRT=="0.0.1.1.0" | tr$DISTRT=="0.0.1.1.1"),]
# plots_cut<-tr[which(tr$DISTRT=="0.1.0.0.1"),]
# 
# plots_ndnt$dist_type<-"NDNT"
# plots_fire$dist_type<-"FIRE"
# plots_insdis$dist_type<-"INSDIS"
# plots_cut$dist_type<-"CUT"
# 
# plots_dist<-rbind(plots_fire,plots_insdis,plots_cut)

disdis<-count(plots_dist,dist_type)

plots_dist2<-plots_dist[which(plots_dist$STDAGE.1>=20),]

plots_dist3<-plots_dist2[which(plots_dist2$STDSZCD.1<5),]

disdis2<-count(plots_dist3,dist_type)

plots_dist3$fia_sev<-ifelse(plots_dist3$STDAGE.2<=plots_dist3$REMPER.2,1,0)

disdis3<-count(plots_dist3,dist_type,fia_sev)
disdis4<-count(plots_dist3,dist_type,fia_sev,mtbs_sev)

full_fia<-read.csv("all_plots_data_with_seedling_8_24.csv")


pdam18<-separate(plots_dist3, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)

sdam2b<-separate(pdam18, NUNIDS.2, 
                 into = c("st","cty","unt","pl"), remove = FALSE)
sdam2<-sdam2b[which(sdam2b$st!=2),]

sdam2<-sdam2[which(sdam2$st!=15),]



sdam2<-sdam2[which(sdam2$st<57 |sdam2$st>70 |sdam2$st==6),]



plot<-read.csv("../../data/PLOT.csv")

survey<-read.csv("../../data/SURVEY.csv")
survey_sel<-survey[which(survey$P3_OZONE_IND=="N"),]
survey_sel2<-survey_sel[which(survey_sel$ANN_INVENTORY=="Y"),]


# plots_all<-plots
library(dplyr)
library(tidyverse)
plot1<-plot %>% select(CN,SRV_CN,INVYR,STATECD,UNITCD,COUNTYCD,
                       PLOT,PLOT_STATUS_CD,MEASYEAR,REMPER,LAT,LON,ELEV,
                       ECOSUBCD,KINDCD,DESIGNCD)



plot2<-plot1[(plot1$SRV_CN %in% survey_sel2$CN),]

rm(plot)
rm(plot1)

plot2$pplotid<-paste0(plot2$STATECD,"-",plot2$UNITCD,"-",
                      plot2$COUNTYCD,"-",plot2$PLOT)
plot2$pplotidyr<-paste0(plot2$STATECD,"-",plot2$UNITCD,"-",
                        plot2$COUNTYCD,"-",plot2$PLOT,"-",plot2$INVYR)



sdam3<-sdam2[(sdam2$NUNIDS.2 %in% plot2$pplotid),]




seedling_all<-read.csv("../data/SEEDLING.CSV")


seedling_all$nunid<-paste0(seedling_all$STATECD,"-",seedling_all$UNITCD,"-",
                           seedling_all$COUNTYCD,"-", seedling_all$PLOT,
                           "-",seedling_all$INVYR)


fplot1<-aggregate(TREECOUNT~nunid, seedling_all, FUN=sum)
fplot3<-count(seedling_all,nunid,SPCD)
fplot4<-count(fplot3,nunid)

fdam1<-merge(fplot1,fplot4,by="nunid")

seedling_all$con<-ifelse(seedling_all$SPGRPCD<25|seedling_all$SPGRPCD==51|
                           seedling_all$SPGRPCD==52,1,0)

seedling12b<-seedling_all[which(seedling_all$con==1),]

fplot7<-aggregate(TREECOUNT~nunid, seedling12b, FUN=sum)

fplot9b<-count(seedling12b,nunid,SPCD)
fplot9<-count(fplot9b,nunid)


fdam4<-merge(fdam1,fplot7,by="nunid",all.x=TRUE)

fdam6<-merge(fdam4,fplot9,by="nunid",all.x=TRUE)


colnames(fdam6)<-c("nunid","seed_count","sd_spp_rich",
                   "seed_count_con","sd_spp_rich_con")

fdam6$nunid_cp<-fdam6$nunid
fdam6$seed_count_con[is.na(fdam6$seed_count_con)] <- 0
fdam6$sd_spp_rich_con[is.na(fdam6$sd_spp_rich_con)] <- 0


#
# seedling_data$nunid.1<-paste0(seedling_data$NUNIDS,"-",seedling_data$INVYR.x)
# seedling_data$nunid.2<-paste0(seedling_data$NUNIDS,"-",seedling_data$INVYR.y)


plots_seed_n<-merge(sdam3,fdam6,by.x="NUNID.1",by.y="nunid",all.x=TRUE)
plots_seed_n2<-merge(plots_seed_n,fdam6,by.x="NUNID.2",by.y="nunid",all.x=TRUE)


colnames(plots_seed_n2)[53]<-"seed_count.1"
colnames(plots_seed_n2)[54]<-"sd_spp_rich.1"
colnames(plots_seed_n2)[55]<-"seed_count_con.1"
colnames(plots_seed_n2)[56]<-"sd_spp_rich_con.1"
colnames(plots_seed_n2)[58]<-"seed_count.2"
colnames(plots_seed_n2)[59]<-"sd_spp_rich.2"
colnames(plots_seed_n2)[60]<-"seed_count_con.2"
colnames(plots_seed_n2)[61]<-"sd_spp_rich_con.2"



# 
# sdam2b<-separate(plots_seed_n2, NUNID.1, 
#                  into = c("st.1","county.1","unit.1","plot.1","invyr.1"), remove = FALSE)
# 
# hhh<-count(sdam2b,st.1,invyr.1,seed_count.1)
# sdam2b$na<-ifelse(sdam2b$seed_count.1>0,"data","nodata")
# iii<-count(sdam2b,st.1,invyr.1,na)
# 
# 
# iii$state_inv<-paste0(iii$st.1,"-",iii$invyr.1)
# 
# agg1<-iii[which(iii$na=="data"),]
# agg2<-iii[is.na(iii$na),]
# 
# agg3<-merge(agg1,agg2,by="state_inv",all=TRUE)
# 
# 
# agg3$tot<-agg3$n.x+agg3$n.y
# agg3$per_missing<-(agg3$n.y/agg3$tot)*100
# 
# agg3$per_missing2<-ifelse(is.na(agg3$n.x),100,
#                           ifelse(is.na(agg3$n.y),0,agg3$per_missing))
#                           
# 
# agg4<-agg3[,c(1,2,3,5,9,12)]
# colnames(agg4)<-c("STATE-INVYR","STATE","INVYR","Present","Missing",
#                   "Percentage of missing data")
# 
# 
# 
# sdam2c<-separate(plots_seed_n2, NUNID.2, 
#                  into = c("st.2","county.2","unit.2","plot.2","invyr.2"), remove = FALSE)
# 
# hhh2<-count(sdam2c,st.2,invyr.2,seed_count.2)
# sdam2c$na<-ifelse(sdam2c$seed_count.2>0,"data","nodata")
# iii2<-count(sdam2c,st.2,invyr.2,na)
# 
# 
# iii2$state_inv2<-paste0(iii2$st.2,"-",iii2$invyr.2)
# 
# agg11<-iii2[which(iii2$na=="data"),]
# agg21<-iii2[is.na(iii2$na),]
# 
# agg31<-merge(agg11,agg21,by="state_inv2",all=TRUE)
# 
# 
# agg31$tot<-agg31$n.x+agg31$n.y
# agg31$per_missing<-(agg31$n.y/agg31$tot)*100
# 
# agg31$per_missing2<-ifelse(is.na(agg31$n.x),100,
#                           ifelse(is.na(agg31$n.y),0,agg31$per_missing))
# 
# 
# agg41<-agg31[,c(1,2,3,5,9,12)]
# colnames(agg41)<-c("STATE-INVYR2","STATE2","INVYR2","Present2","Missing2",
#                   "Percentage of missing data2")
# 
# 
# agg_both<-merge(agg4,agg41,by.x="STATE",by.y="STATE2")
# 
# write.csv(agg4,"pre_dist_seedling_presence.csv")
# write.csv(agg41,"post_dist_seedling_presence.csv")


na_first<-plots_seed_n2[is.na(plots_seed_n2$seed_count.1),]
na_second<-plots_seed_n2[is.na(plots_seed_n2$seed_count.2),]
# plots_seed<-plots_seed_n2[!is.na(plots_seed_n2$seed_count.2) |
#                                     !is.na (plots_seed_n2$seed_count.1),]

# plots_seed<-merge(sdam12,seedling_data,by.x="NUNID.1",by.y="nunid.1",all.x=TRUE)

plots_seed<-plots_seed_n2

plots_seed$seed_count.1[is.na(plots_seed$seed_count.1)] <- 0
plots_seed$seed_count.2[is.na(plots_seed$seed_count.2)] <- 0
plots_seed$sd_spp_rich.1[is.na(plots_seed$sd_spp_rich.1)] <- 0
plots_seed$sd_spp_rich.2[is.na(plots_seed$sd_spp_rich.2)] <- 0

plots_seed$seed_count_con.1[is.na(plots_seed$seed_count_con.1)] <- 0
plots_seed$seed_count_con.2[is.na(plots_seed$seed_count_con.2)] <- 0
plots_seed$sd_spp_rich_con.1[is.na(plots_seed$sd_spp_rich_con.1)] <- 0
plots_seed$sd_spp_rich_con.2[is.na(plots_seed$sd_spp_rich_con.2)] <- 0

plots_seed$seed_count.1<-plots_seed$seed_count.1*74.96*2.471
plots_seed$seed_count.2<-plots_seed$seed_count.2*74.96*2.471

plots_seed$seed_count_con.1<-plots_seed$seed_count_con.1*74.96*2.471
plots_seed$seed_count_con.2<-plots_seed$seed_count_con.2*74.96*2.471


plots_seed$seed_ct_ch<-plots_seed$seed_count.2-plots_seed$seed_count.1
plots_seed$sdsp_rh_ch<-plots_seed$sd_spp_rich.2-plots_seed$sd_spp_rich.1

plots_seed$seed_ct_ch_con<-plots_seed$seed_count_con.2-plots_seed$seed_count_con.1
plots_seed$sdsp_rh_ch_con<-plots_seed$sd_spp_rich_con.2-plots_seed$sd_spp_rich_con.1

# plots_seed$AGB_CHP<-plots_seed$AGB.2-plots_seed$AGB_CHP


sdam3<-plots_seed

sdam3$AGB.11<-(sdam3$AGB.1*453.6*2.471)/1000000
sdam3$AGB.22<-(sdam3$AGB.2*453.6*2.471)/1000000
sdam3$BALIVE.11<-(sdam3$BALIVE.1*30.48*30.48*2.471)/10000
sdam3$BALIVE.22<-(sdam3$BALIVE.2*30.48*30.48*2.471)/10000

sdam3$mtbs_sev1<-ifelse(sdam3$mtbs_sev==1 & sdam3$AGB.22<50,1,0)
sss<-count(sdam3,mtbs_sev,mtbs_sev1)

# 
# sdam3_sev1<-sdam3_sev[which(sdam3_sev$AGB.22<50),]
# # sdam3_sev2<-sdam3_sev1[which(sdam3_sev1$seed_count.2<50000),]
# sdam3_sev2<-sdam3_sev1

# sdam3f<-rbind(sdam3_sev2,sdam3_mild)

sdam4<-merge(full_fia,sdam3,by="NUNID.2",all=TRUE)

sdam4$dist_type2<-ifelse(sdam4$dist_type.x=="FIRE","FIRE",
                         ifelse(sdam4$dist_type.x=="NDNT","NDNT","OTHER"))

iii<-count(sdam4,fia_sev2,fia_sev1)
kkk<-count(sdam3,mtbs_sev,mtbs_sev1)
lll<-count(sdam4,mtbs_sev1,fia_sev1,dist_type.x)
mmm<-count(sdam4,mtbs_sev1,fia_sev1,dist_type2)

cross<-data.frame(table(sdam4$mtbs_sev1,sdam4$fia_sev1,sdam4$dist_type2))

sdam5<-sdam4[!is.na(sdam4$mtbs_sev),]

ecosel<-read.csv("../disturbance/eco_select.csv")

sdam4$ecocode <- trimws(sdam4$Spl_1.x, which = c("left"))

sdam5$ecocode <- trimws(sdam5$Spl_1.x, which = c("left"))

library(operators)
sdam3b<-sdam4[(sdam4$ecocode %in% ecosel$econew),]
sdam3a<-sdam4[(sdam4$ecocode %!in% ecosel$econew),]


sdam3bf<-sdam5[(sdam5$ecocode %in% ecosel$econew),]
sdam3af<-sdam5[(sdam5$ecocode %!in% ecosel$econew),]

table_dist_sev_west<-count(sdam3b,dist_type2, mtbs_sev1,fia_sev1)

table_dist_sev_east<-count(sdam3a,dist_type2, mtbs_sev1,fia_sev1)


table_dist_sev_west_f<-count(sdam3bf,dist_type2, mtbs_sev1,fia_sev1)

table_dist_sev_east_f<-count(sdam3af,dist_type2, mtbs_sev1,fia_sev1)


write.csv(table_dist_sev_west,"tabl_dist_sev_west_mtbs_fia.csv")
write.csv(table_dist_sev_east,"tabl_dist_sev_east_mtbs_fia.csv")



write.csv(table_dist_sev_west_f,"tabl_dist_sev_west_mtbs.csv")
write.csv(table_dist_sev_east_f,"tabl_dist_sev_east_mtbs.csv")

write.csv(sdam4,"plots_data_with_fia_mtbs_seedling_7_14.csv")
write.csv(sdam5,"plots_data_with_only_mtbs_seedling_f_7_14.csv")






tr<-pdam16
cut_agnt <- c(10)

tr$CUT <- ifelse(tr$TRTCD1.2 %in% cut_agnt | tr$TRTCD2.2 %in% cut_agnt 
                 | tr$TRTCD3.2 %in% cut_agnt, 1,  0)

fire_agnt <- c(30,31,32)

tr$FIRE_ALL <- ifelse(tr$DSTRBCD1.2 %in% fire_agnt | tr$DSTRBCD2.2 %in% fire_agnt 
                      | tr$DSTRBCD3.2 %in% fire_agnt, 1,  0)

insdis_agnt <- c(10,11,12,20,21,22)
tr$INSDIS <- ifelse(tr$DSTRBCD1.2 %in% insdis_agnt | tr$DSTRBCD2.2 %in% insdis_agnt 
                    | tr$DSTRBCD3.2 %in% insdis_agnt, 1,  0)



tr$Damage<-ifelse(tr$DSTRBCD1.2|tr$DSTRBCD2.2|tr$DSTRBCD3.2>0,1,0)
tr$Treatment<-ifelse(tr$TRTCD1.2|tr$TRTCD2.2|tr$TRTCD3.2>0,1,0)
tr$fia_sev<-ifelse(tr$STDAGE.1>tr$STDAGE.2 & tr$STDAGE.2<=tr$REMPER.2,1,0)

kkk<-count(tr,mtbs_sev,FIRE_ALL,fia_sev)


write.csv(tr,"plots_with_MTBS.csv")

tr$dist1<-ifelse(tr$DSTRBCD1.1>0,1,0)
tr$dist2<-ifelse(tr$DSTRBCD1.2>0 & !is.na (tr$DSTRBCD1.2),1,0)
tr$dist3<-ifelse(tr$DSTRBCD1.x>0 & !is.na (tr$DSTRBCD1.x),1,0)
tr$dist4<-ifelse(tr$DSTRBCD1.y>0 & !is.na (tr$DSTRBCD1.y),1,0)
tr$trt1<-ifelse(tr$TRTCD1.1>0 & !is.na (tr$TRTCD1.1),1,0)
tr$trt2<-ifelse(tr$TRTCD1.2>0 & !is.na (tr$TRTCD1.2),1,0)
tr$trt3<-ifelse(tr$TRTCD1.x>0 & !is.na (tr$TRTCD1.x),1,0)
tr$trt4<-ifelse(tr$TRTCD1.y>0 & !is.na (tr$TRTCD1.y),1,0)

## remove disturbed and treated plots in first inventory

tr$dist1234<-interaction(tr$dist1,tr$dist2,tr$dist3,tr$dist4)
tr$trt1234<-interaction(tr$trt1,tr$trt2,tr$trt3,tr$trt4)

tr$D1<-ifelse(tr$dist1234=="1.0.0.0","IDND",
                ifelse(tr$dist1234=="1.1.0.0"|
                         tr$dist1234=="1.1.1.0"|tr$dist1234=="1.1.0.1"|
                         tr$dist1234=="1.1.1.1","IDFD",
                       ifelse(tr$dist1234=="0.0.0.0","ND",
                              ifelse(tr$dist1234=="0.1.0.0"|
                                       tr$dist1234=="0.1.1.0"|tr$dist1234=="0.1.0.1"|
                                       tr$dist1234=="0.1.1.1","NDFD",
                                     ifelse(tr$dist1234=="1.0.1.0"|
                                              tr$dist1234=="1.0.1.1","IDSD",
                                            ifelse(tr$dist1234=="0.0.1.0"|
                                                     tr$dist1234=="0.0.1.1","NDSD",
                                                   ifelse(tr$dist1234=="1.0.0.1","IDTD",
                                                          ifelse(tr$dist1234=="0.0.0.1","NDTD",
                                                                 "NN"))))))))

kkk<-count(tr,D1)

tr$T1<-ifelse(tr$trt1234=="1.0.0.0","ITNT",
                ifelse(tr$trt1234=="1.1.0.0"|
                         tr$trt1234=="1.1.1.0"|tr$trt1234=="1.1.0.1"|
                         tr$trt1234=="1.1.1.1","ITFT",
                       ifelse(tr$trt1234=="0.0.0.0","NT",
                              ifelse(tr$trt1234=="0.1.0.0"|
                                       tr$trt1234=="0.1.1.0"|tr$trt1234=="0.1.0.1"|
                                       tr$trt1234=="0.1.1.1","NTFT",
                                     ifelse(tr$trt1234=="1.0.1.0"|
                                              tr$trt1234=="1.0.1.1","ITST",
                                            ifelse(tr$trt1234=="0.0.1.0"|
                                                     tr$trt1234=="0.0.1.1","NTST",
                                                   ifelse(tr$trt1234=="1.0.0.1","ITTT",
                                                          ifelse(tr$trt1234=="0.0.0.1","NTTT",
                                                                 "NN"))))))))












trees_fire_11<-trees_data_all



# calculate since fire 
trees_fire_11$since_fire<- trees_fire_11$MEASYEAR.2 - trees_fire_11$r_fire_yr



# table compare tree status for different severity levels
table_11<-data.frame(count(trees_fire_11,mtbs_sev1,STATUSCD.1,STATUSCD.2))


colnames(table_11)<- c("MTBS severity","Pre-fire status","Post-fire status","n")

df2 <- trees_fire_11 %>% 
  group_by(mtbs_sev1,STATUSCD.1,STATUSCD.2) %>% 
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
plot3<-aggregate(mtbs_sev1~NUNIDS.2, trees_fire_11, FUN=mean)


dam11<-merge(plot1,plot2,by="NUNIDS.2")
dam12<-merge(dam11,plot22,by="NUNIDS.2")
dam1<-merge(dam12,plot3,by="NUNIDS.2")


dam1<-dam1[which(dam1$since_fire>0),]

dam1$rep_stand<-ifelse(dam1$STDAGE.2<=dam1$since_fire,1,0)

write.csv(trees_fire_11,"nndata_with_MTBS_only1.csv")
write.csv(dam1,"nnplots_with_MTBS_only1.csv")


library(ggplot2)
library(gridExtra)

dev.new()
png("nall_states_sincefire_stdage22.jpeg", width = 600, height = 600)
par(xpd = T, mar = c(12,6,8,6))
par(mfrow=c(2,1))


p1<-ggplot(data=dam1) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(mtbs_sev1))) +
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
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(mtbs_sev1))) +
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

table_sev_rep<-data.frame(count(dam1, mtbs_sev1,rep_stand))
colnames(table_sev_rep)<-c("MTBS severity","stand replaced","n")
write.csv(table_sev_rep,"all_states_MTBS_severity_stand_replaced1.csv")

dam22<-dam1[which(dam1$rep_stand==1),]

dev.new()
png("all_states_sincefire_stdage_std_rep11.jpeg", width = 600, height = 400)
par(xpd = T, mar = c(6,6,6,6))
par(mfrow=c(2,1))



p3<- ggplot(data=dam22) +
  geom_jitter(aes(x=since_fire, y=STDAGE.1,color=as.factor(mtbs_sev1))) +
  scale_color_manual(name="fire severity",labels=c("low","high"), 
                     values=c("0"="gold1","1"="red")) + theme_bw()



p4<- ggplot(data=dam22) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(mtbs_sev1))) +
  scale_color_manual(name="fire severity",labels=c("low","high"), 
                     values=c("0"="gold1","1"="red")) + theme_bw()


q2<-grid.arrange(p3,p4)

dev.off()

hum.names <- as_labeller(c('0' = "Low fire intensity", '1' = "High fire intensity"))

dev.new()
png("all_states_stacked_numbers_status22.jpeg", width = 400, height = 400)
par(xpd = T, mar = c(6,6,6,6))

trees_fire_11 %>%
  ggplot() +
  geom_bar(aes(x = factor(STATUSCD.1), fill = factor(STATUSCD.2))) +
  facet_wrap(~mtbs_sev1,nrow=1,labeller = hum.names)+
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

# splot16<-aggregate(severity~NUNIDS.2, trees_fire_11, FUN=mean)
splot16<-aggregate(mtbs_sev~NUNIDS.2, trees_fire_11, FUN=mean)

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
sdam_MTBS3<-merge(sdam_MTBS2,splot16,by="NUNIDS.2")

saveRDS(sdam_MTBS2,"nnn2_all_plots_with_disturb_MTBS411.RDS")


pdam16$cut_plot<-ifelse(pdam16$CUT>0,1,0)
pdam16$fire_plot<-ifelse(pdam16$FIRE_ALL>0,1,0)
pdam16$insdis_plot<-ifelse(pdam16$INSDIS>0,1,0)

pdam16$dist_int<-interaction(pdam16$cut_plot,pdam16$fire_plot,pdam16$insdis_plot)




#small table for MTBS only plots
pdam_MTBS<-merge(pdam16,pplot8,by="NUNIDSS.2")
pdam_MTBS2<-merge(pdam_MTBS,pplot16,by="NUNIDSS.2")

# tr<-tr[which(tr$STATUSCD.2==1),]

saveRDS(pdam_MTBS2,"nall_plots_with_disturb_MTBS_411.RDS")
saveRDS(pdam16,"nall_plots_with_disturb_FIA_411.RDS")
saveRDS(tr,"nall_trees_with_disturb_FIA_411.RDS")

# pdam_MTBS2<-readRDS("nall_plots_with_disturb_MTBS.RDS")
# 
# pdam16<-readRDS("nall_plots_with_disturb_FIA.RDS")


write.csv(pdam_MTBS2,"nall_plots_with_disturb_MTBS.csv")
write.csv(pdam16,"nall_plots_with_disturb_FIA.csv")

pdam17<-separate(pdam16, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)

# write.csv(pdam17,"all_plots_with_disturb_FIA_ECO1.csv")




trees_fire_22<-trfr[trfr$NUNIDS.2 %in% fire$nplotid_cp,]

#data with first measurements less than fire year
trees_fire_33<-trees_fire_22[which(trees_fire_22$MEASYEAR.1<=trees_fire_22$MTBS_fireyear),]

#data with first measurements less and second measurements greater than fire year
trees_fire_44<-trees_fire_33[which(trees_fire_33$MEASYEAR.2>=trees_fire_33$MTBS_fireyear),]

trees_fire_44$time<-1

tablepp<-count(trees_fire_44,NUNIDS.2)

#data with first measurements less than fire year
trees_fire_55<-trees_fire_22[which(trees_fire_22$MEASYEAR.2<=trees_fire_22$MTBS_fireyear),]

trees_fire_66<-trees_fire_55[which(trees_fire_55$MEASYEAR.x>=trees_fire_55$MTBS_fireyear),]

trees_fire_66$time<-2
#data with first measurements less than fire year
trees_fire_77<-trees_fire_22[which(trees_fire_22$MEASYEAR.x<=trees_fire_22$MTBS_fireyear),]

trees_fire_88<-trees_fire_77[which(trees_fire_77$MEASYEAR.y>=trees_fire_77$MTBS_fireyear),]

trees_fire_88$time<-3
#data with first &second measureemtns less and third measurements greater than fire year
# trees_fire_55<-trees_fire_33[which(trees_fire_33$MEASYEAR.2<trees_fire_33$MTBS_fireyear
#                                & trees_fire_33$MEASYEAR>trees_fire_33$MTBS_fireyear),]


trees_data_first<-cbind(trees_fire_44[,1:147],trees_fire_44[,294:312])
trees_data_first1<-select(trees_data_first,-c("NUNIDS.1"))


trees_data_second<-cbind(trees_fire_66[,1:2],trees_fire_66[,76:220],trees_fire_66[,294:312])
trees_data_second1<-select(trees_data_second,-c("NUNIDS.x"))

trees_data_third<-cbind(trees_fire_88[,1:2],trees_fire_88[,148:293],trees_fire_88[,294:312])
trees_data_third1<-select(trees_data_third,-c("NUNIDS.x","NUNIDS.y"))

colnames1<-colnames(trees_data_first1)

colnames(trees_data_second1)<-colnames1
colnames(trees_data_third1)<-colnames1



trees_data_all<-rbind(trees_data_first1,trees_data_second1,trees_data_third1)
trees_data_all$mtbs_sev1<-ifelse(trees_data_all$sev_mean>2,1,0)
trees_data_all$first_dis_pres<-ifelse(trees_data_all$DSTRBCD1.1|
                                        trees_data_all$DSTRBCD2.1|
                                        trees_data_all$DSTRBCD3.1>0,1,0)

trees_data_all$second_dis_pres<-ifelse(trees_data_all$DSTRBCD1.2|
                                         trees_data_all$DSTRBCD2.2|
                                         trees_data_all$DSTRBCD3.2>0,1,0)
count(trees_data_all,first_dis_pres,second_dis_pres)




trees_fire_11<-trees_data_all



# calculate since fire 
trees_fire_11$since_fire<- trees_fire_11$MEASYEAR.2 - trees_fire_11$r_fire_yr



# table compare tree status for different severity levels
table_11<-data.frame(count(trees_fire_11,mtbs_sev1,STATUSCD.1,STATUSCD.2))


colnames(table_11)<- c("MTBS severity","Pre-fire status","Post-fire status","n")

df2 <- trees_fire_11 %>% 
  group_by(mtbs_sev1,STATUSCD.1,STATUSCD.2) %>% 
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
plot3<-aggregate(mtbs_sev1~NUNIDS.2, trees_fire_11, FUN=mean)


dam11<-merge(plot1,plot2,by="NUNIDS.2")
dam12<-merge(dam11,plot22,by="NUNIDS.2")
dam1<-merge(dam12,plot3,by="NUNIDS.2")


dam1<-dam1[which(dam1$since_fire>0),]

dam1$rep_stand<-ifelse(dam1$STDAGE.2<=dam1$since_fire,1,0)

write.csv(trees_fire_11,"nndata_with_MTBS_only1.csv")
write.csv(dam1,"nnplots_with_MTBS_only1.csv")


library(ggplot2)
library(gridExtra)

dev.new()
png("nall_states_sincefire_stdage22.jpeg", width = 600, height = 600)
par(xpd = T, mar = c(12,6,8,6))
par(mfrow=c(2,1))


p1<-ggplot(data=dam1) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(mtbs_sev1))) +
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
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(mtbs_sev1))) +
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

table_sev_rep<-data.frame(count(dam1, mtbs_sev1,rep_stand))
colnames(table_sev_rep)<-c("MTBS severity","stand replaced","n")
write.csv(table_sev_rep,"all_states_MTBS_severity_stand_replaced1.csv")

dam22<-dam1[which(dam1$rep_stand==1),]

dev.new()
png("all_states_sincefire_stdage_std_rep11.jpeg", width = 600, height = 400)
par(xpd = T, mar = c(6,6,6,6))
par(mfrow=c(2,1))



p3<- ggplot(data=dam22) +
  geom_jitter(aes(x=since_fire, y=STDAGE.1,color=as.factor(mtbs_sev1))) +
  scale_color_manual(name="fire severity",labels=c("low","high"), 
                     values=c("0"="gold1","1"="red")) + theme_bw()



p4<- ggplot(data=dam22) +
  geom_jitter(aes(x=since_fire, y=STDAGE.2,color=as.factor(mtbs_sev1))) +
  scale_color_manual(name="fire severity",labels=c("low","high"), 
                     values=c("0"="gold1","1"="red")) + theme_bw()


q2<-grid.arrange(p3,p4)

dev.off()

hum.names <- as_labeller(c('0' = "Low fire intensity", '1' = "High fire intensity"))

dev.new()
png("all_states_stacked_numbers_status22.jpeg", width = 400, height = 400)
par(xpd = T, mar = c(6,6,6,6))

trees_fire_11 %>%
  ggplot() +
  geom_bar(aes(x = factor(STATUSCD.1), fill = factor(STATUSCD.2))) +
  facet_wrap(~mtbs_sev1,nrow=1,labeller = hum.names)+
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

# splot16<-aggregate(severity~NUNIDS.2, trees_fire_11, FUN=mean)
splot16<-aggregate(mtbs_sev~NUNIDS.2, trees_fire_11, FUN=mean)

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
sdam_MTBS3<-merge(sdam_MTBS2,splot16,by="NUNIDS.2")

saveRDS(sdam_MTBS2,"nnn2_all_plots_with_disturb_MTBS411.RDS")


pdam16$cut_plot<-ifelse(pdam16$CUT>0,1,0)
pdam16$fire_plot<-ifelse(pdam16$FIRE_ALL>0,1,0)
pdam16$insdis_plot<-ifelse(pdam16$INSDIS>0,1,0)

pdam16$dist_int<-interaction(pdam16$cut_plot,pdam16$fire_plot,pdam16$insdis_plot)

pdam16<-separate(pdam16, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)

sdam2b<-separate(pdam16, NUNIDSS.2, 
                 into = c("st","cty","unt","pl","time"), remove = FALSE)
sdam2<-sdam2b[which(sdam2b$st!=2),]

sdam2<-sdam2[which(sdam2$st!=15),]



sdam2<-sdam2[which(sdam2$st<57 |sdam2$st>70 |sdam2$st==6),]


ecosel<-read.csv("../disturbance/eco_select.csv")

sdam2$ecocode <- trimws(sdam2$Spl_1, which = c("left"))

library(operators)
sdam3b<-sdam2[(sdam2$ecocode %in% ecosel$econew),]
sdam3b<-sdam2[(sdam2$ecocode %!in% ecosel$econew),]

sdam3b$fia_sev<-ifelse(sdam3b$STDAGE.2<=sdam3b$REMPER.2,1,0)


only_cuts<-sdam3b[which(sdam3b$dist_int=="1.0.0"),]
only_fires<-sdam3b[which(sdam3b$dist_int=="0.1.0"),]
only_insdis<-sdam3b[which(sdam3b$dist_int=="0.0.1"),]


sev_data<-sdam3b[which(sdam3b$STDAGE.1>10),]

dist_types<-count(sdam3b,rep_stand_all2)






# mtbs_all<-readRDS("nall_plots_with_disturb_MTBS.RDS")
# 
# fia_all<-readRDS("nall_plots_with_disturb_FIA.RDS")

mtbs_all<-sdam_MTBS2
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

