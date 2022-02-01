setwd('C:/Karuns_documents/fire_MTBS/fire_forest_ecosystem')

rm(list=ls())

#test with FL and GA plots
FIA_plots <- read.csv(file = '../data/PLOT.csv', header = TRUE)

# copy CN codes for check
FIA_plots$CNCP <- FIA_plots$CN

options(scipen = 999)

library(tidyr)
library(dplyr)
library(tidyverse)
install.packages("readxl")
library(ggplot2)
library(splitstackshape)
# read MTBS data 
mtbs_data_alle<-data.frame(read.csv("../data/fire_severity_FL.csv"))
mtbs_data_allf <- mtbs_data_alle[ which(mtbs_data_alle$range_sev < 2),]
mtbs_data_allgg <- mtbs_data_allf[ !is.na(mtbs_data_allf$cen_sev),]
mtbs_data_allg<-mtbs_data_allgg[!is.na(mtbs_data_allgg$mean_sev),]
mtbs_data_allg$mean_sev_r<-round(mtbs_data_allg$mean_sev)


# select only GA data from 2013
#mtbs_data_FL <- mtbs_data_allg[which(mtbs_data_allg$statewfire==12),]
mtbs_data_FL<-mtbs_data_allg

# join MTBS GA data with FIA data
mtbs_plots_FL <- merge(mtbs_data_FL,FIA_plots,by.x="prefire_CN_CD",by.y="CN")



# create unique plot id
mtbs_plots_FL$nplotid <-paste0(mtbs_plots_FL$STATECD,"-",mtbs_plots_FL$UNITCD,"-",
                               mtbs_plots_FL$COUNTYCD,"-", mtbs_plots_FL$PLOT)

mtbs_plots_FL$nplotidyr <-paste0(mtbs_plots_FL$STATECD,"-",mtbs_plots_FL$UNITCD,"-",
                                 mtbs_plots_FL$COUNTYCD,"-", mtbs_plots_FL$PLOT,
                                 "-",mtbs_plots_FL$INVYEAR)

FIA_plots$fplotid<-paste0(FIA_plots$STATECD,"-",FIA_plots$UNITCD,"-",
                          FIA_plots$COUNTYCD,"-",FIA_plots$PLOT)

FIA_plots$fplotidyr<-paste0(FIA_plots$STATECD,"-",FIA_plots$UNITCD,"-",
                            FIA_plots$COUNTYCD,"-",FIA_plots$PLOT,"-",FIA_plots$MEASYEAR)

#merge fire data with all time fia plots
mtbs_plots_FL_allyr <- merge(mtbs_plots_FL, FIA_plots,by.x="nplotid",by.y="fplotid")

mtbs_plots_FL_allyr$nnplotidyr<-paste0(mtbs_plots_FL_allyr$STATECD.y,"-",mtbs_plots_FL_allyr$UNITCD.y,"-",
                                       mtbs_plots_FL_allyr$COUNTYCD.y,"-",mtbs_plots_FL_allyr$PLOT.y,
                                       "-",mtbs_plots_FL_allyr$MEASYEAR.y)

# summarizing the table
library(dplyr)
library(reshape)

# select plots before fire year


#i=2005

a<- min(mtbs_plots_FL_allyr$MTBSfireyear)
b<- max(mtbs_plots_FL_allyr$MTBSfireyear)

colnn<-length(mtbs_plots_FL_allyr)



pret2<-data.frame(matrix(ncol = colnn, nrow = 0))

for(i in a:b) {  
  # i-th element of `u1` squared into `i`-th position of `usq`
  
  pret1a <- mtbs_plots_FL_allyr[which(mtbs_plots_FL_allyr$MTBSfireyear==i),]
  
  pret1b <- pret1a[which(pret1a$MEASYEAR.y < i),]
  
  
  pret2<-rbind(pret2,pret1b)
  
}

pre_sub11<-data.frame(count(pret2,fire_id,nplotid))
pre_sub12<-data.frame(count(pret2,nplotid,fire_id))


pre_sub13 <- pre_sub12[which(pre_sub12$n > 1),]

pre_sub14 <- pre_sub12[which(pre_sub12$n == 1),]


pre_lat<-aggregate(LAT.x~nplotid, pret2, FUN=max)
pre_lon<-aggregate(LON.x~nplotid, pret2, FUN=max)
pre_sev_c<-aggregate(cen_sev~nplotid, pret2, FUN=max)
pre_sev_m<-aggregate(mean_sev_r~nplotid, pret2, FUN=max)
pre_fireyr<-aggregate(MTBSfireyear~nplotid, pret2, FUN=mean)


pre_vars1 <- merge(pre_lat, pre_lon, by="nplotid")
pre_vars11 <- merge(pre_vars1, pre_fireyr, by="nplotid")
pre_vars2 <- merge(pre_vars11, pre_sev_c, by="nplotid")
pre_vars3 <- merge(pre_vars2, pre_sev_m, by="nplotid")

pre_sub17<-data.frame(count(pre_sub13,nplotid))
pre_sub18<-data.frame(count(pre_sub14,nplotid))

mtbs_pre_vars1 <- merge(pre_sub14,pre_vars3,by="nplotid")
mtbs_pre_vars2 <- merge(pre_sub13,pre_vars3,by="nplotid")

mtbs_pre_both<-rbind(mtbs_pre_vars1,mtbs_pre_vars2)

mtbs_unique_pre_all<-data.frame(count(pre_vars2,nplotid))
mtbs_unique_pre_1<-data.frame(count(mtbs_pre_vars1,nplotid))
mtbs_unique_pre_2<-data.frame(count(mtbs_pre_vars2,nplotid))



# work on post fire plots

post2<-data.frame(matrix(ncol = colnn, nrow = 0))

for(i in a:b) {  
  # i-th element of `u1` squared into `i`-th position of `usq`
  
  post1a <- mtbs_plots_FL_allyr[which(mtbs_plots_FL_allyr$MTBSfireyear==i),]
  
  post1b <- post1a[which(post1a$MEASYEAR.y > i),]
  
  
  post2<-rbind(post1b,post2)
  
}

post_sub11<-data.frame(count(post2,nplotid))
post_sub12<-data.frame(count(post2,nplotid,fire_id))

post_sub13 <- post_sub12[which(post_sub12$n > 1),]
post_sub14 <- post_sub12[which(post_sub12$n == 1),]

post_lat<-aggregate(LAT.x~nplotid, post2, FUN=max)
post_lon<-aggregate(LON.x~nplotid, post2, FUN=max)
post_sev_c<-aggregate(cen_sev~nplotid, post2, FUN=max)
post_sev_m<-aggregate(mean_sev_r~nplotid, post2, FUN=max)
post_fireyr<-aggregate(MTBSfireyear~nplotid, post2, FUN=mean)


post_vars1 <- merge(post_lat, post_lon, by="nplotid")
post_vars11 <- merge(post_vars1, post_fireyr, by="nplotid")
post_vars2 <- merge(post_vars11, post_sev_c, by="nplotid")
post_vars3 <- merge(post_vars2, post_sev_c, by="nplotid")


post_sub17<-data.frame(count(post_sub13,nplotid))
post_sub18<-data.frame(count(post_sub14,nplotid))

mtbs_post_vars1 <- merge(post_sub14,post_vars3,by="nplotid")
mtbs_post_vars2 <- merge(post_sub13,post_vars3,by="nplotid")
mtbs_post_both<-rbind(mtbs_post_vars1,mtbs_post_vars2)

mtbs_unique_post_all<-data.frame(count(post_vars2,nplotid))
mtbs_unique_post_1<-data.frame(count(mtbs_post_vars1,nplotid))
mtbs_unique_post_2<-data.frame(count(mtbs_post_vars2,nplotid))


mtbs_prepos_11<-merge(mtbs_pre_vars1,mtbs_post_vars1,by="nplotid",all.y=FALSE)
mtbs_uni_prepos_11<-data.frame(count(mtbs_prepos_11,nplotid))

jj1<-aggregate(fire_id.x~nplotid, mtbs_prepos_11, FUN=max)
jj2<-aggregate(LAT.x.x~nplotid, mtbs_prepos_11, FUN=max)
jj3<-aggregate(LON.x.x~nplotid, mtbs_prepos_11, FUN=max)
jj4<-aggregate(n.x~nplotid, mtbs_prepos_11, FUN=max)
jj5<-aggregate(n.y~nplotid, mtbs_prepos_11, FUN=max)
jj6<-aggregate(cen_sev~nplotid, mtbs_prepos_11, FUN=max)
jj7<-aggregate(mean_sev_r~nplotid, mtbs_prepos_11, FUN=max)
jj8<-aggregate(MTBSfireyear.x~nplotid, mtbs_prepos_11, FUN=max)


mm1<-merge(jj1,jj2,by="nplotid")
mm2<-merge(mm1,jj3,by="nplotid")
mm3<-merge(mm2,jj4,by="nplotid")
mm4<-merge(mm3,jj5,by="nplotid")
mm5<-merge(mm4,jj6,by="nplotid")
mm6<-merge(mm5,jj7,by="nplotid")
mm7<-merge(mm6,jj8,by="nplotid")



mtbs_prepos_12<-merge(mtbs_pre_vars1,mtbs_post_vars2,by="nplotid",all.y=FALSE)
mtbs_uni_prepos_12<-data.frame(count(mtbs_prepos_12,nplotid))


bjj1<-aggregate(fire_id.x~nplotid, mtbs_prepos_12, FUN=max)
bjj2<-aggregate(LAT.x.x~nplotid, mtbs_prepos_12, FUN=max)
bjj3<-aggregate(LON.x.x~nplotid, mtbs_prepos_12, FUN=max)
bjj4<-aggregate(n.x~nplotid, mtbs_prepos_12, FUN=max)
bjj5<-aggregate(n.y~nplotid, mtbs_prepos_12, FUN=max)
bjj6<-aggregate(cen_sev~nplotid, mtbs_prepos_12, FUN=max)
bjj7<-aggregate(mean_sev_r~nplotid, mtbs_prepos_12, FUN=max)
bjj8<-aggregate(MTBSfireyear.x~nplotid, mtbs_prepos_12, FUN=mean)


bmm1<-merge(bjj1,bjj2,by="nplotid")
bmm2<-merge(bmm1,bjj3,by="nplotid")
bmm3<-merge(bmm2,bjj4,by="nplotid")
bmm4<-merge(bmm3,bjj5,by="nplotid")
bmm5<-merge(bmm4,bjj6,by="nplotid")
bmm6<-merge(bmm5,bjj7,by="nplotid")
bmm7<-merge(bmm6,bjj8,by="nplotid")




mtbs_prepos_21<-merge(mtbs_pre_vars2,mtbs_post_vars1,by="nplotid",all.y=FALSE)
mtbs_uni_prepos_21<-data.frame(count(mtbs_prepos_21,nplotid))



cjj1<-aggregate(fire_id.x~nplotid, mtbs_prepos_21, FUN=max)
cjj2<-aggregate(LAT.x.x~nplotid, mtbs_prepos_21, FUN=max)
cjj3<-aggregate(LON.x.x~nplotid, mtbs_prepos_21, FUN=max)
cjj4<-aggregate(n.x~nplotid, mtbs_prepos_21, FUN=max)
cjj5<-aggregate(n.y~nplotid, mtbs_prepos_21, FUN=max)
cjj6<-aggregate(cen_sev~nplotid, mtbs_prepos_21, FUN=max)
cjj7<-aggregate(mean_sev_r~nplotid, mtbs_prepos_21, FUN=max)
cjj8<-aggregate(MTBSfireyear.x~nplotid, mtbs_prepos_21, FUN=mean)


cmm1<-merge(cjj1,cjj2,by="nplotid")
cmm2<-merge(cmm1,cjj3,by="nplotid")
cmm3<-merge(cmm2,cjj4,by="nplotid")
cmm4<-merge(cmm3,cjj5,by="nplotid")
cmm5<-merge(cmm4,cjj6,by="nplotid")
cmm6<-merge(cmm5,cjj7,by="nplotid")
cmm7<-merge(cmm6,cjj8,by="nplotid")




mtbs_prepos_22<-merge(mtbs_pre_vars2,mtbs_post_vars2,by="nplotid",all.y=FALSE)
mtbs_uni_prepos_22<-data.frame(count(mtbs_prepos_22,nplotid))


djj1<-aggregate(fire_id.x~nplotid, mtbs_prepos_22, FUN=max)
djj2<-aggregate(LAT.x.x~nplotid, mtbs_prepos_22, FUN=max)
djj3<-aggregate(LON.x.x~nplotid, mtbs_prepos_22, FUN=max)
djj4<-aggregate(n.x~nplotid, mtbs_prepos_22, FUN=max)
djj5<-aggregate(n.y~nplotid, mtbs_prepos_22, FUN=max)
djj6<-aggregate(cen_sev~nplotid, mtbs_prepos_22, FUN=max)
djj7<-aggregate(mean_sev_r~nplotid, mtbs_prepos_22, FUN=max)
djj8<-aggregate(MTBSfireyear.x~nplotid, mtbs_prepos_22, FUN=mean)


dmm1<-merge(djj1,djj2,by="nplotid")
dmm2<-merge(dmm1,djj3,by="nplotid")
dmm3<-merge(dmm2,djj4,by="nplotid")
dmm4<-merge(dmm3,djj5,by="nplotid")
dmm5<-merge(dmm4,djj6,by="nplotid")
dmm6<-merge(dmm5,djj7,by="nplotid")
dmm7<-merge(dmm6,djj8,by="nplotid")


mtbs_prepos_all <- rbind(mm7,bmm7,cmm7,dmm7)

ejj1<-aggregate(fire_id.x~nplotid, mtbs_prepos_all, FUN=max)
ejj2<-aggregate(LAT.x.x~nplotid, mtbs_prepos_all, FUN=max)
ejj3<-aggregate(LON.x.x~nplotid, mtbs_prepos_all, FUN=max)
ejj4<-aggregate(n.x~nplotid, mtbs_prepos_all, FUN=max)
ejj5<-aggregate(n.y~nplotid, mtbs_prepos_all, FUN=max)
ejj6<-aggregate(cen_sev~nplotid, mtbs_prepos_all, FUN=max)
ejj7<-aggregate(mean_sev_r~nplotid, mtbs_prepos_all, FUN=max)
ejj8<-aggregate(MTBSfireyear.x~nplotid, mtbs_prepos_all, FUN=mean)


emm1<-merge(ejj1,ejj2,by="nplotid")
emm2<-merge(emm1,ejj3,by="nplotid")
emm3<-merge(emm2,ejj4,by="nplotid")
emm4<-merge(emm3,ejj5,by="nplotid")
emm5<-merge(emm4,ejj6,by="nplotid")
emm6<-merge(emm5,ejj7,by="nplotid")
emm7<-merge(emm6,ejj8,by="nplotid")


colnames(emm7)<-c("nplotid","fire_id_mtbs","lat_n","lon_n","prefire_n",
                  "post_fire_n","sev_cen","sev_mean","MTBS_fireyear")



write.csv(emm7,"../MTBS_fire_FIA_plots11.csv")


ttt<-emm7[which(emm7$prefire_n==1),]


mtbs_uni_prepos_all<-data.frame(count(mtbs_prepos_all,nplotid))

write.csv(mtbs_uni_prepos_11,"../data/plots_measured_11_FL.csv")
write.csv(mtbs_uni_prepos_12,"../data/plots_measured_12_FL.csv")
write.csv(mtbs_uni_prepos_21,"../data/plots_measured_21_FL.csv")
write.csv(mtbs_uni_prepos_22,"../data/plots_measured_22_FL.csv")


