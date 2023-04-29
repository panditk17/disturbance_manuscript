### plots based on disturbances from FIA data

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000)


plots_all<-read.csv("all_plots_data_with_seedling_8_27.csv")

kkk<-count(plots_all,fia_sev1)


## select only severe plots
plots_all4<-plots_all[which(plots_all$fia_sev1==1),]



plot<-read.csv("../../data/PLOT.csv")
require(tidyverse)
require(dplyr)
library(dplyr)

remove.packages("tidyverse")
library(plyr)
plots_selected<-plot %>% select(CN,PREV_PLT_CN,INVYR,STATECD,UNITCD,COUNTYCD,
                                PLOT,ELEV)

plots_selected<-plot[c("CN","PREV_PLT_CN","INVYR","STATECD","UNITCD","COUNTYCD",
                       "PLOT","ELEV")]

plots_selected$nunid<-paste0(plots_selected$STATECD,"-",plots_selected$UNITCD,"-",
                             plots_selected$COUNTYCD,"-",plots_selected$PLOT)
plots_selected$nunidyr<-paste0(plots_selected$STATECD,"-",plots_selected$UNITCD,"-",
                               plots_selected$COUNTYCD,"-",plots_selected$PLOT,"-",
                               plots_selected$INVYR)

plots_all1<-merge(plots_all,plots_selected,by.x="NUNID.1",by.y="nunidyr",all.x=TRUE)




rm(plot)


cond<-read.csv("../../data/COND.csv")

cond_selected<-cond[c("CN","INVYR","STATECD","UNITCD","COUNTYCD",
                      "PLOT","SLOPE","ASPECT","PHYSCLCD","CONDPROP_UNADJ")]


cond_selected$nunid<-paste0(cond_selected$STATECD,"-",cond_selected$UNITCD,"-",
                            cond_selected$COUNTYCD,"-",cond_selected$PLOT)
cond_selected$nunidyr<-paste0(cond_selected$STATECD,"-",cond_selected$UNITCD,"-",
                              cond_selected$COUNTYCD,"-",cond_selected$PLOT,"-",
                              cond_selected$INVYR)

cond_selected2<-cond_selected[!duplicated(cond_selected$nunidyr), ]

plots_all2<-merge(plots_all1,cond_selected2,by.x="NUNID.1",by.y="nunidyr",all.x=TRUE)


plots_all3<-plots_all2[which(plots_all2$fia_sev1==1),]


sdam3<-plots_all3

plot(plots_all3$STDAGE.2)

sdam3<-separate(sdam3, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)



ecosel<-read.csv("../disturbance/eco_select.csv")

sdam3$ecocode <- trimws(sdam3$Spl_1, which = c("left"))

source("../data/FTGC.R")
sdam3$FOR_GRP<-FTGC(sdam3$FORTYPCD.1)

library(operators)
sdam3b<-sdam3[(sdam3$ecocode %in% ecosel$econew),]
sdam3a<-sdam3[(sdam3$ecocode %!in% ecosel$econew),]


plot(sdam2$AGB.22)

sdam42<-sdam3

sdam42$aspect_shift<-sdam42$ASPECT-45

sdam42$aspect_trans_a<-cos((sdam42$aspect_shift)*(pi/180))
sdam42$aspect_trans<-sdam42$SLOPE*sdam42$aspect_trans_a


# sdam43<-sdam42


sdam42$phy_fac<-as.factor(sdam42$PHYSCLCD)


plot(sdam42$STDAGE.1)

ggplot(data=sdam42, aes(x=STDAGE.1)) + 
  geom_histogram(bins=200)+
  ylab("frequency")+
  xlab("Stand age first measurement")

ggplot(data=sdam42, aes(x=STDAGE.2)) + 
  geom_histogram()+
  ylab("frequency")+
  xlab("Stand age second measurement")


sdam42$ECOREG<-as.factor(sdam42$Spl_1)
sdam42$FORGRP<-as.factor(sdam42$FOR_GRP)
sdam42$STDORG<-as.factor(sdam42$STDORGCD.1)


sdam43<-sdam42[which(sdam42$dist_type=="CUT"|sdam42$dist_type=="FIRE"|
                       sdam42$dist_type=="INSDIS"),]
sdam5a<-sdam43[(sdam43$ecocode %in% ecosel$econew),]
sdam5b<-sdam43[!(sdam43$ecocode %in% ecosel$econew),]

# remove.packages("tidyverse")
# install.package("tidyverse")
library(tidyverse)
count(sdam5b,dist_type)



ddd<-"2022-09-25"
model_cut_agb_kf<-readRDS(paste0("model_east_cut_agb_kf_",ddd,".RDS"))
model_fire_agb_kf<-readRDS(paste0("model_east_fire_agb_kf_",ddd,".RDS"))
model_all_agb_kf<-readRDS(paste0("model_east_all_agb_kf_",ddd,".RDS"))
model_cut_seed_kf<-readRDS(paste0("model_east_cut_seed_kf_",ddd,".RDS"))
model_fire_seed_kf<-readRDS(paste0("model_east_fire_seed_kf_",ddd,".RDS"))
model_all_seed_kf<-readRDS(paste0("model_east_all_seed_kf_",ddd,".RDS"))

print(model_cut_agb_kf)
varImp(model_fire_seed_kf)
model_fire_seed_kf$finalModel



data_all_agb1<-sdam5b[c("AGB.22","AGB.11","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                        "ECOREG","dist_type","ELEV","aspect_trans",
                        "SLOPE","phy_fac")]
colnames(data_all_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","Disturbance_types","elevation","aspect","slope",
                           "physiography")

data_all_agb<-na.omit(data_all_agb1)

library(randomForest)
### severe
set.seed(51)

train_all_agb<-sample(1:nrow(data_all_agb),0.80*(nrow(data_all_agb)))
valid_all_agb<-data_all_agb[-train_all_agb,]
learn_all_agb<-data_all_agb[train_all_agb,]



data_all_seed1<-sdam5b[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                         "ECOREG","dist_type","ELEV","aspect_trans",
                         "SLOPE","phy_fac")]
colnames(data_all_seed1)<-c("Seed_count.2","Pre_dist_seed_count","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","Disturbance_types","elevation","aspect","slope",
                            "physiography")

data_all_seed<-na.omit(data_all_seed1)

library(randomForest)
### severe
set.seed(51)

train_all_seed<-sample(1:nrow(data_all_seed),0.80*(nrow(data_all_seed)))
valid_all_seed<-data_all_seed[-train_all_seed,]
learn_all_seed<-data_all_seed[train_all_seed,]





sdam6<-sdam5b[which(sdam5b$dist_type=="FIRE"),]



data_fire_agb1<-sdam6[c("AGB.22","AGB.11","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                        "ECOREG","ELEV","aspect_trans",
                        "SLOPE","phy_fac")]
colnames(data_fire_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","elevation","aspect","slope",
                            "physiography")

data_fire_agb<-na.omit(data_fire_agb1)

library(randomForest)
### severe
set.seed(51)

train_fire_agb<-sample(1:nrow(data_fire_agb),0.80*(nrow(data_fire_agb)))
valid_fire_agb<-data_fire_agb[-train_fire_agb,]
learn_fire_agb<-data_fire_agb[train_fire_agb,]




data_fire_seed1<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                         "ECOREG","ELEV","aspect_trans",
                         "SLOPE","phy_fac")]
colnames(data_fire_seed1)<-c("Seed_count.2","Pre_dist_seed_count","Stand_origin","Forest_Group",
                             "Pre_dist_stand_age","Post_dist_stand_age",
                             "Ecoregion","elevation","aspect","slope",
                             "physiography")

data_fire_seed<-na.omit(data_fire_seed1)

library(randomForest)
### severe
set.seed(51)

train_fire_seed<-sample(1:nrow(data_fire_seed),0.80*(nrow(data_fire_seed)))
valid_fire_seed<-data_fire_seed[-train_fire_seed,]
learn_fire_seed<-data_fire_seed[train_fire_seed,]




sdam6<-sdam5b[which(sdam5b$dist_type=="CUT"),]




data_cut_agb1<-sdam6[c("AGB.22","AGB.11","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                       "ECOREG","ELEV","aspect_trans",
                       "SLOPE","phy_fac")]
colnames(data_cut_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","elevation","aspect","slope",
                           "physiography")

data_cut_agb<-na.omit(data_cut_agb1)

library(randomForest)
### severe
set.seed(51)

train_cut_agb<-sample(1:nrow(data_cut_agb),0.80*(nrow(data_cut_agb)))
valid_cut_agb<-data_cut_agb[-train_cut_agb,]
learn_cut_agb<-data_cut_agb[train_cut_agb,]





data_cut_seed1<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                        "ECOREG","ELEV","aspect_trans",
                        "SLOPE","phy_fac")]
colnames(data_cut_seed1)<-c("Seed_count.2","Pre_dist_seed_count","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","elevation","aspect","slope",
                            "physiography")

data_cut_seed<-na.omit(data_cut_seed1)

library(randomForest)
### severe
set.seed(51)

train_cut_seed<-sample(1:nrow(data_cut_seed),0.80*(nrow(data_cut_seed)))
valid_cut_seed<-data_cut_seed[-train_cut_seed,]
learn_cut_seed<-data_cut_seed[train_cut_seed,]



library(caret)
library(ggplot2)
pred_all_agb_kf<-predict(model_all_agb_kf$finalModel, valid_all_agb)
r2_all_agb_kf<-R2(pred_all_agb_kf,valid_all_agb$AGB.2)
rmse_all_agb_kf<-RMSE(pred_all_agb_kf,valid_all_agb$AGB.2)


data_agb_all<-as.data.frame(cbind(valid_all_agb$AGB.2,pred_all_agb_kf))
colnames(data_agb_all)<-c("observed","predicted")

rrr1<-paste0("rsq=", round(r2_all_agb_kf,digits=3))
x1<-max(valid_all_agb$AGB.2)-5
y1<-max(pred_all_agb_kf)-2

p1<-ggplot(data=data_agb_all,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("East")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x1, y=y1, label=rrr1)+
   theme_bw()



pred_all_seed_kf<-predict(model_all_seed_kf$finalModel, valid_all_seed)
r2_all_seed_kf<-R2(pred_all_seed_kf,valid_all_seed$Seed_count.2)
rmse_all_seed_kf<-RMSE(pred_all_seed_kf,valid_all_seed$Seed_count.2)

data_seed_all<-as.data.frame(cbind(valid_all_seed$Seed_count.2,pred_all_seed_kf))
colnames(data_seed_all)<-c("observed","predicted")

rrr2<-paste0("rsq=", round(r2_all_seed_kf,digits=3))
x2<-max(valid_all_seed$Seed_count.2)-500
y2<-max(pred_all_seed_kf)-200

p2<-ggplot(data=data_seed_all,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=75000, y=y2, label=rrr2)+
  scale_x_continuous(limits=c(0,80000))+
   theme_bw()




pred_fire_agb_kf<-predict(model_fire_agb_kf$finalModel, valid_fire_agb)
r2_fire_agb_kf<-R2(pred_fire_agb_kf,valid_fire_agb$AGB.2)
rmse_fire_agb_kf<-RMSE(pred_fire_agb_kf,valid_fire_agb$AGB.2)

data_agb_fire<-as.data.frame(cbind(valid_fire_agb$AGB.2,pred_fire_agb_kf))
colnames(data_agb_fire)<-c("observed","predicted")

rrr3<-paste0("rsq=", round(r2_fire_agb_kf,digits=3))
x3<-max(valid_fire_agb$AGB.2)-1
y3<-max(pred_fire_agb_kf)-1

p3<-ggplot(data=data_agb_fire,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("East")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x3, y=y3, label=rrr3)+
   theme_bw()



pred_fire_seed_kf<-predict(model_fire_seed_kf$finalModel, valid_fire_seed)
r2_fire_seed_kf<-R2(pred_fire_seed_kf,valid_fire_seed$Seed_count.2)
rmse_fire_seed_kf<-RMSE(pred_fire_seed_kf,valid_fire_seed$Seed_count.2)

data_seed_fire<-as.data.frame(cbind(valid_fire_seed$Seed_count.2,pred_fire_seed_kf))
colnames(data_seed_fire)<-c("observed","predicted")

rrr4<-paste0("rsq=", round(r2_fire_seed_kf,digits=3))
x4<-max(valid_fire_seed$Seed_count.2)-5000
y4<-max(pred_fire_seed_kf)-100

p4<-ggplot(data=data_seed_fire,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=x4, y=y4, label=rrr4)+
   theme_bw()






pred_cut_agb_kf<-predict(model_cut_agb_kf$finalModel, valid_cut_agb)
r2_cut_agb_kf<-R2(pred_cut_agb_kf,valid_cut_agb$AGB.2)
rmse_cut_agb_kf<-RMSE(pred_cut_agb_kf,valid_cut_agb$AGB.2)

data_agb_cut<-as.data.frame(cbind(valid_cut_agb$AGB.2,pred_cut_agb_kf))
colnames(data_agb_cut)<-c("observed","predicted")

rrr5<-paste0("rsq=", round(r2_cut_agb_kf,digits=3))
x5<-max(valid_cut_agb$AGB.2)-5
y5<-max(pred_cut_agb_kf)-2

p5<-ggplot(data=data_agb_cut,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("East")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x5, y=y5, label=rrr5)+
   theme_bw()



pred_cut_seed_kf<-predict(model_cut_seed_kf$finalModel, valid_cut_seed)
r2_cut_seed_kf<-R2(pred_cut_seed_kf,valid_cut_seed$Seed_count.2)
rmse_cut_seed_kf<-RMSE(pred_cut_seed_kf,valid_cut_seed$Seed_count.2)

data_seed_cut<-as.data.frame(cbind(valid_cut_seed$Seed_count.2,pred_cut_seed_kf))
colnames(data_seed_cut)<-c("observed","predicted")

rrr6<-paste0("rsq=", round(r2_cut_seed_kf,digits=3))
x6<-max(valid_cut_seed$Seed_count.2)-500
y6<-max(pred_cut_seed_kf)-200

p6<-ggplot(data=data_seed_cut,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=x6, y=y6, label=rrr6)+
   theme_bw()



# 
# 
# pred_ins_agb_kf<-predict(model_ins_agb_kf$finalModel, valid_insdis_agb)
# r2_ins_agb_kf<-R2(pred_ins_agb_kf,valid_insdis_agb$AGB.2)
# data_agb_ins<-as.data.frame(cbind(valid_insdis_agb$AGB.2,pred_ins_agb_kf))
# colnames(data_agb_ins)<-c("observed","predicted")
# 
# rrr7<-paste0("rsq=", round(r2_ins_agb_kf,digits=3))
# x7<-max(valid_insdis_agb$AGB.2)-5
# y7<-max(pred_ins_agb_kf)-2
# 
# p7<-ggplot(data=data_agb_ins,aes(x=observed,y=predicted))+
#    geom_point()+
#    ggtitle("Western foresters")+
#    xlab("observed AGB (Mg/ha)")+
#    ylab("predicted AGB (Mg/ha)")+
#    geom_text(x=x7, y=y7, label=rrr7)+
#    theme_bw()
# 
# 
# 
# pred_ins_seed_kf<-predict(model_ins_seed_kf$finalModel, valid_insdis_seed)
# r2_ins_seed_kf<-R2(pred_ins_seed_kf,valid_insdis_seed$seed_count.2)
# data_seed_ins<-as.data.frame(cbind(valid_insdis_seed$seed_count.2,pred_ins_seed_kf))
# colnames(data_seed_ins)<-c("observed","predicted")
# 
# rrr8<-paste0("rsq=", round(r2_ins_seed_kf,digits=3))
# x8<-max(valid_insdis_seed$seed_count.2)-500
# y8<-max(pred_ins_seed_kf)-200
# 
# p8<-ggplot(data=data_seed_ins,aes(x=observed,y=predicted))+
#    geom_point()+
#    ggtitle("Western foresters")+
#    xlab("observed seed density per ha")+
#    ylab("predicted seed density per ha")+
#    geom_text(x=x8, y=y8, label=rrr8)+
#    theme_bw()

rsquare1<-c(r2_all_agb_kf,r2_fire_agb_kf,r2_cut_agb_kf,
            r2_all_seed_kf,r2_fire_seed_kf,r2_cut_seed_kf)
rmse1<-c(rmse_all_agb_kf,rmse_fire_agb_kf,rmse_cut_agb_kf,
         rmse_all_seed_kf,rmse_fire_seed_kf,rmse_cut_seed_kf)


rm(list = setdiff(ls(), c('p1','p2','p3','p4','p5','p6',
                          'rsquare1','rmse1','sdam5a','sdam5b')))






ddd<-"2022-09-25"
model_ins_agb_kf<-readRDS(paste0("model_west_ins_agb_kf_",ddd,".RDS"))
model_cut_agb_kf<-readRDS(paste0("model_west_cut_agb_kf_",ddd,".RDS"))
model_fire_agb_kf<-readRDS(paste0("model_west_fire_agb_kf_",ddd,".RDS"))
model_all_agb_kf<-readRDS(paste0("model_west_all_agb_kf_",ddd,".RDS"))
model_ins_seed_kf<-readRDS(paste0("model_west_ins_seed_kf_",ddd,".RDS"))
model_cut_seed_kf<-readRDS(paste0("model_west_cut_seed_kf_",ddd,".RDS"))
model_fire_seed_kf<-readRDS(paste0("model_west_fire_seed_kf_",ddd,".RDS"))
model_all_seed_kf<-readRDS(paste0("model_west_all_seed_kf_",ddd,".RDS"))







data_all_agb1_west<-sdam5a[c("AGB.22","AGB.11","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                             "ECOREG","dist_type","ELEV","aspect_trans",
                             "SLOPE","phy_fac")]
colnames(data_all_agb1_west)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                                "Pre_dist_stand_age","Post_dist_stand_age",
                                "Ecoregion","Disturbance_types","elevation","aspect","slope",
                                "physiography")

data_all_agb_west<-na.omit(data_all_agb1_west)

library(randomForest)
### severe
set.seed(51)

train_all_agb_west<-sample(1:nrow(data_all_agb_west),0.80*(nrow(data_all_agb_west)))
valid_all_agb_west<-data_all_agb_west[-train_all_agb_west,]
learn_all_agb_west<-data_all_agb_west[train_all_agb_west,]




data_all_seed1_west<-sdam5a[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                              "ECOREG","dist_type","ELEV","aspect_trans",
                              "SLOPE","phy_fac")]
colnames(data_all_seed1_west)<-c("Seed_count.2","Pre_dist_seed_count","Stand_origin","Forest_Group",
                                 "Pre_dist_stand_age","Post_dist_stand_age",
                                 "Ecoregion","Disturbance_types","elevation","aspect","slope",
                                 "physiography")

data_all_seed_west<-na.omit(data_all_seed1_west)

library(randomForest)
### severe
set.seed(51)

train_all_seed_west<-sample(1:nrow(data_all_seed_west),0.80*(nrow(data_all_seed_west)))
valid_all_seed_west<-data_all_seed_west[-train_all_seed_west,]
learn_all_seed_west<-data_all_seed_west[train_all_seed_west,]





sdam6<-sdam5a[which(sdam5a$dist_type=="FIRE"),]



data_fire_agb1_west<-sdam6[c("AGB.22","AGB.11","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                             "ECOREG","ELEV","aspect_trans",
                             "SLOPE","phy_fac")]
colnames(data_fire_agb1_west)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                                 "Pre_dist_stand_age","Post_dist_stand_age",
                                 "Ecoregion","elevation","aspect","slope",
                                 "physiography")

data_fire_agb_west<-na.omit(data_fire_agb1_west)

library(randomForest)
### severe
set.seed(51)

train_fire_agb_west<-sample(1:nrow(data_fire_agb_west),0.80*(nrow(data_fire_agb_west)))
valid_fire_agb_west<-data_fire_agb_west[-train_fire_agb_west,]
learn_fire_agb_west<-data_fire_agb_west[train_fire_agb_west,]




data_fire_seed1_west<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                              "ECOREG","ELEV","aspect_trans",
                              "SLOPE","phy_fac")]
colnames(data_fire_seed1_west)<-c("Seed_count.2","Pre_dist_seed_count","Stand_origin","Forest_Group",
                                  "Pre_dist_stand_age","Post_dist_stand_age",
                                  "Ecoregion","elevation","aspect","slope",
                                  "physiography")

data_fire_seed_west<-na.omit(data_fire_seed1_west)

library(randomForest)
### severe
set.seed(51)

train_fire_seed_west<-sample(1:nrow(data_fire_seed_west),0.80*(nrow(data_fire_seed_west)))
valid_fire_seed_west<-data_fire_seed_west[-train_fire_seed_west,]
learn_fire_seed_west<-data_fire_seed_west[train_fire_seed_west,]





sdam6<-sdam5a[which(sdam5a$dist_type=="CUT"),]




data_cut_agb1_west<-sdam6[c("AGB.22","AGB.11","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                            "ECOREG","ELEV","aspect_trans",
                            "SLOPE","phy_fac")]
colnames(data_cut_agb1_west)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                                "Pre_dist_stand_age","Post_dist_stand_age",
                                "Ecoregion","elevation","aspect","slope",
                                "physiography")

data_cut_agb_west<-na.omit(data_cut_agb1_west)

library(randomForest)
### severe
set.seed(51)

train_cut_agb_west<-sample(1:nrow(data_cut_agb_west),0.80*(nrow(data_cut_agb_west)))
valid_cut_agb_west<-data_cut_agb_west[-train_cut_agb_west,]
learn_cut_agb_west<-data_cut_agb_west[train_cut_agb_west,]




data_cut_seed1_west<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                             "ECOREG","ELEV","aspect_trans",
                             "SLOPE","phy_fac")]
colnames(data_cut_seed1_west)<-c("Seed_count.2","Pre_dist_seed_count","Stand_origin","Forest_Group",
                                 "Pre_dist_stand_age","Post_dist_stand_age",
                                 "Ecoregion","elevation","aspect","slope",
                                 "physiography")

data_cut_seed_west<-na.omit(data_cut_seed1_west)

library(randomForest)
### severe
set.seed(51)

train_cut_seed_west<-sample(1:nrow(data_cut_seed_west),0.80*(nrow(data_cut_seed_west)))
valid_cut_seed_west<-data_cut_seed_west[-train_cut_seed_west,]
learn_cut_seed_west<-data_cut_seed_west[train_cut_seed_west,]





sdam6<-sdam5a[which(sdam5a$dist_type=="INSDIS"),]




data_ins_agb1_west<-sdam6[c("AGB.22","AGB.11","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                            "ECOREG","ELEV","aspect_trans",
                            "SLOPE","phy_fac")]
colnames(data_ins_agb1_west)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                                "Pre_dist_stand_age","Post_dist_stand_age",
                                "Ecoregion","elevation","aspect","slope",
                                "physiography")

data_ins_agb_west<-na.omit(data_ins_agb1_west)

library(randomForest)
### severe
set.seed(51)

train_ins_agb_west<-sample(1:nrow(data_ins_agb_west),0.80*(nrow(data_ins_agb_west)))
valid_ins_agb_west<-data_ins_agb_west[-train_ins_agb_west,]
learn_ins_agb_west<-data_ins_agb_west[train_ins_agb_west,]




data_ins_seed1_west<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                             "ECOREG","ELEV","aspect_trans",
                             "SLOPE","phy_fac")]
colnames(data_ins_seed1_west)<-c("Seed_count.2","Pre_dist_seed_count","Stand_origin","Forest_Group",
                                 "Pre_dist_stand_age","Post_dist_stand_age",
                                 "Ecoregion","elevation","aspect","slope",
                                 "physiography")

data_ins_seed_west<-na.omit(data_ins_seed1_west)

library(randomForest)
### severe
set.seed(51)

train_ins_seed_west<-sample(1:nrow(data_ins_seed_west),0.80*(nrow(data_ins_seed_west)))
valid_ins_seed_west<-data_ins_seed_west[-train_ins_seed_west,]
learn_ins_seed_west<-data_ins_seed_west[train_ins_seed_west,]




library(caret)
library(ggplot2)
pred_all_agb_kf<-predict(model_all_agb_kf$finalModel, valid_all_agb_west)
r2_all_agb_kf<-R2(pred_all_agb_kf,valid_all_agb_west$AGB.2)
rmse_all_agb_kf<-RMSE(pred_all_agb_kf,valid_all_agb_west$AGB.2)

data_agb_all<-as.data.frame(cbind(valid_all_agb_west$AGB.2,pred_all_agb_kf))
colnames(data_agb_all)<-c("observed","predicted")

rrr1<-paste0("rsq=", round(r2_all_agb_kf,digits=3))
x1<-max(valid_all_agb_west$AGB.2)-5
y1<-max(pred_all_agb_kf)-2

p9<-ggplot(data=data_agb_all,aes(x=observed,y=predicted))+
  geom_point()+
  ggtitle("West")+
  xlab("observed AGB (Mg/ha)")+
  ylab("predicted AGB (Mg/ha)")+
  geom_text(x=x1, y=y1, label=rrr1)+
  theme_bw()



pred_all_seed_kf<-predict(model_all_seed_kf$finalModel, valid_all_seed_west)
r2_all_seed_kf<-R2(pred_all_seed_kf,valid_all_seed_west$Seed_count.2)
rmse_all_seed_kf<-RMSE(pred_all_seed_kf,valid_all_seed_west$Seed_count.2)

data_seed_all<-as.data.frame(cbind(valid_all_seed_west$Seed_count.2,pred_all_seed_kf))
colnames(data_seed_all)<-c("observed","predicted")

rrr2<-paste0("rsq=", round(r2_all_seed_kf,digits=3))
x2<-max(valid_all_seed_west$Seed_count.2)-500
y2<-max(pred_all_seed_kf)-200

p10<-ggplot(data=data_seed_all,aes(x=observed,y=predicted))+
  geom_point()+
  # ggtitle("Western foresters")+
  xlab("observed seed density per ha")+
  ylab("predicted seed density per ha")+
  geom_text(x=35000, y=y2, label=rrr2)+
  scale_x_continuous(limits=c(0,40000))+
  theme_bw()




pred_fire_agb_kf<-predict(model_fire_agb_kf$finalModel, valid_fire_agb_west)
r2_fire_agb_kf<-R2(pred_fire_agb_kf,valid_fire_agb_west$AGB.2)
rmse_fire_agb_kf<-RMSE(pred_fire_agb_kf,valid_fire_agb_west$AGB.2)

data_agb_fire<-as.data.frame(cbind(valid_fire_agb_west$AGB.2,pred_fire_agb_kf))
colnames(data_agb_fire)<-c("observed","predicted")

rrr3<-paste0("rsq=", round(r2_fire_agb_kf,digits=3))
x3<-max(valid_fire_agb_west$AGB.2)-2
y3<-max(pred_fire_agb_kf)-1

p11<-ggplot(data=data_agb_fire,aes(x=observed,y=predicted))+
  geom_point()+
  ggtitle("West")+
  xlab("observed AGB (Mg/ha)")+
  ylab("predicted AGB (Mg/ha)")+
  geom_text(x=x3, y=y3, label=rrr3)+
  theme_bw()



pred_fire_seed_kf<-predict(model_fire_seed_kf$finalModel, valid_fire_seed_west)
r2_fire_seed_kf<-R2(pred_fire_seed_kf,valid_fire_seed_west$Seed_count.2)
rmse_fire_seed_kf<-RMSE(pred_fire_seed_kf,valid_fire_seed_west$Seed_count.2)

data_seed_fire<-as.data.frame(cbind(valid_fire_seed_west$Seed_count.2,pred_fire_seed_kf))
colnames(data_seed_fire)<-c("observed","predicted")

rrr4<-paste0("rsq=", round(r2_fire_seed_kf,digits=3))
x4<-max(valid_fire_seed_west$Seed_count.2)-5000
y4<-max(pred_fire_seed_kf)-100

p12<-ggplot(data=data_seed_fire,aes(x=observed,y=predicted))+
  geom_point()+
  # ggtitle("Western foresters")+
  xlab("observed seed density per ha")+
  ylab("predicted seed density per ha")+
  geom_text(x=x4, y=y4, label=rrr4)+
  theme_bw()






pred_cut_agb_kf<-predict(model_cut_agb_kf$finalModel, valid_cut_agb_west)
r2_cut_agb_kf<-R2(pred_cut_agb_kf,valid_cut_agb_west$AGB.2)
rmse_cut_agb_kf<-RMSE(pred_cut_agb_kf,valid_cut_agb_west$AGB.2)

data_agb_cut<-as.data.frame(cbind(valid_cut_agb_west$AGB.2,pred_cut_agb_kf))
colnames(data_agb_cut)<-c("observed","predicted")

rrr5<-paste0("rsq=", round(r2_cut_agb_kf,digits=3))
x5<-max(valid_cut_agb_west$AGB.2)-5
y5<-max(pred_cut_agb_kf)-2

p13<-ggplot(data=data_agb_cut,aes(x=observed,y=predicted))+
  geom_point()+
  ggtitle("West")+
  xlab("observed AGB (Mg/ha)")+
  ylab("predicted AGB (Mg/ha)")+
  geom_text(x=x5, y=y5, label=rrr5)+
  theme_bw()



pred_cut_seed_kf<-predict(model_cut_seed_kf$finalModel, valid_cut_seed_west)
r2_cut_seed_kf<-R2(pred_cut_seed_kf,valid_cut_seed_west$Seed_count.2)
rmse_cut_seed_kf<-RMSE(pred_cut_seed_kf,valid_cut_seed_west$Seed_count.2)

data_seed_cut<-as.data.frame(cbind(valid_cut_seed_west$Seed_count.2,pred_cut_seed_kf))
colnames(data_seed_cut)<-c("observed","predicted")

rrr6<-paste0("rsq=", round(r2_cut_seed_kf,digits=3))
x6<-max(valid_cut_seed_west$Seed_count.2)-500
y6<-max(pred_cut_seed_kf)-200

p14<-ggplot(data=data_seed_cut,aes(x=observed,y=predicted))+
  geom_point()+
  # ggtitle("Western foresters")+
  xlab("observed seed density per ha")+
  ylab("predicted seed density per ha")+
  geom_text(x=x6, y=y6, label=rrr6)+
  theme_bw()




pred_ins_agb_kf<-predict(model_ins_agb_kf$finalModel, valid_ins_agb_west)
r2_ins_agb_kf<-R2(pred_ins_agb_kf,valid_ins_agb_west$AGB.2)
rmse_ins_agb_kf<-RMSE(pred_ins_agb_kf,valid_ins_agb_west$AGB.2)

data_agb_ins<-as.data.frame(cbind(valid_ins_agb_west$AGB.2,pred_ins_agb_kf))
colnames(data_agb_ins)<-c("observed","predicted")

rrr7<-paste0("rsq=", round(r2_ins_agb_kf,digits=3))
x7<-max(valid_ins_agb_west$AGB.2)-5
y7<-max(pred_ins_agb_kf)-2

p15<-ggplot(data=data_agb_ins,aes(x=observed,y=predicted))+
   geom_point()+
   ggtitle("West")+
   xlab("observed AGB (Mg/ha)")+
   ylab("predicted AGB (Mg/ha)")+
   geom_text(x=x7, y=y7, label=rrr7)+
   theme_bw()



pred_ins_seed_kf<-predict(model_ins_seed_kf$finalModel, valid_ins_seed_west)
r2_ins_seed_kf<-R2(pred_ins_seed_kf,valid_ins_seed_west$Seed_count.2)
rmse_ins_seed_kf<-RMSE(pred_ins_seed_kf,valid_ins_seed_west$Seed_count.2)

data_seed_ins<-as.data.frame(cbind(valid_ins_seed_west$Seed_count.2,pred_ins_seed_kf))
colnames(data_seed_ins)<-c("observed","predicted")

rrr8<-paste0("rsq=", round(r2_ins_seed_kf,digits=3))
x8<-max(valid_ins_seed_west$Seed_count.2)-500
y8<-max(pred_ins_seed_kf)-200

p16<-ggplot(data=data_seed_ins,aes(x=observed,y=predicted))+
   geom_point()+
   # ggtitle("Western foresters")+
   xlab("observed seed density per ha")+
   ylab("predicted seed density per ha")+
   geom_text(x=x8, y=y8, label=rrr8)+
   theme_bw()

rsquare1_west<-c(r2_all_agb_kf,r2_fire_agb_kf,r2_cut_agb_kf,r2_ins_agb_kf,
              r2_all_seed_kf,r2_fire_seed_kf,r2_cut_seed_kf,r2_ins_seed_kf)
rmse1_west<-c(rmse_all_agb_kf,rmse_fire_agb_kf,rmse_cut_agb_kf,rmse_ins_agb_kf,
         rmse_all_seed_kf,rmse_fire_seed_kf,rmse_cut_seed_kf,rmse_ins_seed_kf)



r_all<-cbind(rsquare1,rsquare1_west)
rmse_all<-cbind(rmse1,rmse1_west)
write.csv(r_all,"all_r_validations.csv")
write.csv(rmse_all,"all_rmse_validations.csv")



curr_date<-Sys.Date()
png(filename=paste0("all_validation_map_",curr_date,".png"), res=100, width = 1200, height = 800)
par(mfrow=c(2,3))

p25<-grid.arrange(p9,p1,p10,p2,nrow=2)
# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# p7<-grid.arrange(p1,p2,p3,p6,p2,p4,p6,nrow=2, "+", margin)

dev.off()


png(filename=paste0("fire_validation_map_",curr_date,".png"), res=100, width = 1200, height = 800)
par(mfrow=c(2,3))

p26<-grid.arrange(p11,p3,p12,p4,nrow=2)
# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# p7<-grid.arrange(p1,p2,p3,p6,p2,p4,p6,nrow=2, "+", margin)

dev.off()

png(filename=paste0("cut_validation_map_",curr_date,".png"), res=100, width = 1200, height = 800)
par(mfrow=c(2,3))

p27<-grid.arrange(p13,p5,p14,p6,nrow=2)
# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# p7<-grid.arrange(p1,p2,p3,p6,p2,p4,p6,nrow=2, "+", margin)

dev.off()


png(filename=paste0("ins_validation_map_",curr_date,".png"), res=100, width = 1200, height = 800)
par(mfrow=c(2,3))

p28<-grid.arrange(p15,p16,nrow=2)
# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# p7<-grid.arrange(p1,p2,p3,p6,p2,p4,p6,nrow=2, "+", margin)

dev.off()

