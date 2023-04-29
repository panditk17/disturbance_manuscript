### plots based on disturbances from FIA data

# setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000)

plots_all<-read.csv("plots_seedling_disturbances_FIA_include_mild.csv")

kkk<-count(plots_all,cut1,insdis1,fire1)


plot<-read.csv("../../data/PLOT.csv")
library(tidyverse)

plots_selected<-plot %>% select(CN,PREV_PLT_CN,INVYR,STATECD,UNITCD,COUNTYCD,
                            PLOT,ELEV)

plots_seed<-read.csv("plots_with_disturbance_seedlings_FIA.csv")

plots_selected$nunid<-paste0(plots_selected$STATECD,"-",plots_selected$UNITCD,"-",
                             plots_selected$COUNTYCD,"-",plots_selected$PLOT)
plots_selected$nunidyr<-paste0(plots_selected$STATECD,"-",plots_selected$UNITCD,"-",
                             plots_selected$COUNTYCD,"-",plots_selected$PLOT,"-",
                             plots_selected$INVYR)

plots_all1<-merge(plots_all,plots_selected,by.x="NUNID.1",by.y="nunidyr",all.x=TRUE)




rm(plot)


cond<-read.csv("../../data/COND.csv")
cond_filt<-cond[which(cond$CONDID==1),]

cond_selected<-cond_filt %>% select(CN,INVYR,STATECD,UNITCD,COUNTYCD,
                                PLOT,SLOPE,ASPECT,PHYSCLCD,CONDPROP_UNADJ)


cond_selected$nunid<-paste0(cond_selected$STATECD,"-",cond_selected$UNITCD,"-",
                             cond_selected$COUNTYCD,"-",cond_selected$PLOT)
cond_selected$nunidyr<-paste0(cond_selected$STATECD,"-",cond_selected$UNITCD,"-",
                               cond_selected$COUNTYCD,"-",cond_selected$PLOT,"-",
                               cond_selected$INVYR)

table<-count(cond_selected,nunidyr,CONDPROP_UNADJ)

cond_selected1<-cond_selected %>% group_by(nunidyr) %>% 
  filter(abs(CONDPROP_UNADJ) == max(CONDPROP_UNADJ))

cond_selected2<-cond_selected1[!duplicated(cond_selected1$nunidyr), ]


plots_all2<-merge(plots_all1,cond_selected2,by.x="NUNID.1",by.y="nunidyr",both=TRUE)

jjj<-count(plots_all2,NUNID.1)

kkk<-count(cond_selected1,nunidyr)

write.csv(plots_all2,"plots_disturbance_FIA_new_with_cond_plot.csv")

plots_all3<-plots_all2[which(plots_all2$dist_all2=="1.0.0"|
                               plots_all2$dist_all2=="0.1.0" |
                               plots_all2$dist_all2=="0.0.1"),]


plots_all4<-plots_all3[which(plots_all3$rep_std_fia2==1),]
sdam3<-plots_all4

plot(plots_all4$STDAGE.2)
 
 sdam3<-separate(sdam3, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)
 
 
 
 sdam2b<-separate(sdam3, NUNIDS.2, 
                  into = c("st","cty","unt","pl"), remove = FALSE)
 sdam2<-sdam2b[which(sdam2b$st!=2),]
 
 sdam2<-sdam2[which(sdam2$st!=15),]
 
 
 
 sdam2<-sdam2[which(sdam2$st<57 |sdam2$st>70 |sdam2$st==6),]
 
 
 ecosel<-read.csv("../disturbance/eco_select.csv")
 
 sdam2$ecocode <- trimws(sdam2$Spl_1, which = c("left"))
 
 source("../data/FTGC.R")
 sdam2$FOR_GRP<-FTGC(sdam2$FORTYPCD.1)
 
 library(operators)
 sdam3b<-sdam2[(sdam2$ecocode %in% ecosel$econew),]
 sdam3a<-sdam2[(sdam2$ecocode %!in% ecosel$econew),]
 


plot(sdam2$AGB.2)


sdam2$AGB.1<-(sdam2$AGB.1*453.6*2.471)/1000000
sdam2$AGB.2<-(sdam2$AGB.2*453.6*2.471)/1000000



plot(sdam2$AGB.2)

sdam42<-sdam2[which(sdam2$AGB.2<500),]

# sdam42<-sdam41[which(sdam41$seed_count.2<50000),]


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

sdam42$dist_type<-ifelse(sdam42$dist_all2=="1.0.0","harvest",
                         ifelse(sdam42$dist_all2=="0.1.0","fire",
                                ifelse(sdam42$dist_all2=="0.0.1","insdis","none")))


sdam5a<-sdam42[(sdam42$ecocode %in% ecosel$econew),]
sdam5b<-sdam42[(sdam42$ecocode %!in% ecosel$econew),]

count(sdam5b,dist_type)

data_all_agb1<-sdam5b[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                      "ECOREG","dist_type","distrt_his","ELEV","aspect_trans",
                      "SLOPE","phy_fac")]
colnames(data_all_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                          "Pre_dist_stand_age","Post_dist_stand_age",
                          "Ecoregion","Disturbance_types","Disturbance_history",
                          "elevation","aspect","slope",
                          "physiography")

data_all_agb<-na.omit(data_all_agb1)

library(randomForest)
### severe
set.seed(51)

train_all_agb<-sample(1:nrow(data_all_agb),0.80*(nrow(data_all_agb)))
valid_all_agb<-data_all_agb[-train_all_agb,]
learn_all_agb<-data_all_agb[train_all_agb,]



require(randomForest)

library(caret)


fitcontrol1<-trainControl(method="repeatedCV",number=5,
                          search="grid",repeats=3)

tunegrid <- expand.grid(.mtry = (1:12)) 


model_all_agb_kf<-train(y = learn_all_agb$AGB.2,
                        x = learn_all_agb[,colnames(learn_all_agb)!= "AGB.2"],
                        method="rf",
                        trControl=fitcontrol1,tuneGrid=tunegrid,ntree=100)

print(model_all_agb_kf)
varImp(model_all_agb_kf)
model_all_agb_kf$finalModel




## for only fire agb

sdam6<-sdam5b[which(sdam5b$dist_type=="fire"),]


data_fire_agb1<-sdam6[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                        "ECOREG","ELEV","distrt_his","aspect_trans","SLOPE","phy_fac")]


colnames(data_fire_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","elevation","disturbance_history","aspect","slope",
                            "physiography")

data_fire_agb<-na.omit(data_fire_agb1)
set.seed(51)

train_fire_agb<-sample(1:nrow(data_fire_agb),0.80*(nrow(data_fire_agb)))
valid_fire_agb<-data_fire_agb[-train_fire_agb,]
learn_fire_agb<-data_fire_agb[train_fire_agb,]


tunegrid1 <- expand.grid(.mtry = (1:11)) 
model_fire_agb_kf<-train(y = learn_fire_agb$AGB.2,
                        x = learn_fire_agb[,colnames(learn_fire_agb)!= "AGB.2"],
                        method="rf",
                        trControl=fitcontrol1,tuneGrid=tunegrid1,ntree=100)
print(model_fire_agb_kf)
varImp(model_fire_agb_kf)
model_fire_agb_kf$finalModel



sdam7<-sdam5b[which(sdam5b$dist_type=="harvest"),]


data_cut_agb1<-sdam7[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                        "ECOREG","ELEV","distrt_his","aspect_trans","SLOPE","phy_fac")]


colnames(data_cut_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","elevation","disturbance_history","aspect","slope",
                            "physiography")


data_cut_agb<-na.omit(data_cut_agb1)

set.seed(51)

train_cut_agb<-sample(1:nrow(data_cut_agb),0.80*(nrow(data_cut_agb)))
valid_cut_agb<-data_cut_agb[-train_cut_agb,]
learn_cut_agb<-data_cut_agb[train_cut_agb,]


tunegrid1 <- expand.grid(.mtry = (1:11)) 
model_cut_agb_kf<-train(y = learn_cut_agb$AGB.2,
                        x = learn_cut_agb[,colnames(learn_cut_agb)!= "AGB.2"],
                        method="rf",
                        trControl=fitcontrol1,tuneGrid=tunegrid1,ntree=100)

print(model_cut_agb_kf)
varImp(model_cut_agb_kf)
model_cut_agb_kf$finalModel



curr_date<-Sys.Date()
saveRDS(model_cut_agb_kf, file=paste0("model_east_cut_agb_kf_",curr_date,".RDS"))
saveRDS(model_fire_agb_kf, file=paste0("model_east_fire_agb_kf_",curr_date,".RDS"))
saveRDS(model_all_agb_kf, file=paste0("model_east_all_agb_kf_",curr_date,".RDS"))

rm(list=ls())

################# seedling models
###### new set of data

plots_all_seed<-read.csv("plots_with_disturbance_seedlings_FIA.csv")

plot<-read.csv("../../data/PLOT.csv")

plots_selected<-plot %>% select(CN,PREV_PLT_CN,INVYR,STATECD,UNITCD,COUNTYCD,
                                PLOT,ELEV)

plots_selected$nunid<-paste0(plots_selected$STATECD,"-",plots_selected$UNITCD,"-",
                             plots_selected$COUNTYCD,"-",plots_selected$PLOT)
plots_selected$nunidyr<-paste0(plots_selected$STATECD,"-",plots_selected$UNITCD,"-",
                               plots_selected$COUNTYCD,"-",plots_selected$PLOT,"-",
                               plots_selected$INVYR)

plots_all_seed1<-merge(plots_all_seed,plots_selected,by.x="NUNID.1",by.y="nunidyr",all.x=TRUE)

rm(plot)


cond<-read.csv("../../data/COND.csv")
cond_filt<-cond[which(cond$CONDID==1),]

cond_selected<-cond %>% select(CN,INVYR,STATECD,UNITCD,COUNTYCD,
                               PLOT,SLOPE,ASPECT,PHYSCLCD,CONDPROP_UNADJ)


cond_selected$nunid<-paste0(cond_selected$STATECD,"-",cond_selected$UNITCD,"-",
                            cond_selected$COUNTYCD,"-",cond_selected$PLOT)
cond_selected$nunidyr<-paste0(cond_selected$STATECD,"-",cond_selected$UNITCD,"-",
                              cond_selected$COUNTYCD,"-",cond_selected$PLOT,"-",
                              cond_selected$INVYR)

table<-count(cond_selected,nunidyr,CONDPROP_UNADJ)

cond_selected1<-cond_selected %>% group_by(nunidyr) %>% 
  filter(abs(CONDPROP_UNADJ) == max(CONDPROP_UNADJ))

cond_selected2<-cond_selected1[!duplicated(cond_selected1$nunidyr), ]


plots_all_seed2<-merge(plots_all_seed1,cond_selected2,by.x="NUNID.1",by.y="nunidyr",both=TRUE)

jjj<-count(plots_all_seed2,NUNID.1)

kkk<-count(cond_selected1,nunidyr)

rm(cond)
write.csv(plots_all_seed2,"plots_disturbance_FIA_with_seedlings_new.csv")

plots_all_seed3<-plots_all_seed2[which(plots_all_seed2$dist_all2=="1.0.0"|
                               plots_all_seed2$dist_all2=="0.1.0" |
                               plots_all_seed2$dist_all2=="0.0.1"),]


plots_all_seed4<-plots_all_seed3[which(plots_all_seed3$rep_std_fia2==1),]
sdam3<-plots_all_seed4

plot(plots_all_seed4$STDAGE.2)

sdam3<-separate(sdam3, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)

sdam2b<-separate(sdam3, NUNIDS.2, 
                 into = c("st","cty","unt","pl"), remove = FALSE)
sdam2<-sdam2b[which(sdam2b$st!=2),]

sdam2<-sdam2[which(sdam2$st!=15),]

sdam2<-sdam2[which(sdam2$st<57 |sdam2$st>70 |sdam2$st==6),]


ecosel<-read.csv("../disturbance/eco_select.csv")

sdam2$ecocode <- trimws(sdam2$Spl_1, which = c("left"))

source("../data/FTGC.R")
sdam2$FOR_GRP<-FTGC(sdam2$FORTYPCD.1)


sdam2$AGB.1<-(sdam2$AGB.1*453.6*2.471)/1000000
sdam2$AGB.2<-(sdam2$AGB.2*453.6*2.471)/1000000

sdam42<-sdam2[which(sdam2$AGB.2<500),]

# sdam42<-sdam41[which(sdam41$seed_count.2<50000),]


sdam42$aspect_shift<-sdam42$ASPECT-45

sdam42$aspect_trans_a<-cos((sdam42$aspect_shift)*(pi/180))
sdam42$aspect_trans<-sdam42$SLOPE*sdam42$aspect_trans_a


# sdam43<-sdam42


sdam42$phy_fac<-as.factor(sdam42$PHYSCLCD)



sdam42$ECOREG<-as.factor(sdam42$Spl_1)
sdam42$FORGRP<-as.factor(sdam42$FOR_GRP)
sdam42$STDORG<-as.factor(sdam42$STDORGCD.1)

sdam42$dist_type<-ifelse(sdam42$dist_all2=="1.0.0","harvest",
                         ifelse(sdam42$dist_all2=="0.1.0","fire",
                                ifelse(sdam42$dist_all2=="0.0.1","insdis","none")))

library(operators)
sdam5a<-sdam42[(sdam42$ecocode %in% ecosel$econew),]
sdam5b<-sdam42[(sdam42$ecocode %!in% ecosel$econew),]


data_all_seed1<-sdam5b[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                        "ECOREG","dist_type","ELEV","aspect_trans","SLOPE","phy_fac")]
colnames(data_all_seed1)<-c("seed_count.2","Pre_dist_seedling_density","Stand_origin","Forest_Group",
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



require(randomForest)

library(caret)


fitcontrol1<-trainControl(method="repeatedCV",number=5,
                          search="grid",repeats=3)

tunegrid <- expand.grid(.mtry = (1:11)) 


model_all_seed_kf<-train(y = learn_all_seed$seed_count.2,
                        x = learn_all_seed[,colnames(learn_all_seed)!= "seed_count.2"],
                        method="rf",
                        trControl=fitcontrol1,tuneGrid=tunegrid,ntree=100)

print(model_all_seed_kf)
varImp(model_all_seed_kf)
model_all_seed_kf$finalModel




## for only fire agb

sdam6<-sdam5b[which(sdam5b$dist_type=="fire"),]


data_fire_seed1<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                        "ECOREG","ELEV","aspect_trans","SLOPE","phy_fac")]


colnames(data_fire_seed1)<-c("seed_count.2","Pre_dist_seedling_density","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","elevation","aspect","slope",
                            "physiography")

data_fire_seed<-na.omit(data_fire_seed1)
set.seed(51)

train_fire_seed<-sample(1:nrow(data_fire_seed),0.80*(nrow(data_fire_seed)))
valid_fire_seed<-data_fire_seed[-train_fire_seed,]
learn_fire_seed<-data_fire_seed[train_fire_seed,]


tunegrid1 <- expand.grid(.mtry = (1:10)) 
model_fire_seed_kf<-train(y = learn_fire_seed$seed_count.2,
                         x = learn_fire_seed[,colnames(learn_fire_seed)!= "seed_count.2"],
                         method="rf",
                         trControl=fitcontrol1,tuneGrid=tunegrid1,ntree=100)

print(model_fire_seed_kf)
varImp(model_fire_seed_kf)
model_fire_seed_kf$finalModel



sdam7<-sdam5b[which(sdam5b$dist_type=="harvest"),]


data_cut_seed1<-sdam7[c("seed_count.2","AGB.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
                       "ECOREG","ELEV","aspect_trans","SLOPE","phy_fac")]


colnames(data_cut_seed1)<-c("seed_count.2","Pre_dist_seedling_density","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","elevation","aspect","slope",
                           "physiography")


data_cut_seed<-na.omit(data_cut_seed1)

set.seed(51)

train_cut_seed<-sample(1:nrow(data_cut_seed),0.80*(nrow(data_cut_seed)))
valid_cut_seed<-data_cut_seed[-train_cut_seed,]
learn_cut_seed<-data_cut_seed[train_cut_seed,]


tunegrid1 <- expand.grid(.mtry = (1:10)) 
model_cut_seed_kf<-train(y = learn_cut_seed$seed_count.2,
                        x = learn_cut_seed[,colnames(learn_cut_seed)!= "seed_count.2"],
                        method="rf",
                        trControl=fitcontrol1,tuneGrid=tunegrid1,ntree=100)

print(model_cut_seed_kf)
varImp(model_cut_seed_kf)
model_cut_seed_kf$finalModel





curr_date<-Sys.Date()
saveRDS(model_cut_seed_kf, file=paste0("model_east_cut_seed_kf_",curr_date,".RDS"))
saveRDS(model_fire_seed_kf, file=paste0("model_east_fire_seed_kf_",curr_date,".RDS"))
saveRDS(model_all_seed_kf, file=paste0("model_east_all_seed_kf_",curr_date,".RDS"))

