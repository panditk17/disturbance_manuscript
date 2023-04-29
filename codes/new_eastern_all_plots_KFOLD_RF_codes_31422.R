### plots based on disturbances from FIA data

# setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000)

sdam2<-read.csv("../disturbance/new_plot_data.csv")


sdam1<-read.csv("../../data/data_with_disturbance_3_12.csv")
sdam11<-sdam1[c("NUNID_1","STDSZCD")]

sdam21<-merge(sdam2,sdam11,by="NUNID_1")

sdam22<-sdam21[which(sdam21$STDAGE_1 >= 10),]

sdam3<-sdam22[which(sdam22$STDSZCD<4),]

# sdam3<-sdam2[which(sdam2$STDAGE_1<250),]


plot(sdam3$STDAGE_1,sdam3$STDAGE_2)


sdam3$ECOREG<-as.factor(sdam3$Spl_1)
sdam3$FORGRP<-as.factor(sdam3$FOR_GRP)
sdam3$STDORG<-as.factor(sdam3$STDORGCD_1)
sdam3$DISCOD<-as.factor(sdam3$dist_shift)

sdam3$DISCOD<-droplevels(sdam3$DISCOD)
# sdam42<-sdam3
# sdam41<-sdam3[which(sdam3$AGB.2<50),]
# 
# sdam42<-sdam41[which(sdam41$seed_count.2<50000),]


table33<-count(sdam42,damtrt1,damtrt2,dist_shift_new)
colnames(table33)<-c("Previous inventory","Current Inventory","combination","n")
write.csv(table33,"all_data_shift.csv")

sdam42$dist_shift_short<-ifelse(sdam42$damtrt2=="F" & sdam42$damtrt1=="ND","ND.F",
                             ifelse(sdam42$damtrt2=="F" & sdam42$damtrt1=="I","I.F",
                              ifelse(sdam42$damtrt2=="F" & sdam42$damtrt1=="F","F.F",
                                     ifelse(sdam42$damtrt2=="F" & sdam42$damtrt1=="C","C.F",
                               ifelse(sdam42$damtrt2=="F" ,"O.F",
                                ifelse(sdam42$damtrt2=="C" & sdam42$damtrt1=="ND","ND.C",
                                       ifelse(sdam42$damtrt2=="C" & sdam42$damtrt1=="F","F.C",
                                              ifelse(sdam42$damtrt2=="C" & sdam42$damtrt1=="I","I.C",
                                                     ifelse(sdam42$damtrt2=="C" & sdam42$damtrt1=="C","C.C",
                                 ifelse(sdam42$damtrt2=="C","O.C",
                                  ifelse(sdam42$damtrt2=="I" & sdam42$damtrt1=="ND","ND.I",
                                         ifelse(sdam42$damtrt2=="I" & sdam42$damtrt1=="F","F.I",
                                                ifelse(sdam42$damtrt2=="I" & sdam42$damtrt1=="I","I.I",
                                                       ifelse(sdam42$damtrt2=="I" & sdam42$damtrt1=="C","C.I",
                                   ifelse(sdam42$damtrt2=="I","O.I","O.O"
                             )))))))))))))))

sdam42$dist_shift_sh<-as.factor(sdam42$dist_shift_short)
table34<-count(sdam42,damtrt1,damtrt2,dist_shift_sh)

table34<-count(sdam42,dist_shift_sh)

# write.csv(sdam4,"west11_data.csv")
# 
# 
#  sdam4<-sdam4[which(sdam4$dist_codes_prev=="ND"),]

 
sdam42$aspect_shift<-sdam42$ASPECT.x-45

sdam42$aspect_trans_a<-cos((sdam42$aspect_shift)*(pi/180))
sdam42$aspect_trans<-sdam42$SLOPE.x*sdam42$aspect_trans_a


# sdam43<-sdam42


sdam42$phy_fac<-as.factor(sdam42$phy_fac)



sdam42$dist_shift_sh<-as.factor(sdam42$dist_shift_short)


ecosel<-read.csv("../disturbance/eco_select.csv")

sdam42$ecocode <- trimws(sdam42$Spl_1, which = c("left"))

sdam42b<-separate(sdam42, NUNIDS_2, 
                  into = c("st","cty","unt","pl"), remove = FALSE)

sdam42<-sdam42b[which(sdam42b$st!=2),]

sdam42<-sdam42[which(sdam42$st!=15),]

sdam42<-sdam42[which(sdam42$st!=60),]

table_freq<-count(sdam42,STDAGE_1)

# sdam422<-sdam42[which((sdam42$STDAGE_1)>(sdam42$STDAGE_2)),]

sdam422<-sdam42[which((sdam42$STDAGE_1)>=10),]

ggplot(data=sdam422, aes(x=STDAGE_1)) + 
  geom_histogram(bins=200)+
  ylab("frequency")+
  xlab("Stand age first measurement")

ggplot(data=sdam422, aes(x=STDAGE_2)) + 
  geom_histogram()+
  ylab("frequency")+
  xlab("Stand age second measurement")

library(operators)
sdam43<-sdam42[(sdam42$ecocode %!in% ecosel$econew),]

# sdam44<-sdam43[which(sdam43$FOR_GRP<400),]
table34b<-count(sdam43,dist_shift_sh)

sdam4<-sdam43
plot(sdam4$AGB.1,sdam4$AGB.2)



summary(sdam4$FORGRP)

 
data_all_agb1<-sdam4[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                      "ECOREG","damtrt2","ELEV","aspect_trans","SLOPE.x","phy_fac")]
colnames(data_all_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                          "Pre_dist_stand_age","Post_dist_stand_age",
                          "Ecoregion","Disturbance_types","elevation","aspect","slope",
                          "physiography")
data_all_agb<-na.omit(data_all_agb1)


data_all_seed1<-sdam4[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","damtrt2","ELEV","aspect_trans","SLOPE.x","phy_fac")]

colnames(data_all_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                      "Pre_dist_stand_age","Post_dist_stand_age",
                      "Ecoregion","Disturbance_types","elevation","aspect","slope",
                      "physiography")


data_all_seed<-na.omit(data_all_seed1)


library(randomForest)
### severe
set.seed(51)

train_all_agb<-sample(1:nrow(data_all_agb),0.80*(nrow(data_all_agb)))
valid_all_agb<-data_all_agb[-train_all_agb,]
learn_all_agb<-data_all_agb[train_all_agb,]

train_all_seed<-sample(1:nrow(data_all_seed),0.80*(nrow(data_all_seed)))
valid_all_seed<-data_all_seed[-train_all_seed,]
learn_all_seed<-data_all_seed[train_all_seed,]





## for only fire agb

sdam5<-sdam4[which(sdam4$damtrt2=="F"),]


data_fire_agb1<-sdam5[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.y","phy_fac")]

tab54<-count(data_fire_agb1,dist_shift_sh)

colnames(data_fire_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","previous_disturbance","elevation","aspect","slope",
                            "physiography")

data_fire_agb<-na.omit(data_fire_agb1)
set.seed(51)

train_fire_agb<-sample(1:nrow(data_fire_agb),0.80*(nrow(data_fire_agb)))
valid_fire_agb<-data_fire_agb[-train_fire_agb,]
learn_fire_agb<-data_fire_agb[train_fire_agb,]



## only fire agb

data_fire_seed1<-sdam5[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                         "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]



colnames(data_fire_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                             "Pre_dist_stand_age","Post_dist_stand_age",
                             "Ecoregion","previous_disturbance","elevation","aspect","slope",
                             "physiography")

data_fire_seed<-na.omit(data_fire_seed1)


set.seed(51)

train_fire_seed<-sample(1:nrow(data_fire_seed),0.80*(nrow(data_fire_seed)))
valid_fire_seed<-data_fire_seed[-train_fire_seed,]
learn_fire_seed<-data_fire_seed[train_fire_seed,]


## fire data


## for only fire agb

sdam6<-sdam4[which(sdam4$damtrt2=="C"),]


data_cut_agb1<-sdam6[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]


table55<-count(data_cut_agb1,dist_shift_sh)
colnames(data_cut_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","previous_disturbance","elevation","aspect","slope",
                           "physiography")
data_cut_agb<-na.omit(data_cut_agb1)

set.seed(51)

train_cut_agb<-sample(1:nrow(data_cut_agb),0.8*(nrow(data_cut_agb)))
valid_cut_agb<-data_cut_agb[-train_cut_agb,]
learn_cut_agb<-data_cut_agb[train_cut_agb,]

## only fire agb

data_cut_seed1<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.y","phy_fac")]



colnames(data_cut_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","previous_disturbance","elevation","aspect","slope",
                            "physiography")
data_cut_seed<-na.omit(data_cut_seed1)
set.seed(51)

train_cut_seed<-sample(1:nrow(data_cut_seed),0.8*(nrow(data_cut_seed)))
valid_cut_seed<-data_cut_seed[-train_cut_seed,]
learn_cut_seed<-data_cut_seed[train_cut_seed,]

## for only fire agb

sdam7<-sdam4[which(sdam4$dist_codes=="I"),]


data_ins_agb1<-sdam7[c("AGB.2","AGB.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                       "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]

table56<-count(data_ins_agb1,dist_shift_sh)

colnames(data_ins_agb1)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
                           "Pre_dist_stand_age","Post_dist_stand_age",
                           "Ecoregion","previous_disturbance","elevation","aspect","slope",
                           "physiography")
data_ins_agb<-na.omit(data_ins_agb1)
set.seed(51)

train_ins_agb<-sample(1:nrow(data_ins_agb),0.8*(nrow(data_ins_agb)))
valid_ins_agb<-data_ins_agb[-train_ins_agb,]
learn_ins_agb<-data_ins_agb[train_ins_agb,]


## only fire agb

data_ins_seed1<-sdam7[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE_1","STDAGE_2",
                        "ECOREG","dist_shift_sh","ELEV","aspect_trans","SLOPE.x","phy_fac")]



colnames(data_ins_seed1)<-c("seed_count.2","Pre_dist_seedcount","Stand_origin","Forest_Group",
                            "Pre_dist_stand_age","Post_dist_stand_age",
                            "Ecoregion","previous_disturbance","elevation","aspect","slope",
                            "physiography")

data_ins_seed<-na.omit(data_ins_seed1)
set.seed(51)

train_ins_seed<-sample(1:nrow(data_ins_seed),0.8*(nrow(data_ins_seed)))
valid_ins_seed<-data_ins_seed[-train_ins_seed,]
learn_ins_seed<-data_ins_seed[train_ins_seed,]

require(randomForest)

library(caret)


fitcontrol1<-trainControl(method="repeatedCV",number=5,
                          search="grid",repeats=3)

tunegrid <- expand.grid(.mtry = (1:11)) 


model_all_agb_kf<-train(y = learn_all_agb$AGB.2,
      x = learn_all_agb[,colnames(learn_all_agb)!= "AGB.2"],
      method="rf",
      trControl=fitcontrol1,tuneGrid=tunegrid,ntree=1000)

print(model_all_agb_kf)
varImp(model_all_agb_kf)
model_all_agb_kf$finalModel




## seedling model
model_all_seed_kf<-train(y = learn_all_seed$seed_count.2,
                       x = learn_all_seed[,colnames(learn_all_seed)!= "seed_count.2"],
                       method="rf",
                       trControl=fitcontrol1,tuneGrid=tunegrid,ntree=1000)

### only fire model

model_fire_agb_kf<-train(y = learn_fire_agb$AGB.2,
                        x = learn_fire_agb[,colnames(learn_fire_agb)!= "AGB.2"],
                        method="rf",
                        trControl=fitcontrol1,tuneGrid=tunegrid,ntree=1000)


model_fire_seed_kf<-train(y = learn_fire_seed$seed_count.2,
                         x = learn_fire_seed[,colnames(learn_fire_seed)!= "seed_count.2"],
                         method="rf",
                         trControl=fitcontrol1,tuneGrid=tunegrid,ntree=1000)

model_cut_agb_kf<-train(y = learn_cut_agb$AGB.2,
                        x = learn_cut_agb[,colnames(learn_cut_agb)!= "AGB.2"],
                        method="rf",
                        trControl=fitcontrol1,tuneGrid=tunegrid,ntree=1000)

model_cut_seed_kf<-train(y = learn_cut_seed$seed_count.2,
                         x = learn_cut_seed[,colnames(learn_cut_seed)!= "seed_count.2"],
                         method="rf",
                         trControl=fitcontrol1,tuneGrid=tunegrid,ntree=1000)

model_ins_agb_kf<-train(y = learn_ins_agb$AGB.2,
                        x = learn_ins_agb[,colnames(learn_ins_agb)!= "AGB.2"],
                        method="rf",
                        trControl=fitcontrol1,tuneGrid=tunegrid,ntree=1000)

model_ins_seed_kf<-train(y = learn_ins_seed$seed_count.2,
                         x = learn_ins_seed[,colnames(learn_ins_seed)!= "seed_count.2"],
                         method="rf",
                         trControl=fitcontrol1,tuneGrid=tunegrid,ntree=1000)

saveRDS(model_ins_seed_kf, file="n_model_east_ins_seed_kf.RDS")
saveRDS(model_ins_agb_kf, file="n_model_east_ins_agb_kf.RDS")
saveRDS(model_cut_seed_kf, file="n_model_east_cut_seed_kf.RDS")
saveRDS(model_cut_agb_kf, file="n_model_east_cut_agb_kf.RDS")
saveRDS(model_fire_seed_kf, file="n_model_east_fire_seed_kf.RDS")
saveRDS(model_fire_agb_kf, file="n_model_east_fire_agb_kf.RDS")
saveRDS(model_all_seed_kf, file="n_model_east_all_seed_kf.RDS")
saveRDS(model_all_agb_kf, file="n_model_east_all_agb_kf.RDS")


