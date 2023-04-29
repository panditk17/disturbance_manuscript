### plots based on disturbances from FIA data

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)
memory.limit(size=100000000)

setwd('C:/Karuns_docs/disturbance_files')

plots_all<-read.csv("all_plots_data_with_seedling_8_27.csv")

plots_all4<-plots_all[which(plots_all$fia_sev1==1),]


# 
# plot<-read.csv("PLOT.csv")
# require(tidyverse)
# require(dplyr)
# library(dplyr)
# # 
# # library(rFIA)
# # 
# # plot_data<-getFIA(states=c('MA'), common=TRUE,tables=NULL, load=TRUE)
# remove.packages("tidyverse")
# library(plyr)
# plots_selected<-plot %>% select(CN,PREV_PLT_CN,INVYR,STATECD,UNITCD,COUNTYCD,
#                                 PLOT,ELEV)
# 
# plots_selected<-plot[c("CN","PREV_PLT_CN","INVYR","STATECD","UNITCD","COUNTYCD",
#                        "PLOT","ELEV")]
# 
# plots_selected$nunid<-paste0(plots_selected$STATECD,"-",plots_selected$UNITCD,"-",
#                              plots_selected$COUNTYCD,"-",plots_selected$PLOT)
# plots_selected$nunidyr<-paste0(plots_selected$STATECD,"-",plots_selected$UNITCD,"-",
#                                plots_selected$COUNTYCD,"-",plots_selected$PLOT,"-",
#                                plots_selected$INVYR)
# 
# plots_all1<-merge(plots_all,plots_selected,by.x="NUNID.1",by.y="nunidyr",all.x=TRUE)
# 
# 
# 
# 
# rm(plot)
# 
# 
# cond<-read.csv("COND.csv")
# 
# cond_selected<-cond[c("CN","INVYR","STATECD","UNITCD","COUNTYCD",
#                       "PLOT","SLOPE","ASPECT","PHYSCLCD","CONDPROP_UNADJ")]
# 
# 
# cond_selected$nunid<-paste0(cond_selected$STATECD,"-",cond_selected$UNITCD,"-",
#                             cond_selected$COUNTYCD,"-",cond_selected$PLOT)
# cond_selected$nunidyr<-paste0(cond_selected$STATECD,"-",cond_selected$UNITCD,"-",
#                               cond_selected$COUNTYCD,"-",cond_selected$PLOT,"-",
#                               cond_selected$INVYR)
# 
# cond_selected2<-cond_selected[!duplicated(cond_selected$nunidyr), ]
# 
# plots_all2<-merge(plots_all1,cond_selected2,by.x="NUNID.1",by.y="nunidyr",all.x=TRUE)

# write.csv(plots_all2,"data_for_ML.csv")
plots_all2<-read.csv("data_for_ML.csv")

plots_all3<-plots_all2[which(plots_all2$fia_sev1==1),]


sdam3<-plots_all3

plot(plots_all3$STDAGE.2)
plot(plots_all3$ELEV)

library(dplyr)
library(tidyverse)
sdam3<-separate(sdam3, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)



ecosel<-read.csv("eco_select.csv")

sdam3$ecocode <- trimws(sdam3$Spl_1, which = c("left"))

source("FTGC.R")
sdam3$FOR_GRP<-FTGC(sdam3$FORTYPCD.1)

library(operators)
sdam3b<-sdam3[(sdam3$ecocode %in% ecosel$econew),]
sdam3a<-sdam3[(sdam3$ecocode %!in% ecosel$econew),]


plot(sdam2$AGB.22)

sdam42<-sdam3

sdam42$aspect_shift<-as.numeric(sdam42$ASPECT)-45
sdam42$aspect_shift<-sdam42$ASPECT-45


sdam42$aspect_trans_a<-cos((sdam42$aspect_shift)*(pi/180))
sdam42$aspect_trans<-as.numeric(sdam42$SLOPE)*sdam42$aspect_trans_a


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




ddd<-"2022-09-25"
# model_ins_agb_kf<-readRDS(paste0("model_east_ins_agb_kf_",ddd,".RDS"))
model_cut_agb_kf<-readRDS(paste0("model_east_cut_agb_kf_",ddd,".RDS"))
model_fire_agb_kf<-readRDS(paste0("model_east_fire_agb_kf_",ddd,".RDS"))
model_all_agb_kf<-readRDS(paste0("model_east_all_agb_kf_",ddd,".RDS"))
# model_ins_seed_kf<-readRDS(paste0("model_east_ins_seed_kf_",ddd,".RDS"))
model_cut_seed_kf<-readRDS(paste0("model_east_cut_seed_kf_",ddd,".RDS"))
model_fire_seed_kf<-readRDS(paste0("model_east_fire_seed_kf_",ddd,".RDS"))
model_all_seed_kf<-readRDS(paste0("model_east_all_seed_kf_",ddd,".RDS"))


learn_fire_agb<-model_fire_agb_kf$trainingData
learn_cut_agb<-model_cut_agb_kf$trainingData
learn_fire_seed<-model_fire_seed_kf$trainingData
learn_cut_seed<-model_cut_seed_kf$trainingData

library(randomForest)
fire_predist_agb1<-as.data.frame(partialPlot(model_fire_agb_kf$finalModel, learn_fire_agb, Pre_dist_AGB, which.class, 
                                       plot = TRUE, add = FALSE,
                                       
                                       n.pt = min(length(unique(learn_fire_agb[, "Pre_dist_AGB"])), 200),
                                       rug = TRUE, xlab=deparse(substitute(Pre_dist_AGB)), 
                                       ylab="",main=paste("Partial Dependence on", 
                                                          deparse(substitute(Pre_dist_AGB)))))


cut_predist_agb1<-as.data.frame(partialPlot(model_cut_agb_kf$finalModel, learn_cut_agb, Pre_dist_AGB,which.class, 
                                      plot = TRUE, add = FALSE,
                                      
                                      n.pt = min(length(unique(learn_cut_agb[, "Pre_dist_AGB"])), 200),
                                      rug = TRUE, xlab=deparse(substitute(Pre_dist_AGB)), 
                                      ylab="",main=paste("Partial Dependence on", 
                                                         deparse(substitute(Pre_dist_AGB)))))


# ins_pd_agb1<-as.data.frame(partialPlot(model_ins_agb_kf$finalModel, learn_insdis_agb, Pre_dist_AGB, which.class, 
#                                       plot = TRUE, add = FALSE,
#                                       
#                                       n.pt = min(length(unique(learn_insdis_agb[, "Pre_dist_AGB"])), 200),
#                                       rug = TRUE, xlab=deparse(substitute(Pre_dist_AGB)), 
#                                       ylab="",main=paste("Partial Dependence on", 
#                                                          deparse(substitute(Pre_dist_AGB)))))



fire_postage_agb1<-as.data.frame(partialPlot(model_fire_agb_kf$finalModel, learn_fire_agb, Post_dist_stand_age, which.class, 
                                         plot = TRUE, add = FALSE,
                                         
                                         n.pt = min(length(unique(learn_fire_agb[, "Post_dist_stand_age"])), 200),
                                         rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                         ylab="",main=paste("Partial Dependence on", 
                                                            deparse(substitute(Post_dist_stand_age)))))



cut_postage_agb1<-as.data.frame(partialPlot(model_cut_agb_kf$finalModel, learn_cut_agb, Post_dist_stand_age, which.class, 
                                        plot = TRUE, add = FALSE,
                                        
                                        n.pt = min(length(unique(learn_cut_agb[, "Post_dist_stand_age"])), 200),
                                        rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                        ylab="",main=paste("Partial Dependence on", 
                                                           deparse(substitute(Post_dist_stand_age)))))


# ins_pd_stage1<-as.data.frame(partialPlot(model_ins_agb_kf$finalModel, learn_insdis_agb, Post_dist_stand_age, which.class, 
#                                         plot = TRUE, add = FALSE,
#                                         
#                                         n.pt = min(length(unique(learn_insdis_agb[, "Post_dist_stand_age"])), 200),
#                                         rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
#                                         ylab="",main=paste("Partial Dependence on", 
#                                                            deparse(substitute(Post_dist_stand_age)))))


fire_predist_seed_count1<-as.data.frame(partialPlot(model_fire_seed_kf$finalModel, learn_fire_seed, Pre_dist_seed_count, which.class, 
                                              plot = TRUE, add = FALSE,
                                              
                                              n.pt = min(length(unique(learn_fire_seed[, "Pre_dist_seed_count"])), 200),
                                              rug = TRUE, xlab=deparse(substitute(Pre_dist_seed_count)), 
                                              ylab="",main=paste("Partial Dependence on", 
                                                                 deparse(substitute(Pre_dist_seed_count)))))

cut_predist_seed_count1<-as.data.frame(partialPlot(model_cut_seed_kf$finalModel, learn_cut_seed, Pre_dist_seed_count, which.class, 
                                             plot = TRUE, add = FALSE,
                                             
                                             n.pt = min(length(unique(learn_cut_seed[, "Pre_dist_seed_count"])), 200),
                                             rug = TRUE, xlab=deparse(substitute(Pre_dist_seed_count)), 
                                             ylab="",main=paste("Partial Dependence on", 
                                                                deparse(substitute(Pre_dist_seed_count)))))

# ins_pd_seed_count1<-as.data.frame(partialPlot(model_ins_seed_kf$finalModel, learn_insdis_seed, Pre_dist_seedling_density, which.class, 
#                                              plot = TRUE, add = FALSE,
#                                              
#                                              n.pt = min(length(unique(learn_insdis_seed[, "Pre_dist_seedling_density"])), 200),
#                                              rug = TRUE, xlab=deparse(substitute(Pre_dist_seedling_density)), 
#                                              ylab="",main=paste("Partial Dependence on", 
#                                                                 deparse(substitute(Pre_dist_seedling_density)))))

fire_postage_seed_count1<-as.data.frame(partialPlot(model_fire_seed_kf$finalModel, learn_fire_seed, Post_dist_stand_age, which.class, 
                                            plot = TRUE, add = FALSE,
                                            
                                            n.pt = min(length(unique(learn_fire_seed[, "Post_dist_stand_age"])), 200),
                                            rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                            ylab="",main=paste("Partial Dependence on", 
                                                               deparse(substitute(Post_dist_stand_age)))))

cut_postage_seed_count1<-as.data.frame(partialPlot(model_cut_seed_kf$finalModel, learn_cut_seed, Post_dist_stand_age, which.class, 
                                           plot = TRUE, add = FALSE,
                                           
                                           n.pt = min(length(unique(learn_cut_seed[, "Post_dist_stand_age"])), 200),
                                           rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                           ylab="",main=paste("Partial Dependence on", 
                                                              deparse(substitute(Post_dist_stand_age)))))

# ins_pd_seed_age1<-as.data.frame(partialPlot(model_ins_seed_kf$finalModel, learn_insdis_seed, Post_dist_stand_age, which.class, 
#                                            plot = TRUE, add = FALSE,
#                                            
#                                            n.pt = min(length(unique(learn_insdis_seed[, "Post_dist_stand_age"])), 200),
#                                            rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
#                                            ylab="",main=paste("Partial Dependence on", 
#                                                               deparse(substitute(Post_dist_stand_age)))))
# 


rm(list = setdiff(ls(), c('fire_predist_agb1','cut_predist_agb1',
                          'fire_postage_agb1','cut_postage_agb1',
                          'fire_predist_seed_count1','cut_predist_seed_count1',
                          'fire_postage_seed_count1','cut_postage_seed_count1',
                          'sdam5a','sdam5b','ecosel')))






ddd<-"2022-09-25"
model_ins_agb_kf<-readRDS(paste0("model_west_ins_agb_kf_",ddd,".RDS"))
model_cut_agb_kf<-readRDS(paste0("model_west_cut_agb_kf_",ddd,".RDS"))
model_fire_agb_kf<-readRDS(paste0("model_west_fire_agb_kf_",ddd,".RDS"))
model_all_agb_kf<-readRDS(paste0("model_west_all_agb_kf_",ddd,".RDS"))
model_ins_seed_kf<-readRDS(paste0("model_west_ins_seed_kf_",ddd,".RDS"))
model_cut_seed_kf<-readRDS(paste0("model_west_cut_seed_kf_",ddd,".RDS"))
model_fire_seed_kf<-readRDS(paste0("model_west_fire_seed_kf_",ddd,".RDS"))
model_all_seed_kf<-readRDS(paste0("model_west_all_seed_kf_",ddd,".RDS"))


learn_fire_agb_west<-model_fire_agb_kf$trainingData
learn_cut_agb_west<-model_cut_agb_kf$trainingData
learn_fire_seed_west<-model_fire_seed_kf$trainingData
learn_cut_seed_west<-model_cut_seed_kf$trainingData
learn_ins_agb_west<-model_ins_agb_kf$trainingData
learn_ins_seed_west<-model_ins_seed_kf$trainingData


# data_all_agb1_west<-sdam5a[c("AGB.22","AGB.11","STDORG","FORGRP","STDAGE.1","STDAGE.2",
#                              "ECOREG","dist_type","ELEV","aspect_trans",
#                              "SLOPE","phy_fac")]
# colnames(data_all_agb1_west)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
#                                 "Pre_dist_stand_age","Post_dist_stand_age",
#                                 "Ecoregion","Disturbance_types","elevation","aspect","slope",
#                                 "physiography")
# 
# data_all_agb_west<-na.omit(data_all_agb1_west)
# 
# library(randomForest)
# ### severe
# set.seed(51)
# 
# train_all_agb_west<-sample(1:nrow(data_all_agb_west),0.80*(nrow(data_all_agb_west)))
# valid_all_agb_west<-data_all_agb_west[-train_all_agb_west,]
# learn_all_agb_west<-data_all_agb_west[train_all_agb_west,]
# 
# 
# 
# 
# data_all_seed1_west<-sdam5a[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
#                               "ECOREG","dist_type","ELEV","aspect_trans",
#                               "SLOPE","phy_fac")]
# colnames(data_all_seed1_west)<-c("Seed_count.2","Pre_dist_seed_count","Stand_origin","Forest_Group",
#                                  "Pre_dist_stand_age","Post_dist_stand_age",
#                                  "Ecoregion","Disturbance_types","elevation","aspect","slope",
#                                  "physiography")
# 
# data_all_seed_west<-na.omit(data_all_seed1_west)
# 
# library(randomForest)
# ### severe
# set.seed(51)
# 
# train_all_seed_west<-sample(1:nrow(data_all_seed_west),0.80*(nrow(data_all_seed_west)))
# valid_all_seed_west<-data_all_seed_west[-train_all_seed_west,]
# learn_all_seed_west<-data_all_seed_west[train_all_seed_west,]
# 
# 
# 
# 
# 
# sdam6<-sdam5a[which(sdam5a$dist_type=="FIRE"),]
# 
# 
# 
# data_fire_agb1_west<-sdam6[c("AGB.22","AGB.11","STDORG","FORGRP","STDAGE.1","STDAGE.2",
#                              "ECOREG","ELEV","aspect_trans",
#                              "SLOPE","phy_fac")]
# colnames(data_fire_agb1_west)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
#                                  "Pre_dist_stand_age","Post_dist_stand_age",
#                                  "Ecoregion","elevation","aspect","slope",
#                                  "physiography")
# 
# data_fire_agb_west<-na.omit(data_fire_agb1_west)
# 
# library(randomForest)
# ### severe
# set.seed(51)
# 
# train_fire_agb_west<-sample(1:nrow(data_fire_agb_west),0.80*(nrow(data_fire_agb_west)))
# valid_fire_agb_west<-data_fire_agb_west[-train_fire_agb_west,]
# learn_fire_agb_west<-data_fire_agb_west[train_fire_agb_west,]
# 
# 
# 
# 
# data_fire_seed1_west<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
#                               "ECOREG","ELEV","aspect_trans",
#                               "SLOPE","phy_fac")]
# colnames(data_fire_seed1_west)<-c("Seed_count.2","Pre_dist_seed_count","Stand_origin","Forest_Group",
#                                   "Pre_dist_stand_age","Post_dist_stand_age",
#                                   "Ecoregion","elevation","aspect","slope",
#                                   "physiography")
# 
# data_fire_seed_west<-na.omit(data_fire_seed1_west)
# 
# library(randomForest)
# ### severe
# set.seed(51)
# 
# train_fire_seed_west<-sample(1:nrow(data_fire_seed_west),0.80*(nrow(data_fire_seed_west)))
# valid_fire_seed_west<-data_fire_seed_west[-train_fire_seed_west,]
# learn_fire_seed_west<-data_fire_seed_west[train_fire_seed_west,]
# 
# 
# 
# 
# 
# sdam6<-sdam5a[which(sdam5a$dist_type=="CUT"),]
# 
# 
# 
# 
# data_cut_agb1_west<-sdam6[c("AGB.22","AGB.11","STDORG","FORGRP","STDAGE.1","STDAGE.2",
#                             "ECOREG","ELEV","aspect_trans",
#                             "SLOPE","phy_fac")]
# colnames(data_cut_agb1_west)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
#                                 "Pre_dist_stand_age","Post_dist_stand_age",
#                                 "Ecoregion","elevation","aspect","slope",
#                                 "physiography")
# 
# data_cut_agb_west<-na.omit(data_cut_agb1_west)
# 
# library(randomForest)
# ### severe
# set.seed(51)
# 
# train_cut_agb_west<-sample(1:nrow(data_cut_agb_west),0.80*(nrow(data_cut_agb_west)))
# valid_cut_agb_west<-data_cut_agb_west[-train_cut_agb_west,]
# learn_cut_agb_west<-data_cut_agb_west[train_cut_agb_west,]
# 
# 
# 
# 
# data_cut_seed1_west<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
#                              "ECOREG","ELEV","aspect_trans",
#                              "SLOPE","phy_fac")]
# colnames(data_cut_seed1_west)<-c("Seed_count.2","Pre_dist_seed_count","Stand_origin","Forest_Group",
#                                  "Pre_dist_stand_age","Post_dist_stand_age",
#                                  "Ecoregion","elevation","aspect","slope",
#                                  "physiography")
# 
# data_cut_seed_west<-na.omit(data_cut_seed1_west)
# 
# library(randomForest)
# ### severe
# set.seed(51)
# 
# train_cut_seed_west<-sample(1:nrow(data_cut_seed_west),0.80*(nrow(data_cut_seed_west)))
# valid_cut_seed_west<-data_cut_seed_west[-train_cut_seed_west,]
# learn_cut_seed_west<-data_cut_seed_west[train_cut_seed_west,]
# 
# 
# 
# 
# 
# sdam6<-sdam5a[which(sdam5a$dist_type=="INSDIS"),]
# 
# 
# 
# 
# data_ins_agb1_west<-sdam6[c("AGB.22","AGB.11","STDORG","FORGRP","STDAGE.1","STDAGE.2",
#                             "ECOREG","ELEV","aspect_trans",
#                             "SLOPE","phy_fac")]
# colnames(data_ins_agb1_west)<-c("AGB.2","Pre_dist_AGB","Stand_origin","Forest_Group",
#                                 "Pre_dist_stand_age","Post_dist_stand_age",
#                                 "Ecoregion","elevation","aspect","slope",
#                                 "physiography")
# 
# data_ins_agb_west<-na.omit(data_ins_agb1_west)
# 
# library(randomForest)
# ### severe
# set.seed(51)
# 
# train_ins_agb_west<-sample(1:nrow(data_ins_agb_west),0.80*(nrow(data_ins_agb_west)))
# valid_ins_agb_west<-data_ins_agb_west[-train_ins_agb_west,]
# learn_ins_agb_west<-data_ins_agb_west[train_ins_agb_west,]
# 
# 
# 
# 
# data_ins_seed1_west<-sdam6[c("seed_count.2","seed_count.1","STDORG","FORGRP","STDAGE.1","STDAGE.2",
#                              "ECOREG","ELEV","aspect_trans",
#                              "SLOPE","phy_fac")]
# colnames(data_ins_seed1_west)<-c("Seed_count.2","Pre_dist_seed_count","Stand_origin","Forest_Group",
#                                  "Pre_dist_stand_age","Post_dist_stand_age",
#                                  "Ecoregion","elevation","aspect","slope",
#                                  "physiography")
# 
# data_ins_seed_west<-na.omit(data_ins_seed1_west)
# 
# library(randomForest)
# ### severe
# set.seed(51)
# 
# train_ins_seed_west<-sample(1:nrow(data_ins_seed_west),0.80*(nrow(data_ins_seed_west)))
# valid_ins_seed_west<-data_ins_seed_west[-train_ins_seed_west,]
# learn_ins_seed_west<-data_ins_seed_west[train_ins_seed_west,]



fire_predist_agb3<-as.data.frame(partialPlot(model_fire_agb_kf$finalModel, learn_fire_agb_west, Pre_dist_AGB, which.class, 
                                        plot = TRUE, add = FALSE,
                                        
                                        n.pt = min(length(unique(learn_fire_agb_west[, "Pre_dist_AGB"])), 200),
                                        rug = TRUE, xlab=deparse(substitute(Pre_dist_AGB)), 
                                        ylab="",main=paste("Partial Dependence on", 
                                                           deparse(substitute(Pre_dist_AGB)))))



cut_predist_agb3<-as.data.frame(partialPlot(model_cut_agb_kf$finalModel, learn_cut_agb_west, Pre_dist_AGB, which.class, 
                                       plot = TRUE, add = FALSE,
                                       
                                       n.pt = min(length(unique(learn_cut_agb_west[, "Pre_dist_AGB"])), 200),
                                       rug = TRUE, xlab=deparse(substitute(Pre_dist_AGB)), 
                                       ylab="",main=paste("Partial Dependence on", 
                                                          deparse(substitute(Pre_dist_AGB)))))


ins_predist_agb3<-as.data.frame(partialPlot(model_ins_agb_kf$finalModel, learn_ins_agb_west, Pre_dist_AGB, which.class,
                                       plot = TRUE, add = FALSE,

                                       n.pt = min(length(unique(learn_ins_agb_west[, "Pre_dist_AGB"])), 200),
                                       rug = TRUE, xlab=deparse(substitute(Pre_dist_AGB)),
                                       ylab="",main=paste("Partial Dependence on",
                                                          deparse(substitute(Pre_dist_AGB)))))



fire_postage_agb3<-as.data.frame(partialPlot(model_fire_agb_kf$finalModel, learn_fire_agb_west, Post_dist_stand_age, which.class, 
                                          plot = TRUE, add = FALSE,
                                          
                                          n.pt = min(length(unique(learn_fire_agb_west[, "Post_dist_stand_age"])), 200),
                                          rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                          ylab="",main=paste("Partial Dependence on", 
                                                             deparse(substitute(Post_dist_stand_age)))))



cut_postage_agb3<-as.data.frame(partialPlot(model_cut_agb_kf$finalModel, learn_cut_agb_west, Post_dist_stand_age, which.class, 
                                         plot = TRUE, add = FALSE,
                                         
                                         n.pt = min(length(unique(learn_cut_agb_west[, "Post_dist_stand_age"])), 200),
                                         rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                         ylab="",main=paste("Partial Dependence on", 
                                                            deparse(substitute(Post_dist_stand_age)))))


ins_postage_agb3<-as.data.frame(partialPlot(model_ins_agb_kf$finalModel, learn_ins_agb_west, Post_dist_stand_age, which.class,
                                         plot = TRUE, add = FALSE,

                                         n.pt = min(length(unique(learn_ins_agb_west[, "Post_dist_stand_age"])), 200),
                                         rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)),
                                         ylab="",main=paste("Partial Dependence on",
                                                            deparse(substitute(Post_dist_stand_age)))))


fire_predist_seed_count3<-as.data.frame(partialPlot(model_fire_seed_kf$finalModel, learn_fire_seed_west, Pre_dist_seed_count, which.class, 
                                               plot = TRUE, add = FALSE,
                                               
                                               n.pt = min(length(unique(learn_fire_seed_west[, "Pre_dist_seed_count"])), 200),
                                               rug = TRUE, xlab=deparse(substitute(Pre_dist_seed_count)), 
                                               ylab="",main=paste("Partial Dependence on", 
                                                                  deparse(substitute(Pre_dist_seed_count)))))

cut_predist_seed_count3<-as.data.frame(partialPlot(model_cut_seed_kf$finalModel, learn_cut_seed_west, Pre_dist_seed_count, which.class, 
                                              plot = TRUE, add = FALSE,
                                              
                                              n.pt = min(length(unique(learn_cut_seed_west[, "Pre_dist_seed_count"])), 200),
                                              rug = TRUE, xlab=deparse(substitute(Pre_dist_seed_count)), 
                                              ylab="",main=paste("Partial Dependence on", 
                                                                 deparse(substitute(Pre_dist_seed_count)))))

ins_predist_seed_count3<-as.data.frame(partialPlot(model_ins_seed_kf$finalModel, learn_ins_seed_west, Pre_dist_seed_count, which.class,
                                              plot = TRUE, add = FALSE,

                                              n.pt = min(length(unique(learn_ins_seed_west[, "Pre_dist_seed_count"])), 200),
                                              rug = TRUE, xlab=deparse(substitute(Pre_dist_seed_count)),
                                              ylab="",main=paste("Partial Dependence on",
                                                                 deparse(substitute(Pre_dist_seed_count)))))

fire_postage_seed_count3<-as.data.frame(partialPlot(model_fire_seed_kf$finalModel, learn_fire_seed_west, Post_dist_stand_age, which.class, 
                                             plot = TRUE, add = FALSE,
                                             
                                             n.pt = min(length(unique(learn_fire_seed_west[, "Post_dist_stand_age"])), 200),
                                             rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                             ylab="",main=paste("Partial Dependence on", 
                                                                deparse(substitute(Post_dist_stand_age)))))

cut_postage_seed_count3<-as.data.frame(partialPlot(model_cut_seed_kf$finalModel, learn_cut_seed_west, Post_dist_stand_age, which.class, 
                                            plot = TRUE, add = FALSE,
                                            
                                            n.pt = min(length(unique(learn_cut_seed_west[, "Post_dist_stand_age"])), 200),
                                            rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)), 
                                            ylab="",main=paste("Partial Dependence on", 
                                                               deparse(substitute(Post_dist_stand_age)))))

ins_postage_seed_count3<-as.data.frame(partialPlot(model_ins_seed_kf$finalModel, learn_ins_seed_west, Post_dist_stand_age, which.class,
                                            plot = TRUE, add = FALSE,

                                            n.pt = min(length(unique(learn_ins_seed_west[, "Post_dist_stand_age"])), 200),
                                            rug = TRUE, xlab=deparse(substitute(Post_dist_stand_age)),
                                            ylab="",main=paste("Partial Dependence on",
                                                               deparse(substitute(Post_dist_stand_age)))))
dev.off()
# dev.new()

library(gridExtra)

sdamw<-sdam42[(sdam42$ecocode %in% ecosel$econew),]
sdame<-sdam42[(sdam42$ecocode %!in% ecosel$econew),]






current_date<-Sys.Date()

sdame<-sdam5b
sdamw<-sdam5a
sdame<-sdame[which(sdame$dist_type!="INSDIS"),]

# sdamw$dist_type<-factor(sdamw$dist_type,levels=c("FIRE","CUT","INSDIS"))
# sdame$dist_type<-factor(sdame$dist_type,levels=c("FIRE","CUT"))

current_date<-Sys.Date()

png(paste0("1d_prev_cond_n_",current_date,".png"), res=135, width = 1200, height = 1400)
par(mfrow=c(3,2))

p1<-ggplot() + 
    geom_histogram(data=sdamw,aes(x=AGB.11,y=stat(count)/10,fill="gray",
                                  color="gray60"),bins=50,
                   color="gray60",alpha=0.4)+
    scale_fill_manual(name="Frequency", values=c("Pre-disturbance AGB" = "gray60"), 
                      labels=c("gray80"="blue values")) +
    geom_line(data = fire_predist_agb3, aes(x = x, y = y,color = "#D95F02"),size=1) +
    geom_line(data = cut_predist_agb3, aes(x = x, y = y, color = "#7570B3"),size=1) +
    geom_line(data = ins_predist_agb3, aes(x = x, y = y, color = "#1B9E77"),size=1) +
    # theme_bw()+   scale_color_identity(name = "Model fit",
    scale_color_identity(name = "Partial dependence",
                         breaks = c("#D95F02", "#7570B3","#1B9E77"),
                         labels = c("Fire", "Harvest", "Insect/disease"),
                         guide = "legend")+
    scale_y_continuous(name = "post-disturbance AGB (Mg/ha)",
                       sec.axis = sec_axis(~.*10, 
                                           name = "frequency of pre-disturbance AGB"
                       ),limits=c(0,25)) +
    xlab('pre-disturbance AGB (Mg/ha)') +
    ggtitle("")+
    theme(legend.position = "none")+
    theme_bw()
  
  
  
  
p2<-ggplot() + 
    geom_histogram(data=sdame,aes(x=AGB.11,y=stat(count)/10,fill="gray",
                                  color="gray60"),bins=50,
                   color="gray60",alpha=0.4)+
    scale_fill_manual(name="frequency", values=c("pre-disturbance AGB" = "gray60"), 
                      labels=c("gray80"="blue values")) +
    geom_line(data = fire_predist_agb1, aes(x = x, y = y,color = "#D95F02"),size=1) +
    geom_line(data = cut_predist_agb1, aes(x = x, y = y, color = "#7570B3"),size=1) +
    # geom_line(data = ins_pd_agb3, aes(x = x, y = y, color = "#1B9E77"),size=1) +
    # theme_bw()+   scale_color_identity(name = "Model fit",
    scale_color_identity(name = "partial dependence",
                         breaks = c("#D95F02", "#7570B3","#1B9E77"),
                         labels = c("Fire", "Harvest", "Insect/disease"),
                         guide = "legend")+
    scale_y_continuous(name = "post-disturbance AGB (Mg/ha)",
                       sec.axis = sec_axis(~.*10, 
                                           name = "frequency of pre-disturbance AGB"
                       ),limits=c(0,25)) +
    xlab('pre-disturbance AGB (Mg/ha)') +
    ggtitle("")+
    theme(legend.position = "none")+
    theme_bw()
  
  
  
  

p3<-ggplot() + 
   geom_histogram(data=sdamw,aes(x=seed_count.1,y=stat(count)*30,fill="seed_count.1",
                                 color="gray60"),bins=50,
                 color="gray60",alpha=0.7)+
scale_fill_manual(name="frequency", values=c("pre-disturbance seedling density" = "gray60"), 
                    labels=c("gray80"="blue values")) +
  geom_line(data = fire_predist_seed_count3, aes(x = x, y = y,color = "#D95F02"),size=1) +
  geom_line(data = cut_predist_seed_count3, aes(x = x, y = y, color = "#7570B3"),size=1) +
  geom_line(data = ins_predist_seed_count3, aes(x = x, y = y, color = "#1B9E77"),size=1) +
  # theme_bw()+   scale_color_identity(name = "Model fit",
  scale_color_identity(name = "partial dependence",
                       breaks = c("#D95F02", "#7570B3","#1B9E77"),
                       labels = c("Fire", "Harvest", "Insect/disease"),
                       guide = "legend")+
  scale_y_continuous(name = "post-disturbance seedling density (ha)",
                     sec.axis = sec_axis(~./20, 
                      name = "frequency of pre-disturbance seedling density"
                     ),limits=c(0,12000)) +
  scale_x_continuous(limits=c(-2,30000))+
  xlab('pre-disturbance seedling density (ha)') +
  ggtitle("")+
  theme(legend.position = "none")+
  theme_bw()



p4<-ggplot() + 
  geom_histogram(data=sdame,aes(x=seed_count.1,y=stat(count)*30,fill="seed_count.1",
                                color="gray60"),bins=50,
                 color="gray60",alpha=0.7)+
  scale_fill_manual(name="frequency", values=c("pre-disturbance seedling density" = "gray60"), 
                    labels=c("gray80"="blue values")) +
  geom_line(data = fire_predist_seed_count1, aes(x = x, y = y,color = "#D95F02"),size=1) +
  geom_line(data = cut_predist_seed_count1, aes(x = x, y = y, color = "#7570B3"),size=1) +
  # geom_line(data = ins_pd_seed_count3, aes(x = x, y = y, color = "#1B9E77"),size=1) +
  # theme_bw()+   scale_color_identity(name = "Model fit",
  scale_color_identity(name = "partial dependence",
                       breaks = c("#D95F02", "#7570B3","#1B9E77"),
                       labels = c("Fire", "Harvest", "Insect/disease"),
                       guide = "legend")+
  scale_y_continuous(name = "post-disturbance seedling density (ha)",
                     sec.axis = sec_axis(~./20, 
                                         name = "frequency of pre-disturbance seedling density"
                     ),limits=c(0,12000)) +
  scale_x_continuous(limits=c(0,30000))+
  xlab('pre-disturbance seedling density (ha)') +
  ggtitle("")+
  theme(legend.position = "none")+
  theme_bw()


p_leg<-ggplot() + 
  geom_histogram(data=sdamw,aes(x=seed_count.1,y=stat(count)*60,fill="seed_count.1",
                                color="gray60"),bins=80,
                 color="gray60",alpha=0.7)+
  scale_fill_manual(name="frequency", values=c("pre-disturbance seedling density or AGB" = "gray60"), 
                    labels=c("gray80"="blue values")) +
  geom_line(data = fire_predist_seed_count3, aes(x = x, y = y,color = "#D95F02"),size=1) +
  geom_line(data = cut_predist_seed_count3, aes(x = x, y = y, color = "#7570B3"),size=1) +
  geom_line(data = ins_predist_seed_count3, aes(x = x, y = y, color = "#1B9E77"),size=1) +
  # theme_bw()+   scale_color_identity(name = "Model fit",
  scale_color_identity(name = "partial dependence",
                       breaks = c("#D95F02", "#7570B3","#1B9E77"),
                       labels = c("Fire", "Harvest", "Insect/disease"),
                       guide = "legend")+
  scale_y_continuous(name = "post-disturbance seedling density (ha)",
                     sec.axis = sec_axis(~./60, 
                                         name = "frequency of pre-disturbance AGB"
                     ),limits=c(0,32000)) +
  xlab('pre-disturbance seedling density (ha)') +
  ggtitle("")+
  theme(legend.position = "none")+
  theme_bw()

library(grid)
library(gridExtra)

library(gtable)

tg1 <- textGrob('West', gp = gpar(fontsize = 15, fontface = 'bold'))
tg2 <- textGrob('East', gp = gpar(fontsize = 15, fontface = 'bold'))


library(grid)
grid.arrange(tg1,
             tg2,
             p1 + theme(legend.position="none"), 
             p2 + theme(legend.position="none"),
             p3 + theme(legend.position="none"),
             p4 + theme(legend.position="none"),
             p_leg,
             # legend2,
             heights=c(0.1,1.1, 1.1, 1.1),
             nrow = 4)

dev.off()



# p7<-grid.arrange(p1,p3,p4,p6,nrow=2)
# dev.off()

table(sdame$dist_type)

# sdame$distype2<-factor(sdame$dist_type,levels=c("fire","cut","harvest"))
# sdamw$distype2<-factor(sdamw$dist_type,levels=c("fire","cut","harvest"))



png(paste0("1d_stand_age_all_n_",current_date,".png"), res=135, width = 1200, height = 1400)
par(mfrow=c(3,2))

 p5<-ggplot() + 
   geom_histogram(data=sdamw,aes(x=STDAGE.2,y=stat(count)/25,fill=dist_type
                                 ),binwidth=1,
                  alpha=0.5)+
   scale_fill_manual(name="frequency",labels=c("post-disturbance stand age, harvest",
                                               "post-disturbance stand age, fire",
                                               "post-disturbance stand age, insect/disease"),
   values=c("#7570B3","#D95F02","#1B9E77"))+
   # scale_fill_manual(name="Frequency", values=c("Pre-disturbance AGB" = "gray60"), 
   #                   labels=c("gray80"="blue values")) 
   geom_line(data = cut_postage_agb3, aes(x = x, y = y, color = "#7570B3"),size=1) +
   geom_line(data = fire_postage_agb3, aes(x = x, y = y,color = "#D95F02"),size=1) +
   geom_line(data = ins_postage_agb3, aes(x = x, y = y, color = "#1B9E77"),size=1) +
   # theme_bw()+   scale_color_identity(name = "Model fit",
   scale_color_identity(name = "partial dependence",
                        breaks = c("#D95F02", "#7570B3","#1B9E77"),
                        labels = c("Fire", "harvest", "insect/disease"),
                        guide = "legend")+
   scale_y_continuous(name = "post-disturbance AGB (Mg/ha)",
                      sec.axis = sec_axis(~.*25, 
                                          name = "frequency of post-disturbance stand age"
                      ),limits=c(0,21))+ 
   xlab('post-disturbance stand age') +
   ggtitle("")+
   theme(legend.position = "none")+
   theme_bw()
 
 
p6<-ggplot() + 
   geom_histogram(data=sdame,aes(x=STDAGE.2,y=stat(count)/25,fill=dist_type
   ),binwidth=1,
   alpha=0.5)+
   scale_fill_manual(name="frequency",labels=c("post-disturbance stand age, harvest",
                                               "post-disturbance stand age, fire",
                                               "post-disturbance stand age, insect/disease"),
                     values=c("#7570B3","#D95F02","#1B9E77"))+
   # scale_fill_manual(name="Frequency", values=c("Pre-disturbance AGB" = "gray60"), 
   #                   labels=c("gray80"="blue values")) +
   geom_line(data = fire_postage_agb1, aes(x = x, y = y,color = "#D95F02"),size=1) +
   geom_line(data = cut_postage_agb1, aes(x = x, y = y, color = "#7570B3"),size=1) +
   # geom_line(data = ins_pd_stage3, aes(x = x, y = y, color = "#1B9E77"),size=1) +
   # theme_bw()+   scale_color_identity(name = "Model fit",
   scale_color_identity(name = "Partial dependence",
                        breaks = c("#D95F02", "#7570B3","#1B9E77"),
                        labels = c("Fire", "Harvest", "Insect/disease"),
                        guide = "legend")+
   scale_y_continuous(name = "post-disturbance AGB (Mg/ha)",
                      sec.axis = sec_axis(~.*25, 
                                          name = "frequency of post-disturbance stand age"
                      ),limits=c(0,21))+ 
   xlab('post-disturbance stand age') +
   ggtitle("")+
   theme(legend.position = "none")+
   theme_bw()
 
 
 p7<-ggplot() + 
   geom_histogram(data=sdamw,aes(x=STDAGE.2,y=stat(count)*20,fill=dist_type
   ),binwidth=1,
   alpha=0.5)+
   scale_fill_manual(name="frequency",labels=c("post-disturbance stand age, harvest",
                                               "post-disturbance stand age, fire",
                                               "post-disturbance stand age, insect/disease"),
                     values=c("#7570B3","#D95F02","#1B9E77"))+
   # scale_fill_manual(name="Frequency", values=c("Pre-disturbance AGB" = "gray60"), 
   #                   labels=c("gray80"="blue values")) +
   geom_line(data = fire_postage_seed_count3, aes(x = x, y = y,color = "#D95F02"),size=1) +
   geom_line(data = cut_postage_seed_count3, aes(x = x, y = y, color = "#7570B3"),size=1) +
   geom_line(data = ins_postage_seed_count3, aes(x = x, y = y, color = "#1B9E77"),size=1) +
   # theme_bw()+   scale_color_identity(name = "Model fit",
   scale_color_identity(name = "Partial dependence",
                        breaks = c("#D95F02", "#7570B3","#1B9E77"),
                        labels = c("Fire", "Harvest", "Insect/disease"),
                        guide = "legend")+
   scale_y_continuous(name = "post-disturbance seedling density (ha)",
                      sec.axis = sec_axis(~./20, 
                                          name = "frequency of post-disturbance stand age",
                                          breaks=c(seq(0,500,100))
                      ),limits=c(0,10000))+ 
   xlab('post-disturbance stand age') +
   ggtitle("")+
   theme(legend.position = "none")+
   theme_bw()
 
 
 p8<-ggplot() + 
   geom_histogram(data=sdame,aes(x=STDAGE.2,y=stat(count)*20,fill=dist_type
   ),binwidth=1,
   alpha=0.5)+
   scale_fill_manual(name="frequency",labels=c("post-disturbance stand age, harvest",
                                               "post-disturbance stand age, fire",
                                               "post-disturbance stand age, insect/disease"),
                     values=c("#7570B3","#D95F02","#1B9E77"))+
   # scale_fill_manual(name="Frequency", values=c("Pre-disturbance AGB" = "gray60"), 
   #                   labels=c("gray80"="blue values")) +
   geom_line(data = fire_postage_seed_count1, aes(x = x, y = y,color = "#D95F02"),size=1) +
   geom_line(data = cut_postage_seed_count1, aes(x = x, y = y, color = "#7570B3"),size=1) +
   # geom_line(data = ins_pd_seed_age3, aes(x = x, y = y, color = "#1B9E77"),size=1) +
   # theme_bw()+   scale_color_identity(name = "Model fit",
   scale_color_identity(name = "Partial dependence",
                        breaks = c("#D95F02", "#7570B3","#1B9E77"),
                        labels = c("Fire", "Harvest", "Insect/disease"),
                        guide = "legend")+
   scale_y_continuous(name = "post-disturbance seedling density (ha)",
                      sec.axis = sec_axis(~./20, 
                                          name = "frequency of post-disturbance stand age",
                                          breaks=c(seq(0,500,100))
                      ),limits=c(0,10000))+ 
   xlab('post-disturbance stand age') +
   ggtitle("")+
   theme(legend.position = "none")+
   theme_bw()
 

 
 
 
 library(gtable)
 
 tg1 <- textGrob('West', gp = gpar(fontsize = 15, fontface = 'bold'))
 tg2 <- textGrob('East', gp = gpar(fontsize = 15, fontface = 'bold'))
 
 
 library(grid)
 library(gridExtra)
 grid.arrange(tg1,
              tg2,
              p5 + theme(legend.position="none"), 
              p6 + theme(legend.position="none"),
              p7 + theme(legend.position="none"),
              p8 + theme(legend.position="none"),
              p7,
              # legend2,
              heights=c(0.1,1.1, 1.1, 1.1),
              nrow = 4)
 
 dev.off()
 
 
 
