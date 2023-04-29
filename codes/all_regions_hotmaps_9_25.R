### plots based on disturbances from FIA data

# setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')

rm(list=ls())
# remove.packages("tidyverse")

library(tidyr)
library(dplyr)
library(reshape2)
# memory.limit(size=10000)


ddd<-Sys.Date()
model_ins_agb_kf1<-readRDS(paste0("model_west_ins_agb_kf_",ddd,".RDS"))
model_cut_agb_kf1<-readRDS(paste0("model_west_cut_agb_kf_",ddd,".RDS"))
model_fire_agb_kf1<-readRDS(paste0("model_west_fire_agb_kf_",ddd,".RDS"))
model_all_agb_kf1<-readRDS(paste0("model_west_all_agb_kf_",ddd,".RDS"))
model_ins_seed_kf1<-readRDS(paste0("model_west_ins_seed_kf_",ddd,".RDS"))
model_cut_seed_kf1<-readRDS(paste0("model_west_cut_seed_kf_",ddd,".RDS"))
model_fire_seed_kf1<-readRDS(paste0("model_west_fire_seed_kf_",ddd,".RDS"))
model_all_seed_kf1<-readRDS(paste0("model_west_all_seed_kf_",ddd,".RDS"))

# model_ins_agb_kf2<-readRDS(paste0("model_east_insdis_agb_kf_",ddd,".RDS"))
model_cut_agb_kf2<-readRDS(paste0("model_east_cut_agb_kf_",ddd,".RDS"))
model_fire_agb_kf2<-readRDS(paste0("model_east_fire_agb_kf_",ddd,".RDS"))
model_all_agb_kf2<-readRDS(paste0("model_east_all_agb_kf_",ddd,".RDS"))
# model_ins_seed_kf<-readRDS(paste0("model_east_insdis_seed_kf_",ddd,".RDS"))
model_cut_seed_kf2<-readRDS(paste0("model_east_cut_seed_kf_",ddd,".RDS"))
model_fire_seed_kf2<-readRDS(paste0("model_east_fire_seed_kf_",ddd,".RDS"))
model_all_seed_kf2<-readRDS(paste0("model_east_all_seed_kf_",ddd,".RDS"))

# model_all_seed_kf$results$Rsquared
# model_all_seed_kf$bestTune


library(randomForest)
library(caret)
library(pdp)



# model_all_seed_kf$results$Rsquared
# model_all_seed_kf$bestTune


imp11<-varImp(model_all_agb_kf1$finalModel,scale=TRUE)
imp11$model<-"All_w"
imp11$var<-rownames(imp11)


imp21<-varImp(model_fire_agb_kf1$finalModel,scale=FALSE)
imp21$model<-"WF"
aaa1<-rownames(imp21)
rownames(imp21)<-aaa1
imp21$var<-rownames(imp21)


imp31<-varImp(model_cut_agb_kf1$finalModel,scale=FALSE)
imp31$model<-"WH"
aaa1<-rownames(imp31)
rownames(imp31)<-aaa1

imp31$var<-rownames(imp31)


imp41<-varImp(model_ins_agb_kf1$finalModel,scale=FALSE)
imp41$model<-"WI"
aaa1<-rownames(imp41)
rownames(imp41)<-aaa1

imp41$var<-rownames(imp41)

imp51<-varImp(model_all_seed_kf1$finalModel,scale=FALSE)
imp51$model<-"All_w"
imp51$var<-rownames(imp51)

imp61<-varImp(model_fire_seed_kf1$finalModel,scale=FALSE)
imp61$model<-"WF"
aaa1<-rownames(imp61)
rownames(imp61)<-aaa1

imp61$var<-rownames(imp61)

imp71<-varImp(model_cut_seed_kf1$finalModel,scale=FALSE)
imp71$model<-"WH"
aaa1<-rownames(imp71)
rownames(imp71)<-aaa1

imp71$var<-rownames(imp71)

imp81<-varImp(model_ins_seed_kf1$finalModel,scale=FALSE)
imp81$model<-"WI"
aaa<-rownames(imp81)
# bbb<-replace(aaa,7,"Disturbance_types")
rownames(imp81)<-aaa1

imp81$var<-rownames(imp81)




imp12<-varImp(model_all_agb_kf2$finalModel,scale=TRUE)
imp12$model<-"All+e"
imp12$var<-rownames(imp12)


imp22<-varImp(model_fire_agb_kf2$finalModel,scale=FALSE)
imp22$model<-"EF"
aaa2<-rownames(imp22)
rownames(imp22)<-aaa2
imp22$var<-rownames(imp22)


imp32<-varImp(model_cut_agb_kf2$finalModel,scale=FALSE)
imp32$model<-"EH"
aaa2<-rownames(imp32)
rownames(imp32)<-aaa2

imp32$var<-rownames(imp32)


# imp42<-varImp(model_ins_agb_kf2$finalModel,scale=FALSE)
# imp42$model<-"Insect"
# aaa2<-rownames(imp42)
# rownames(imp42)<-aaa2

# imp42$var<-rownames(imp42)

imp52<-varImp(model_all_seed_kf2$finalModel,scale=FALSE)
imp52$model<-"All_e"
imp52$var<-rownames(imp52)

imp62<-varImp(model_fire_seed_kf2$finalModel,scale=FALSE)
imp62$model<-"EF"
aaa2<-rownames(imp62)
rownames(imp62)<-aaa2

imp62$var<-rownames(imp62)

imp72<-varImp(model_cut_seed_kf2$finalModel,scale=FALSE)
imp72$model<-"EH"
aaa2<-rownames(imp72)
rownames(imp72)<-aaa2

imp72$var<-rownames(imp72)

# imp82<-varImp(model_ins_seed_kf2$finalModel,scale=FALSE)
# imp82$model<-"Insect_e"
# aaa<-rownames(imp82)
# # bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp82)<-aaa2
# 
# imp82$var<-rownames(imp82)



agb_imp<-rbind(imp21,imp31,imp41,imp22,imp32)


seed_imp<-rbind(imp61,imp71,imp81,imp62,imp72)

west_imp<-rbind(imp21,imp31,imp41,imp61,imp71,imp81)


east_imp<-rbind(imp22,imp32,imp62,imp72)


library(dplyr)
library(tidyverse)
agb_imp1<-group_by(agb_imp, model) %>% mutate(percent = (Overall/max(Overall)*100))
seed_imp1<-group_by(seed_imp, model) %>% mutate(percent = (Overall/max(Overall)*100))


# west_imp1<-group_by(west_imp, model) %>% mutate(percent = (Overall/max(Overall)*100))
# east_imp1<-group_by(east_imp, model) %>% mutate(percent = (Overall/max(Overall)*100))


 b=c(0,100)
 
 
 # agb_imp1$var2<-ifelse(agb_imp1$var=="previous_disturbance", "Disturbance_types",agb_imp1$var)
 # seed_imp1$var2<-ifelse(seed_imp1$var=="previous_disturbance", "Disturbance_types",seed_imp1$var)
 # 
 
 
dev.off()
dev.new()
p1<-ggplot(data=agb_imp1,aes(x=(model),y=reorder(var,percent)))+
   geom_tile(aes(fill=percent),color="gray70")+
   theme(
      panel.background = element_rect(fill = "white"),
      # plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      # panel.margin=margin(0.5,0.5,0.5,0.5,"cm"),
      plot.background = element_rect(
         fill = "grey90",
         colour = "black",
         size = 1)
      
   )+
   scale_fill_gradientn(limits = c(0,100),
                        colours=c("Yellow", "Orange", "red","brown"),
                        breaks=b, labels=format(b))+
   scale_y_discrete(labels=c("Pre_dist_AGB"="Pre disturbance AGB",
                             "Disturbance_types"= "Disturbance type",
                             # "Disturbance_history" = "Disturbance history"
                             "Forest_Group"= "Forest type",
                             "elevation"= "Elevation",
                             "Ecoregion"= "Ecoprovince",
                             "slope"= "Slope",
                             "aspect"= "Aspect",
                             "Post_dist_stand_age"= "Post disturbance stand age",
                             "Pre_dist_stand_age"= "Pre disturbance stand age",
                             # "previous_disturbance"= "Disturbance history",
                             "physiography"= "Physiography",
                             "Stand_origin"= "Stand origin"))+

scale_x_discrete(limits = c("WF", "EF",
                            "WH","EH",
                            "WI"))+

   xlab("")+
    ylab("")+
   ggtitle("AGB models")+
  
   # theme(axis.title.y = element_text(size = rel(1.8), angle = 90))+
   coord_equal()+
   theme_bw() +
   theme(panel.grid.major.x = element_blank(),
         panel.grid.major.y=element_blank(),
         axis.title.y=element_text(size=16),
         axis.text.y=element_text(size=11),
         axis.text.x=element_text(size=11),
         plot.title=element_text(hjust=0.5,size=12),
      
         legend.position="none")


p2<-ggplot(data=seed_imp1,aes(x=(model),y=reorder(var,percent)))+
   geom_tile(aes(fill=percent),color="gray70")+
   theme(
      panel.background = element_rect(fill = "white"),
      # plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
      # panel.margin=margin(0.5,0.5,0.5,0.5,"cm"),
      plot.background = element_rect(
         fill = "grey90",
         colour = "black",
         size = 1)
      
   )+
   scale_fill_gradientn(limits = c(0,100),
                        colours=c("Yellow", "Orange", "red","brown"),
                        breaks=b, labels=format(b))+
   scale_y_discrete(labels=c("Pre_dist_seedling_density"="Pre disturbance seed density",
                             "Disturbance_types"= "Disturbance history",
                             "Forest_Group"= "Forest type",
                             "elevation"= "Elevation",
                             "Ecoregion"= "Ecoprovince",
                             "slope"= "Slope",
                             "aspect"= "Aspect",
                             "Post_dist_stand_age"= "Post disturbance stand age",
                             "Pre_dist_stand_age"= "Pre disturbance stand age",
                             "previous_disturbance"= "Disturbance history",
                             "physiography"= "Physiography",
                             "Stand_origin"= "Stand origin"))+
  scale_x_discrete(limits = c("WF", "EF",
                              "WH","EH",
                              "WI"))+
   xlab("")+
   ylab("")+
  ggtitle("Seedling density models") +
   
      coord_equal()+
   theme_bw() +

   theme(panel.grid.major.x = element_blank(),
         panel.grid.major.y=element_blank(),
         axis.title.y=element_text(size=16),
         axis.text.y=element_text(size=11),
         axis.text.x=element_text(size=11),
         legend.key.height=unit(1.6,'cm'),
         legend.key.size=unit(1,'cm'),
         legend.text=element_text(size=10),
         legend.title=element_text(size=11),
         plot.title = element_text(hjust = 0.5),
         legend.position="none")


library(ggpubr)
library(cowplot)
legend<-get_legend(p2)

lll<-get_legend((p2) + theme(legend.position = "right"))
dev.new()
png(filename=paste0("all_heatmap_vif_",ddd,".png"), res=150, width = 1650, height = 1600)
par(oma = c(0, 0, 0, 0))  

pgrid<-plot_grid(plotlist = c(p1,p2), ncol = 3)
plot_grid(p1,p2, lll, ncol = 3, nrow=1,rel_widths=c(1,1,0.3))
dev.off()

