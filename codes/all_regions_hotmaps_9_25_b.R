### plots based on disturbances from FIA data

# setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')

rm(list=ls())
# remove.packages("tidyverse")

library(tidyr)
library(dplyr)
library(reshape2)
# memory.limit(size=10000)





ddd<-"2022-09-25"
model_ins_agb_kf<-readRDS(paste0("model_west_ins_agb_kf_",ddd,".RDS"))
model_cut_agb_kf<-readRDS(paste0("model_west_cut_agb_kf_",ddd,".RDS"))
model_fire_agb_kf<-readRDS(paste0("model_west_fire_agb_kf_",ddd,".RDS"))
model_all_agb_kf<-readRDS(paste0("model_west_all_agb_kf_",ddd,".RDS"))
model_ins_seed_kf<-readRDS(paste0("model_west_ins_seed_kf_",ddd,".RDS"))
model_cut_seed_kf<-readRDS(paste0("model_west_cut_seed_kf_",ddd,".RDS"))
model_fire_seed_kf<-readRDS(paste0("model_west_fire_seed_kf_",ddd,".RDS"))
model_all_seed_kf<-readRDS(paste0("model_west_all_seed_kf_",ddd,".RDS"))




model_all_seed_kf$results$Rsquared
model_all_seed_kf$bestTune


library(randomForest)
library(caret)
library(pdp)

imp1<-varImp(model_all_agb_kf$finalModel,scale=FALSE)
imp1$model<-"All"
imp1$var<-rownames(imp1)


imp2<-varImp(model_fire_agb_kf$finalModel,scale=FALSE)
imp2$model<-"Fire"
# aaa<-rownames(imp2)
# bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp2)<-bbb
imp2$var<-rownames(imp2)




imp3<-varImp(model_cut_agb_kf$finalModel,scale=FALSE)
imp3$model<-"Harvest"
# aaa<-rownames(imp3)
# bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp3)<-bbb

imp3$var<-rownames(imp3)


imp4<-varImp(model_ins_agb_kf$finalModel,scale=FALSE)
imp4$model<-"Insect"
# aaa<-rownames(imp4)
# bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp4)<-bbb

imp4$var<-rownames(imp4)

imp5<-varImp(model_all_seed_kf$finalModel,scale=FALSE)
imp5$model<-"All"
imp5$var<-rownames(imp5)

imp6<-varImp(model_fire_seed_kf$finalModel,scale=FALSE)
imp6$model<-"Fire"
# aaa<-rownames(imp6)
# bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp6)<-bbb

imp6$var<-rownames(imp6)

imp7<-varImp(model_cut_seed_kf$finalModel,scale=FALSE)
imp7$model<-"Harvest"
# aaa<-rownames(imp7)
# bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp7)<-bbb

imp7$var<-rownames(imp7)

imp8<-varImp(model_ins_seed_kf$finalModel,scale=FALSE)
imp8$model<-"Insect"
# # aaa<-rownames(imp2)
# # bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp8)<-bbb

imp8$var<-rownames(imp8)


agb_imp<-rbind(imp1,imp2,imp3,imp4)


seed_imp<-rbind(imp5,imp6,imp7,imp8)


library(dplyr)
library(tidyverse)
agb_imp1<-group_by(agb_imp, model) %>% mutate(percent = (Overall/max(Overall)*100))
seed_imp1<-group_by(seed_imp, model) %>% mutate(percent = (Overall/max(Overall)*100))


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
                             "Forest_Group"= "Forest type",
                             "elevation"= "Elevation",
                             "Ecoregion"= "Ecoprovince",
                             "slope"= "Slope",
                             "aspect"= "Aspect",
                             "Post_dist_stand_age"= "Post disturbance stand age",
                             "Pre_dist_stand_age"= "Pre disturbance stand age",
                             "physiography"= "Physiography",
                             "Stand_origin"= "Stand origin"))+
   
   xlab("")+
   ylab("AGB models")+
   ggtitle("West")+
   # theme(axis.title.y = element_text(size = rel(1.8), angle = 90))+
   coord_equal()+
   theme_bw() +
   theme(panel.grid.major.x = element_blank(),
         panel.grid.major.y=element_blank(),
         axis.title.y=element_text(size=16),
         axis.text.y=element_text(size=11),
         axis.text.x=element_text(size=11),
         
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
   scale_y_discrete(labels=c("Pre_dist_seed_count"="Pre disturbance seed density",
                             "Disturbance_types"= "Disturbance type",
                             "Forest_Group"= "Forest type",
                             "elevation"= "Elevation",
                             "Ecoregion"= "Ecoprovince",
                             "slope"= "Slope",
                             "aspect"= "Aspect",
                             "Post_dist_stand_age"= "Post disturbance stand age",
                             "Pre_dist_stand_age"= "Pre disturbance stand age",
                             "physiography"= "Physiography",
                             "Stand_origin"= "Stand origin"))+
   xlab("")+
   ylab("Seedling density models")+
   
      coord_equal()+
   theme_bw() +
   
   theme(panel.grid.major.x = element_blank(),
         panel.grid.major.y=element_blank(),
         axis.title.y=element_text(size=16),
         axis.text.y=element_text(size=11),
         axis.text.x=element_text(size=11),
         legend.position="none")



rm(list = setdiff(ls(), c('p1','p2')))


ddd="2022-09-25"
# model_ins_agb_kf2<-readRDS(paste0("model_east_insdis_agb_kf_",ddd,".RDS"))
model_cut_agb_kf<-readRDS(paste0("model_east_cut_agb_kf_",ddd,".RDS"))
model_fire_agb_kf<-readRDS(paste0("model_east_fire_agb_kf_",ddd,".RDS"))
model_all_agb_kf<-readRDS(paste0("model_east_all_agb_kf_",ddd,".RDS"))
# model_ins_seed_kf<-readRDS(paste0("model_east_insdis_seed_kf_",ddd,".RDS"))
model_cut_seed_kf<-readRDS(paste0("model_east_cut_seed_kf_",ddd,".RDS"))
model_fire_seed_kf<-readRDS(paste0("model_east_fire_seed_kf_",ddd,".RDS"))
model_all_seed_kf<-readRDS(paste0("model_east_all_seed_kf_",ddd,".RDS"))

# model_all_seed_kf$results$Rsquared
# model_all_seed_kf$bestTune


library(randomForest)
library(caret)
library(pdp)

imp1<-varImp(model_all_agb_kf$finalModel,scale=FALSE)
imp1$model<-"All"
imp1$var<-rownames(imp1)


imp2<-varImp(model_fire_agb_kf$finalModel,scale=FALSE)
imp2$model<-"Fire"
# aaa<-rownames(imp2)
# bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp2)<-bbb
imp2$var<-rownames(imp2)




imp3<-varImp(model_cut_agb_kf$finalModel,scale=FALSE)
imp3$model<-"Harvest"
# aaa<-rownames(imp3)
# bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp3)<-bbb

imp3$var<-rownames(imp3)

# 
# imp4<-varImp(model_ins_agb_kf$finalModel,scale=FALSE)
# imp4$model<-"Insect"
# aaa<-rownames(imp4)
# bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp4)<-bbb
# 
# imp4$var<-rownames(imp4)

imp5<-varImp(model_all_seed_kf$finalModel,scale=FALSE)
imp5$model<-"All"
imp5$var<-rownames(imp5)

imp6<-varImp(model_fire_seed_kf$finalModel,scale=FALSE)
imp6$model<-"Fire"
# aaa<-rownames(imp6)
# bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp6)<-bbb

imp6$var<-rownames(imp6)

imp7<-varImp(model_cut_seed_kf$finalModel,scale=FALSE)
imp7$model<-"Harvest"
# aaa<-rownames(imp7)
# bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp7)<-bbb

imp7$var<-rownames(imp7)

# imp8<-varImp(model_ins_seed_kf$finalModel,scale=FALSE)
# imp8$model<-"Insect"
# # aaa<-rownames(imp2)
# # bbb<-replace(aaa,7,"Disturbance_types")
# rownames(imp8)<-bbb
# 
# imp8$var<-rownames(imp8)


agb_imp<-rbind(imp1,imp2,imp3)


seed_imp<-rbind(imp5,imp6,imp7)


library(dplyr)
agb_imp1<-group_by(agb_imp, model) %>% mutate(percent = (Overall/max(Overall)*100))
seed_imp1<-group_by(seed_imp, model) %>% mutate(percent = (Overall/max(Overall)*100))


# levels(p3$G) <- c("C", "A", "B")

# agb_imp1$rank<-ifelse(agb_imp1$model=="All",agb_imp1$percent,"0")

# 
# png(filename="west_all_heatmap_vif1.png", res=160, width = 1600, height = 800)
# par(mfrow=c(1,2))
b=c(0,100)


p3<-ggplot(data=agb_imp1,aes(x=(model),y=reorder(var,percent)))+
   geom_tile(aes(fill=percent),color="gray70")+
   scale_fill_gradientn(limits = c(0,100),
                        colours=c("Yellow", "Orange", "red","brown"),
                        breaks=b, labels=format(b))+
   scale_y_discrete(labels=c("Pre_dist_AGB"="Pre disturbance AGB",
                             "Disturbance_types"= "Disturbance type",
                             "Forest_Group"= "Forest type",
                             "elevation"= "Elevation",
                             "Ecoregion"= "Ecoprovince",
                             "slope"= "Slope",
                             "aspect"= "Aspect",
                             "Post_dist_stand_age"= "Post disturbance stand age",
                             "Pre_dist_stand_age"= "Pre disturbance stand age",
                             "physiography"= "Physiography",
                             "Stand_origin"= "Stand origin"))+
   xlab("")+
   ylab("")+
   ggtitle("East")+
   
   coord_equal()+
   theme_bw() +
   theme(panel.grid.major.x = element_blank(),
         panel.grid.major.y=element_blank(),
         axis.title.y=element_text(size=16),
         axis.text.y=element_text(size=11),
         axis.text.x=element_text(size=11),
         
         legend.position="none")



p4<-ggplot(data=seed_imp1,aes(x=(model),y=reorder(var,percent)))+
   geom_tile(aes(fill=percent),color="gray70")+
   scale_fill_gradientn(limits = c(0,100),
                        colours=c("Yellow", "Orange", "red","brown"),
                        breaks=b, labels=format(b))+
   scale_y_discrete(labels=c("Pre_dist_seed_count"="Pre disturbance seed density",
                             "Disturbance_types"= "Disturbance type",
                             "Forest_Group"= "Forest type",
                             "elevation"= "Elevation",
                             "Ecoregion"= "Ecoprovince",
                             "slope"= "Slope",
                             "aspect"= "Aspect",
                             "Post_dist_stand_age"= "Post disturbance stand age",
                             "Pre_dist_stand_age"= "Pre disturbance stand age",
                             "physiography"= "Physiography",
                             "Stand_origin"= "Stand origin"))+
   xlab("")+
   ylab("")+
   
   coord_equal()+
   theme_bw() +
   theme(panel.grid.major.x = element_blank(),
         panel.grid.major.y=element_blank(),
         axis.title.y=element_text(size=16),
         axis.text.y=element_text(size=11),
         axis.text.x=element_text(size=11),
         
         legend.position="none")


rm(list = setdiff(ls(), c('p1','p2','p3','p4')))


ddd1<-Sys.Date()
png(filename=paste0("all_heatmap_vif_",ddd1,".png"), res=125, width = 1650, height = 1600)
par(oma = c(0, 0, 0, 0))  
par(mfrow=c(2,2))
# p7<-grid.arrange(p1,p5,p2,p6,nrow=2)
library(grid)
library(gridExtra)

p7<-grid.arrange(p1,p3,p2,p4,nrow=2)
# margin = theme(plot.margin = unit(c(2,2,2,2), "cm"))
# p7<-grid.arrange(p1,p2,p3,p6,p2,p4,p6,nrow=2, "+", margin)

dev.off()



