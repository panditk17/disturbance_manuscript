### plots based on disturbances from FIA data

# setwd('C:/Karuns_documents/fire_MTBS/all_disturbance/disturbance')
setwd('C:/Karuns_docs/disturbance_files/')

rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)


sdam1<-read.csv("all_plots_data_with_seedling_8_27.csv")

sdam2<-separate(sdam1, ECOSUBCD.1, into = c("Spl_1", "Spl_2"), 
               sep = 4, remove = FALSE)

ecosel<-read.csv("../data/eco_select.csv")

sdam2$ecocode <- trimws(sdam2$Spl_1, which = c("left"))

library(operators)
sdam3<-sdam2[(sdam2$ecocode %in% ecosel$econew),]

sdam3$AGB_CHP<-sdam3$AGB.22-sdam3$AGB.11

pdam16<-sdam3

## plots with no disturbance

trall_1a_ds<-pdam16[which(pdam16$dist_type=="FIRE" & pdam16$fia_sev1==0),]
trall_1b_ds<-pdam16[which(pdam16$dist_type=="FIRE" & pdam16$fia_sev1==1),]


aaa2a<-data.frame(trall_1a_ds$AGB_CHP)
aaa2s<-data.frame(trall_1a_ds$seed_ct_ch)
aaa2a1<-data.frame(trall_1a_ds$AGB.11)
aaa2a2<-data.frame(trall_1a_ds$AGB.22)
aaa2a3<-data.frame(trall_1a_ds$seed_count.1)
aaa2a4<-data.frame(trall_1a_ds$seed_count.1)


aaa2<-cbind(aaa2a1,aaa2a2,aaa2a3,aaa2a3,aaa2a,aaa2s)

aaa2$type<-"MF"
colnames(aaa2)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")


aaa3a<-data.frame(trall_1b_ds$AGB_CHP)
aaa3s<-data.frame(trall_1b_ds$seed_ct_ch)
aaa3a1<-data.frame(trall_1b_ds$AGB.11)
aaa3a2<-data.frame(trall_1b_ds$AGB.22)
aaa3a3<-data.frame(trall_1b_ds$seed_count.1)
aaa3a4<-data.frame(trall_1b_ds$seed_count.1)

aaa3<-cbind(aaa3a1,aaa3a2,aaa3a3,aaa3a3,aaa3a,aaa3s)


aaa3$type<-"SF"

colnames(aaa3)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")



fire_dist<-rbind(aaa2,aaa3)




trall_1a_dsc<-pdam16[which(pdam16$dist_type=="CUT" & pdam16$fia_sev1==0),]
trall_1b_dsc<-pdam16[which(pdam16$dist_type=="CUT" & pdam16$fia_sev1==1),]


caaa2a<-data.frame(trall_1a_dsc$AGB_CHP)
caaa2b<-data.frame(trall_1a_dsc$seed_ct_ch)
caaa2a1<-data.frame(trall_1a_dsc$AGB.11)
caaa2a2<-data.frame(trall_1a_dsc$AGB.22)
caaa2a3<-data.frame(trall_1a_dsc$seed_count.1)
caaa2a4<-data.frame(trall_1a_dsc$seed_count.2)


caaa2<-cbind(caaa2a1,caaa2a2,caaa2a3,caaa2a4,caaa2a,caaa2b)

caaa2$type<-"MC"
colnames(caaa2)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")

caaa3a<-data.frame(trall_1b_dsc$AGB_CHP)
caaa3b<-data.frame(trall_1b_dsc$seed_ct_ch)
caaa3a1<-data.frame(trall_1b_dsc$AGB.11)
caaa3a2<-data.frame(trall_1b_dsc$AGB.22)
caaa3a3<-data.frame(trall_1b_dsc$seed_count.1)
caaa3a4<-data.frame(trall_1b_dsc$seed_count.2)


caaa3<-cbind(caaa3a1,caaa3a2,caaa3a3,caaa3a4,caaa3a,caaa3b)
caaa3$type<-"SC"

colnames(caaa3)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")



trall_1a_dsi<-pdam16[which(pdam16$dist_type=="INSDIS" & pdam16$fia_sev1==0),]
trall_1b_dsi<-pdam16[which(pdam16$dist_type=="INSDIS" & pdam16$fia_sev1==1),]



iaaa2a<-data.frame(trall_1a_dsi$AGB_CHP)
iaaa2b<-data.frame(trall_1a_dsi$seed_ct_ch)
iaaa2a1<-data.frame(trall_1a_dsi$AGB.11)
iaaa2a2<-data.frame(trall_1a_dsi$AGB.22)
iaaa2a3<-data.frame(trall_1a_dsi$seed_count.1)
iaaa2a4<-data.frame(trall_1a_dsi$seed_count.2)


iaaa2<-cbind(iaaa2a1,iaaa2a2,iaaa2a3,iaaa2a4,iaaa2a,iaaa2b)
iaaa2$type<-"MI"
colnames(iaaa2)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")

iaaa3a<-data.frame(trall_1b_dsi$AGB_CHP)
iaaa3b<-data.frame(trall_1b_dsi$seed_ct_ch)
iaaa3a1<-data.frame(trall_1b_dsi$AGB.11)
iaaa3a2<-data.frame(trall_1b_dsi$AGB.22)
iaaa3a3<-data.frame(trall_1b_dsi$seed_count.1)
iaaa3a4<-data.frame(trall_1b_dsi$seed_count.2)


iaaa3<-cbind(iaaa3a1,iaaa3a2,iaaa3a3,iaaa3a4,iaaa3a,iaaa3b)

iaaa3$type<-"SI"

colnames(iaaa3)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")



all_sev_dist<-rbind(aaa3,caaa3,iaaa3)

all_mild_dist<-rbind(aaa2,caaa2,iaaa2)


med_west_AGBCH<-median(all_sev_dist$AGBCH,by="type")


library("ggpubr")
ggline(all_sev_dist, x = "type", y = "AGBCH", 
       add = c("mean_se", "jitter"), 
       order = c("SF", "SC", "SI"),
       ylab = "AGBCH", xlab = "disturbances")

mean_type_severe_AGBCH<-aggregate(AGBCH~type,all_sev_dist,FUN=mean)



test_severe_west_AGBCH<-pairwise.wilcox.test(all_sev_dist$AGBCH, all_sev_dist$type, p.adjust.method = "BH")
print(test_severe_west_AGBCH)




model11a <- aov(AGB1 ~ type, data = all_sev_dist)
summary(model11a)

tukey11a<-TukeyHSD(model11a, conf.level=.95)
tukey11a

model11b <- aov(AGB2 ~ type, data = all_sev_dist)
summary(model11b)

tukey11b<-TukeyHSD(model11b, conf.level=.95)
tukey11b

model11c <- aov(Seed1 ~ type, data = all_sev_dist)
summary(model11c)

tukey11c<-TukeyHSD(model11c, conf.level=.95)
tukey11c

model11d <- aov(Seed2 ~ type, data = all_sev_dist)
summary(model11d)

tukey11d<-TukeyHSD(model11d, conf.level=.95)
tukey11d


krustest11a<-kruskal.test(AGB1 ~ type, data = all_sev_dist)
krustest11b<-kruskal.test(AGB2 ~ type, data = all_sev_dist)
krustest11c<-kruskal.test(Seed1 ~ type, data = all_sev_dist)
krustest11d<-kruskal.test(Seed2 ~ type, data = all_sev_dist)

krustest11<-kruskal.test(AGBCH ~ type, data = all_sev_dist)


library(dunn.test)
dunntest11a<-as.data.frame(dunn.test(all_sev_dist$AGB1,all_sev_dist$type,method="bonferroni",alpha=0.05))
dunntest11b<-as.data.frame(dunn.test(all_sev_dist$AGB2,all_sev_dist$type,method="bonferroni",alpha=0.05))
dunntest11c<-as.data.frame(dunn.test(all_sev_dist$Seed1,all_sev_dist$type,method="bonferroni",alpha=0.05))
dunntest11d<-as.data.frame(dunn.test(all_sev_dist$Seed2,all_sev_dist$type,method="bonferroni",alpha=0.05))

dunntest11<-as.data.frame(dunn.test(all_sev_dist$AGBCH,all_sev_dist$type,method="bonferroni",alpha=0.05))


# kruskal_test_AGBCH<-pairwise.kruskal.test(AGBCH ~ type, data = all_sev_dist)
# print(kruskal_test_AGBCH)


ggline(all_sev_dist, x = "type", y = "seedch", 
       add = c("mean_se", "jitter"), 
       order = c("SF", "SC", "SI"),
       ylab = "seedch", xlab = "disturbances")

mean_type_severe_seedch<-aggregate(seedch~type,all_sev_dist,FUN=mean)



test_severe_west_seedch<-pairwise.wilcox.test(all_sev_dist$seedch, all_sev_dist$type, p.adjust.method = "BH")

print(test_severe_west_seedch)


krustest12<-kruskal.test(seedch ~ type, data = all_sev_dist)

dunntest12<-as.data.frame(dunn.test(all_sev_dist$seedch,all_sev_dist$type,method="bonferroni",alpha=0.05))


### normality test
shapiro.test(all_sev_dist$AGBCH)

hist(all_sev_dist$AGBCH)

ggline(all_mild_dist, x = "type", y = "AGBCH", 
       add = c("mean_se", "jitter"), 
       order = c("MF", "MC", "MI"),
       ylab = "AGBCH", xlab = "disturbances")

mean_type_mildere_AGBCH<-aggregate(AGBCH~type,all_mild_dist,FUN=mean)


test_mildere_west_AGBCH<-pairwise.wilcox.test(all_mild_dist$AGBCH, all_mild_dist$type, p.adjust.method = "BH")

print(test_mildere_west_AGBCH)


model13a <- aov(AGB1 ~ type, data = all_mild_dist)
summary(model13a)

tukey13a<-TukeyHSD(model13a, conf.level=.95)
tukey13a

model13b <- aov(AGB2 ~ type, data = all_mild_dist)
summary(model13b)

tukey13b<-TukeyHSD(model13b, conf.level=.95)
tukey13b

model13c <- aov(Seed1 ~ type, data = all_mild_dist)
summary(model13c)

tukey13c<-TukeyHSD(model13c, conf.level=.95)
tukey13c

model13d <- aov(Seed2 ~ type, data = all_mild_dist)
summary(model13d)

tukey13d<-TukeyHSD(model13d, conf.level=.95)
tukey13d


tukeywest<-list(rbind(tukey11a$type,tukey11b$type,tukey11c$type,tukey11d$type,
                      tukey13a$type,tukey13b$type,tukey13c$type,tukey13d$type))


krustest13a<-kruskal.test(AGB1 ~ type, data = all_mild_dist)
krustest13b<-kruskal.test(AGB2 ~ type, data = all_mild_dist)
krustest13c<-kruskal.test(Seed1 ~ type, data = all_mild_dist)
krustest13d<-kruskal.test(Seed2 ~ type, data = all_mild_dist)

krustest13<-kruskal.test(AGBCH ~ type, data = all_mild_dist)

dunntest13a<-as.data.frame(dunn.test(all_mild_dist$AGB1,all_mild_dist$type,method="bonferroni",alpha=0.05))
dunntest13b<-as.data.frame(dunn.test(all_mild_dist$AGB2,all_mild_dist$type,method="bonferroni",alpha=0.05))
dunntest13c<-as.data.frame(dunn.test(all_mild_dist$Seed1,all_mild_dist$type,method="bonferroni",alpha=0.05))
dunntest13d<-as.data.frame(dunn.test(all_mild_dist$Seed2,all_mild_dist$type,method="bonferroni",alpha=0.05))

dunntest13<-as.data.frame(dunn.test(all_mild_dist$AGBCH,all_mild_dist$type,method="bonferroni",alpha=0.05))



ggline(all_mild_dist, x = "type", y = "seedch", 
       add = c("mean_se", "jitter"), 
       order = c("MF", "MC", "MI"),
       ylab = "seedch", xlab = "disturbances")

mean_type_mildere_seedch<-aggregate(seedch~type,all_mild_dist,FUN=mean)


test_mildere_west_seedch<-pairwise.wilcox.test(all_mild_dist$seedch, all_mild_dist$type, p.adjust.method = "BH")

print(test_mildere_west_seedch)

krustest14<-kruskal.test(seedch ~ type, data = all_mild_dist)

dunntest14<-as.data.frame(dunn.test(all_mild_dist$seedch,all_mild_dist$type,method="bonferroni",alpha=0.05))





rm(list = setdiff(ls(), c('krustest11',
                          'krustest11a','krustest11b','krustest11c','krustest11d',
                          'dunntest11','dunntest11a','dunntest11b','dunntest11c','dunntest11d',
                          'krustest12','dunntest12',
                           'krustest13','krustest13a','krustest13b','krustest13c','krustest13d',
                          'dunntest13','dunntest13a','dunntest13b','dunntest13c','dunntest13d',
                          'krustest14','dunntest14','sdam2','ecosel','tukeywest')))


sdam3<-sdam2[(!sdam2$ecocode %in% ecosel$econew),]

sdam3$AGB_CHP<-sdam3$AGB.22-sdam3$AGB.11
pdam16<-sdam3

## plots with no disturbance

trall_1a_ds<-pdam16[which(pdam16$dist_type=="FIRE" & pdam16$fia_sev1==0),]
trall_1b_ds<-pdam16[which(pdam16$dist_type=="FIRE" & pdam16$fia_sev1==1),]


aaa2a<-data.frame(trall_1a_ds$AGB_CHP)
aaa2s<-data.frame(trall_1a_ds$seed_ct_ch)
aaa2a1<-data.frame(trall_1a_ds$AGB.11)
aaa2a2<-data.frame(trall_1a_ds$AGB.22)
aaa2a3<-data.frame(trall_1a_ds$seed_count.1)
aaa2a4<-data.frame(trall_1a_ds$seed_count.1)


aaa2<-cbind(aaa2a1,aaa2a2,aaa2a3,aaa2a3,aaa2a,aaa2s)

aaa2$type<-"MF"
colnames(aaa2)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")


aaa3a<-data.frame(trall_1b_ds$AGB_CHP)
aaa3s<-data.frame(trall_1b_ds$seed_ct_ch)
aaa3a1<-data.frame(trall_1b_ds$AGB.11)
aaa3a2<-data.frame(trall_1b_ds$AGB.22)
aaa3a3<-data.frame(trall_1b_ds$seed_count.1)
aaa3a4<-data.frame(trall_1b_ds$seed_count.1)

aaa3<-cbind(aaa3a1,aaa3a2,aaa3a3,aaa3a3,aaa3a,aaa3s)


aaa3$type<-"SF"

colnames(aaa3)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")



fire_dist<-rbind(aaa2,aaa3)

trall_1a_dsc<-pdam16[which(pdam16$dist_type=="CUT" & pdam16$fia_sev1==0),]
trall_1b_dsc<-pdam16[which(pdam16$dist_type=="CUT" & pdam16$fia_sev1==1),]


caaa2a<-data.frame(trall_1a_dsc$AGB_CHP)
caaa2b<-data.frame(trall_1a_dsc$seed_ct_ch)
caaa2a1<-data.frame(trall_1a_dsc$AGB.11)
caaa2a2<-data.frame(trall_1a_dsc$AGB.22)
caaa2a3<-data.frame(trall_1a_dsc$seed_count.1)
caaa2a4<-data.frame(trall_1a_dsc$seed_count.2)


caaa2<-cbind(caaa2a1,caaa2a2,caaa2a3,caaa2a4,caaa2a,caaa2b)

caaa2$type<-"MC"
colnames(caaa2)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")

caaa3a<-data.frame(trall_1b_dsc$AGB_CHP)
caaa3b<-data.frame(trall_1b_dsc$seed_ct_ch)
caaa3a1<-data.frame(trall_1b_dsc$AGB.11)
caaa3a2<-data.frame(trall_1b_dsc$AGB.22)
caaa3a3<-data.frame(trall_1b_dsc$seed_count.1)
caaa3a4<-data.frame(trall_1b_dsc$seed_count.2)


caaa3<-cbind(caaa3a1,caaa3a2,caaa3a3,caaa3a4,caaa3a,caaa3b)
caaa3$type<-"SC"

colnames(caaa3)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")



trall_1a_dsi<-pdam16[which(pdam16$dist_type=="INSDIS" & pdam16$fia_sev1==0),]
trall_1b_dsi<-pdam16[which(pdam16$dist_type=="INSDIS" & pdam16$fia_sev1==1),]



iaaa2a<-data.frame(trall_1a_dsi$AGB_CHP)
iaaa2b<-data.frame(trall_1a_dsi$seed_ct_ch)
iaaa2a1<-data.frame(trall_1a_dsi$AGB.11)
iaaa2a2<-data.frame(trall_1a_dsi$AGB.22)
iaaa2a3<-data.frame(trall_1a_dsi$seed_count.1)
iaaa2a4<-data.frame(trall_1a_dsi$seed_count.2)


iaaa2<-cbind(iaaa2a1,iaaa2a2,iaaa2a3,iaaa2a4,iaaa2a,iaaa2b)
iaaa2$type<-"MI"
colnames(iaaa2)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")

iaaa3a<-data.frame(trall_1b_dsi$AGB_CHP)
iaaa3b<-data.frame(trall_1b_dsi$seed_ct_ch)
iaaa3a1<-data.frame(trall_1b_dsi$AGB.11)
iaaa3a2<-data.frame(trall_1b_dsi$AGB.22)
iaaa3a3<-data.frame(trall_1b_dsi$seed_count.1)
iaaa3a4<-data.frame(trall_1b_dsi$seed_count.2)


iaaa3<-cbind(iaaa3a1,iaaa3a2,iaaa3a3,iaaa3a4,iaaa3a,iaaa3b)

iaaa3$type<-"SI"

colnames(iaaa3)<-c("AGB1","AGB2","Seed1","Seed2","AGBCH","seedch","type")


all_sev_dist<-rbind(aaa3,caaa3,iaaa3)
all_mild_dist<-rbind(aaa2,caaa2,iaaa2)


# med_west_AGBCH<-median(all_sev_dist$AGBCH,by="type")

# 
# library("ggpubr")
ggline(all_sev_dist, x = "type", y = "AGBCH", 
       add = c("mean_se", "jitter"), 
       order = c("SF", "SC", "SI"),
       ylab = "AGBCH", xlab = "disturbances")


model1a <- aov(AGB1 ~ type, data = all_sev_dist)
summary(model1a)

tukey1a<-TukeyHSD(model1a, conf.level=.95)
tukey1a

model1b <- aov(AGB2 ~ type, data = all_sev_dist)
summary(model1b)

tukey1b<-TukeyHSD(model1b, conf.level=.95)
tukey1b

model1c <- aov(Seed1 ~ type, data = all_sev_dist)
summary(model1c)

tukey1c<-TukeyHSD(model1c, conf.level=.95)
tukey1c

model1d <- aov(Seed2 ~ type, data = all_sev_dist)
summary(model1d)

tukey1d<-TukeyHSD(model1d, conf.level=.95)
tukey1d

krustest1a<-kruskal.test(AGB1 ~ type, data = all_sev_dist)
krustest1b<-kruskal.test(AGB2 ~ type, data = all_sev_dist)
krustest1c<-kruskal.test(Seed1 ~ type, data = all_sev_dist)
krustest1d<-kruskal.test(Seed2 ~ type, data = all_sev_dist)

krustest1<-kruskal.test(AGBCH ~ type, data = all_sev_dist)





# library(dunn.test)
dunntest1a<-as.data.frame(dunn.test(all_sev_dist$AGB1,all_sev_dist$type,method="bonferroni",alpha=0.05))
dunntest1b<-as.data.frame(dunn.test(all_sev_dist$AGB2,all_sev_dist$type,method="bonferroni",alpha=0.05))
dunntest1c<-as.data.frame(dunn.test(all_sev_dist$Seed1,all_sev_dist$type,method="bonferroni",alpha=0.05))
dunntest1d<-as.data.frame(dunn.test(all_sev_dist$Seed2,all_sev_dist$type,method="bonferroni",alpha=0.05))

dunntest1<-as.data.frame(dunn.test(all_sev_dist$AGBCH,all_sev_dist$type,method="bonferroni",alpha=0.05))


# kruskal_test_AGBCH<-pairwise.kruskal.test(AGBCH ~ type, data = all_sev_dist)
# print(kruskal_test_AGBCH)


ggline(all_sev_dist, x = "type", y = "seedch", 
       add = c("mean_se", "jitter"), 
       order = c("SF", "SC", "SI"),
       ylab = "seedch", xlab = "disturbances")

mean_type_severe_seedch<-aggregate(seedch~type,all_sev_dist,FUN=mean)


krustest2<-kruskal.test(seedch ~ type, data = all_sev_dist)

dunntest2<-as.data.frame(dunn.test(all_sev_dist$seedch,all_sev_dist$type,method="bonferroni",alpha=0.05))


### normality test
shapiro.test(all_sev_dist$AGBCH)

hist(all_sev_dist$AGBCH)

ggline(all_mild_dist, x = "type", y = "AGBCH", 
       add = c("mean_se", "jitter"), 
       order = c("MF", "MC", "MI"),
       ylab = "AGBCH", xlab = "disturbances")

mean_type_mildere_AGBCH<-aggregate(AGBCH~type,all_mild_dist,FUN=mean)


test_mildere_west_AGBCH<-pairwise.wilcox.test(all_mild_dist$AGBCH, all_mild_dist$type, p.adjust.method = "BH")

print(test_mildere_west_AGBCH)

model3a <- aov(AGB1 ~ type, data = all_mild_dist)
summary(model3a)

tukey3a<-TukeyHSD(model3a, conf.level=.95)
tukey3a

model3b <- aov(AGB2 ~ type, data = all_mild_dist)
summary(model3b)

tukey3b<-TukeyHSD(model3b, conf.level=.95)
tukey3b

model3c <- aov(Seed1 ~ type, data = all_mild_dist)
summary(model3c)

tukey3c<-TukeyHSD(model3c, conf.level=.95)
tukey3c

model3d <- aov(Seed2 ~ type, data = all_mild_dist)
summary(model3d)

tukey3d<-TukeyHSD(model3d, conf.level=.95)
tukey3d


tukeyeast<-list(rbind(tukey1a$type,tukey1b$type,tukey1c$type,tukey1d$type,
                      tukey3a$type,tukey3b$type,tukey3c$type,tukey3d$type))

krustest3a<-kruskal.test(AGB1 ~ type, data = all_mild_dist)
krustest3b<-kruskal.test(AGB2 ~ type, data = all_mild_dist)
krustest3c<-kruskal.test(Seed1 ~ type, data = all_mild_dist)
krustest3d<-kruskal.test(Seed2 ~ type, data = all_mild_dist)

krustest3<-kruskal.test(AGBCH ~ type, data = all_mild_dist)

dunntest3a<-as.data.frame(dunn.test(all_mild_dist$AGB1,all_mild_dist$type,method="bonferroni",alpha=0.05))
dunntest3b<-as.data.frame(dunn.test(all_mild_dist$AGB2,all_mild_dist$type,method="bonferroni",alpha=0.05))
dunntest3c<-as.data.frame(dunn.test(all_mild_dist$Seed1,all_mild_dist$type,method="bonferroni",alpha=0.05))
dunntest3d<-as.data.frame(dunn.test(all_mild_dist$Seed2,all_mild_dist$type,method="bonferroni",alpha=0.05))
dunntest3<-as.data.frame(dunn.test(all_mild_dist$AGBCH,all_mild_dist$type,method="bonferroni",alpha=0.05))



ggline(all_mild_dist, x = "type", y = "seedch", 
       add = c("mean_se", "jitter"), 
       order = c("MF", "MC", "MI"),
       ylab = "seedch", xlab = "disturbances")

mean_type_mildere_seedch<-aggregate(seedch~type,all_mild_dist,FUN=mean)


test_mildere_west_seedch<-pairwise.wilcox.test(all_mild_dist$seedch, all_mild_dist$type, p.adjust.method = "BH")

print(test_mildere_west_seedch)

krustest4<-kruskal.test(seedch ~ type, data = all_mild_dist)

dunntest4<-as.data.frame(dunn.test(all_mild_dist$seedch,all_mild_dist$type,method="bonferroni",alpha=0.05))


kurseveast<-rbind(krustest1a,krustest1b,krustest1c,krustest1d,krustest1,krustest2)
dunnseveast<-rbind(dunntest1a,dunntest1b,dunntest1c,dunntest1d,dunntest1,dunntest2)

kurmildeast<-rbind(krustest3a,krustest3b,krustest3c,krustest3d,krustest3,krustest4)
dunnmildeast<-rbind(dunntest3a,dunntest3b,dunntest3c,dunntest3d,dunntest3,dunntest4)

kursevwest<-rbind(krustest11a,krustest11b,krustest11c,krustest11d,krustest11,krustest12)
dunnsevwest<-rbind(dunntest11a,dunntest11b,dunntest11c,dunntest11d,dunntest11,dunntest12)

kurmildwest<-rbind(krustest13a,krustest13b,krustest13c,krustest13d,krustest13,krustest14)
dunnmildwest<-rbind(dunntest13a,dunntest13b,dunntest13c,dunntest13d,dunntest13,dunntest14)


write.csv(kurseveast,"kurseveast_828.csv")
write.csv(dunnseveast,"dunnseveast_828.csv")
write.csv(kursevwest,"kursevwest_828.csv")
write.csv(dunnsevwest,"dunnsevwest_828.csv")
write.csv(kurmildeast,"kurmildeast_828.csv")
write.csv(dunnmildeast,"dunnmildeast_828.csv")
write.csv(kurmildwest,"kurmildwest_828.csv")
write.csv(dunnmildwest,"dunnmildwest_828.csv")


tukey_all<-cbind(tukeywest,tukeyeast)

write.csv(tukeyeast,"tukey_tests_east_828.csv")
write.csv(tukeywest,"tukey_tests_west_828.csv")



