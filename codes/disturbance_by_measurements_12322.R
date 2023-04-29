# read all data with disturbances and produce 
# temporal data with disturbances to observe
# pattern of disturbances through measurements
# ------------- Karun Pandit, 2022 ------------

rm(list=ls())

library(tidyr)
library(plyr)
library(dplyr)
library(reshape2)
library(ggplot2)
library(gridExtra)
library(tidyverse)
memory.limit(size=1000000000)

# 
all_plots<-readRDS("../data/dist_data_all_cond.RDS")
plots_with_tree<-read.csv("../../data/plots_with_tree.csv")

all_plots_a<-all_plots[(all_plots$NUNIDS.x %in% plots_with_tree$pplotid),]

all_plots1<-all_plots_a[which(all_plots_a$DESIGNCD==1),]
all_plots1<-all_plots1[which(all_plots1$KINDCD==1|all_plots1$KINDCD==2),]

all_plots1$rep_std<-ifelse(all_plots1$STDAGE<=all_plots1$REMPER,1,0)

ecosel<-read.csv("../disturbance/eco_select.csv")


library(operators)

all_plots2<-separate(all_plots1, ECOSUBCD, into = c("Spl_1", "Spl_2"), sep = 4, remove = FALSE)

library(tidyverse)

all_plots3<-all_plots2[(all_plots2$Spl_1 %in% ecosel$econew),]

all_plots3$damcod<-interaction(all_plots3$cd1,all_plots3$tcd1)


all_plots3$distrt<-ifelse(all_plots3$damcod=="F.C"|all_plots3$damcod=="I.C"|all_plots3$damcod=="O.C"
                      |all_plots3$damcod=="F.OT" |all_plots3$damcod=="I.OT"
                      |all_plots3$damcod=="O.OT","M",
                      ifelse(all_plots3$damcod=="ND.C","C",
                             ifelse(all_plots3$damcod=="F.NT","F",
                                    ifelse(all_plots3$damcod=="I.NT","I",
                                           ifelse(all_plots3$damcod=="ND.NT","ND",
                                                  ifelse(all_plots3$damcod=="O.NT","O",
                                                         ifelse(all_plots3$damcod=="ND.OT","OT","other"
                                                         )))))))

 # remove.packages("tidyverse")

require(tidyverse)

ttt<-data.frame(table(all_plots3$rep_std,all_plots3$distrt))

all_plots33<-all_plots3[is.na(all_plots3$rep_std),]

ttt_only_repstd_na<-data.frame(table(all_plots33$distrt))

ttt_only_rep_std_na<-count(all_plots33$distrt)

firstall<-all_plots3 %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 


firstall1<-firstall %>% group_by(NUNIDS.x) %>%  slice(1) 

mtbs<-read.csv("../../MTBS_fire_FIa_plots.csv") 


unique<-count(mtbs,nplotid)

trees_fire11<-firstall1[firstall1$NUNIDS.x %in% mtbs$nplotid,]

# trees_fire_33<-trees_fire_22
# 
# #data with first measurements less than fire year
# trees_fire_33<-trees_fire_22[which(trees_fire_22$MEASYEAR.1<trees_fire_22$MTBS_fireyear),]
# 
# #data with first measurements less and second measurements greater than fire year
# trees_fire_44<-trees_fire_33[which(trees_fire_33$MEASYEAR.2>trees_fire_33$MTBS_fireyear),]
# 
# tablepp<-count(trees_fire_44,NUNIDS.2)
# 
# #data with first &second measureemtns less and third measurements greater than fire year
# trees_fire_55<-trees_fire_33[which(trees_fire_33$MEASYEAR.2<trees_fire_33$MTBS_fireyear
#                                    & trees_fire_33$MEASYEAR>trees_fire_33$MTBS_fireyear),]


one_all<-firstall1 %>% group_by(distrt,rep_std) %>% summarise(n = n())

onon<-count(firstall1,NUNID)


one_sev<-(firstall1[which(firstall1$rep_std==1),])
tone_sev<-one_sev %>% group_by(distrt) %>% summarise(n = n())

one_mild<-firstall1[which(firstall1$rep_std==0),]
tone_mild<-one_mild %>% group_by(distrt) %>% summarise(n = n())

one_na<-firstall1[is.na(firstall1$rep_std),]
tone_na<-one_na %>% group_by(distrt) %>% summarise(n = n())


one_seva<-one_sev[c("NUNIDS.x","NUNID","distrt")]
two_seva<-two_sev[c("NUNIDS.x","NUNID","distrt")]
three_seva<-three_sev[c("NUNIDS.x","NUNID","distrt")]
four_seva<-four_sev[c("NUNIDS.x","NUNID","distrt")]
five_seva<-five_sev[c("NUNIDS.x","NUNID","distrt")]


one_two_sev<-merge(one_seva,two_seva,by="NUNIDS.x",all=TRUE)
one_two_three_sev<-merge(one_two_sev,three_seva,by="NUNIDS.x",all=TRUE)

colnames(one_two_three_sev)<-c("NUNIDS.x","NUNID.a","distrt.a","NUNID.b",
                               "distrt.b","NUNID.c","distrt.c")


colnames(four_seva)<-c("NUNIDS.x","NUNID.d","distrt.d")

one_two_three_four_sev<-merge(one_two_three_sev,four_seva,by="NUNIDS.x",all=TRUE)


colnames(five_seva)<-c("NUNIDS.x","NUNID.e","distrt.e")

one_to_five_sev<-merge(one_two_three_four_sev,five_seva,by="NUNIDS.x",all=TRUE)




uuu<-count(one_to_five_sev,distrt.a,distrt.b,distrt.c,distrt.d,distrt.e)

uuu<-count(one_to_five_sev,distrt.b,distrt.c,distrt.d)

uuu1<-count(one_two_three_sev,distrt.b,distrt.c)


uuu$all_dist<-paste0(uuu$distrt.a,uuu$distrt.b,uuu$distrt.c,uuu$distrt.d,uuu$distrt.e)
uuu$all_dist<-paste0(uuu$distrt.b,",",uuu$distrt.c,",",uuu$distrt.d)

uuu1$all_dist<-paste0(uuu1$distrt.c,",",uuu1$distrt.d)

one_to_five_sev$all_dist<-paste0(one_to_five_sev$distrt.b,",",one_to_five_sev$distrt.c,",",one_to_five_sev$distrt.d)

one_two_three_sev$all_dist<-paste0(one_two_three_sev$distrt.b,",",one_two_three_sev$distrt.c)


sss<-count(one_to_five_sev,all_dist)

sss1<-count(one_two_three_sev,all_dist)

vvv<-sss[which(sss$n>10),]
vvv1<-sss1[which(sss1$n>5),]

ggplot()+
  geom_bar(vvv,aes(x=all_dist,y=stat(n)))

vvv<-vvv[which(vvv$all_dist!="NA,NA,NA"),]



library(ggplot2)
ggplot() + 
  geom_point(data=vvv1,aes(x=all_dist,y=n),alpha=0.5)+
 
  
  ylab("number of plots")+
  
  xlab("disturbance during second, third and fourth measurements")+
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

ggplot(one_to_five_sev, aes(x = factor(all_dist))) +
  geom_bar()


hist(table(vvv), freq=TRUE, xlab = levels(vvv), ylab = "Frequencies")

barplot(prop.table(table(one_to_five_sev)))

ggplot(data.frame(one_to_five_sev), aes(x=all_dist)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

plot(sss$all_dist,sss$n)

hist(vvv$all_dist, sort.group = "decreasing", cum.percent = TRUE)

ppp_allb<-merge(tone_sev,tone_mild,by="distrt",all=TRUE)
ppp_allc<-merge(ppp_allb,tone_na,by="distrt",all=TRUE)

colnames(ppp_allc)<-c("disturbance","severe","mild","unknown")


nofirstall<-all_plots3 %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR)) 
  
secondall<-nofirstall %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 
secondall1<-secondall %>% group_by(NUNIDS.x) %>% 
  slice(1) 

two_all<-secondall1 %>% group_by(distrt,rep_std) %>% summarise(n = n())


two_sev<-(secondall1[which(secondall1$rep_std==1),])
ttwo_sev<-two_sev %>% group_by(distrt) %>% summarise(n = n())

two_mild<-secondall1[which(secondall1$rep_std==0),]
ttwo_mild<-two_mild %>% group_by(distrt) %>% summarise(n = n())

two_na<-secondall1[is.na(secondall1$rep_std),]
ttwo_na<-two_na %>% group_by(distrt) %>% summarise(n = n())


qqq_allb<-merge(ttwo_sev,ttwo_mild,by="distrt",all=TRUE)
qqq_allc<-merge(qqq_allb,ttwo_na,by="distrt",all=TRUE)

colnames(qqq_allc)<-c("disturbance","severe","mild","unknown")

nosecondall<-nofirstall %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

thirdall<-nosecondall %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))
thirdall1<-thirdall %>% group_by(NUNIDS.x) %>% 
  slice(1)


three_all<-thirdall1 %>% group_by(distrt,rep_std) %>% summarise(n = n())


three_sev<-(thirdall1[which(thirdall1$rep_std==1),])
tthree_sev<-three_sev %>% group_by(distrt) %>% summarise(n = n())

three_mild<-thirdall1[which(thirdall1$rep_std==0),]
tthree_mild<-three_mild %>% group_by(distrt) %>% summarise(n = n())

three_na<-thirdall1[is.na(thirdall1$rep_std),]
tthree_na<-three_na %>% group_by(distrt) %>% summarise(n = n())


rrr_allb<-merge(tthree_sev,tthree_mild,by="distrt",all=TRUE)
rrr_allc<-merge(rrr_allb,tthree_na,by="distrt",all=TRUE)

colnames(rrr_allc)<-c("disturbance","severe","mild","unknown")


nothirdall<-nosecondall %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

fourthall<-nothirdall %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

fourthall1<-fourthall %>% group_by(NUNIDS.x) %>% 
  slice(1)

four_all<-fourthall1 %>% group_by(distrt,rep_std) %>% summarise(n = n())


four_sev<-(fourthall1[which(fourthall1$rep_std==1),])
tfour_sev<-four_sev %>% group_by(distrt) %>% summarise(n = n())

four_mild<-fourthall1[which(fourthall1$rep_std==0),]
tfour_mild<-four_mild %>% group_by(distrt) %>% summarise(n = n())

four_na<-fourthall1[is.na(fourthall1$rep_std),]
tfour_na<-four_na %>% group_by(distrt) %>% summarise(n = n())


sss_allb<-merge(tfour_sev,tfour_mild,by="distrt",all=TRUE)
sss_allc<-merge(sss_allb,tfour_na,by="distrt",all=TRUE)

colnames(sss_allc)<-c("disturbance","severe","mild","unknown")

nofourthall<-nothirdall %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

fifthall<-nofourthall %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))
fifthall1<-fifthall %>% group_by(NUNIDS.x) %>% 
  slice(1)

five_all<-fifthall1 %>% group_by(distrt,rep_std) %>% summarise(n = n())

five_sev<-(fifthall1[which(fifthall1$rep_std==1),])
tfive_sev<-five_sev %>% group_by(distrt) %>% summarise(n = n())

five_mild<-fifthall1[which(fifthall1$rep_std==0),]
tfive_mild<-five_mild %>% group_by(distrt) %>% summarise(n = n())

five_na<-fifthall1[is.na(fifthall1$rep_std),]
tfive_na<-five_na %>% group_by(distrt) %>% summarise(n = n())


ttt_allb<-merge(tfive_sev,tfive_mild,by="distrt",all=TRUE)
ttt_allc<-merge(ttt_allb,tfive_na,by="distrt",all=TRUE)

colnames(ttt_allc)<-c("disturbance","severe","mild","unknown")



nofifthall<-nofourthall %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))




all_list1<-cbind(ppp_allc, qqq_allc,rrr_allc,sss_allc,ttt_allc)


write.csv(all_list1,"east_all_dist.csv")




one_all$time<-'first'
two_all$time<-'second'
three_all$time<-'third'
four_all$time<-'fourth'
five_all$time<-'fifth'



all_list_v1<-rbind(one_all, two_all,three_all,four_all,five_all)

# all_list_v2<-all_list_v1

all_list_v2<-all_list_v1[which(all_list_v1$distrt!="ND"),]

all_list_v2$time<-factor(all_list_v2$time, levels = c("fifth","fourth","third", "second", "first"))

all_list_v2$distrt

all_list_v2$rep_std[is.na(all_list_v2$rep_std)] <- 2

all_list_v2b<-all_list_v2[which(all_list_v2$rep_std<2),]

p1<-ggplot(data = all_list_v2b, aes(x=distrt, y=n, fill = time)) + 
  geom_bar(stat='identity') + 
  scale_fill_discrete(name="inventory")+
  ggtitle("East") +
  
  scale_x_discrete(name="",labels=c("C"="Harvest","F"="Fire","I"="Insect/Disease",
                                               "M"="Mixed disturbances & treatment","O"="Other disturbances",
                                               "OT"="Other treatments"))+

  facet_wrap(~rep_std,labeller=labeller(rep_std=c("0"="Mild","1"="Severe","2"="Unknown"))) +
   scale_y_continuous(name = "number of plots") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,hjust=1))
 # theme(panel.background = element_rect(fill = "white"))


rm(list = setdiff(ls(), c('all_list1','p1','all_plots2','ecosel')))



all_plots3<-all_plots2[(all_plots2$Spl_1 %in% ecosel$econew),]
all_plots3$damcod<-interaction(all_plots3$cd1,all_plots3$tcd1)


all_plots3$distrt<-ifelse(all_plots3$damcod=="F.C"|all_plots3$damcod=="I.C"|all_plots3$damcod=="O.C"
                          |all_plots3$damcod=="F.OT" |all_plots3$damcod=="I.OT"
                          |all_plots3$damcod=="O.OT","M",
                          ifelse(all_plots3$damcod=="ND.C","C",
                                 ifelse(all_plots3$damcod=="F.NT","F",
                                        ifelse(all_plots3$damcod=="I.NT","I",
                                               ifelse(all_plots3$damcod=="ND.NT","ND",
                                                      ifelse(all_plots3$damcod=="O.NT","O",
                                                             ifelse(all_plots3$damcod=="ND.OT","OT","other"
                                                             )))))))



firstall<-all_plots3 %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 

firstall1<-firstall %>% group_by(NUNIDS.x) %>% 
  slice(1) 

one_all<-firstall1 %>% group_by(distrt,rep_std) %>% summarise(n = n())


one_sev<-(firstall1[which(firstall1$rep_std==1),])
tone_sev<-one_sev %>% group_by(distrt) %>% summarise(n = n())

one_mild<-firstall1[which(firstall1$rep_std==0),]
tone_mild<-one_mild %>% group_by(distrt) %>% summarise(n = n())

one_na<-firstall1[is.na(firstall1$rep_std),]
tone_na<-one_na %>% group_by(distrt) %>% summarise(n = n())



ppp_allb<-merge(tone_sev,tone_mild,by="distrt",all=TRUE)
ppp_allc<-merge(ppp_allb,tone_na,by="distrt",all=TRUE)

colnames(ppp_allc)<-c("disturbance","severe","mild","unknown")





nofirstall<-all_plots3 %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR)) 

secondall<-nofirstall %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 
secondall1<-secondall %>% group_by(NUNIDS.x) %>% 
  slice(1) 



two_all<-secondall1 %>% group_by(distrt,rep_std) %>% summarise(n = n())


two_sev<-(secondall1[which(secondall1$rep_std==1),])
ttwo_sev<-two_sev %>% group_by(distrt) %>% summarise(n = n())

two_mild<-secondall1[which(secondall1$rep_std==0),]
ttwo_mild<-two_mild %>% group_by(distrt) %>% summarise(n = n())

two_na<-secondall1[is.na(secondall1$rep_std),]
ttwo_na<-two_na %>% group_by(distrt) %>% summarise(n = n())


qqq_allb<-merge(ttwo_sev,ttwo_mild,by="distrt",all=TRUE)
qqq_allc<-merge(qqq_allb,ttwo_na,by="distrt",all=TRUE)

colnames(qqq_allc)<-c("disturbance","severe","mild","unknown")





nosecondall<-nofirstall %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

thirdall<-nosecondall %>% group_by(NUNIDS.x) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))
thirdall1<-thirdall %>% group_by(NUNIDS.x) %>% 
  slice(1)


three_all<-thirdall1 %>% group_by(distrt,rep_std) %>% summarise(n = n())


three_sev<-(thirdall1[which(thirdall1$rep_std==1),])
tthree_sev<-three_sev %>% group_by(distrt) %>% summarise(n = n())

three_mild<-thirdall1[which(thirdall1$rep_std==0),]
tthree_mild<-three_mild %>% group_by(distrt) %>% summarise(n = n())

three_na<-thirdall1[is.na(thirdall1$rep_std),]
tthree_na<-three_na %>% group_by(distrt) %>% summarise(n = n())


rrr_allb<-merge(tthree_sev,tthree_mild,by="distrt",all=TRUE)
rrr_allc<-merge(rrr_allb,tthree_na,by="distrt",all=TRUE)

colnames(rrr_allc)<-c("disturbance","severe","mild","unknown")



all_list1b<-merge(ppp_allc,qqq_allc,by="disturbance",all=TRUE)
all_list11<-merge(all_list1b,rrr_allc,by="disturbance",all=TRUE)


write.csv(all_list11,"west_all_dist.csv")



one_all$time<-'first'
two_all$time<-'second'
three_all$time<-'third'



all_list_v1<-rbind(one_all, two_all,three_all)

# all_list_v2<-all_list_v1

all_list_v2<-all_list_v1[which(all_list_v1$distrt!="ND"),]

all_list_v2$time<-factor(all_list_v2$time, levels = c("third", "second", "first"))

all_list_v2$distrt

all_list_v2$rep_std[is.na(all_list_v2$rep_std)] <- 2

all_list_v2b<-all_list_v2[which(all_list_v2$rep_std<2),]


p2<-ggplot(data = all_list_v2b, aes(x=distrt, y=n, fill = time)) + 
  geom_bar(stat='identity') + 
  scale_fill_discrete(name="inventory")+
  ggtitle("West") +
  
  scale_x_discrete(name="",labels=c("C"="Harvest","F"="Fire","I"="Insect/Disease",
                                    "M"="Mixed disturbances & treatment","O"="Other disturbances",
                                    "OT"="Other treatments"))+
  
  facet_wrap(~rep_std,labeller=labeller(rep_std=c("0"="Mild","1"="Severe","2"="Unknown"))) +
  scale_y_continuous(name = "number of plots") +
  theme_bw()+
  theme(axis.text.x = element_text(angle = 90,hjust=1))
# theme(panel.background = element_rect(fill = "white"))


library(gridExtra)
current_date<-Sys.Date()

png(paste0("disturbance_all_time_",current_date,".jpeg"), width = 1000, height = 1200,res=100)
par(xpd = F, mar = c(10,5,3,7))
par(oma=c(10,5,0,0))

par(mfrow=c(2,1))

grid.arrange(p1,
             p2,
             heights=c(1.1, 1.1),
             nrow = 2)
dev.off()


