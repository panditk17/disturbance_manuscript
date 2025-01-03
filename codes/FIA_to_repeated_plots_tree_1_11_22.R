# read raw plot, condition and tree tables from FIA data
# and prepare tables showing time-series data
# depending upon the avaiability of measurements each plot has
# -------------- Karun Pandit, 2022 --------------------------

rm(list=ls())
memory.limit(size=1000000000)

# remove.packages("tidyverse")
# install.packages("tidyverse")
library(tidyverse)

plots<-read.csv("../../data/PLOT.CSV")
# plots_all<-plots

plots_all<-plots %>% select(CN,PREV_PLT_CN,INVYR,STATECD,UNITCD,COUNTYCD,
                            PLOT,PLOT_STATUS_CD,MEASYEAR,MEASMON, MEASDAY,
                            REMPER,LAT,LON,ELEV,
                            ECOSUBCD,KINDCD,DESIGNCD)


jjj<-count(plots_all,STATECD)
rm(plots)

plots_all$pplotid<-paste0(plots_all$STATECD,"-",plots_all$UNITCD,"-",
                          plots_all$COUNTYCD,"-",plots_all$PLOT)
plots_all$pplotidyr<-paste0(plots_all$STATECD,"-",plots_all$UNITCD,"-",
                            plots_all$COUNTYCD,"-",plots_all$PLOT,"-",plots_all$INVYR)

jjj<-count(plots_all,CN)
kkk<-count(plots_all,pplotidyr)
lll<-count(plots_all,pplotid)

library(tidyr)
library(dplyr)

conds<-read.csv("../../data/COND.csv")



cond_all<-conds %>% select(PLT_CN,INVYR,STATECD,UNITCD,COUNTYCD,PLOT,CONDID,
                           COND_STATUS_CD,
                          FORTYPCD,STDAGE,FLDAGE,FLDSZCD,SITECLCD,SICOND,
                          STDORGCD,STDSZCD,SLOPE,
                          ASPECT,DSTRBCD1,DSTRBYR1,DSTRBCD2,DSTRBYR2,DSTRBCD3,
                          DSTRBYR3,TRTCD1,TRTYR1,TRTCD2,TRTYR2,TRTCD3,TRTYR3,
                          BALIVE,CONDPROP_UNADJ,MICRPROP_UNADJ)

hhh<-count(cond_all,STATECD)
rm(conds)
cond_all$cplotid<-paste0(cond_all$STATECD,"-",cond_all$UNITCD,"-",
                         cond_all$COUNTYCD,"-",cond_all$PLOT)
cond_all$cplotidyr<-paste0(cond_all$STATECD,"-",cond_all$UNITCD,"-",
                           cond_all$COUNTYCD,"-",cond_all$PLOT,"-",cond_all$INVYR)

mmm<-count(cond_all,cplotid)
filt_cond1<-cond_all[which(cond_all$COND_STATUS_CD==1),]
nnn<-count(filt_cond1,cplotid)

iii<-count(filt_cond1,STATECD)

filt_cond2<-filt_cond1[which(filt_cond1$CONDPROP_UNADJ>=0.9),]
filt_cond3<-filt_cond2[which(filt_cond2$MICRPROP_UNADJ>=0.9),]

ppp<-count(filt_cond3,STATECD)
ppp<-count(filt_cond3,cplotid)


# library(tidyverse)
# filt_cond4<-filt_cond1 %>% group_by(cplotidyr) %>% 
#   filter(abs(CONDPROP_UNADJ) == max(CONDPROP_UNADJ)) 


rm(cond_all)

tree_all<-readRDS("../../data/subset_tree_all.RDS") # subset of tree table with
                                                  # selected columns  
kkk<-count(tree_all,STATECD)

tree_all$tplotid<-paste0(tree_all$STATECD,"-",tree_all$UNITCD,"-",
                         tree_all$COUNTYCD,"-",tree_all$PLOT)
tree_all$tplotidyr<-paste0(tree_all$STATECD,"-",tree_all$UNITCD,"-",
                          tree_all$COUNTYCD,"-",tree_all$PLOT,"-",tree_all$INVYR)

tree_cond<-merge(tree_all,filt_cond3,by.x="tplotidyr",by.y="cplotidyr",
                 suffixes=c(".TR",".CND"),all.y=TRUE)

lll<-count(tree_cond,STATECD.TR)
rm(tree_all)
rm(filt_cond3)

tree_cond_plot<-merge(tree_cond,plots_all,by.x="tplotidyr",by.y="pplotidyr",
                      suffixes=c(".tc",".pl"),all.x=TRUE)
rm(tree_cond)
rm(plots_all)
#save file
saveRDS(tree_cond_plot,"../../data/tree_cond_plot_all_true.RDS")
# tree_cond_plot<-readRDS("../../data/tree_cond_plot_all_true.RDS")

mmm<-count(tree_cond_plot,STATECD)

alltreex<-count(tree_cond_plot,tplotid)
alltreexyr<-count(tree_cond_plot,tplotidyr)
all_plot_n<-count(alltreexyr,tplotidyr)

tree_cond_plot$pplotid<-paste0(tree_cond_plot$STATECD,"-",tree_cond_plot$UNITCD,"-",
                               tree_cond_plot$COUNTYCD,"-",tree_cond_plot$PLOT)
tree_cond_plot$pplotidyr<-paste0(tree_cond_plot$STATECD,"-",tree_cond_plot$UNITCD,"-",
                                     tree_cond_plot$COUNTYCD,"-",tree_cond_plot$PLOT,"-",tree_cond_plot$INVYR)


jkjk<-count(tree_cond_plot,pplotid,tplotid)

plots_with_no_tree<-jkjk[is.na(jkjk$tplotid),]


sr<-tree_cond_plot
rm(tree_cond_plot)

# sr2<-sr[which(sr$DESIGNCD==1),]
# sss<-count(sr,STATECD,DESIGNCD)

# sr3<-sr[which(sr$KINDCD==1|sr$KINDCD==2),]
# 
# ttt<-count(sr3,STATECD,KINDCD)
# 
# uuu<-count(sr3,STATECD,DESIGNCD)
# 
# plots_all<-count(sr3,pplotid)
# 

sr3<-sr
sr3$pplotidyr<-paste0(sr3$STATECD,"-",sr3$UNITCD,"-",
                      sr3$COUNTYCD,"-",sr3$PLOT,"-",sr3$INVYR)
plots_all_a<-count(sr3,pplotidyr)

write.csv(plots_all_a,"plots_selected_analysis.csv")

# ooo<-read.csv("plots_selected_analysis.csv")

# plots_with_tree<-plots_all_a[!is.na(plots_all_a$tplotidyr),]
# 
# write.csv(plots_with_tree,"../data/plots_with_tree.csv")


srsr1<-count(sr1,pplotid)
srsr1y<-count(sr1,pplotidyr)

sr<-sr3
rm(sr2)
rm(sr3)


rm(sr1)
nnn<-count(sr,STATECD.TR)

sr$uplotid<-paste0(sr$STATECD.CND,"-",sr$UNITCD.CND,"-",
                   sr$COUNTYCD.CND,"-",sr$PLOT.CND)
sr$NUNID <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                   sr$PLOT,"-",sr$INVYR)
sr$NUNIDS <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",sr$PLOT)

sr$TREEID <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                    sr$PLOT,"-",sr$SUBP,"-",sr$TREE)
sr$TREEIDYR <- paste0(sr$STATECD,"-",sr$UNITCD,"-",sr$COUNTYCD,"-",
                      sr$PLOT,"-",sr$SUBP,"-",sr$TREE,"-",sr$INVYR)

firstall<-sr %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 

first_p1<-count(firstall,NUNIDS)
first_p1all<-count(firstall,NUNID)

nofirstall<-sr %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR)) 

secondall<-nofirstall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

second_p1<-count(secondall,pplotid)
second_p1all<-count(secondall,pplotidyr)

nosecondall<-nofirstall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

thirdall<-nosecondall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

third_p1<-count(thirdall,pplotid)
third_p1all<-count(thirdall,pplotidyr)

nothirdall<-nosecondall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

fourthall<-nothirdall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))
fourth_p1<-count(fourthall,pplotid)
fourth_p1all<-count(fourthall,pplotidyr)

nofourthall<-nothirdall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))


fifthall<-nofourthall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nofifthall<-nofourthall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

sixthall<-nofifthall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nnoopp<-data.frame(count(sr,NUNIDS))
rm(sr)
rm(nofirstall)
rm(nosecondall)
rm(nothirdall)
rm(nofourthall)
rm(nofifthall)

dataall12<-merge(firstall,secondall, by="TREEID",all=TRUE,suffixes=c(".1",".2"))
saveRDS(dataall12,"../../data/all_two_repeated_tree1.RDS")
rm(firstall)
rm(secondall)

dataall123<-merge(dataall12,thirdall, by="TREEID",all=TRUE)
saveRDS(dataall123,"../../data/all_three_repeated_tree1.RDS")


rm(dataall12)
rm(thirdall)


dataall1234<-merge(dataall123,fourthall, by="TREEID",all=TRUE)
saveRDS(dataall1234,"../../data/all_four_repeated_tree1.RDS")


dataall12345<-merge(dataall1234,fifthall, by="TREEID",all=TRUE)
saveRDS(dataall12345,"../../data/all_five_repeated_tree1.RDS")

dataall12$diff_meas<-dataall12$MEASYEAR.2 - dataall12$MEASYEAR.1

dataall34<-merge(thirdall,fourthall,by="TREEID",all=TRUE,suffixes=c(".3",".4"))
saveRDS(dataall34,"../../data/all_three_four_tree.RDS")

rm(thirdall)
rm(fourthall)
rm(nnoopp)

dataall12<-readRDS("../../data/all_states_tree_repeated.RDS")
dataall34<-readRDS("../../data/all_three_four_tree.RDS")

dataall1234<-readRDS("../../data/all_states_tree_repeated_3_4_times.RDS")
dataall56<-readRDS("../../data/all_states_fivesix.RDS")


memory.limit(size=10000000000)



memory.limit(size=100000000000)
rm(fifthall)
rm(sixthall)


dataall123456<-merge(dataall1234,dataall56,by="TREEID",all=TRUE)

saveRDS(dataall123456,"all_states_tree_repeated_five_six.RDS")


table_damage<-data.frame(count(dataall1234,STATUSCD.1,STATUSCD.2))




plots_11<-data.frame(read.csv("../data/plots_measured_11_FL.csv"))
plots_12<-data.frame(read.csv("../data/plots_measured_12_FL.csv"))
plots_22<-data.frame(read.csv("../data/plots_measured_22_FL.csv"))


jjj<-plots_11[,2]

#ss<-data_with_all[jjj,]




sr11<-tree_cond_plot[tree_cond_plot$uplotid %in% plots_11$nplotid,]
sr12<-tree_cond_plot[tree_cond_plot$uplotid %in% plots_12$nplotid,]
sr22<-tree_cond_plot[tree_cond_plot$uplotid %in% plots_22$nplotid,]



table_plot44<-data.frame(count(data11, uplotid))


# select plots before fire year
sr11$NUNID <- paste0(sr11$STATECD,"-",sr11$UNITCD,"-",sr11$COUNTYCD,"-",
                     sr11$PLOT,"-",sr11$INVYR)

sr11$NUNIDS <- paste0(sr11$STATECD,"-",sr11$UNITCD,"-",sr11$COUNTYCD,"-",sr11$PLOT)


sr11$TREEID <- paste0(sr11$STATECD,"-",sr11$UNITCD,"-",sr11$COUNTYCD,"-",
                      sr11$PLOT,"-",sr11$SUBP,"-",sr11$TREE)

sr11$TREEIDYR <- paste0(sr11$STATECD,"-",sr11$UNITCD,"-",sr11$COUNTYCD,"-",
                        sr11$PLOT,"-",sr11$SUBP,"-",sr11$TREE,"-",sr11$INVYR)


plots_n4<-data.frame(count(nofirst,NUNIDS))


# select separate inventories
first<-sr11 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 

nofirst<-sr11 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR)) 

second<-nofirst %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nosecond<-second %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))


data12<-merge(first,second, by="TREEID",all=TRUE,suffixes=c(".frst",".scnd"))


plot22<-aggregate(NUNID.frst~NUNIDS.frst, data12, FUN=max)

table_trees<-data.frame(count(data12,DAMTYP1.frst))




sr12$NUNID <- paste0(sr12$STATECD,"-",sr12$UNITCD,"-",sr12$COUNTYCD,"-",
                     sr12$PLOT,"-",sr12$INVYR)

sr12$NUNIDS <- paste0(sr12$STATECD,"-",sr12$UNITCD,"-",sr12$COUNTYCD,"-",sr12$PLOT)


sr12$TREEID <- paste0(sr12$STATECD,"-",sr12$UNITCD,"-",sr12$COUNTYCD,"-",
                      sr12$PLOT,"-",sr12$SUBP,"-",sr12$TREE)

sr12$TREEIDYR <- paste0(sr12$STATECD,"-",sr12$UNITCD,"-",sr12$COUNTYCD,"-",
                        sr12$PLOT,"-",sr12$SUBP,"-",sr12$TREE,"-",sr12$INVYR)


firstsr12<-sr12 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 

nofirstsr12<-sr12 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR)) 

secondsr12<-nofirstsr12 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nosecondsr12<-nofirstsr12 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

thirdsr12<-nosecondsr12 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))


datasr1212<-merge(firstsr12,secondsr12,by="TREEID",all=TRUE,suffixes=c(".frst",".scnd",".thrd"))
datasr1212b<-merge(datasr1212,thirdsr12, by="TREEID",all=TRUE,suffixes=c(".a",".thrd"))

plotsr12<-aggregate(NUNID.frst~NUNIDS.frst, datasr1212b, FUN=max)

table_trees<-data.frame(count(datasr1212b,AGENT_CODEscnd,DAMAGE_AGENT_CD1))



sr22$NUNID <- paste0(sr22$STATECD,"-",sr22$UNITCD,"-",sr22$COUNTYCD,"-",
                     sr22$PLOT,"-",sr22$INVYR)

sr22$NUNIDS <- paste0(sr22$STATECD,"-",sr22$UNITCD,"-",sr22$COUNTYCD,"-",sr22$PLOT)


sr22$TREEID <- paste0(sr22$STATECD,"-",sr22$UNITCD,"-",sr22$COUNTYCD,"-",
                      sr22$PLOT,"-",sr22$SUBP,"-",sr22$TREE)

sr22$TREEIDYR <- paste0(sr22$STATECD,"-",sr22$UNITCD,"-",sr22$COUNTYCD,"-",
                        sr22$PLOT,"-",sr22$SUBP,"-",sr22$TREE,"-",sr22$INVYR)


firstsr22<-sr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR)) 

nofirstsr22<-sr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR)) 

secondsr22<-nofirstsr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nosecondsr22<-nofirstsr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

thirdsr22<-nosecondsr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

nothirdsr22<-nosecondsr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

thirdsr22<-nosecondsr22 %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))


datasr2222<-merge(firstsr22,secondsr22,by="TREEID",all=TRUE,suffixes=c(".frst",".scnd",".thrd"))
datasr2222b<-merge(datasr2222,thirdsr22, by="TREEID",all=TRUE,suffixes=c(".a",".thrd"))

plotsr22<-aggregate(NUNID.frst~NUNIDS.frst, datasr2222b, FUN=max)

table_trees_2222<-data.frame(count(datasr2222b,DAMAGE_AGENT_CD1.scnd,DAMAGE_AGENT_CD1))

