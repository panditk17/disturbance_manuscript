# read raw plot, condition and tree tables from FIA data
# and prepare tables showing time-series data
# depending upon the avaiability of measurements each plot has
# -------------- Karun Pandit, 2022 --------------------------

rm(list=ls())
memory.limit(size=100000000)

library(tidyverse)

plots<-read.csv("../data/PLOT.CSV")
plots_all<-plots %>% select(CN,PREV_PLT_CN,INVYR,STATECD,UNITCD,COUNTYCD,
                            PLOT,PLOT_STATUS_CD,MEASYEAR,REMPER,LAT,LON,ELEV,
                            ECOSUBCD,KINDCD,DESIGNCD)
rm(plots)

plots_all$pplotid<-paste0(plots_all$STATECD,"-",plots_all$UNITCD,"-",
                          plots_all$COUNTYCD,"-",plots_all$PLOT)
plots_all$pplotidyr<-paste0(plots_all$STATECD,"-",plots_all$UNITCD,"-",
                            plots_all$COUNTYCD,"-",plots_all$PLOT,"-",plots_all$INVYR)

library(tidyr)
library(dplyr)

conds<-read.csv("../data/COND.csv")

cond_all<-conds %>% select(PLT_CN,INVYR,STATECD,UNITCD,COUNTYCD,PLOT,CONDID,
                          FORTYPCD,STDAGE,SITECLCD,SICOND,STDORGCD,SLOPE,
                          ASPECT,DSTRBCD1,DSTRBYR1,DSTRBCD2,DSTRBYR2,DSTRBCD3,
                          DSTRBYR3,TRTCD1,TRTYR1,TRTCD2,TRTYR2,TRTCD3,TRTYR3,
                          BALIVE)
rm(conds)
cond_all$cplotid<-paste0(cond_all$STATECD,"-",cond_all$UNITCD,"-",
                         cond_all$COUNTYCD,"-",cond_all$PLOT)
cond_all$cplotidyr<-paste0(cond_all$STATECD,"-",cond_all$UNITCD,"-",
                           cond_all$COUNTYCD,"-",cond_all$PLOT,"-",cond_all$INVYR)

filt_cond<-cond_all[which(cond_all$CONDID==1),]
rm(cond_all)

tree_all<-readRDS("../data/subset_tree_all.RDS") # subset of tree table with
                                                  # selected columns          

tree_all$tplotid<-paste0(tree_all$STATECD,"-",tree_all$UNITCD,"-",
                         tree_all$COUNTYCD,"-",tree_all$PLOT)
tree_all$tplotidyr<-paste0(tree_all$STATECD,"-",tree_all$UNITCD,"-",
                          tree_all$COUNTYCD,"-",tree_all$PLOT,"-",tree_all$INVYR)

tree_cond<-merge(tree_all,filt_cond,by.x="tplotidyr",by.y="cplotidyr",
                 suffixes=c(".TR",".CND"),all=TRUE)
rm(tree_all)
rm(filt_cond)

tree_cond_plot<-merge(tree_cond,plots_all,by.x="tplotidyr",by.y="pplotidyr",
                      suffixes=c(".tc",".pl"),all=TRUE)
rm(tree_cond)
rm(plots_all)
#save file
saveRDS(tree_cond_plot,"../tree_cond_plot_all_true.RDS")
tree_cond_plot<-readRDS("../tree_cond_plot_all_true.RDS")

alltreex<-count(tree_cond_plot,tplotid)
alltreexyr<-count(tree_cond_plot,tplotidyr)


tree_cond_plot$pplotid<-paste0(tree_cond_plot$STATECD,"-",tree_cond_plot$UNITCD,"-",
                               tree_cond_plot$COUNTYCD,"-",tree_cond_plot$PLOT)
tree_cond_plot$pplotidyr<-paste0(tree_cond_plot$STATECD,"-",tree_cond_plot$UNITCD,"-",
                                     tree_cond_plot$COUNTYCD,"-",tree_cond_plot$PLOT,"-",tree_cond_plot$INVYR)


jkjk<-count(tree_cond_plot,pplotid,tplotid)

plots_with_no_tree<-jkjk[is.na(jkjk$tplotid),]

sums<-sum(dfdf$n)

sr<-tree_cond_plot
rm(tree_cond_plot)

sr2<-sr[which(sr$DESIGNCD==1),]
sr1<-sr2[which(sr2$KINDCD==1|sr2$KINDCD==2),]

plots_all<-count(sr1,pplotid)
plots_all_a<-count(sr1,pplotid,tplotid)


plots_with_tree<-plots_all_a[!is.na(plots_all_a$tplotid),]

write.csv(plots_with_tree,"../data/plots_with_tree.csv")

plots_with_tree<-

srsr1<-count(sr1,pplotid)
srsr1y<-count(sr1,pplotidyr)

rm(sr2)
sr<-sr1

plot11<-count(sr1,pplotid)

rm(sr1)

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

first_p1<-count(firstall,pplotid)
first_p1all<-count(firstall,pplotidyr)

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

nothirdall<-nosecondall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) != min(MEASYEAR))

fourthall<-nothirdall %>% group_by(NUNIDS) %>% 
  filter(abs(MEASYEAR) == min(MEASYEAR))

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
saveRDS(dataall12,"all_two_repeated_tree1.RDS")
rm(firstall)
rm(secondall)

dataall123<-merge(dataall12,thirdall, by="TREEID",all=TRUE)
saveRDS(dataall123,"all_three_repeated_tree1.RDS")


ppp12<-count(dataall1234,NUNIDS.y)

rm(dataall12)
rm(thridall)


dataall1234<-merge(dataall123,fourthall, by="TREEID",all=TRUE)
saveRDS(dataall1234,"all_four_repeated_tree1.RDS")


dataall12345<-merge(dataall1234,fifthall, by="TREEID",all=TRUE)
saveRDS(dataall12345,"all_five_repeated_tree1.RDS")

dataall12$diff_meas<-dataall12$MEASYEAR.2 - dataall12$MEASYEAR.1

dataall34<-merge(thirdall,fourthall,by="TREEID",all=TRUE,suffixes=c(".3",".4"))
saveRDS(dataall34,"all_three_four_tree.RDS")

rm(thirdall)
rm(fourthall)
rm(nnoopp)

dataall12<-readRDS("all_states_tree_repeated.RDS")
dataall34<-readRDS("all_three_four_tree.RDS")

dataall1234<-readRDS("all_states_tree_repeated_3_4_times.RDS")
dataall56<-readRDS("all_states_fivesix.RDS")


memory.limit(size=10000000000)



saveRDS(dataall1234,"all_states_tree_repekated_3_4_times.RDS")
rm(dataall12)
rm(dataall34)

dataall56<-merge(fifthall,sixthall,by="TREEID",all=TRUE,suffixes=c(".5",".6"))



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

