
rm(list=ls())

library(tidyr)
library(dplyr)
library(reshape2)


#read seedling files
seed<-read.csv("../data/SEEDLING.CSV")


# seed<-seed1[!is.na(seed1$TREECOUNT),]

# seed$ratio<-seed$TPA_UNADJ/seed$TREECOUNT

seed<-seed[which(seed$TREECOUNT!=seed$SPCD),]

seed$NUNID <- paste0(seed$STATECD,"-",seed$UNITCD,"-",seed$COUNTYCD,"-",
                     seed$PLOT,"-",seed$INVYR)

seed$NUNIDS <- paste0(seed$STATECD,"-",seed$UNITCD,"-",seed$COUNTYCD,"-",seed$PLOT)

table1<-data.frame(count(seed,NUNIDS,NUNID))
table2<-data.frame(count(table1,NUNIDS))

# select plot_ids with 2 measurements
table3<-table2

table3<-table2[which(table2$n>1),]

subseed<-seed[which(seed$NUNIDS %in% table3$NUNIDS),]

table5<-data.frame(count(subseed,NUNID,NUNIDS))
table6<-data.frame(count(table5,NUNIDS))

library(tidyverse)
firstseed<-subseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) == min(INVYR)) 

nofirstseed<-subseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) != min(INVYR)) 

secondseed<-nofirstseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) == min(INVYR))

nosecondseed<-nofirstseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) != min(INVYR))

thirdseed<-nosecondseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) == min(INVYR))

nothirdseed<-nosecondseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) != min(INVYR))


fourthseed<-nothirdseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) == min(INVYR))

nofourthseed<-nothirdseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) != min(INVYR))


fifthseed<-nofourthseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) == min(INVYR))

nofifthseed<-nofourthseed %>% group_by(NUNIDS) %>% 
  filter(abs(INVYR) != min(INVYR))


seedling12<-merge(firstseed,secondseed,by="NUNIDS",all=TRUE)
rm(fdam7)

fplot1<-aggregate(TREECOUNT.x~NUNIDS, seedling12, FUN=sum)
fplot2<-aggregate(TREECOUNT.y~NUNIDS, seedling12, FUN=sum)
fplot3<-count(seedling12,NUNIDS,SPCD.x)
fplot4<-count(fplot3,NUNIDS)

fplot5<-count(seedling12,NUNIDS,SPCD.y)
fplot6<-count(fplot5,NUNIDS)

fdam1<-merge(fplot1,fplot2,by="NUNIDS")
fdam2<-merge(fdam1,fplot4,by="NUNIDS",all.x=TRUE)
fdam3<-merge(fdam2,fplot6,by="NUNIDS",all.x=TRUE)
colnames(fdam3)<-c("NUNIDS","seed_count.1","seed_count.2",
                   "sd_spp_rich.1","sd_spp_rich.2")


seedling12$con<-ifelse(seedling12$SPGRPCD.x<25|seedling12$SPGRPCD.x==51|
                         seedling12$SPGRPCD.x==52,1,0)

seedling12b<-seedling12[which(seedling12$con==1),]

fplot7<-aggregate(TREECOUNT.x~NUNIDS, seedling12b, FUN=sum)
fplot8<-aggregate(TREECOUNT.y~NUNIDS, seedling12b, FUN=sum)


fplot9b<-count(seedling12b,NUNIDS,SPCD.x)
fplot9<-count(fplot9b,NUNIDS)

fplot10b<-count(seedling12b,NUNIDS,SPCD.y)
fplot10<-count(fplot10b,NUNIDS)





fdam4<-merge(fdam3,fplot7,by="NUNIDS",all.x=TRUE)

fdam5<-merge(fdam4,fplot8,by="NUNIDS",all.x=TRUE)

fdam6<-merge(fdam5,fplot9,by="NUNIDS",all.x=TRUE)

fdam7<-merge(fdam6,fplot10,by="NUNIDS",all.x=TRUE)

colnames(fdam7)<-c("NUNIDS","seed_count.1","seed_count.2",
                   "sd_spp_rich.1","sd_spp_rich.2",
                   "seed_count_con.1","seed_count_con.2",
                   "sd_spp_rich_con.1","sd_spp_rich_con.2")


fdam7$seed_count_con.1[is.na(fdam7$seed_count_con.1)] <- 0
fdam7$seed_count_con.2[is.na(fdam7$seed_count_con.2)] <- 0


fdam7$sd_spp_rich_con.1[is.na(fdam7$sd_spp_rich_con.1)] <- 0
fdam7$sd_spp_rich_con.2[is.na(fdam7$sd_spp_rich_con.2)] <- 0

fplot11<-aggregate(INVYR.x~NUNIDS, seedling12, FUN=max)
fplot12<-aggregate(INVYR.y~NUNIDS, seedling12, FUN=max)

fdam8<-merge(fdam7,fplot11,by="NUNIDS",all.x=TRUE)
fdam9<-merge(fdam8,fplot12,by="NUNIDS",all.x=TRUE)


fdam7_12<-fdam9


saveRDS(fdam7_12,"seedlings_12.RDS")





seedling23<-merge(secondseed,thirdseed,by="NUNIDS")

rm(fdam9)


fplot1<-aggregate(TREECOUNT.x~NUNIDS, seedling23, FUN=sum)
fplot2<-aggregate(TREECOUNT.y~NUNIDS, seedling23, FUN=sum)
fplot3<-count(seedling23,NUNIDS,SPCD.x)
fplot4<-count(fplot3,NUNIDS)

fplot5<-count(seedling23,NUNIDS,SPCD.y)
fplot6<-count(fplot5,NUNIDS)

fdam1<-merge(fplot1,fplot2,by="NUNIDS")
fdam2<-merge(fdam1,fplot4,by="NUNIDS",all.x=TRUE)
fdam3<-merge(fdam2,fplot6,by="NUNIDS",all.x=TRUE)
colnames(fdam3)<-c("NUNIDS","seed_count.1","seed_count.2",
                   "sd_spp_rich.1","sd_spp_rich.2")


seedling23$con<-ifelse(seedling23$SPGRPCD.x<25|seedling23$SPGRPCD.x==51|
                         seedling23$SPGRPCD.x==52,1,0)

seedling23b<-seedling23[which(seedling23$con==1),]

fplot7<-aggregate(TREECOUNT.x~NUNIDS, seedling23b, FUN=sum)
fplot8<-aggregate(TREECOUNT.y~NUNIDS, seedling23b, FUN=sum)


fplot9b<-count(seedling23b,NUNIDS,SPCD.x)
fplot9<-count(fplot9b,NUNIDS)

fplot10b<-count(seedling23b,NUNIDS,SPCD.y)
fplot10<-count(fplot10b,NUNIDS)




fdam4<-merge(fdam3,fplot7,by="NUNIDS",all.x=TRUE)

fdam5<-merge(fdam4,fplot8,by="NUNIDS",all.x=TRUE)

fdam6<-merge(fdam5,fplot9,by="NUNIDS",all.x=TRUE)

fdam7<-merge(fdam6,fplot10,by="NUNIDS",all.x=TRUE)

colnames(fdam7)<-c("NUNIDS","seed_count.1","seed_count.2",
                   "sd_spp_rich.1","sd_spp_rich.2",
                   "seed_count_con.1","seed_count_con.2",
                   "sd_spp_rich_con.1","sd_spp_rich_con.2")


fdam7$seed_count_con.1[is.na(fdam7$seed_count_con.1)] <- 0
fdam7$seed_count_con.2[is.na(fdam7$seed_count_con.2)] <- 0


fdam7$sd_spp_rich_con.1[is.na(fdam7$sd_spp_rich_con.1)] <- 0
fdam7$sd_spp_rich_con.2[is.na(fdam7$sd_spp_rich_con.2)] <- 0


fplot11<-aggregate(INVYR.x~NUNIDS, seedling23, FUN=max)
fplot12<-aggregate(INVYR.y~NUNIDS, seedling23, FUN=max)

fdam8<-merge(fdam7,fplot11,by="NUNIDS",all.x=TRUE)
fdam9<-merge(fdam8,fplot12,by="NUNIDS",all.x=TRUE)


fdam7_23<-fdam9


saveRDS(fdam7_23,"seedlings_23.RDS")


seedling34<-merge(thirdseed,fourthseed,by="NUNIDS")


rm(fdam9)


fplot1<-aggregate(TREECOUNT.x~NUNIDS, seedling34, FUN=sum)
fplot2<-aggregate(TREECOUNT.y~NUNIDS, seedling34, FUN=sum)
fplot3<-count(seedling34,NUNIDS,SPCD.x)
fplot4<-count(fplot3,NUNIDS)

fplot5<-count(seedling34,NUNIDS,SPCD.y)
fplot6<-count(fplot5,NUNIDS)

fdam1<-merge(fplot1,fplot2,by="NUNIDS")
fdam2<-merge(fdam1,fplot4,by="NUNIDS",all.x=TRUE)
fdam3<-merge(fdam2,fplot6,by="NUNIDS",all.x=TRUE)
colnames(fdam3)<-c("NUNIDS","seed_count.1","seed_count.2",
                   "sd_spp_rich.1","sd_spp_rich.2")


seedling34$con<-ifelse(seedling34$SPGRPCD.x<25|seedling34$SPGRPCD.x==51|
                         seedling34$SPGRPCD.x==52,1,0)

seedling34b<-seedling34[which(seedling34$con==1),]

fplot7<-aggregate(TREECOUNT.x~NUNIDS, seedling34b, FUN=sum)
fplot8<-aggregate(TREECOUNT.y~NUNIDS, seedling34b, FUN=sum)


fplot9b<-count(seedling34b,NUNIDS,SPCD.x)
fplot9<-count(fplot9b,NUNIDS)

fplot10b<-count(seedling34b,NUNIDS,SPCD.y)
fplot10<-count(fplot10b,NUNIDS)




fdam4<-merge(fdam3,fplot7,by="NUNIDS",all.x=TRUE)

fdam5<-merge(fdam4,fplot8,by="NUNIDS",all.x=TRUE)

fdam6<-merge(fdam5,fplot9,by="NUNIDS",all.x=TRUE)

fdam7<-merge(fdam6,fplot10,by="NUNIDS",all.x=TRUE)

colnames(fdam7)<-c("NUNIDS","seed_count.1","seed_count.2",
                   "sd_spp_rich.1","sd_spp_rich.2",
                   "seed_count_con.1","seed_count_con.2",
                   "sd_spp_rich_con.1","sd_spp_rich_con.2")


fdam7$seed_count_con.1[is.na(fdam7$seed_count_con.1)] <- 0
fdam7$seed_count_con.2[is.na(fdam7$seed_count_con.2)] <- 0


fdam7$sd_spp_rich_con.1[is.na(fdam7$sd_spp_rich_con.1)] <- 0
fdam7$sd_spp_rich_con.2[is.na(fdam7$sd_spp_rich_con.2)] <- 0


fplot11<-aggregate(INVYR.x~NUNIDS, seedling34, FUN=max)
fplot12<-aggregate(INVYR.y~NUNIDS, seedling34, FUN=max)

fdam8<-merge(fdam7,fplot11,by="NUNIDS",all.x=TRUE)
fdam9<-merge(fdam8,fplot12,by="NUNIDS",all.x=TRUE)


fdam7_34<-fdam9


saveRDS(fdam7_34,"seedlings_34.RDS")




seedling45<-merge(fourthseed,fifthseed,by="NUNIDS")

rm(fdam9)


fplot1<-aggregate(TREECOUNT.x~NUNIDS, seedling45, FUN=sum)
fplot2<-aggregate(TREECOUNT.y~NUNIDS, seedling45, FUN=sum)
fplot3<-count(seedling45,NUNIDS,SPCD.x)
fplot4<-count(fplot3,NUNIDS)

fplot5<-count(seedling45,NUNIDS,SPCD.y)
fplot6<-count(fplot5,NUNIDS)

fdam1<-merge(fplot1,fplot2,by="NUNIDS")
fdam2<-merge(fdam1,fplot4,by="NUNIDS",all.x=TRUE)
fdam3<-merge(fdam2,fplot6,by="NUNIDS",all.x=TRUE)
colnames(fdam3)<-c("NUNIDS","seed_count.1","seed_count.2",
                   "sd_spp_rich.1","sd_spp_rich.2")


seedling45$con<-ifelse(seedling45$SPGRPCD.x<25|seedling45$SPGRPCD.x==51|
                         seedling45$SPGRPCD.x==52,1,0)

seedling45b<-seedling45[which(seedling45$con==1),]

fplot7<-aggregate(TREECOUNT.x~NUNIDS, seedling45b, FUN=sum)
fplot8<-aggregate(TREECOUNT.y~NUNIDS, seedling45b, FUN=sum)


fplot9b<-count(seedling45b,NUNIDS,SPCD.x)
fplot9<-count(fplot9b,NUNIDS)

fplot10b<-count(seedling45b,NUNIDS,SPCD.y)
fplot10<-count(fplot10b,NUNIDS)




fdam4<-merge(fdam3,fplot7,by="NUNIDS",all.x=TRUE)

fdam5<-merge(fdam4,fplot8,by="NUNIDS",all.x=TRUE)

fdam6<-merge(fdam5,fplot9,by="NUNIDS",all.x=TRUE)

fdam7<-merge(fdam6,fplot10,by="NUNIDS",all.x=TRUE)

colnames(fdam7)<-c("NUNIDS","seed_count.1","seed_count.2",
                   "sd_spp_rich.1","sd_spp_rich.2",
                   "seed_count_con.1","seed_count_con.2",
                   "sd_spp_rich_con.1","sd_spp_rich_con.2")


fdam7$seed_count_con.1[is.na(fdam7$seed_count_con.1)] <- 0
fdam7$seed_count_con.2[is.na(fdam7$seed_count_con.2)] <- 0


fdam7$sd_spp_rich_con.1[is.na(fdam7$sd_spp_rich_con.1)] <- 0
fdam7$sd_spp_rich_con.2[is.na(fdam7$sd_spp_rich_con.2)] <- 0


fplot11<-aggregate(INVYR.x~NUNIDS, seedling45, FUN=max)
fplot12<-aggregate(INVYR.y~NUNIDS, seedling45, FUN=max)

fdam8<-merge(fdam7,fplot11,by="NUNIDS",all.x=TRUE)
fdam9<-merge(fdam8,fplot12,by="NUNIDS",all.x=TRUE)

fdam7_45<-fdam9


saveRDS(fdam7_45,"seedlings_45.RDS")

all_seedlings<-rbind(fdam7_12,fdam7_23,fdam7_34,fdam7_45)

saveRDS(all_seedlings,"all_seedlings_pre_post.RDS")


####  ###################################
#######################

jjj<-count(seedling12,NUNIDS,NUNID.x,NUNID.y)
kkk<-count(seedling45,NUNIDS,NUNID.x,NUNID.y)

four_seeds<-rbind(seedling12,seedling23,seedling34,
                  seedling45)


fplot1<-aggregate(TREECOUNT.x~NUNID.x, four_seeds, FUN=sum)

fplot2<-aggregate(TREECOUNT~NUNID, firstseed, FUN=sum)
colnames(fplot2)<-c("NUNID","seed_count.1")

fplot3<-count(firstseed,NUNID,SPCD)
fplot4<-count(fplot3,NUNID,NUNIDS)
colnames(fplot4)<-c("NUNIDS","NUNID","sd_spp_rich.1")
fplot5<-fplot4[c("NUNID","sd_spp_rich.1")]

firstseed$con<-ifelse(firstseed$SPGRPCD<25|firstseed$SPGRPCD==51|
                        firstseed$SPGRPCD==52,1,0)

firstseedb<-firstseed[which(firstseed$con==1),]
firstseedh<-firstseed[which(firstseed$con==0),]


fplot1b<-aggregate(TREECOUNT~NUNIDS, firstseedb, FUN=sum)

fplot2b<-aggregate(TREECOUNT~NUNID, firstseedb, FUN=sum)
colnames(fplot2b)<-c("NUNID","seed_count_con.1")

fplot3b<-count(firstseedb,NUNID,SPCD)
fplot4b<-count(fplot3b,NUNID,NUNIDS)
colnames(fplot4b)<-c("NUNIDS","NUNID","sd_spp_rich_con.1")
fplot5b<-fplot4b[c("NUNID","sd_spp_rich_con.1")]


fdam1<-merge(fplot2,fplot5,by="NUNID")
fdam2<-merge(fdam1,fplot2b,by="NUNID",all.x=TRUE)
fdam3<-merge(fdam2,fplot5b,by="NUNID",all.x=TRUE)




gplot2<-aggregate(TREECOUNT~NUNID, secondseed, FUN=sum)
colnames(gplot2)<-c("NUNID","seed_count.2")

gplot3<-count(secondseed,NUNID,SPCD)
gplot4<-count(gplot3,NUNID,NUNIDS)
colnames(gplot4)<-c("NUNIDS","NUNID","sd_spp_rich.2")
gplot5<-gplot4[c("NUNID","sd_spp_rich.2")]




secondseed$con<-ifelse(secondseed$SPGRPCD<25|secondseed$SPGRPCD==51|
                         secondseed$SPGRPCD==52,1,0)

secondseedb<-secondseed[which(secondseed$con==1),]



gplot1b<-aggregate(TREECOUNT~NUNIDS, secondseedb, FUN=sum)

gplot2b<-aggregate(TREECOUNT~NUNID, secondseedb, FUN=sum)
colnames(gplot2b)<-c("NUNID","seed_count_con.2")

gplot3b<-count(secondseedb,NUNID,SPCD)
gplot4b<-count(gplot3b,NUNID,NUNIDS)
colnames(gplot4b)<-c("NUNIDS","NUNID","sd_spp_rich_con.2")
gplot5b<-gplot4b[c("NUNID","sd_spp_rich_con.2")]


gdam1<-merge(gplot2,gplot5,by="NUNID")
gdam2<-merge(gdam1,gplot2b,by="NUNID",all.x=TRUE)
gdam3<-merge(gdam2,gplot5b,by="NUNID",all.x=TRUE)



fdam3$seed_count_con.1[is.na(fdam3$seed_count_con.1)] <- 0
gdam3$seed_count_con.2[is.na(gdam3$seed_count_con.2)] <- 0


fdam3$sd_spp_rich_con.1[is.na(fdam3$sd_spp_rich_con.1)] <- 0
gdam3$sd_spp_rich_con.2[is.na(gdam3$sd_spp_rich_con.2)] <- 0




saveRDS(fdam3,"all_seedlings_first.RDS")
saveRDS(gdam3,"all_seedlings_second.RDS")



splot1<-aggregate(TREECOUNT~NUNIDS, secondseed, FUN=sum)

splot2<-aggregate(TREECOUNT~NUNID, secondseed, FUN=sum)

plot_12<-merge(plot1,plot2,by="NUNIDS")
colnames(plot_12)<-c("NUNIDS","seedling.1","seedling.2")

write.csv(plot_12,"repeated_seedling_table.csv")


write.csv(seedling_three,"seedling_data_combined.csv")

# select plots before fire year
seedling_three$NUNID <- paste0(seedling_three$STATECD,"-",seedling_three$UNITCD,"-",seedling_three$COUNTYCD,"-",
                               seedling_three$PLOT,"-",seedling_three$INVYR)

seedling_three$NUNIDS <- paste0(seedling_three$STATECD,"-",seedling_three$UNITCD,"-",seedling_three$COUNTYCD,"-",seedling_three$PLOT)

# check plots remeasurements

plot11<-aggregate(NUNID~NUNIDS,seedling_three , FUN=max)

plot11<-data.frame(count(seedling_three,NUNIDS,NUNID))
plot12<-data.frame(count(plot11,NUNIDS))


seedling_n1<-seedling_three[which(seedling_three$TREECOUNT<50),]
