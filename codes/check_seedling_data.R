
rm(list=ls())
seedling_all<-read.csv("../data/SEEDLING.CSV")


seedling_all$nunid<-paste0(seedling_all$STATECD,"-",seedling_all$UNITCD,"-",
                           seedling_all$COUNTYCD,"-", seedling_all$PLOT,
                           "-",seedling_all$INVYR)



plots<-read.csv("../../data/PLOT.CSV")
plots2<-plots[which(plots$PLOT_STATUS_CD==1),]

plots2$nunid<-paste0(plots2$STATECD,"-",plots2$UNITCD,"-",
                           plots2$COUNTYCD,"-", plots2$PLOT,
                           "-",plots2$INVYR)

library(tidyverse)
jkk<-count(plots2,STATECD)

plot<-read.csv("plots_selected_analysis.csv")

plot_n<-plot[(plot$pplotidy%in%plots2$nunid),]

table15<-count(plot_n,pplotidyr)


# seed<-seedling_all[which(seedling_all$TREECOUNT!=seedling_all$SPCD),]
seed<-seedling_all
seed$NUNID <- paste0(seed$STATECD,"-",seed$UNITCD,"-",seed$COUNTYCD,"-",
                     seed$PLOT,"-",seed$INVYR)

seed$NUNIDS <- paste0(seed$STATECD,"-",seed$UNITCD,"-",seed$COUNTYCD,"-",seed$PLOT)

table16<-count(seed,NUNID)

library(tidyverse)
table1<-data.frame(count(seed,NUNIDS,NUNID))
table2<-data.frame(count(table1,NUNIDS))

# # select plot_ids with 2 measurements
# table3<-table2
# 
# table3<-table2[which(table2$n>1),]
# 
# subseed<-seed[which(seed$NUNIDS %in% table3$NUNIDS),]
# 
# 
# table5<-data.frame(count(subseed,NUNID,NUNIDS))
# table6<-data.frame(count(table5,NUNIDS))


fplot1<-aggregate(TREECOUNT~NUNID, seed, FUN=sum,NA.rm=TRUE)
fplot3<-count(seed,NUNID,SPCD)
fplot4<-count(fplot3,NUNID)

fdam2<-merge(fplot1,fplot4,by="NUNID",all.x=TRUE)
colnames(fdam2)<-c("NUNID","seed_count","sd_spp_rich")



plot_seed<-merge(plot_n,fdam2,by.x="pplotidyr",by.y="NUNID",all=TRUE)



sdam2b<-separate(plot_seed, pplotidyr, 
                 into = c("st.1","county.1","unit.1","plot.1","invyr.1"), remove = FALSE)
sdam2b$na<-ifelse(sdam2b$seed_count>0,"data","nodata")
# hhh<-count(sdam2b,st.1,invyr.1,na)

iii<-count(sdam2b,st.1,invyr.1,na)


# iii$state_inv_ct<-paste0(iii$st.1,"-",iii$invyr.1,"-",iii$county.1,"-",iii$unit.1)
iii$state_inv<-paste0(iii$st.1,"-",iii$invyr.1)

agg1<-iii[which(iii$na=="data"),]
agg2<-iii[is.na(iii$na),]

agg3<-merge(agg1,agg2,by="state_inv",all=TRUE)


agg3$tot<-agg3$n.x+agg3$n.y
agg3$per_missing<-(agg3$n.y/agg3$tot)*100

agg3$per_missing2<-ifelse(is.na(agg3$n.x),100,
                          ifelse(is.na(agg3$n.y),0,agg3$per_missing))


agg4<-agg3[,c(1,2,3,5,9,10,12)]
colnames(agg4)<-c("STATE-INVYR","STATE","INVYR",
"Present","Missing","Total","Percentage of missing data")

agg4$state_inv_<-as.character(agg4$`STATE-INVYR`)
write.csv(agg4,"plots_seedling_missing_data_514_c.csv")

plot(agg4$`STATE-INVYR`,agg4$`Percentage of missing data`)

table_st<-count(agg4,STATE)

agg5<-agg4[which(agg4$STATE==12),]
plot(agg5$INVYR,agg5$`Percentage of missing data`,xlab="",ylim=c(0,100))
axis(1, at=seq(1994,2020, by=1), labels = seq(1994,2020,1))


dev.new()

png(paste0("plots1a.png"), width = 800, height = 1100,res=90)

par(mfrow=c(4,3))

for (rrr in 1:12){
  st<-table_st[rrr,1]
   st_name=NULL
  agg5<-agg4[which(agg4$STATE==st),]
  if (st==56) {st_name="WY"}
  if (st==55) {st_name="WI"}
  if (st==54) {st_name="WV"}
  if (st==53) {st_name="WA"}
  if (st==51) {st_name="VA"}
  if (st==50) {st_name="VT"}
  if (st==49) {st_name="UT"}
  if (st==48) {st_name="TX"}
  if (st==47) {st_name="TN"}
  if (st==46) {st_name="SD"}
  if (st==45) {st_name="SC"}
  if (st==44) {st_name="RI"}
  if (st==42) {st_name="PA"}
  if (st==41) {st_name="OR"}
  if (st==40) {st_name="OK"}
  if (st==39) {st_name="OH"}
  if (st==38) {st_name="ND"}
  if (st==37) {st_name="NC"}

  if (st==36) {st_name="NY"}
  if (st==35) {st_name="NM"}
  if (st==34) {st_name="NJ"}
  if (st==33) {st_name="NH"}
  if (st==32) {st_name="NV"}
  if (st==31) {st_name="NE"}
  if (st==30) {st_name="MT"}
  if (st==29) {st_name="MO"}
  if (st==28) {st_name="MS"}
  if (st==27) {st_name="MN"}
  if (st==26) {st_name="MI"}
  if (st==25) {st_name="MA"}
  if (st==24) {st_name="MD"}
  if (st==23) {st_name="ME"}
  if (st==22) {st_name="LA"}
  if (st==21) {st_name="KY"}
  if (st==20) {st_name="KS"}
  if (st==19) {st_name="IA"}
  if (st==18) {st_name="IN"}
  if (st==17) {st_name="IL"}
  if (st==16) {st_name="ID"}
  if (st==15) {st_name="HI"}
  if (st==13) {st_name="GA"}
  if (st==12) {st_name="FL"}
  if (st==11) {st_name="DC"}
  if (st==10) {st_name="DE"}
  if (st==9) {st_name="CT"}
  if (st==8) {st_name="CO"}
  if (st==6) {st_name="CA"}
  if (st==5) {st_name="AR"}
  if (st==4) {st_name="AZ"}
  if (st==1) {st_name="AL"}
  if (st==2) {st_name="AK"}
  

plot(agg5$INVYR,agg5$`Percentage of missing data`,
     xlim=c(1994,2020),ylim=c(0,100),
     ylab="percentage of missing seedling plots",
     xlab= "inventory year",
     main= st_name)
abline(v = seq(1994,2020,1),
       lty = 2, col = "gray87")

abline(h = seq(0,100,20),
       lty = 2, col = "gray87")

points(agg5$INVYR,agg5$`Percentage of missing data`,pch=19,col="blue",
       xlim=c(1994,2020),ylim=c(0,100),
       ylab="percentage of missing seedling plots",
       xlab= "inventory year",
       main= st_name)



}

dev.off()


png(paste0("plots2a.png"), width = 800, height = 1100,res=90)

par(mfrow=c(4,3))

for (rrr in 13:24){
  st<-table_st[rrr,1]
  st_name=NULL
  agg5<-agg4[which(agg4$STATE==st),]
  if (st==56) {st_name="WY"}
  if (st==55) {st_name="WI"}
  if (st==54) {st_name="WV"}
  if (st==53) {st_name="WA"}
  if (st==51) {st_name="VA"}
  if (st==50) {st_name="VT"}
  if (st==49) {st_name="UT"}
  if (st==48) {st_name="TX"}
  if (st==47) {st_name="TN"}
  if (st==46) {st_name="SD"}
  if (st==45) {st_name="SC"}
  if (st==44) {st_name="RI"}
  if (st==42) {st_name="PA"}
  if (st==41) {st_name="OR"}
  if (st==40) {st_name="OK"}
  if (st==39) {st_name="OH"}
  if (st==38) {st_name="ND"}
  if (st==37) {st_name="NC"}
  
  if (st==36) {st_name="NY"}
  if (st==35) {st_name="NM"}
  if (st==34) {st_name="NJ"}
  if (st==33) {st_name="NH"}
  if (st==32) {st_name="NV"}
  if (st==31) {st_name="NE"}
  if (st==30) {st_name="MT"}
  if (st==29) {st_name="MO"}
  if (st==28) {st_name="MS"}
  if (st==27) {st_name="MN"}
  if (st==26) {st_name="MI"}
  if (st==25) {st_name="MA"}
  if (st==24) {st_name="MD"}
  if (st==23) {st_name="ME"}
  if (st==22) {st_name="LA"}
  if (st==21) {st_name="KY"}
  if (st==20) {st_name="KS"}
  if (st==19) {st_name="IA"}
  if (st==18) {st_name="IN"}
  if (st==17) {st_name="IL"}
  if (st==16) {st_name="ID"}
  if (st==15) {st_name="HI"}
  if (st==13) {st_name="GA"}
  if (st==12) {st_name="FL"}
  if (st==11) {st_name="DC"}
  if (st==10) {st_name="DE"}
  if (st==9) {st_name="CT"}
  if (st==8) {st_name="CO"}
  if (st==6) {st_name="CA"}
  if (st==5) {st_name="AR"}
  if (st==4) {st_name="AZ"}
  if (st==1) {st_name="AL"}
  if (st==2) {st_name="AK"}
  
  
  # png(paste0("plot_",st_name,".png"), width = 700, height = 600,res=100)
  # filename<-paste0("plot_",st)

  plot(agg5$INVYR,agg5$`Percentage of missing data`,
       xlim=c(1994,2020),ylim=c(0,100),
       ylab="percentage of missing seedling plots",
       xlab= "inventory year",
       main= st_name)
  abline(v = seq(1994,2020,1),
         lty = 2, col = "gray87")
  
  abline(h = seq(0,100,20),
         lty = 2, col = "gray87")
  
  points(agg5$INVYR,agg5$`Percentage of missing data`,pch=19,col="blue",
         xlim=c(1994,2020),ylim=c(0,100),
         ylab="percentage of missing seedling plots",
         xlab= "inventory year",
         main= st_name)
  
  
}

dev.off()

png(paste0("plots3a.png"), width = 800, height = 1100,res=90)

par(mfrow=c(4,3))

for (rrr in 25:36){
  st<-table_st[rrr,1]
  st_name=NULL
  agg5<-agg4[which(agg4$STATE==st),]
  if (st==56) {st_name="WY"}
  if (st==55) {st_name="WI"}
  if (st==54) {st_name="WV"}
  if (st==53) {st_name="WA"}
  if (st==51) {st_name="VA"}
  if (st==50) {st_name="VT"}
  if (st==49) {st_name="UT"}
  if (st==48) {st_name="TX"}
  if (st==47) {st_name="TN"}
  if (st==46) {st_name="SD"}
  if (st==45) {st_name="SC"}
  if (st==44) {st_name="RI"}
  if (st==42) {st_name="PA"}
  if (st==41) {st_name="OR"}
  if (st==40) {st_name="OK"}
  if (st==39) {st_name="OH"}
  if (st==38) {st_name="ND"}
  if (st==37) {st_name="NC"}
  
  if (st==36) {st_name="NY"}
  if (st==35) {st_name="NM"}
  if (st==34) {st_name="NJ"}
  if (st==33) {st_name="NH"}
  if (st==32) {st_name="NV"}
  if (st==31) {st_name="NE"}
  if (st==30) {st_name="MT"}
  if (st==29) {st_name="MO"}
  if (st==28) {st_name="MS"}
  if (st==27) {st_name="MN"}
  if (st==26) {st_name="MI"}
  if (st==25) {st_name="MA"}
  if (st==24) {st_name="MD"}
  if (st==23) {st_name="ME"}
  if (st==22) {st_name="LA"}
  if (st==21) {st_name="KY"}
  if (st==20) {st_name="KS"}
  if (st==19) {st_name="IA"}
  if (st==18) {st_name="IN"}
  if (st==17) {st_name="IL"}
  if (st==16) {st_name="ID"}
  if (st==15) {st_name="HI"}
  if (st==13) {st_name="GA"}
  if (st==12) {st_name="FL"}
  if (st==11) {st_name="DC"}
  if (st==10) {st_name="DE"}
  if (st==9) {st_name="CT"}
  if (st==8) {st_name="CO"}
  if (st==6) {st_name="CA"}
  if (st==5) {st_name="AR"}
  if (st==4) {st_name="AZ"}
  if (st==1) {st_name="AL"}
  if (st==2) {st_name="AK"}
  
  # png(paste0("plot_",st_name,".png"), width = 700, height = 600,res=100)
  # filename<-paste0("plot_",st)
  plot(agg5$INVYR,agg5$`Percentage of missing data`,
       xlim=c(1994,2020),ylim=c(0,100),
       ylab="percentage of missing seedling plots",
       xlab= "inventory year",
       main= st_name)
  abline(v = seq(1994,2020,1),
         lty = 2, col = "gray87")
  
  abline(h = seq(0,100,20),
         lty = 2, col = "gray87")
  
  points(agg5$INVYR,agg5$`Percentage of missing data`,pch=19,col="blue",
        xlim=c(1994,2020),ylim=c(0,100),
        ylab="percentage of missing seedling plots",
        xlab= "inventory year",
        main= st_name)
  
  # dev.off()
}

dev.off()


png(paste0("plots4a.png"), width = 800, height = 1100,res=90)

par(mfrow=c(4,3))

for (rrr in 37:48){
  st<-table_st[rrr,1]
  st_name=NULL
  agg5<-agg4[which(agg4$STATE==st),]
  if (st==56) {st_name="WY"}
  if (st==55) {st_name="WI"}
  if (st==54) {st_name="WV"}
  if (st==53) {st_name="WA"}
  if (st==51) {st_name="VA"}
  if (st==50) {st_name="VT"}
  if (st==49) {st_name="UT"}
  if (st==48) {st_name="TX"}
  if (st==47) {st_name="TN"}
  if (st==46) {st_name="SD"}
  if (st==45) {st_name="SC"}
  if (st==44) {st_name="RI"}
  if (st==42) {st_name="PA"}
  if (st==41) {st_name="OR"}
  if (st==40) {st_name="OK"}
  if (st==39) {st_name="OH"}
  if (st==38) {st_name="ND"}
  if (st==37) {st_name="NC"}
  
  if (st==36) {st_name="NY"}
  if (st==35) {st_name="NM"}
  if (st==34) {st_name="NJ"}
  if (st==33) {st_name="NH"}
  if (st==32) {st_name="NV"}
  if (st==31) {st_name="NE"}
  if (st==30) {st_name="MT"}
  if (st==29) {st_name="MO"}
  if (st==28) {st_name="MS"}
  if (st==27) {st_name="MN"}
  if (st==26) {st_name="MI"}
  if (st==25) {st_name="MA"}
  if (st==24) {st_name="MD"}
  if (st==23) {st_name="ME"}
  if (st==22) {st_name="LA"}
  if (st==21) {st_name="KY"}
  if (st==20) {st_name="KS"}
  if (st==19) {st_name="IA"}
  if (st==18) {st_name="IN"}
  if (st==17) {st_name="IL"}
  if (st==16) {st_name="ID"}
  if (st==15) {st_name="HI"}
  if (st==13) {st_name="GA"}
  if (st==12) {st_name="FL"}
  if (st==11) {st_name="DC"}
  if (st==10) {st_name="DE"}
  if (st==9) {st_name="CT"}
  if (st==8) {st_name="CO"}
  if (st==6) {st_name="CA"}
  if (st==5) {st_name="AR"}
  if (st==4) {st_name="AZ"}
  if (st==1) {st_name="AL"}
  if (st==2) {st_name="AK"}
  
  
  # png(paste0("plot_",st_name,".png"), width = 700, height = 600,res=100)
  # filename<-paste0("plot_",st)
  plot(agg5$INVYR,agg5$`Percentage of missing data`,
       xlim=c(1994,2020),ylim=c(0,100),
       ylab="percentage of missing seedling plots",
       xlab= "inventory year",
       main= st_name)
  abline(v = seq(1994,2020,1),
         lty = 2, col = "gray87")
  
  abline(h = seq(0,100,20),
         lty = 2, col = "gray87")
  
  points(agg5$INVYR,agg5$`Percentage of missing data`,pch=19,col="blue",
         xlim=c(1994,2020),ylim=c(0,100),
         ylab="percentage of missing seedling plots",
         xlab= "inventory year",
         main= st_name)
  # dev.off()
}

dev.off()

png(paste0("plots5a.png"), width = 800, height = 1100,res=90)

par(mfrow=c(4,3))

for (rrr in 56:57){
  st<-table_st[rrr,1]
  st_name=NULL
  agg5<-agg4[which(agg4$STATE==st),]
  if (st==56) {st_name="WY"}
  if (st==55) {st_name="WI"}
  if (st==54) {st_name="WV"}
  if (st==53) {st_name="WA"}
  if (st==51) {st_name="VA"}
  if (st==50) {st_name="VT"}
  if (st==49) {st_name="UT"}
  if (st==48) {st_name="TX"}
  if (st==47) {st_name="TN"}
  if (st==46) {st_name="SD"}
  if (st==45) {st_name="SC"}
  if (st==44) {st_name="RI"}
  if (st==42) {st_name="PA"}
  if (st==41) {st_name="OR"}
  if (st==40) {st_name="OK"}
  if (st==39) {st_name="OH"}
  if (st==38) {st_name="ND"}
  if (st==37) {st_name="NC"}
  
  if (st==36) {st_name="NY"}
  if (st==35) {st_name="NM"}
  if (st==34) {st_name="NJ"}
  if (st==33) {st_name="NH"}
  if (st==32) {st_name="NV"}
  if (st==31) {st_name="NE"}
  if (st==30) {st_name="MT"}
  if (st==29) {st_name="MO"}
  if (st==28) {st_name="MS"}
  if (st==27) {st_name="MN"}
  if (st==26) {st_name="MI"}
  if (st==25) {st_name="MA"}
  if (st==24) {st_name="MD"}
  if (st==23) {st_name="ME"}
  if (st==22) {st_name="LA"}
  if (st==21) {st_name="KY"}
  if (st==20) {st_name="KS"}
  if (st==19) {st_name="IA"}
  if (st==18) {st_name="IN"}
  if (st==17) {st_name="IL"}
  if (st==16) {st_name="ID"}
  if (st==15) {st_name="HI"}
  if (st==13) {st_name="GA"}
  if (st==12) {st_name="FL"}
  if (st==11) {st_name="DC"}
  if (st==10) {st_name="DE"}
  if (st==9) {st_name="CT"}
  if (st==8) {st_name="CO"}
  if (st==6) {st_name="CA"}
  if (st==5) {st_name="AR"}
  if (st==4) {st_name="AZ"}
  if (st==1) {st_name="AL"}
  if (st==2) {st_name="AK"}
  
  # else  {
  #   st_name=st
  #   }
  # png(paste0("plot_",st_name,".png"), width = 700, height = 600,res=100)
  # filename<-paste0("plot_",st)
  plot(agg5$INVYR,agg5$`Percentage of missing data`,
       xlim=c(1994,2020),ylim=c(0,100),
       ylab="percentage of missing seedling plots",
       xlab= "inventory year",
       main= st_name)
  abline(v = seq(1994,2020,1),
         lty = 2, col = "gray87")
  
  abline(h = seq(0,100,20),
         lty = 2, col = "gray87")
  
  points(agg5$INVYR,agg5$`Percentage of missing data`,pch=19,col="blue",
         xlim=c(1994,2020),ylim=c(0,100),
         ylab="percentage of missing seedling plots",
         xlab= "inventory year",
         main= st_name)
  

  # dev.off()
}

dev.off()



agg5<-agg4[which(agg4$STATE==5),]
plot1<-plot(agg5$`Percentage of missing data`)

agg4$state_inv<-paste0("ID_",agg4$`STATE-INVYR`)

library(data.table)


record<-sdam2b %>% 
  group_by(yr,st) %>% 
  summarize(mean = mean(seed_count,NA.rm=TRUE))

record1<-sdam2b %>% 
  group_by(yr,st) %>% 
  summarize(count = frequency(seed_count))

record1<-record[which(record$INVYR>1994),]

write.csv(record1,"seedling_data_by_year_state.csv")

fplot1<-aggregate(TREECOUNT~nunid, seedling_all, FUN=sum)
fplot3<-count(seedling_all,nunid,SPCD)
fplot4<-count(fplot3,nunid)

fdam1<-merge(fplot1,fplot4,by="nunid")

seedling_all$con<-ifelse(seedling_all$SPGRPCD<25|seedling_all$SPGRPCD==51|
                           seedling_all$SPGRPCD==52,1,0)

seedling12b<-seedling_all[which(seedling_all$con==1),]

fplot7<-aggregate(TREECOUNT~nunid, seedling12b, FUN=sum)

fplot9b<-count(seedling12b,nunid,SPCD)
fplot9<-count(fplot9b,nunid)


fdam4<-merge(fdam1,fplot7,by="nunid",all.x=TRUE)

fdam6<-merge(fdam4,fplot9,by="nunid",all.x=TRUE)


colnames(fdam6)<-c("nunid","seed_count","sd_spp_rich",
                   "seed_count_con","sd_spp_rich_con")

fdam6$nunid_cp<-fdam6$nunid
fdam6$seed_count_con[is.na(fdam6$seed_count_con)] <- 0
fdam6$sd_spp_rich_con[is.na(fdam6$sd_spp_rich_con)] <- 0


#
# seedling_data$nunid.1<-paste0(seedling_data$NUNIDS,"-",seedling_data$INVYR.x)
# seedling_data$nunid.2<-paste0(seedling_data$NUNIDS,"-",seedling_data$INVYR.y)


plots_seed_n<-merge(plots_all_three,fdam6,by.x="NUNID.1",by.y="nunid",all.x=TRUE)
plots_seed_n2<-merge(plots_seed_n,fdam6,by.x="NUNID.2",by.y="nunid",all.x=TRUE)


colnames(plots_seed_n2)[35]<-"seed_count.1"
colnames(plots_seed_n2)[36]<-"sd_spp_rich.1"
colnames(plots_seed_n2)[37]<-"seed_count_con.1"
colnames(plots_seed_n2)[38]<-"sd_spp_rich_con.1"
colnames(plots_seed_n2)[40]<-"seed_count.2"
colnames(plots_seed_n2)[41]<-"sd_spp_rich.2"
colnames(plots_seed_n2)[42]<-"seed_count_con.2"
colnames(plots_seed_n2)[43]<-"sd_spp_rich_con.2"




sdam2b<-separate(plots_seed_n2, NUNID.1, 
                 into = c("st.1","county.1","unit.1","plot.1","invyr.1"), remove = FALSE)

hhh<-count(sdam2b,st.1,invyr.1,seed_count.1)
sdam2b$na<-ifelse(sdam2b$seed_count.1>0,"data","nodata")
iii<-count(sdam2b,st.1,invyr.1,na)


iii$state_inv<-paste0(iii$st.1,"-",iii$invyr.1)

agg1<-iii[which(iii$na=="data"),]
agg2<-iii[is.na(iii$na),]

agg3<-merge(agg1,agg2,by="state_inv",all=TRUE)


agg3$tot<-agg3$n.x+agg3$n.y
agg3$per_missing<-(agg3$n.y/agg3$tot)*100

agg3$per_missing2<-ifelse(is.na(agg3$n.x),100,
                          ifelse(is.na(agg3$n.y),0,agg3$per_missing))


agg4<-agg3[,c(1,2,3,5,9,12)]
colnames(agg4)<-c("STATE-INVYR","STATE","INVYR","Present","Missing",
                  "Percentage of missing data")



sdam2c<-separate(plots_seed_n2, NUNID.2, 
                 into = c("st.2","county.2","unit.2","plot.2","invyr.2"), remove = FALSE)

hhh2<-count(sdam2c,st.2,invyr.2,seed_count.2)
sdam2c$na<-ifelse(sdam2c$seed_count.2>0,"data","nodata")
iii2<-count(sdam2c,st.2,invyr.2,na)


iii2$state_inv2<-paste0(iii2$st.2,"-",iii2$invyr.2)

agg11<-iii2[which(iii2$na=="data"),]
agg21<-iii2[is.na(iii2$na),]

agg31<-merge(agg11,agg21,by="state_inv2",all=TRUE)


agg31$tot<-agg31$n.x+agg31$n.y
agg31$per_missing<-(agg31$n.y/agg31$tot)*100

agg31$per_missing2<-ifelse(is.na(agg31$n.x),100,
                           ifelse(is.na(agg31$n.y),0,agg31$per_missing))


agg41<-agg31[,c(1,2,3,5,9,12)]
colnames(agg41)<-c("STATE-INVYR2","STATE2","INVYR2","Present2","Missing2",
                   "Percentage of missing data2")


agg_both<-merge(agg4,agg41,by.x="STATE",by.y="STATE2")

write.csv(agg4,"pre_dist_seedling_presence.csv")
write.csv(agg41,"post_dist_seedling_presence.csv")

