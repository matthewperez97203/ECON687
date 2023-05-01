library(data.table) #Version 1.12.4
library(bife) #Version 0.6
library(dplyr) #Version 0.8.3
library(lfe) #Version 2.8-3

rm(list=ls())
#setwd("Set_your_working_directory")
getwd()
setwd("/Users/matthewperez/Documents/ECON_687Project")

dartsdata<-fread("International.csv")
info_table <- as.data.frame(matrix(0,nrow=1,ncol=0))
info_table$TotLegs <- nrow(dartsdata%>% group_by(GameNo,LegNo) %>%  slice(1)) #total number of legs
info_table$TotGame <- nrow(dartsdata%>% group_by(GameNo) %>%  slice(1)) #total number of legs
info_table$No_Throws <- nrow(dartsdata) #number of throws
info_table$No_Players <- nrow(dartsdata%>% group_by(Playername) %>%  slice(1)) #number of players
info_table$Legs_Match <- info_table$TotLegs/info_table$TotGame
info_table$Throws_Leg <- info_table$No_Throws/info_table$TotLegs
info_table$AverageScore <- mean(dartsdata$score)
info_table$Firstthreescore <- mean(dartsdata$score[dartsdata$throw_no <= 3])
info_table$startwin <- mean(dartsdata$Win[dartsdata$Start==1])
info_table$finishop <-sum(dartsdata$MinFinishPre <= 1)
info_table$finishsuccess <- sum(dartsdata$MinFinishPre <= 1 & dartsdata$Win== 1 & dartsdata$RemainPost == 0)/sum(dartsdata$MinFinishPre <= 1)

print(info_table)
options(scipen = 999)
t(info_table)
df_intl <- gather(info_table, key = "Statistic", value = "value")
print(df_intl)


dartsdataSL<-fread("SuperLeague.csv")
info_table2 <- as.data.frame(matrix(0,nrow=1,ncol=0))
info_table2$TotLegs <- nrow(dartsdataSL%>% group_by(GameNo,LegNo) %>%  slice(1)) #total number of legs
info_table2$TotGame <- nrow(dartsdataSL%>% group_by(GameNo) %>%  slice(1)) #total number of legs
info_table2$No_Throws <- nrow(dartsdataSL) #number of throws
info_table2$No_Players <- nrow(dartsdataSL%>% group_by(Playername) %>%  slice(1)) #number of players
info_table2$Legs_Match <- info_table2$TotLegs/info_table2$TotGame
info_table2$Throws_Leg <- info_table2$No_Throws/info_table2$TotLegs
info_table2$AverageScore <- mean(dartsdataSL$score)
info_table2$Firstthreescore <- mean(dartsdataSL$score[dartsdataSL$throw_no <= 3])
info_table2$startwin <- mean(dartsdataSL$Win[dartsdataSL$Start==1])
info_table2$finishop <-sum(dartsdataSL$MinFinishPre <= 1)
info_table2$finishsuccess <- sum(dartsdataSL$MinFinishPre <= 1 & dartsdataSL$Win== 1 & dartsdataSL$RemainPost == 0)/sum(dartsdataSL$MinFinishPre <= 1)

print(info_table2)
options(scipen = 999)
t(info_table2)
df_sl <- gather(info_table2, key = "Statistic", value = "value")
print(df_sl)


dartsdataB<-fread("BICC.csv")
info_table3 <- as.data.frame(matrix(0,nrow=1,ncol=0))
info_table3$TotLegs <- nrow(dartsdataB%>% group_by(GameNo,LegNo) %>%  slice(1)) #total number of legs
info_table3$TotGame <- nrow(dartsdataB%>% group_by(GameNo) %>%  slice(1)) #total number of legs
info_table3$No_Throws <- nrow(dartsdataB) #number of throws
info_table3$No_Players <- nrow(dartsdataB%>% group_by(Playername) %>%  slice(1)) #number of players
info_table3$Legs_Match <- info_table3$TotLegs/info_table3$TotGame
info_table3$Throws_Leg <- info_table3$No_Throws/info_table3$TotLegs
info_table3$AverageScore <- mean(dartsdataB$score)
info_table3$Firstthreescore <- mean(dartsdataB$score[dartsdataB$throw_no <= 3])
info_table3$startwin <- mean(dartsdataB$Win[dartsdataB$Start==1])
info_table3$finishop <-sum(dartsdataB$MinFinishPre <= 1)
info_table3$finishsuccess <- sum(dartsdataB$MinFinishPre <= 1 & dartsdataB$Win== 1 & dartsdataB$RemainPost == 0)/sum(dartsdataB$MinFinishPre <= 1)

print(info_table3)
options(scipen = 999)
t(info_table3)
df_bicc <- gather(info_table3, key = "Statistic", value = "value")
print(df_bicc)

dartsdataY<-fread("Youth.csv")
info_table4 <- as.data.frame(matrix(0,nrow=1,ncol=0))
info_table4$TotLegs <- nrow(dartsdataY%>% group_by(GameNo,LegNo) %>%  slice(1)) #total number of legs
info_table4$TotGame <- nrow(dartsdataY%>% group_by(GameNo) %>%  slice(1)) #total number of legs
info_table4$No_Throws <- nrow(dartsdataY) #number of throws
info_table4$No_Players <- nrow(dartsdataY%>% group_by(Playername) %>%  slice(1)) #number of players
info_table4$Legs_Match <- info_table4$TotLegs/info_table4$TotGame
info_table4$Throws_Leg <- info_table4$No_Throws/info_table4$TotLegs
info_table4$AverageScore <- mean(dartsdataY$score)
info_table4$Firstthreescore <- mean(dartsdataY$score[dartsdataY$throw_no <= 3])
info_table4$startwin <- mean(dartsdataY$Win[dartsdataY$Start==1])
info_table4$finishop <-sum(dartsdataY$MinFinishPre <= 1)
info_table4$finishsuccess <- sum(dartsdataY$MinFinishPre <= 1 & dartsdataY$Win== 1 & dartsdataY$RemainPost == 0)/sum(dartsdataY$MinFinishPre <= 1)

print(info_table4)
options(scipen = 999)
t(info_table4)
df_y <- gather(info_table4, key = "Statistic", value = "value")
print(df_y)


Replication_Table<-cbind(df_intl,df_sl,df_y,df_bicc)
print(Replication_Table)
Replication_Table_Darts <- Replication_Table[,-c(3,5,7)]
print(Replication_Table_Darts)
names(Replication_Table_Darts) <- c("Variable", "International", "BICC", "SuperLeague", "Youth")
print(Replication_Table_Darts)
