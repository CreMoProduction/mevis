#mevis v0.1 alpha

library("readxl")
library(gridExtra)
library(ggplot2)
library(dplyr)
#----------------Пользовательские переменные 
Difference=2
Pvalue=0.5
Metabolite<-8
dataset <- read_excel("D:\\Ga Processed Data.xlsx", sheet ="SIMCA 2.1")
#----------------Объявляю глобальные переменные
Ncol= ncol(dataset)
metabolitedata <- NULL
predataMetaboliteName<- c()
predata <- c()
data <- c()
nrowGaN=NULL        #кол-во строк Галлий
nrowCtrlN=NULL      #кол-во строк контроля
nrowTotalN=NULL     #кол-во строок суммарно
meanGaN
meanCtrlN
#---------------- 

#------ базовая фигня для данных
  colnames(dataset)
  TotalMetabolites<- dim(dataset)
  TotalMetabolites<- toString(TotalMetabolites[2])
  dataset
  TotalMetabolites




#Ф-ция подготовки данных
  
#---- получаю названия групп Control, Ga500
gaN<- data.frame(dataset[31:36, 4])
ctrlN<- data.frame(dataset[1:6, 4])
chemicalN_column<- rbind(gaN, ctrlN)
chemicalN_column
nrowGaN= nrow(gaN)
nrowCtrlN= nrow(ctrlN)
nrowTotalN=nrow(chemicalN_column)


Data_Fun <- function(Metabolite) {
  
  #print(paste("plot Metabolite No", Metabolite))
  
  #---- получаю величину  пика
  gaValue<- data.frame(dataset[31:36, Metabolite])
  gaValue
  ctrlValue<- data.frame(dataset[1:6, Metabolite])
  ctrlValue
  chemicalValue_column <-rbind(gaValue, ctrlValue)
  chemicalValue_column
  
  #---- получаю имя метаболита МОЖНО УДАЛИТЬ
  columnName<-dimnames(chemicalValue_column)
  metaboliteName<-toString(columnName[2])
  metaboliteName
  #---- сливаю 2 столбца в таблицу
  metabolitedata<- data.frame(chemicalN_column, chemicalValue_column)
  metabolitedata
  colnames(metabolitedata)[2]="value"
  colnames(metabolitedata)[2]
  #metabolitedata<-mutate(Q=chemicalN_column, W=chemicalValue_column)
  #---
  return(metabolitedata)
}    
#------
rm(predata)
rm(predataMetaboliteName)
predata
predataMetaboliteName
for (i in 8:Ncol) {
  Metabolite=i
  metabolitedata=Data_Fun(i)
  predata <-c(predata, metabolitedata)
  #print(predata)
  metaboliteName<-colnames(dataset)[i]
  #print(metaboliteName)
  predataMetaboliteName=c(predataMetaboliteName, metaboliteName)
}

#считаю среднее для метаболита и ищу различия
rm(data)
data <- c()
data= c(data, predata[1])
for (i in 1:length(predata)) {
  if((i %% 2) == 0) {
    df= data.frame(predata[i])
    meanGaN=mean(df[1:nrowGaN,])
    meanCtrlN=mean(df[nrowGaN+1:nrowGaN,])
    DifferenceNup=meanGaN/meanCtrlN
    DifferenceNdown=meanCtrlN/meanGaN
    tTest=t.test(cbind(data.frame(df[1:nrowGaN,]), data.frame(df[nrowGaN+1:nrowGaN,])))[["p.value"]]
    DifferenceNup
    DifferenceNdown
    
    if (DifferenceNup>=Difference | DifferenceNdown>=Difference & tTest<=Pvalue){ 
           data= c(data, predata[i])
           print(paste("No",i,"| p value ",tTest, ". Difference Up and Down", DifferenceNup, " ", DifferenceNdown))
           } else {0}
  } else {
    print(paste("No",i,"| does not meet criterias"))
  }
  
}

for (i in 1:length(data)) {}
plotdata=cbind(chemicalN_column, data.frame(data[2]))

g <- ggplot(plotdata, aes(x=Chemical, y=value, color=Chemical, shape=Chemical))
g + theme_classic()+
  geom_boxplot(color = "grey70", width=0.2)+
  geom_jitter(width = 0.25, size=3)+ 
  labs(subtitle="metaboliteName", 
       y="y", 
       x="",
       title=paste("p value" )
  )+
  theme(plot.title = element_text(size=10), 
        plot.subtitle = element_text(size=10),
        legend.position = "none") #удаляю легенду
#geom_text(aes(label=round(value, 2)), size=3)+ #указываю величину площади пика и округляю ее
#ylim(0, 5000) #указываю мин макс значения для y axis







{#ggplot



p <- list()
for (i in 1:10){
  Metabolite=i
  
  p[[i]]<-Plot_Fun(Metabolite)
  #print(Metabolite)
}
do.call(grid.arrange, c(p, ncol = 4))
#------
}

#подготавливаю данные для t-теста

gaValue_t_test= gaValue
colnames(gaValue_t_test)<- "gaValue"
gaValue_t_test

ctrlValue_t_test= ctrlValue
colnames(ctrlValue_t_test)<- "ctrlValue"
ctrlValue_t_test

df_t_test= cbind(gaValue_t_test, ctrlValue_t_test)
df_t_test
#t(df_t_test)  #транспонировать
colnames(df_t_test)


t.test(df_t_test)
t.test(df_t_test)[["p.value"]]
ifelse(t.test(df_t_test)[["p.value"]]< 0.05,
       1,
       0
       )





rm(Q)


 





