#for (x in dice) {
#  print(x)
#}

library("readxl")
dataset <- read_excel("D:\\Ga Processed Data.xlsx", sheet ="SIMCA 2.1")

library(gridExtra)
library(ggplot2)
library(dplyr)
Metabolite<-19

#------ базовая фигня для данных
  colnames(dataset)
  TotalMetabolites<- dim(dataset)
  TotalMetabolites<- toString(TotalMetabolites[2])
  dataset
  TotalMetabolites


#Основная ф-ция постройки графика
Plot_Fun <- function(Metabolite) {
  
  print(paste("plot Metabolite No", Metabolite))
  
  #---- получаю названия групп Control, Ga500
  gaN<- data.frame(dataset[31:36, 4])
  ctrlN<- data.frame(dataset[1:6, 4])
  chemicalN_column<- rbind(gaN, ctrlN)
  chemicalN_column
  
  #---- получаю величину  пика
  gaValue<- data.frame(dataset[31:36, Metabolite])
  gaValue
  ctrlValue<- data.frame(dataset[1:6, Metabolite])
  ctrlValue
  chemicalValue_column <-rbind(gaValue, ctrlValue)
  chemicalValue_column
  
  #---- получаю имя метаболита
  columnName<-dimnames(chemicalValue_column)
  metaboliteName<-toString(columnName[2])
  metaboliteName
  #---- сливаю 2 столбца в таблицу
  plotdata<- data.frame(chemicalN_column, chemicalValue_column)
  plotdata
  colnames(plotdata)[2]="value"
  colnames(plotdata)[2]
  #plotdata<-mutate(Q=chemicalN_column, W=chemicalValue_column)
  #---
  
  
  
  g <- ggplot(plotdata, aes(x=Chemical, y=value, color=Chemical, shape=Chemical))
    g + theme_classic()+
    geom_boxplot(color = "grey70", width=0.2)+
    geom_jitter(width = 0.25, size=3)+ 
    labs(subtitle=metaboliteName, 
        y="y", 
        x="",
       title="Control and GaCit 500 uM of the"
       )+
    theme(plot.title = element_text(size=10), 
        plot.subtitle = element_text(size=10),
        legend.position = "none") #удаляю легенду
    #geom_text(aes(label=round(value, 2)), size=3)+ #указываю величину площади пика и округляю ее
    #ylim(0, 5000) #указываю мин макс значения для y axis
    
}    

#------
p <- list()
for (i in 1:10){
  Metabolite=i
  
  p[[i]]<-Plot_Fun(Metabolite)
  #print(Metabolite)
}
do.call(grid.arrange, c(p, ncol = 4))
#------

gaValue
ctrlValue

gaValue_t_test= gaValue
colnames(gaValue_t_test)<- "gaValue"
gaValue_t_test

ctrlValue_t_test= ctrlValue
colnames(ctrlValue_t_test)<- "ctrlValue"
ctrlValue_t_test

df_t_test= cbind(gaValue_t_test, ctrlValue_t_test)
df_t_test
t(df_t_test)
colnames(df_t_test)


t.test(gaValue)



set.seed(1)
x <- sample(1:10, 200, TRUE)
y <- 3 * x + rnorm(200, 0, 5)
plot(y ~ jitter(x, 1), pch = 15)

rm(Q)


 





