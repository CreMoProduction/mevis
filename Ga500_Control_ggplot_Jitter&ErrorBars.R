#mevis v0.2 alpha

print("Welcom to mevis")
#--------
#проверяю устнаволенные пакеты, отсутствуюшие устанавливаю
packages <- c("ggplot2", "dplyr", "readxl", "gridExtra", "tidyverse", "yaml", "svDialogs", "progress")
install.packages(setdiff(packages, rownames(installed.packages())))

#-----------
library(readxl)
library(gridExtra) #нужно для построения большого графика
library(ggplot2)
library(dplyr)
library(tidyverse) #использую для определния пути к этому скрипту
library(yaml)  #для импорта настроек
library(svDialogs) #для окна ввода popup promt window
library(progress) #делаю proogress bar

#----------------
#Объявляю глобальные переменные
#--определяю путь к выходным файлам
mainDir = datapath
mainDir=dirname(datapath)
mainFile<- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(datapath)) #удаляю расширение из имени файла
subDir <- paste("mevis_output -",mainFile)
subDir2 <- sub("CEST","",Sys.time())
subDir2<- gsub(" ", "_", subDir2)
subDir2<- gsub(":", "-", subDir2)

#---
Ncol= ncol(dataset)
metabolitedata <- NULL
predataMetaboliteName<- c()
predata <- c()
data <- c()
nrowGaN=NULL        #кол-во строк Галлий
nrowCtrlN=NULL      #кол-во строк контроля
nrowTotalN=NULL     #кол-во строок суммарно
meanGaN=NULL        #среднее дла Ga
meanCtrlN=NULL      #среднее дла Ctrl
#---------------- 
Difference=NULL
Pvalue=NULL
Metabolite= NULL
ctrl_name_row = NULL
sample_name_row = NULL
name_column = NULL
data_path = NULL
excel_sheet = NULL
dataset = NULL

{#Импорт данных
  #---получаю путь к этому файлу
  getCurrentFileLocation <-  function(){
    this_file <- commandArgs() %>% 
      tibble::enframe(name = NULL) %>%
      tidyr::separate(col=value, into=c("key", "value"), sep="=", fill='right') %>%
      dplyr::filter(key == "--file") %>%
      dplyr::pull(value)
    if (length(this_file)==0)
    {
      this_file <- rstudioapi::getSourceEditorContext()$path
    }
    return(dirname(this_file))
  }
  currentfillelocation = getCurrentFileLocation()
  
  #---импортирую настройки из файла config.yml
  config = yaml.load_file(file.path(currentfillelocation, "config.yml"))
  
  #----------------
  #Пользовательские переменные 
  Difference=config$difference
  Pvalue=config$p_value
  
  Metabolite= config$metabolite_start_column
  ctrl_name_row= config$ctrl_name_row
  sample_name_row = config$sample_name_row
  name_column = as.numeric(config$name_column)
  data_path = config$data_path
  excel_sheet = config$excel_sheet
  
  ctrl_name_row=as.numeric(gsub(":.*","",ctrl_name_row)):as.numeric(gsub(".*:","",ctrl_name_row))
  sample_name_row= as.numeric(gsub(":.*","",sample_name_row)):as.numeric(gsub(".*:","",sample_name_row))
  
  #---импорт excel файла
  open_popup_window = config$open_popup_window
  if (open_popup_window==TRUE) {
    datapath <- choose.files(default = "", caption = "Select input data (.xlsx)",
                                    multi = FALSE)
    sheet <- dlgInput("Enter a number", "Sheet1")$res
    dataset <- read_excel(datapath, sheet = sheet)
  } else {
    #datapath="D://Ga Processed Data.xlsx"
    #sheet= "SIMCA 2.1"
    datapath=data_path
    sheet= excel_sheet
    dataset <- read_excel(datapath, sheet = sheet)
  }
}


#------ 
#базовая фигня для данных
  colnames(dataset)
  TotalMetabolites<- dim(dataset)
  TotalMetabolites<- toString(TotalMetabolites[2])
  dataset
  TotalMetabolites

#----------
#получаю названия групп Control, Ga500
gaN<- data.frame(dataset[sample_name_row, name_column])
ctrlN<- data.frame(dataset[ctrl_name_row, name_column])
chemicalN_column<- rbind(gaN, ctrlN)
chemicalN_column
nrowGaN= nrow(gaN)
nrowCtrlN= nrow(ctrlN)
nrowTotalN=nrow(chemicalN_column)


Data_Fun <- function(Metabolite) {
  
  #print(paste("plot Metabolite No", Metabolite))
  
  #---- получаю величину  пика
  gaValue<- data.frame(dataset[sample_name_row, Metabolite])
  gaValue
  ctrlValue<- data.frame(dataset[ctrl_name_row, Metabolite])
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
  #colnames(metabolitedata)[2]="value"
  #colnames(metabolitedata)[2]
  #metabolitedata<-mutate(Q=chemicalN_column, W=chemicalValue_column)
  #---
  return(metabolitedata)
}    
#------
#подготавливаю данные
rm(predata)
rm(predataMetaboliteName)
predata=NULL
predataMetaboliteName=NULL
for (i in Metabolite:Ncol) {
  metabolitedata=Data_Fun(i)
  predata <-c(predata, metabolitedata)
  #print(predata)
  metaboliteName<-colnames(dataset)[i]
  #print(metaboliteName)
}

#------
#считаю среднее, p value для метаболита и ищу различия
rm(data)
rm(pvaluedata)
data <- c()
pvaluedata <- c()
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
           pvaluedata= c(pvaluedata, tTest)
           print(paste(colnames(df)," | p value ",tTest, ". Difference Up and Down", DifferenceNup, " ", DifferenceNdown))
           } else {0}
  } else {
    print(paste(colnames(df)," | does not meet criterias"))
  }
  
}
#------
#строю графики и записываю в список p
p <- list()
for (i in 2:length(data)) {
plotdata=cbind(chemicalN_column, data.frame(data[i]))
x=names(plotdata)[1]
y=names(plotdata)[2]
 g<- ggplot(plotdata, aes_string(x=x, y=y, color=x, shape=x)) +
  theme_classic()+
  geom_boxplot(color = "grey70", width=0.2)+
  geom_jitter(width = 0.25, size=3)+ 
  labs(subtitle=colnames(plotdata)[2], 
       y="peak area", 
       x="",
       title=paste("p value=", format(round( pvaluedata[i-1], digits = 10), scientific=FALSE))
  )+
  theme(plot.title = element_text(size=7), 
        plot.subtitle = element_text(size=7),
        legend.position = "none") #удаляю легенду
#geom_text(aes(label=round(value, 2)), size=3)+ #указываю величину площади пика и округляю ее
#ylim(0, 5000) #указываю мин макс значения для y axis
 g
p[[i-1]]<-g
}
#print(p)



#-------------
#создаю папки
ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
ifelse(!dir.exists(file.path(mainDir, subDir, subDir2)), dir.create(file.path(mainDir, subDir, subDir2)), FALSE)
file.path(mainDir, subDir, subDir2)
#------------
#сохраняю картинки
Ncol= 8
Width= Ncol*10
Height= length(data)/Ncol*(Width/Ncol)

pb <- progress_bar$new(total = length(p))
print("saving grid plot")
ggsave(do.call(grid.arrange, c(p, ncol = Ncol)), file=file.path(mainDir, subDir, subDir2,paste("grid_layout", i-1,".png")), width = Width, height = Height, units = "cm")
print("saving each plots")
for (i in 1:length(p)) {
  pb$tick() #progress bar
  Sys.sleep(1 / length(p))
  ggsave(p[[i]], file=file.path(mainDir, subDir, subDir2, paste("A", i-1,".png")), width = 800, height = 900, units = "px")
}

print("I'm done")












 





