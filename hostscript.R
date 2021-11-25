#mevis v0.2 alpha

print("Welcome to mevis")
#--------
rm(list=ls()) #clear environment
#проверяю устнаволенные пакеты, отсутствуюшие устанавливаю
packages <- c("ggplot2", 
              "dplyr", 
              "readxl", 
              "gridExtra", #нужно для построения большого графика
              "tidyverse", #использую для определния пути к этому скрипту
              "yaml",      #для импорта настроек
              "svDialogs", #для окна ввода popup prompt window
              "progress",  #делаю progress bar
              "rio"        #экспорт xlsx файл
              )
install.packages(setdiff(packages, rownames(installed.packages())))

#-----------

#---можно удалить-00000000000
#library(readxl)
#library(gridExtra) #нужно для построения большого графика
#library(ggplot2)
#library(dplyr)
#library(tidyverse) #использую для определния пути к этому скрипту
#library(yaml)  #для импорта настроек
#library(svDialogs) #для окна ввода popup prompt window
#library(progress) #делаю progress bar
#---
lapply(packages, require, character.only = TRUE)

#----------------
#Объявляю глобальные переменные


#---
Ncol= NULL
metabolitedata <- NULL
predataMetaboliteName<- c()
predata <- c()
data <- c()
nrowGaN=NULL        #кол-во строк Галлий
nrowCtrlN=NULL      #кол-во строк контроля
nrowTotalN=NULL     #кол-во строк суммарно
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

#Импорт данных
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
  currentfillelocation = gsub("/res","",currentfillelocation) 
  
  #---импортирую настройки из файла config.yml
  config = yaml.load_file(file.path(currentfillelocation, "config.yml"))
  
  #----------------
  #Пользовательские переменные 
  Difference=config$fold_change
  Pvalue=config$p_value
  
  Metabolite= config$metabolite_start_column
  
  prime_condition_name= config$prime_condition_name
  metabolite_list_column_enabled= config$metabolite_list_column_enabled
  metabolite_list_column= config$metabolite_list_column
  
  condition_name_row = as.list(strsplit(config$condition_name_row, ",")[[1]])
  name_column = as.numeric(config$condition_name_column)
  data_path = config$data_path
  excel_sheet = config$excel_sheet
  
  metabolite_list_column_enabled= config$metabolite_list_column_enabled
  metabolite_list_column= as.list(strsplit(config$metabolite_list_column, ",")[[1]])
  
  #export data based on Kruskal Wallis
  export_median_plot= config$export_median_plot
  export_median_grid_plot= config$export_median_grid_plot
  export_median_volcano_plot= config$export_median_volcano_plot
  export_median_xls_list= config$export_median_xls_list
  #export data based on ANOVA
  export_mean_plot= config$export_mean_plot
  export_mean_grid_plot= config$export_mean_grid_plot
  export_mean_volcano_plot= config$export_mean_volcano_plot
  export_mean_xls_list= config$export_mean_xls_list
  
  width= config$width
  height= config$height 
  num_cols_grid= config$num_cols_grid      #кол-во колонок в grid plot
  num_grid= config$num_grid                #кол-во 
  plot_title_size= config$plot_title_size  #размер шрифта для надписей награфике
  
  
  
                                
  
  #---импорт excel файла
  open_popup_window = config$open_popup_window
  if (open_popup_window==TRUE) {
    datapath <- choose.files(default = "", caption = "Select input data (.xlsx)",
                                    multi = FALSE)
    sheet <- dlgInput("Enter a number", "Sheet1")$res
    dataset <- read_excel(datapath, sheet = sheet)
    Ncol= ncol(dataset)
  } else {
    #datapath="D://Ga Processed Data.xlsx"
    #sheet= "SIMCA 2.1"
    datapath=data_path
    sheet= excel_sheet
    dataset <- read_excel(datapath, sheet = sheet)
    Ncol= ncol(dataset)
  }


#--определяю путь к выходным файлам
mainDir = datapath
mainDir=dirname(datapath)
mainFile<- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(datapath)) #удаляю расширение из имени файла
subDir <- paste("mevis_output -",mainFile)
subDir2 <- sub("CEST","",Sys.time())
subDir2<- gsub(" ", "_", subDir2)
subDir2<- gsub(":", "-", subDir2)

#------ 
#базовая фигня для данных
  colnames(dataset)
  TotalMetabolites<- dim(dataset)
  TotalMetabolites<- toString(TotalMetabolites[2])
  dataset
  TotalMetabolites

#----------
#получаю названия групп Control, Ga500
chemicalN_column= NULL
  for (n in 1:length(condition_name_row)) {
    chemicalN = data.frame(dataset[as.numeric(gsub(":.*","",condition_name_row[n])):as.numeric(gsub(".*:","",condition_name_row[n])), name_column])
    chemicalN_column = rbind(chemicalN_column, chemicalN)
  }
  Unique_Conditions = unique(chemicalN_column)
  ChimicalName= names(chemicalN)
#---кажется можно удалить-00000000000
#nrowGaN= nrow(gaN)
#nrowCtrlN= nrow(ctrlN)
#nrowTotalN=nrow(chemicalN_column)
#---


Data_Fun <- function(Metabolite) {
  
  #print(paste("plot Metabolite No", Metabolite))
  
  #---- получаю величину  пика
  chemicalValue_column= NULL
  for (n in 1:length(condition_name_row)) {
    chemicalValueN = data.frame(dataset[as.numeric(gsub(":.*","",condition_name_row[n])):as.numeric(gsub(".*:","",condition_name_row[n])), Metabolite])
    chemicalValue_column = rbind(chemicalValue_column, chemicalValueN)
  }
  chemicalValue_column
  
  #---- получаю имя метаболита МОЖНО УДАЛИТЬ
  columnName<-dimnames(chemicalValue_column)
  metaboliteName<-toString(columnName[2])
  metaboliteName
  #---- сливаю 2 столбца в таблицу
  #metabolitedata<- data.frame(chemicalN_column, chemicalValue_column)
  metabolitedata<- data.frame(chemicalValue_column)
  metabolitedata
  #colnames(metabolitedata)[2]="value"
  #colnames(metabolitedata)[2]
  #metabolitedata<-mutate(Q=chemicalN_column, W=chemicalValue_column)
  #---
  return(metabolitedata)
}    
#------
#подготавливаю данные метаболитов из колонок в двух опциях, если включен или выключен metabolite_list_column_enabled
rm(predata)
rm(predataMetaboliteName)
predata=NULL
predataMetaboliteName=NULL

if (metabolite_list_column_enabled!=TRUE) {
  for (i in Metabolite:Ncol) {
    metabolitedata=Data_Fun(i)
    predata <-c(predata, metabolitedata)
    predata = data.frame(predata)
    metaboliteName<-colnames(dataset)[i]
  }
} else {
  for (n in 1:length(metabolite_list_column)) {
    for (i in metabolite_list_column[n]) {
      for (i in as.numeric(gsub(":.*","",metabolite_list_column[n])):as.numeric(gsub(".*:","",metabolite_list_column[n]))) {
        metabolitedata=Data_Fun(i)
        predata <-c(predata, metabolitedata)
        predata = data.frame(predata)
        metaboliteName<-colnames(dataset)[i]

      }
    }
  }
}


predata=data.frame(chemicalN_column,predata)

rm(data_mean, data_median)

data_mean <- c()
data_median <- c()
meandata <- c()
mediandata <- c()
sd_mean_data <- c()
sd_median_data <- c()
pvalue_anova_data <- c()
pvalue_kruskalwallis_data <- c()
log2_fold_change_mean_data <- c()
log2_fold_change_median_data <- c()
log2_pvalue_anova_data <- c()
log2_pvalue_kruskalwallis_data <- c()
fold_change_mean_data <- c()
fold_change_median_data <- c()

#алгоритм сортировки
for (i in 2:length(predata)) {
  df=data.frame(chemicalN_column,predata[i])
  names(df)[1]= "Condition"
  names(df)[2]= "PeakArea"
  mean= aggregate(df[, 2], chemicalN_column, mean) #получаю среднее
  median= aggregate(df[, 2], chemicalN_column, median) #получаю медиану
  stdev= aggregate(df[, 2], chemicalN_column, sd) #получаю отклоненние
  df.aov <- aov(PeakArea ~ Condition, data = df) #получаю p-value через ANOVA
  df.aov= summary(df.aov)[[1]][["Pr(>F)"]][1]
  df.kruskal <- kruskal.test(PeakArea ~ Condition, data = df)  #получаю p-value через Kruskal Wallis
  df.kruskal= df.kruskal$p.value
  #считаю time fold change
  for (n in 1:nrow(Unique_Conditions)) {
    if (Unique_Conditions[n,1]!=prime_condition_name) {
      DifferenceNup_mean=mean[mean$Chemical==Unique_Conditions[n,1],][1,2]/mean[mean$Chemical==prime_condition_name,][1,2]
      DifferenceNdown_mean=mean[mean$Chemical==prime_condition_name,][1,2]/mean[mean$Chemical==Unique_Conditions[n,1],][1,2]
      DifferenceNup_median=median[median$Chemical==Unique_Conditions[n,1],][1,2]/median[median$Chemical==prime_condition_name,][1,2]
      DifferenceNdown_median=median[median$Chemical==prime_condition_name,][1,2]/median[median$Chemical==Unique_Conditions[n,1],][1,2]
    } else {
    }
  } 
  #соритрую
  if (DifferenceNup_mean>=Difference | DifferenceNdown_mean>=Difference) {
    if (df.aov<=Pvalue) {
      data_mean=c(data_mean, predata[i])
      meandata= c(meandata, mean)
      sd_mean_data= c(sd_mean_data, stdev)
      pvalue_anova_data= c(pvalue_anova_data, df.aov)
      log2_fold_change_mean_data= c(log2_fold_change_mean_data, log2(DifferenceNup_mean))
      log2_pvalue_anova_data= c(log2_pvalue_anova_data, log2(df.aov))
      fold_change_mean_data= c(fold_change_mean_data, max(DifferenceNup_mean, DifferenceNdown_mean))
    }
  }
  if (DifferenceNup_median>=Difference | DifferenceNdown_median>=Difference) {
    if (df.kruskal<=Pvalue) {
      data_median=c(data_median, predata[i])
      mediandata= c(mediandata, median)
      sd_median_data= c(sd_median_data, stdev)
      pvalue_kruskalwallis_data= c(pvalue_kruskalwallis_data, df.kruskal)
      log2_pvalue_kruskalwallis_data= c(log2_pvalue_kruskalwallis_data, log2(df.kruskal))
      log2_fold_change_median_data= c(log2_fold_change_median_data, log2(DifferenceNup_median))
      fold_change_median_data= c(fold_change_median_data, max(DifferenceNup_median, DifferenceNdown_median))
    }
  }
  
  
}

#------
#экспортирую xlsx


colnames(Unique_Conditions)=NULL 
z1=NULL
z2=NULL
z=t(Unique_Conditions)
for (i in 1:ncol(z)) {
  z1[i]=paste("Median ","'", z[i], "'", sep="")
  }
for (i in 1:ncol(z)) {
  z2[i]=paste("StDev ","'", z[i], "'", sep="")
}
q=data.frame(colnames(data.frame(data_median)))
colnames(q)=NULL
q=rbind("Metabolite", q)
w=data.frame(fold_change_median_data)
colnames(w)=NULL
w=rbind("FC", w)
e=data.frame(pvalue_kruskalwallis_data)
colnames(e)=NULL
e=rbind("p value", e)
r=NULL
for (i in 2:length(mediandata)) {
    if((i %% 2) == 0) {
      r= c(r, mediandata[i])
      }
}
r=rbind(z1,t(data.frame(r)))
t=NULL
for (i in 2:length(sd_median_data)) {
  if((i %% 2) == 0) {
    t= c(t, sd_median_data[i])
  }
}
t=rbind(z2, t(data.frame(t)))
File= cbind(q,w,e,r,t)


filepath=file.path(mainDir, subDir, subDir2, paste("A", i-1,".xlsx"))
export(File, filepath)






#------
#строю графики и записываю в список p
p <- list()
p_median <- list()
p_median_grid <- list()
p_mean <- list()
p_mean_grid <- list()

Plot <- function(P_data, pvalue, FC, T_size)  {
  print(data)
  for (i in 1:length(P_data)) {
  plotdata=cbind(chemicalN_column, data.frame(P_data[i]))
  x=names(plotdata)[1]
  y=names(plotdata)[2]
   g<- ggplot(plotdata, aes_string(x=x, y=y, color=x, shape=x)) +
    theme_classic()+
    geom_boxplot(color = "grey70", width=0.2)+
    geom_jitter(width = 0.25, size=3)+ 
    labs(y="peak area", 
         x="",
         title=colnames(plotdata)[2],
         subtitle=paste("p value=", format(round(pvalue[i], digits = 5), scientific=FALSE), "\nFC=", format(round(FC[i], digits = 3), scientific=FALSE)),
    )+
    theme(plot.title = element_text(size=T_size), 
          plot.subtitle = element_text(size=T_size/1.3),
          legend.position = "none") #удаляю легенду
  #geom_text(aes(label=round(value, 2)), size=3)+ #указываю величину площади пика и округляю ее
  #ylim(0, 5000) #указываю мин макс значения для y axis
   g
   p[[i]]<-g
  }
  return(p)
}

p_median= Plot(data_median, pvalue_kruskalwallis_data, fold_change_median_data, plot_title_size)
p_median_grid 
p_mean= Plot(data_mean, pvalue_anova_data, fold_change_mean_data)
p_mean_grid


#-------------
#создаю папки
ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
ifelse(!dir.exists(file.path(mainDir, subDir, subDir2)), dir.create(file.path(mainDir, subDir, subDir2)), FALSE)
file.path(mainDir, subDir, subDir2)
#------------
#сохраняю картинки из списка p
Ncol= num_cols_grid
Width= Ncol*10
Height= length(data)/Ncol*(Width/Ncol)



print("saving grid plot")
print("please wait")
#ggsave(do.call(grid.arrange, c(p, ncol = Ncol)), file=file.path(mainDir, subDir, subDir2,paste("grid_layout", i-1,".png")), width = Width, height = Height, units = "cm")


pb1 <- progress_bar$new(total = length(p_median))
print("saving individual plot")
for (i in 1:length(p_median)) {
  pb1$tick() #progress bar
  Sys.sleep(1 / length(p_median))
  ggsave(p_median[[i]], file=file.path(mainDir, subDir, subDir2, paste("A", i-1,".png")), width = 800, height = 900, units = "px")
}

print("I'm done")












 

