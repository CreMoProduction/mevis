#mevis v0.2 alpha

#debug settings, use --FALSE-- value when running in RStudio

OS_environment = FALSE  #<-------EDIT HERE TO DEBUG MODE


if (OS_environment==TRUE) {
  #install.packages('plyr', repos = "http://cran.us.r-project.org")
  print("running in command prompt")
  options(repos = list(CRAN="http://cran.rstudio.com/"))
} else {
  print("runnning in RStudio environment")
  rm(list=setdiff(ls(), "OS_environment"))   #clear environment
}
#--------
print("Welcome to mevis")
#--------
#проверяю устнаволенные пакеты, отсутствуюшие устанавливаю
packages <- c("ggplot2", 
              "dplyr", 
              "readxl", 
              "gridExtra", #нужно для построения большого графика
              "tidyverse", #использую для определния пути к этому скрипту
              "yaml",      #для импорта настроек
              "progress",  #делаю progress bar
              "rio",       #экспорт xlsx файл
              "Hmisc",     #нужно для error bars в mean plot
              "ggrepel"    #для пометок точек на вулкан плот
              )
install.packages(setdiff(packages, rownames(installed.packages())))

#-----------
lapply(packages, require, character.only = TRUE)
#----------------
#Объявляю глобальные переменные


#---
Ncol= NULL
metabolitedata <- NULL
predataMetaboliteName<- c()
predata <- c()
data <- c()
#---------------- 
Difference=NULL
Pvalue=NULL
Metabolite= NULL
name_column = NULL
data_path = NULL
excel_sheet = NULL
dataset = NULL
#OS_environment = FALSE

#Импорт данных
  #---получаю путь к этому файлу
  if  (OS_environment==FALSE) { 
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
  } else {
    currentfillelocation <- "C:/mevis_data"
  }
  

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
  datapath = config$data_path
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
  
  plot_width= config$plot_width
  plot_height= config$plot_height 
  num_cols_grid= config$num_cols_grid      #кол-во колонок в grid plot
  num_grid= config$num_grid                #кол-во 
  plot_title_size= config$plot_title_size  #размер шрифта для надписей награфике
  plot_x_axis_lable_angle= config$plot_x_axis_lable_angle
  plot_x_axis_lable_horizontal_adjust= config$plot_x_axis_lable_horizontal_adjust
  plot_axis_lable_size= config$plot_axis_lable_size
  plot_axis_title_size= config$plot_axis_title_size
  

  
                                
  
  #---импорт excel файла
  open_popup_window = config$open_popup_window
  if (open_popup_window==TRUE) {
    datapath <- choose.files(default = "", caption = "Select input data table (.xlsx)",
                                    multi = FALSE)
    print(paste("Loading excel file from: ", datapath))
    #sheet <- dlgInput("Enter a number", "Sheet1")$res
    if (length(excel_sheets(path = datapath))==1) {
      sheet= excel_sheets(path = datapath)
    } else {
      sheet= excel_sheet
      if (sheet %in% excel_sheets(path = datapath)== FALSE) {
        stop("could not find specified sheet in an excel workbook")
      }
    }
    dataset <- read_excel(datapath, sheet = sheet)
    Ncol= ncol(dataset)
  } else {
    #datapath="D://Ga Processed Data.xlsx"
    #sheet= "SIMCA 2.1"

    if (length(excel_sheets(path = datapath))==1) {
      sheet= excel_sheets(path = datapath)
    } else {
      sheet= excel_sheet
    }
    print(paste("Loading excel file from: ", datapath))
    dataset <- read_excel(datapath, sheet = sheet)
    Ncol= ncol(dataset)
  }
  #обработка ошибки если путь к файлу не указан
  if (length(datapath)==0) {
    stop("Could not load an excel file. Please, specify path to the file")
    }

#--определяю путь к выходным файлам
mainDir = datapath
mainDir=dirname(datapath)
mainFile<- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(datapath)) #удаляю расширение из имени файла
subDir <- paste("mevis_output -",mainFile)
subDir2 <- sub("CEST","",Sys.time())
subDir2<- gsub(" ", "_", subDir2)
subDir2<- gsub(":", "-", subDir2)
subDir3_median<- "median sort"
subDir3_mean<- "mean sort"

#------ 
#базовая фигня для данных
  colnames(dataset)
  TotalMetabolites<- dim(dataset)
  TotalMetabolites<- toString(TotalMetabolites[2])
  print(paste("Total metabolites found", TotalMetabolites))
  #обработка ошибки если не найдены метаболиты
  if (str_length(TotalMetabolites)==0) {
    stop("Could not load metabolites from file")
  }

#----------
#получаю названия групп
conditionN_column= NULL
  for (n in 1:length(condition_name_row)) {
    conditionN = data.frame(dataset[as.numeric(gsub(":.*","",condition_name_row[n])):as.numeric(gsub(".*:","",condition_name_row[n]))-1, name_column])
    conditionN_column = rbind(conditionN_column, conditionN)
  }
  Unique_Conditions = unique(conditionN_column)
  ChimicalName= names(conditionN)



Data_Fun <- function(Metabolite) {
  
  #print(paste("plot Metabolite No", Metabolite))
  
  #---- получаю величину  пика
  conditionValue_column= NULL
  for (n in 1:length(condition_name_row)) {
    conditionValueN = data.frame(dataset[as.numeric(gsub(":.*","",condition_name_row[n])):as.numeric(gsub(".*:","",condition_name_row[n]))-1, Metabolite])
    conditionValue_column = rbind(conditionValue_column, conditionValueN)
  }
  conditionValue_column
  
  #---- получаю имя метаболита МОЖНО УДАЛИТЬ
  columnName<-dimnames(conditionValue_column)
  metaboliteName<-toString(columnName[2])
  metaboliteName
  #---- сливаю 2 столбца в таблицу
  #metabolitedata<- data.frame(conditionN_column, conditionValue_column)
  metabolitedata<- data.frame(conditionValue_column)
  metabolitedata
  #colnames(metabolitedata)[2]="value"
  #colnames(metabolitedata)[2]
  #metabolitedata<-mutate(Q=conditionN_column, W=conditionValue_column)
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


predata=data.frame(conditionN_column,predata)

#rm(data_mean, data_median)

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
log10_pvalue_anova_data <- c()
log10_pvalue_kruskalwallis_data <- c()
fold_change_mean_data <- c()
fold_change_median_data <- c()

DifferenceNup_mean <- c()
DifferenceNdown_mean <- c()
DifferenceNup_median <- c()
DifferenceNdown_median <- c()
w0 <- c()
w1 <- c()


closest<-function(xv,sv){
  xv[which(abs(xv-sv)==max(abs(xv-sv)))] }


#алгоритм сортировки
for (i in 2:length(predata)) {
  df=data.frame(conditionN_column,predata[i])
  names(df)[1]= "Condition"
  names(df)[2]= "PeakArea"
  mean= aggregate(df[, 2], conditionN_column, mean) #получаю среднее
  colnames(mean)[1]="Condition"
  median= aggregate(df[, 2], conditionN_column, median) #получаю медиану
  colnames(median)[1]="Condition"
  stdev= aggregate(df[, 2], conditionN_column, sd) #получаю отклоненние
  colnames(stdev)[1]="Condition"
  df.aov <- aov(PeakArea ~ Condition, data = df) #получаю p-value через ANOVA
  df.aov= summary(df.aov)[[1]][["Pr(>F)"]][1]
  df.kruskal <- kruskal.test(PeakArea ~ Condition, data = df)  #получаю p-value через Kruskal Wallis
  df.kruskal= df.kruskal$p.value
  #считаю time fold change
  for (n in 1:nrow(Unique_Conditions)) {
    if (Unique_Conditions[n,1]!=prime_condition_name) {
      DifferenceNup_mean= c(DifferenceNup_mean, mean[mean$Condition==Unique_Conditions[n,1],][1,2]/mean[mean$Condition==prime_condition_name,][1,2])
      DifferenceNdown_mean= c(DifferenceNdown_mean, mean[mean$Condition==prime_condition_name,][1,2]/mean[mean$Condition==Unique_Conditions[n,1],][1,2])
      DifferenceNup_median= c(DifferenceNup_median, median[median$Condition==Unique_Conditions[n,1],][1,2]/median[median$Condition==prime_condition_name,][1,2])
      DifferenceNdown_median= c(DifferenceNdown_median, median[median$Condition==prime_condition_name,][1,2]/median[median$Condition==Unique_Conditions[n,1],][1,2])
    } else {
    }
  }
  
  
  for (m in 1:length(DifferenceNup_mean)) {
    if (DifferenceNup_mean[m]<1) {
      w0=c(w0, 1/DifferenceNup_mean[m]*-1)
    } else (
      w0=c(w0, DifferenceNup_mean[m])
    )
  }
  w0=closest(w0, 0)
  if (w0 <1) {
    DifferenceNup_mean=abs(1/w0)
  } else {
    DifferenceNup_mean=w0
  }
  
  
  for (m in 1:length(DifferenceNup_median)) {
    if (DifferenceNup_median[m]<1) {
      w1=c(w1, 1/DifferenceNup_median[m]*-1)
    } else (
      w1=c(w1, DifferenceNup_median[m])
    )
  }
  w1=closest(w1, 0)
  if (w1 <1) {
    DifferenceNup_median=abs(1/w1)
  } else {
    DifferenceNup_median=w1
  }

  DifferenceNdown_mean=closest(DifferenceNdown_mean, 0)
  DifferenceNdown_median=closest(DifferenceNdown_median, 0)
  #соритрую
  if (DifferenceNdown_mean >= Difference & df.aov <= Pvalue) {
    data_mean=c(data_mean, predata[i])
    meandata= c(meandata, mean)
    sd_mean_data= c(sd_mean_data, stdev)
    pvalue_anova_data= c(pvalue_anova_data, df.aov)
    log2_fold_change_mean_data= c(log2_fold_change_mean_data, log2(DifferenceNup_mean))
    log10_pvalue_anova_data= c(log10_pvalue_anova_data, log10(df.aov))
    fold_change_mean_data= c(fold_change_mean_data, abs(DifferenceNdown_mean))
    
  }
  
  #можно удалить
  length(data_mean)
  length(meandata)
  length(sd_mean_data)
  length(pvalue_anova_data)

  #print(paste("log2 FC",length(log2_fold_change_mean_data)))
  #print(paste("log10 p", length(log10_pvalue_anova_data)))
  print("")
  length(fold_change_mean_data)
  length(r)
  #------------
  
  if (DifferenceNdown_median >= Difference & df.kruskal <= Pvalue) {
    data_median=c(data_median, predata[i])
    mediandata= c(mediandata, median)
    sd_median_data= c(sd_median_data, stdev)
    pvalue_kruskalwallis_data= c(pvalue_kruskalwallis_data, df.kruskal)
    log10_pvalue_kruskalwallis_data= c(log10_pvalue_kruskalwallis_data, log10(df.kruskal))
    log2_fold_change_median_data= c(log2_fold_change_median_data, log2(DifferenceNup_median))
    fold_change_median_data= c(fold_change_median_data, abs(DifferenceNdown_median))
  
  }
  DifferenceNup_mean <- NULL
  DifferenceNdown_mean <- NULL
  DifferenceNup_median <- NULL
  DifferenceNdown_median <- NULL
  w0 <- NULL
  w1 <- NULL
}



#------
#создаю xlsx таблицу
export_xlsx <- function(in_data, fc, pvalue, avg_data, sd) {
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
  q=data.frame(colnames(data.frame(in_data)))
  colnames(q)=NULL
  q=rbind("Metabolite", q)
  w=data.frame(fc)
  #w <- data.frame(apply(w, 2, function(x) as.numeric(as.character(x)))) #конвертирую character to numeric
  colnames(w)=NULL
  w=rbind("FC", w)
  e=data.frame(pvalue)
  #e <- data.frame(apply(e, 2, function(x) as.numeric(as.character(x)))) #конвертирую character to numeric
  colnames(e)=NULL
  e=rbind("p value", e)
  r=NULL
  for (i in 2:length(avg_data)) {
      if((i %% 2) == 0) {
        r= c(r, avg_data[i])
        }
  }
  r=rbind(z1,t(data.frame(r)))
  #r <- data.frame(apply(r, 2, function(x) as.numeric(as.character(x)))) #конвертирую character to numeric
  t=NULL
  for (i in 2:length(sd)) {
    if((i %% 2) == 0) {
      t= c(t, sd[i])
    }
  }
  t=rbind(z2, t(data.frame(t)))
  File= cbind(q,w,e,r,t)
  colnames(File) <- File[1,]
  File <- File[-1, ] 
  return(File)
}








#------
#строю графики и записываю в список p
p <- list()
p_median <- list()
p_median_grid <- list()
p_mean <- list()
p_mean_grid <- list()
#================================================
#=====================EDIT HERE==================
Plot <- function(P_data, pvalue, FC, T_size, axis_x_angle, axis_hjust, axis_text, axis_title, switch)  {
  if (switch==1) {
    error_range= geom_boxplot(color = "grey70", width=0.2)
  } else {
    error_range= stat_summary(fun.data="mean_sdl", fun.args = list(mult=1), 
                              geom="pointrange", width=0.2, color="grey70", size=0.5)
  }
  for (i in 1:length(P_data)) {
  plotdata=cbind(conditionN_column, data.frame(P_data[i]))
  x=names(plotdata)[1]
  y=names(plotdata)[2]
   g<- ggplot(plotdata, aes_string(x=x, y=y, color=x, shape=x)) +
    theme_classic()+
    error_range+
    geom_jitter(width = 0.25, size=3)+ 
    labs(y="peak area", 
         x="",
         title=colnames(plotdata)[2],
         subtitle=paste("p value=", format(round(pvalue[i], digits = 7), scientific=TRUE), "\nFC=", format(round(FC[i], digits = 2), scientific=FALSE)),
    )+
    theme(plot.title = element_text(size=T_size), 
          plot.subtitle = element_text(size=T_size/1.3),
          legend.position = "none",                       #удаляю легенду
          axis.text.x = element_text(angle = axis_x_angle, hjust=axis_hjust),
          axis.text=element_text(size=axis_text),
          axis.title=element_text(size=axis_title)
          ) 
  #geom_text(aes(label=round(value, 2)), size=3)+ #указываю величину площади пика и округляю ее
  #ylim(0, 5000) #указываю мин макс значения для y axis
   g
   p[[i]]<-g
  }
  return(p)
}
#================================================
#================================================

vPlot <- function() {}
v_plotdata=cbind(log2_fold_change_mean_data, abs(log10_pvalue_anova_data))
v_plotdata <- data.frame(apply(v_plotdata, 2, function(x) as.numeric(as.character(x))))
v_plotdata=cbind(colnames(data.frame(data_mean)), v_plotdata)
colnames(v_plotdata)[1]="metabolite"
colnames(v_plotdata)[2]
colnames(v_plotdata)[3]="log10_p_value"
x=names(v_plotdata)[2]
y=names(v_plotdata)[3]
g<- ggplot(data=v_plotdata, aes_string(x=x, y=y, label=names(v_plotdata)[1])) +
  geom_point() + 
  theme_minimal() +
  
  scale_color_manual(values=c("blue", "black", "red")) +
  geom_vline(xintercept=c(-log2(1.6), log2(1.6)), col="red") +
  geom_hline(yintercept=-log10(0.05), col="red")+
  labs(y="log10 p value", 
       x="log2 fold change",
       )

g






export(v_plotdata, "D://q.xlsx")




p_median= Plot(data_median,
               pvalue_kruskalwallis_data,
               fold_change_median_data,
               plot_title_size,
               plot_x_axis_lable_angle,
               plot_x_axis_lable_horizontal_adjust,
               plot_axis_lable_size,
               plot_axis_title_size,
               1
               )
p_median_grid= Plot(data_median,
                    pvalue_kruskalwallis_data,
                    fold_change_median_data,
                    plot_title_size+5,
                    plot_x_axis_lable_angle,
                    plot_x_axis_lable_horizontal_adjust,
                    plot_axis_lable_size,
                    plot_axis_title_size, 
                    1
                    )
p_median_volcano= NULL
p_mean= Plot(data_mean,
             pvalue_anova_data,
             fold_change_mean_data,
             plot_title_size,
             plot_x_axis_lable_angle,
             plot_x_axis_lable_horizontal_adjust,
             plot_axis_lable_size,
             plot_axis_title_size,
             0
             )
p_mean_grid= Plot(data_mean,
                  pvalue_anova_data,
                  fold_change_mean_data,
                  plot_title_size+5,
                  plot_x_axis_lable_angle,
                  plot_x_axis_lable_horizontal_adjust,
                  plot_axis_lable_size,
                  plot_axis_title_size,
                  0
                  )
p_mean_volcano= NULL

plotdataq=cbind(conditionN_column, data.frame(data_mean[2]))
#--------можно удалить
#cbind(Unique_Conditions, data.frame(meandata[2])-data.frame(sd_mean_data[2]))
#data_mean[2]
#----------
#-------------
#создаю папки
ifelse(!dir.exists(file.path(mainDir, subDir)), dir.create(file.path(mainDir, subDir)), FALSE)
ifelse(!dir.exists(file.path(mainDir, subDir, subDir2)), dir.create(file.path(mainDir, subDir, subDir2)), FALSE)
ifelse(!dir.exists(file.path(mainDir, subDir, subDir2, subDir3_median)), dir.create(file.path(mainDir, subDir, subDir2, subDir3_median)), FALSE)
ifelse(!dir.exists(file.path(mainDir, subDir, subDir2, subDir3_mean)), dir.create(file.path(mainDir, subDir, subDir2, subDir3_mean)), FALSE)
print(paste("Path to save output:", file.path(mainDir, subDir, subDir2)))
#------------
#сохраняю xlsx файлы
if (export_median_xls_list==TRUE) {
  print("saving median xlsx")
  File= export_xlsx(data_median, fold_change_median_data, pvalue_kruskalwallis_data, mediandata, sd_median_data)
  filepath=file.path(mainDir, subDir, subDir2, paste("median_sort_list",".xlsx", sep=""))
  export(File, filepath)
  File=NULL
}

if (export_mean_xls_list==TRUE) {
  print("saving mean xlsx")
  File= export_xlsx(data_mean, fold_change_mean_data, pvalue_anova_data, meandata, sd_mean_data)
  filepath=file.path(mainDir, subDir, subDir2, paste("mean_sort_list",".xlsx", sep=""))
  export(File, filepath)
  File=NULL
}
#------------


#------------
#сохраняю картинки из списков p

#сохраняю volcano plot



if (export_median_plot==TRUE) {
  #сохраняю median картинки
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(p_median))
  print("saving median plot")
  for (i in 1:length(p_median)) {
    pb$tick() #progress bar
    Sys.sleep(1 / length(p_median))
    ggsave(p_median[[i]], file=file.path(mainDir, subDir, subDir2, subDir3_median, paste(colnames(data.frame(data_median[i])), ".png", sep="")), width = plot_width, height = plot_height, units = "px")
  }
}

if (export_median_grid_plot==TRUE) {
  Ncol= num_cols_grid
  Width= Ncol*num_grid
  Height= length(p_median)/Ncol*(Width/Ncol)
  print("saving median grid plot, please wait")
  ggsave(do.call(grid.arrange, c(p_median_grid, ncol = Ncol)), file=file.path(mainDir, subDir, subDir2,paste("median_grid_plot",".png", sep="")), width = Width, height = Height, units = "cm")
}

if (export_median_volcano_plot==TRUE) {
  print("saving median volcano plot, please wait")
}

if (export_mean_plot==TRUE) {
  #сохраняю mean картинки
  pb <- progress_bar$new(format = "[:bar] :current/:total (:percent)", total = length(p_mean))
  print("saving mean plot")
  for (i in 1:length(p_mean)) {
    pb$tick() #progress bar
    Sys.sleep(1 / length(p_mean))
    ggsave(p_mean[[i]], file=file.path(mainDir, subDir, subDir2, subDir3_mean, paste(colnames(data.frame(data_mean[i])), ".png", sep="")), width = plot_width, height = plot_height, units = "px")
  }
}

if (export_mean_grid_plot==TRUE) {
  Ncol= num_cols_grid
  Width= Ncol*num_grid
  Height= length(p_mean)/Ncol*(Width/Ncol)
  print("saving mean grid plot, please wait")
  ggsave(do.call(grid.arrange, c(p_mean_grid, ncol = Ncol)), file=file.path(mainDir, subDir, subDir2,paste("mean_grid_plot",".png", sep="")), width = Width, height = Height, units = "cm")
}

if (export_mean_volcano_plot==TRUE) {
  print("saving mean volcano plot, please wait")
}

Sys.sleep(3)
print("I'm done")












 

