# mevis  
![](https://img.shields.io/badge/-made%20with%20R-blue) [![Windows](https://svgshare.com/i/ZhY.svg)](https://svgshare.com/i/ZhY.svg)

Compare multiple peak area of annotated metabolites and sort it using criteria, visualize peak area of sorted metabolites among conditions in jitter plots, export dadatable of sorted metabolites in excel file, export volcano plot (log2FC to log10p-value) baesd on metabolites from sorted list.

----------  
### Sorting algorithm
- calculates p value for parametric and non-parametric data using ANOVA and Kruskal Wallis tests respectively
- calcualtes maximum time fold change (FC) for prior condition and the rest of used conditions  
  
 
User input a prior condition name for FC calculate.
Calculates peak areas for all conditions using median or average.
1. Checks if the calculated FC meets minimum user's FC.
2. Сhecks if the calculated p value below the maximum user's p value.   
  
  

Time Fold Change (FC) calculation is a highest qutient between prime condition and a condition with lower peak area.
1. Exports jitter plot for parametric and non-parametric data [`condition vs peak area`]
2. Plots volcano plot for parametric and non-parametric data [`volcano plot of log2FC vs log10p-value`]
3. Exports xlsx file with sorted metabolite list with the following columns:
```
- metabolite name 
- FC based on Median/Mean
- p-value based on Median/Mean
- Median/Mean peak area for compared conditions
- St.Dev for compared conditions
```
  
---------- 
### How to run
By default mevis requires installed R on your computer in `C'\Proogram Files\R...` directory. 
[^note]:
Note! Different R location is not allowed and causes an error. 

Make sure you have installed R, if not download [from here](https://cran.r-project.org/bin/windows/base/).
1. Download `mevis.zip` and unzip it.
2. Enter user's settings in `config.yml` following included tips in config file.
3. Launch `mevis.bat` file   
4. If it is needed you can customize ggplot function in `hostscript.R` file
	-Code to edit is inside the following commetns code: 
	```
	#=====================EDIT HERE================== 
	Plot <- function() {}
	#================================================
	```
> To run **mevis** in debug mode from [RStudio](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwiag7ih_cL0AhVCSfEDHTi5A24QFnoECAkQAQ&url=https%3A%2F%2Fwww.rstudio.com%2F&usg=AOvVaw1bt9MYkG-ySe7hgo9R8XTb) set `OS_enviroment` variable to `TRUE` in `hostscript.R` otherwise run it in command prompt by default.

**mevis** requires listed below open source [CRAN](https://cran.r-project.org) packages: 
```
- ggplot2 
- dplyr 
- readxl 
- gridExtra 
- tidyverse 
- yaml      
- progress  
- rio       
- Hmisc
```  
> All output data will be saved in the same directory to input xlsx data and named as "mevis_output"
  
For example: 
- input data path: `D:\input_data.xlsx`
- output data path: `D:\mevis_output - input_data\[date & time when saved]\...`

