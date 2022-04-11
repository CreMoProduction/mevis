# mevis  
![](https://img.shields.io/badge/-made%20with%20R-blue) [![Windows](https://svgshare.com/i/ZhY.svg)](https://svgshare.com/i/ZhY.svg)

Univariate analysis for comparing peak area of filtered annotated metabolites. It discards unwanted metabolites that do not meet filter criteria. Visualize peak area of filtered metabolites among conditions in jitter plots, export dadatables of filtered metabolites in excel files, export volcano plot (log2(FC) to log10(p-value)) based on metabolites from filtered list, create stacked bar plot for filtered metabolites.

----------  
### Filtering
- calculates p value for parametric and non-parametric data using ANOVA and Kruskal Wallis tests respectively.
- calculates maximum time fold change (FC) for prior group (condition) and the rest of used groups.  
  
 
1. User defines a prior group for FC calculate.
2. Calculates median or mean peak area for all of the groups.
3. Calculates FC between groups
    3.1. Allows those metabolites that calculated FC meets minimum user's FC.
4. Calculates p value based on ANNOVA or Kruskal Wallis
    4.2. Allows those metabolites that p value is below the user's p value.   
  
  

**Note!** If use more than 2 groups than FC calculation is a highest quotient between prime group and a group with the lowest peak area.
1. Exports jitter plot of metabolites based on calculations as parametric and non-parametric data [`groups vs mean peak area`], [`groups vs median peak area`].
2. Eports volcano plot of metabolites based on calculations as parametric and non-parametric data: parametric volcano plot [`log2(FC) vs log10(p-value)`] and non-parametric volcano plot [`log2(FC) vs log10(p-value)`].
3. Exports stacked bar chart with relative frequency (percentage) among groups of filtered metabolites.
4. Exports xlsx file with the list of filtered metabolite with the following columns:
```
- metabolite name 
- FC based on median/mean
- p-value based on median/mean
- median/mean peak area for compared conditions
- St.Dev of compared gropus
```
  
---------- 
### How to run?
By default mevis requires installed R on your computer in `C'\Program Files\R...` directory. 
[^note]:
Note! Different R location is not allowed and causes an error. 

Make sure you have installed R, otherwise download [from here](https://cran.r-project.org/bin/windows/base/).
1. Download `mevis.zip` and unzip it.
2. Enter user's settings in `config.yml` following tips in config file.
3. Launch `mevis.bat` file   
4. If it is needed you can customize charts style in ggplot function in `hostscript.R` file
	-Code to edit is inside of the code below: 
	```
	#=====================EDIT HERE================== 
	Plot <- function() {}
	#================================================
	```
> To run **mevis** in debug mode from [RStudio](https://www.google.com/url?sa=t&rct=j&q=&esrc=s&source=web&cd=&cad=rja&uact=8&ved=2ahUKEwiag7ih_cL0AhVCSfEDHTi5A24QFnoECAkQAQ&url=https%3A%2F%2Fwww.rstudio.com%2F&usg=AOvVaw1bt9MYkG-ySe7hgo9R8XTb) set `OS_enviroment` variable to `TRUE` in `hostscript.R`, otherwise run it in command prompt by default.

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
> All output data will be saved in the same directory with the input xlsx data and named as "mevis_output"
  
For example: 
- input data path: `D:\input_data.xlsx`
- output data path: `D:\mevis_output - input_data\[date & time when saved, threshold FC, threshold p-value, compared groups]\...`

#### Known Issue
Average metabolites peak area of some of the compared groups which is equal to zero may cause calculation failure for FC and statistical significance. So it is better to set zero to one, but it will cause false giant FC and as the consequence wrong overscale volcano and jitter plots. Current mevis version supports calculation with zeros, but in such case it automatically limits a giant FC to 30. 
