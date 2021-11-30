# mevis
Compare peak area of annotated metabolites and sort it using criteria, visualize peak area of sorted metabolites among conditions in jitter plots, export dadatable of sorted metabolites in excel file, export volcano plot (log2FC to log10p-value) baesd on metabolites from sorted list.

**Sorting algorithm:**
- calculates average for parametric and non-parametric data using ANOVA and Kruskal Wallis tests respsectevely
- calcualtes time fold change (FC) among prior condition and the rest of used conditions


User input a prior condition name for FC calculate.
Calculates peak areas for all conditions using median or average.
1. Checks if the calculated FC meets minimum user's FC.
2. Сhecks if the calculated p value below the maximum user's p value. 



Time Fold Change (FC) calculation is a highest qutient between prime condition and a condition with lower peak area.
1. Export jitter plot for parametric and non-parametric data [`condition vs peak area`]
2. Plot volcano plot for parametric and non-parametric data [`volcano plot of log2FC vs log10p-value`]
3. Export xlsx file with sorted metabolite list with the following columns:
- metabolite name
- FC based on Median/Mean
- p-value based on Median/Mean
- Median/Mean peak area for compared conditions
- St.Dev for compared conditions

**How to run**
By default mevis requires installed R on your computer in `C'\Proogram Files\R...` directory. Different R location is not allowed and causes an error. 
Make sure you have installed R, if not download [from here](https://cran.r-project.org/bin/windows/base/).
1. Download `mevis.zip` un unzip it.
2. Enter user settings in `config.yml` by following tips.
3. Launch `mevis.bat` file   

> All output data will be saved in the same directory to input xlsx data and named as "mevis_output"
For example: 
- input data path: `D:\input_data.xlsx`
- output data path: `D:\mevis_output - input_data\[date & time when saved]\...`

