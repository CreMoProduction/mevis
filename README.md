# mevis
compare peak area of annotated metabolites and sort it using criteria, visualize peak area of sorted metabolites among conditions in jitter plots, export dadatable of sorted metabolites in excel file, export volcano plot (log2FC to log10p-value) baesd on metabolites from sorted list.

Sorting algorithm:
- calculate average for parametric and non-parametric data using ANOVA and Kruskal Wallis tests respsectevely
- calcualte time fold change (FC) among prior condition and the rest of used conditions


User input a prior condition name for FC calculate.
Calculate peak areas for all conditions using median or average.
Check if the calculated FC meets minimum user's FC.
Сheck if the calculated p value meets the maximum user's p value. 



Time Fold Change (FC) calculation is a highest qutient between prime condition and a condition with lower peak area.
Export jitter plot for parametric and non-parametric data [condition vs peak area]
Plot  volcano plot for parametric and non-parametric data [volcano plot of log2FC vs log10p-value]
Export xlsx file with sorted metabolite list with the following columns:
- metabolite name
- FC based on Median/Mean
- p-value based on Median/Mean
- Median/Mean peak area for compared conditions
- St.Dev for compared conditions

All output data will be saved in the same directory to input xlsx data and named as "mevis_output"
For example: 
input data path: 
`D:\input_data.slsx`
output data path: 
`D:\mevis_output - input_data\[date & time when saved]\...`