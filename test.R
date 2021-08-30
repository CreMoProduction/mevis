
fun <- function() {
  message("Configure data prcossing. 
                Enter: (value1, value2)")
  x <- readline("Type here:") 
  x <- as.numeric(unlist(strsplit(x, ",")))
  return(x)
}
x=fun()
message("Your value is ", x, " 
        >>Press Enter to start")
fun1 <- function() {
  message("afer") 
} 
fun1()

#cat("aaa")
#stop("qqqq")

f<- function() {
print(paste("The year is", year))
}
#-----------
Metabolite=10
for (i in 1:Metabolite){
  f()
  print(i)
  
}
#-----------
library(gridExtra)
library(ggplot2)
p <- list()
p
for(i in 1:4){
  p[[i]] <- qplot(1:10,10:1,main=i)
  p[[i]]
}
do.call(grid.arrange, p)
#-----------
x<-10
plots = lapply(1:x, function(.x) qplot(1:10,rnorm(10),main=paste("plot",.x)))
require(gridExtra)
do.call(grid.arrange,  plots)
#-----------

# simplify = FALSE to avoid coercion to array
ttestlist <- replicate(1000, t.test(rnorm(10)), simplify = FALSE)
ttestlist
ttest.pval <- sapply(ttestlist, '[[', 'p.value')


x = rnorm(10)
y = rnorm(10)
plot(x,y)
t<- t.test(x,y)
t
ttest.pval <- sapply(t, '[[', 'p.value')


d<-rnorm(10)
plot(d)
#Let's look at the structure of one t.test to see where the p-value is stored
str(t.test(d))
#It is named "p.value, so let's see if we can extract it
t.test(d)[["p.value"]]
#Now let's test if its less than your 0.05 value
ifelse(t.test(d)[["p.value"]]< 0.05,1,0)
#That worked. Now let's replace the code above in your replicate function:
replicate(1000, ifelse(t.test(rnorm(10))[["p.value"]]< 0.05,1,0))
#That worked too, now we can just take the sum of that:
#Make it reproducible this time
set.seed(42)
sum(replicate(1000, ifelse(t.test(rnorm(10))[["p.value"]]< 0.05,1,0)))

