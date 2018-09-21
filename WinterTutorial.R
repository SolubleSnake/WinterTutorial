######################################################
#####                                            #####
#####     SCRIPT FOR FERTILIZER EXERCISE         #####
#####                                            #####
######################################################

getwd()
setwd("c:\\temp") # to set the directory to c:\temp
file.exists("fertilizer.txt") # will return T if file there

# read in the file and assign to data frame "results". 
results <- read.table("c:\\temp\\fertilizer.txt",header=T)
attach(results)lib
names(results)

# install.packages("nlme") if have not installed the nlme package
library("nlme") # to load it into your workspace

# Make a groupedData object out of the dataframe. We specify 
# the nesting structure of the random effects, and indicate 
# the fixed effect by defining fertilizer as outer to 
# this nesting.

results <- groupedData(root~week|plant,outer=~fertilizer,results)

plot(results)

plot(results,outer=T)

model1 <- lme(root~fertilizer,random=~week | plant)
summary(model1)

# See explanation of results in session #2 material

# Let's perform a simple one-way ANOVA for the non-
# pseudoreplicated data from week 10 only:

model2 <- aov(root~fertilizer, subset=(week==10))
summary(model2)get
summary.lm(model2)



