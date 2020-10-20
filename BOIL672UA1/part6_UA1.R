#part6
#task5

library('dplyr')
library('ggplot2')
library('ggpubr')
library('tidyverse')
library('reshape2')
#4.1 use read.table to read in data
dietdata=read.table("diet.csv", header = TRUE, sep = ",")

#check data for variables and formatting
#print (head(dietdata))
#print(summary(dietdata))
#@print(summary.aov(dietdata))

#assign df$fields to variables to make things a bit easier to code
weight_loss<-dietdata$delta_weight
age <- dietdata$Age
height <- dietdata$Height
old_weight <- dietdata$pre.weight
diet<-dietdata$Diet 

weight.data <- data.frame (
  weight_loss,
  age,
  height,
  old_weight,
  diet
)
  

#6 - linear regression - 
#6.1 Run a similar linear regression on your correlations from 5
weight.reg <- lm(formula = weight_loss ~ age, data = weight.data)
summary(weight.reg)
plot(age, weight_loss, pch = 16, cex = 1.3, col = "red", main = "Weight loss against age", abline(4.43570, -0.01509), xlab = "age", ylab = "Weight loss")

print(weight.reg)

#6.2 Compare results

#6.3 When do you use regression?  Correlation?

#6.4 Plot results

