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
#5 - Kruskal Wallis test to examine it without assumptions of normality
ks.diet <- kruskal.test(weight_loss~diet)
summary(ks.diet)

#5.1 Choose 2 or more categories and test correlation between them using Pearson and Spearman Rank methods

#corr <- cor.test(x=age, y=weight_loss, method = 'pearson')
#corr <- cor.test(x="age", y="weight_loss", method = 'spearman')

spearman <- cor(age, weight_loss, method = "spearman")
pearsons <- cor.test(age, weight_loss, method = "pearson")
print(pearsons)
print(spearman)



#5.2 Make scatterplot showing relation

ageplot <- ggplot(weight.data, aes(x=age, y=weight_loss, color="red")) + geom_point(shape=4)
heightplot <- ggplot(weight.data, aes(x=height, y=weight_loss, color="blue")) + geom_point(shape=2)
oldweight.plot <- ggplot(weight.data, aes(x=old_weight, y=weight_loss, color="green")) + geom_point(shape=3)

print(ageplot)
print(heightplot)
print(oldweight.plot)

#5.3 Run a one sample KS test to test for Normality
#visual test:
ggdenplot <- ggdensity(weight_loss, main = "Density plot of Weight Loss", xlab = "weight_loss")
print(ggdenplot)

#KS test
diet.ks <- ks.test(height, age)
summary(diet.ks)

diet.ks <- ks.test(height, old_weight, exact = FALSE)
summary(diet.ks)

#5.4 Evaluated outcomes of non-parametric tests Why ot why not?



#5.5 Output to screen


