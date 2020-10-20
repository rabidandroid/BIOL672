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

#print(weight.data)
#print(summary(weightloss.data))

print(weight.data)
print(summary(weight.data))

#4.2 oneway Anova

weightloss.anova = oneway.test(weight_loss~diet)
print(weightloss.anova)

#4.3 pairwise t-test

pairwise.t.test(weight_loss, diet,
               p.adjust.method = "BH")

#box plot to visually check the data

diet.box <-ggboxplot(weight.data, x = "diet", y = "weight_loss", 
          color = "diet", palette = c("#00AFBB", "#E7B800", "#ff2929"),
          order = c("a", "b", "c"),
          ylab = "Weight lost", xlab = "diet")


#4.2 Error bar charts

diet.bar <- ggbarplot(weight.data, x = "diet", y = "weight_loss", 
          color = "diet", palette = c("#00AFBB", "#E7B800", "#ff2929"),
          order = c("a", "b", "c"),
          ylab = "Weight lost", xlab = "diet",
          add = "mean_se", add.params = list(group = "diet"),
          position = position_dodge(0.8))


print(diet.box)
print(diet.bar)


