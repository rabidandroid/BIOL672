library('dplyr')
library('ggplot2')
library('ggpubr')
library('tidyverse')

gap.data=read.table("gapfinder.csv", header = TRUE, sep = ",")

#check data for variables and formatting
#print (head(dietdata))
print(summary(gap.data))
#@print(summary.aov(dietdata))

#assign df$fields to variables to make things a bit easier to code
continent <- gap.data$continent
life_exp <- gap.data$life_exp
#for further analysis we can add more but for this ANOVA we are looking at these variables

#there are easier ways to do this but for the sake of learning lets make a new data frame wiht just the measures we are comparning
weightloss.data <- data.frame(
  weight_loss,
  group
)

#print(weightloss.data)
print(summary(weightloss.data))

#ANOVA 
weight_loss.anova = oneway.test(weight_loss ~group)
weight_loss.anova


