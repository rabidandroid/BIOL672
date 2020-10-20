#Brett Matzke 
#Windows 10 64bit
#Script 1 of 2
#Script 1 UA assignment one 1 - 6
#Script 2 UA assignment two 7 - 14
#List of Libraries Used
#ggplot2


library('ggplot2')

#Generate 5000 random numbers in a normal distribution 

rand.data <- rnorm(5000)

#Mean and SD for data
rand.mean <- mean(rand.data)
print(paste("Mean =", rand.mean))
rand.sd <- sd(rand.data)
print(paste("Standard Deviation =", rand.sd))

#create dataframe from the random numbers for ggplot histogram
rand.df <- data.frame(
  rand.data
)

#Test Non-ggplot2 histogram
rand.plot = rand.df$rand.data
m <- mean(rand.plot)
std<-sqrt(var(rand.plot))
hist(rand.plot, density=20, breaks=20, prob=TRUE, 
     xlab="number", ylim=c(0, .5), 
     ylab="count",
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
    col="red", lwd=2, add=TRUE)

#TEST Plot with ggplot
rand.df <- data.frame(
  rand.data = c(rnorm(5000)),
  stringsAsFactors = FALSE
)


#Create a file desc.txt that displays mean and standard deviation from dataframe
sink("desc.txt")
#can specify link to file on my computer but want the output to working directory where scrit is running
rand.mean <- mean(rand.data)
print(paste("Mean =", rand.mean))
rand.sd <- sd(rand.data)
print(paste("Standard Deviation =", rand.sd))
sink()
unlink

#rename and save Rplots.pdf as histo.pdf
pdf(file="histo.pdf")


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
summary(weightloss.anova)

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

#5 - Kruskal Wallis test to examine it without assumptions of normality
kruskal.test(weight_loss~diet)

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

#5.3 Run a one sample KS test to test for Normality
#visual test:
ggdensity(weight_loss, 
          main = "Density plot of Weight Loss",
          xlab = "weight_loss")

#KS test
#ks.test(weight_loss, age)
#further troubleshooting needed

#5.4 Evaluated outcomes of non-parametric tests Why ot why not?



#5.5 Output to screen


#end of script 1 printing all relevant out put to a single file

#6 - linear regression - 
#6.1 Run a similar linear regression on your correlations from 5
weight.reg <- lm(formula = weight_loss ~ age, data = weight.data)
summary(weight.reg)
plot(age, weight_loss, pch = 16, cex = 1.3, col = "red", main = "Weight loss against age", abline(4.43570, -0.01509), xlab = "age", ylab = "Weight loss")

print(weight.reg)

#6.2 Compare results

#6.3 When do you use regression?  Correlation?

#6.4 Plot results




#output to file for all the analysis
sink(file = 'bmatzkeUA1part1.txt')

sink()
#stop writing to text file
#output of plots to file 
#need to make sure I have all plots to variables and then printing the variables.
pdf(file = 'bmatzkeUA1part1plots.pdf')


dev.off() 
#close file
