#Brett Matzke 
#Windows 10 64bit
#List of Libraries Used
#ggplot2


library('ggplot2')

#Generate 5000 random numbers in a normal distribution 

rand.data <- rnorm(5000)

#Mean and SD for data
rand.mean <- mean(rand.data)
print(paste("Mean =", rand.mean))
rand.sd <- sd(biol.rand)
print(paste("Standard Deviation =", rand.sd))

#create dataframe from the random numbers for ggplot histogram
rand.df <- data.frame(
  rand.data = c(rnorm(5000)),
  stringsAsFactors = FALSE
)

#Test Non-ggplot2 histogram
rand.plot = rand.df$rand.data
m<-mean(rand.plot)
std<-sqrt(var(rand.plot))
hist(g, density=20, breaks=20, prob=TRUE, 
     xlab="number", ylim=c(0, .5), 
     ylab="count",
     main="normal curve over histogram")
curve(dnorm(x, mean=m, sd=std), 
    col="red", lwd=2, add=TRUE, yaxt="n",)

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
rand.sd <- sd(biol.rand)
print(paste("Standard Deviation =", rand.sd))
sink()
unlink

#rename and save Rplots.pdf as histo.pdf
pdf(file="histo.pdf")


#Use Dataset for 1 way ANOVA 
#dataset is 
diet = read.csv("~data/diet.csv",row.names=1)

diet$weight.loss = diet$initial.weight - diet$final.weight 
boxplot(weight.loss~diet.type,data=diet,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="blue")
