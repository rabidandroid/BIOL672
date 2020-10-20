#Brett Matzke 
#Windows 10 64bit
#Script 2 of 2
#Script 1 UA assignment one 1 - 6
#Script 2 UA assignment two 7 - 14
#List of Libraries Used
#ggplot2

library('dplyr')
library('ggplot2')
library('ggpubr')
library('tidyverse')
library('reshape2')
library('factoextra')
library('psych')
library('fitdistrplus')
library('mixtools')
library('naniar')
library('ggfortify')
library('ggstatsplot')

#7 Use dataset from your field
#Data set should include 4-6 measured quantitative variables

#Not from my field but the availability of COVID data seems interesting to analyze
#I cleaned the BIOLCOVID.xlsx file to get it down to 8 variables I think would be interesting

covid.data <- read.csv("covid3.csv")
#removed N/A from hdi and gdp
#covid.data <- read.csv("covid4.csv")
summary(covid.data)

#check data type for each column commented out after reviewing output
#str(covid.data) 


#prepare data for easy use throughout part 2
continent <- covid.data$continent
cases <- covid.data$total_cases
deaths <- covid.data$total_deaths
casespm <- covid.data$total_cases_per_million
deathspm <- covid.data$total_deaths_per_million
med.age <- covid.data$median_age
gdp <- covid.data$gdp_per_capita
lifeExp <- covid.data$life_expectancy
pop.density <- covid.data$population_density
hdi <- covid.data$human_development_index
country <- covid.data$location

#checking data to see how much of it has NA.  May remove countries with NA in the key independent variable catergories 
#and create a covid4.csv to import and then comment out the command so I can compare 
#results between the two 
#print("Percentage of Code Missing Values")
#gg_miss_var(covid.data)
#print(pct_miss(covid.data))
#removed the 24 countries without GDP/HDI data 

#histogram to check dependent variable 


 

#check to see how a simple boxplot of the 6 catergories (just reviewing the data)

covid.box1 <- ggplot(covid.data, aes(x=continent, y=deathspm)) + geom_boxplot()
#right away looking at this boxplot the deaths per million in South America stands out.  

covid.box2 <- ggplot(covid.data, aes(x=continent, y=casespm)) + geom_boxplot()
#right away looking at this boxplot the deaths per million in South America stands out.  

covid.box3 <- ggplot(covid.data, aes(x=location, y=deathspm)) + geom_boxplot()
#right away looking at this boxplot the deaths per million in South America stands out.  

covid.box4 <- ggplot(covid.data, aes(x=location, y=casespm)) + geom_boxplot()
#right away looking at this boxplot the deaths per million in South America stands out.  




# Create a boxplot of the dataset, outliers are shown as two distinct points
boxplot(covid.data)$out
#Create a boxplot that labels the outliers  
ggbetweenstats(covid.data, deathspm, deaths, gdp, cases, casespm, outlier.tagging = TRUE)

#This identified population and total cases with some major outliers.   I don't use population so I removed it from the ori

print(covid.box1)
print(covid.box2)
print(covid.box3)
print(covid.box4)
#right away we can see there are some major outliers in the datasets.   

summary(covid.data)
summary(continent)

#check data
#set.seed(1234)
#dplyr::sample_n(covid.data, 10)


test.reg <- lm(deathspm ~ continent)
print("Linear Regression")
print(test.reg)

#8 Use Manova functon in R
#use cbind to combine measures into a single test of significance across category

#total cases and deaths
covid.man <- cbind(deaths, cases)
covid.mantest <- manova(covid.man ~ continent)
covid.manova <- summary(covid.mantest)
print("Manova for Total Cases and Deaths")
print(covid.manova)


#total cases and deaths per million
covid.man2 <- cbind(deathspm, casespm)
covid.mantest2 <- manova(covid.man2 ~ continent)
covid.manova2 <- summary(covid.mantest2)
print("Manova for Cases and Deaths per million")
print(covid.manova2)



#9.1  Use lm function in R to conduct multiple regression
covid.multreg <- lm(deathspm ~ hdi + lifeExp + gdp)
covid.mr <- summary(covid.multreg)
print("Multiple Regression Output:")
print(covid.mr)

#9.2 Use lm to conduct same test but within one of your catergories instead of accross them
### need figure out how to specify this for catogory "South America"  covid.mulreg.cat <-(lm(deathspm ~ hdi + lifeExp + gdp)

#10   Create a composite variable
#10.1 Use aov funtion to set up an ANCOVA that comcompares the relation of one variable predicted by by another while controlling for your composite feature
cases.alive=(cases-deaths)
cases.ancova <- aov(cases ~ deaths+cases.alive)
comp.ancova <- summary(cases.ancova)
print(comp.ancova)
#although this doesn't technically tell us recovered since we can only go on the data available which is cases and deaths this can tell us how many who have had it survived or are still alive with it

  
#11   Conduct a principal components analysis of the individual measurements and output that covers the following questions
#11.2 How successful was the reduction of the data (explain using the scree plot as a reference)?
#How would you interpret the loadings of the original measurements on each of the PC’s? 
#Which one best describes overall size variation (i.e. PC with all positive loadings)? 
#Do any of the PC’s appear strongly driven by one particular measurement (see loadings on PC)?

pca.data <- data.frame (
  deaths,
  cases,
  gdp,
  casespm, 
  deathspm, 
  med.age,
  lifeExp,
  hdi 
)

#removed gdp when it was throwing off results
#converted 
pca.data$cases <- as.numeric(as.character(pca.data$cases))
pca.data$deaths <- as.numeric(as.character(pca.data$deaths))


#check for all numerical fields
#str(pca.data)
summary(pca.data)

pca.results <- prcomp(na.omit(pca.data))
pca.output <- summary(pca.results)
print(pca.output)

var_explained_df <- data.frame(PC= paste0("PC",1:8),
                               var_explained=(pca.results$sdev)^2/sum((pca.results$sdev)^2))

#test for populating var_explained_df

head(var_explained_df)
# screeplot
var_explained_df %>%
  ggplot(aes(x=PC,y=var_explained, group=1))+
  geom_point(size=4)+
  geom_line()+
  labs(title="Scree plot")


pca.plot <- fviz_eig(pca.results)

print(pca.plot)


df <- pca.data[1:8]
pca_res <- prcomp(df, scale. = TRUE)

autoplot(pca_res)

autoplot(pca_res, data = pca.data, colour = 'continent')

#not 100% happy with my output from the pca and I am having issues getting the data to work with FA so I am going to redo this part with a prepackaged for FA
#Figured I would redo my pca on it as well since I was not happy with the screeplot and the COVID data set
#part of psych library


#12   FActor Analysis

d.factanal <- factanal(state.x77, factors = 3, scores = 'regression')
autoplot(d.factanal, data = state.x77, colour = 'Income')


autoplot(d.factanal, label = TRUE, label.size = 3,
         loadings = TRUE, loadings.label = TRUE, loadings.label.size  = 3)

  
#13   Scatterplot of most interesting variables

#okay I will admit I went a little crazy here but looking at these factors in a scatterplot was interesting

scatter.hdi <- ggplot(covid.data, aes(x=hdi,y=deathspm, shape=continent, color=continent)) + 
  geom_point() + 
  labs (title="Human Development Index",x= "Human Development Index",y="Deaths per Million")+ 
  theme_light()
print(scatter.hdi)


scatter.gdp <- ggplot(covid.data, aes(x=gdp,y=deathspm, shape=continent, color=continent)) + 
  geom_point() + 
  labs (title="GDP",x= "GDP",y="Deaths per Million")+ 
  theme_bw()
print(scatter.gdp)

#This one graph really shows a relation between life expectancy and deaths per million
scatter.lifex <- ggplot(covid.data, aes(x=lifeExp,y=deathspm, shape=continent, color=continent)) + 
  geom_point() + labs (title="Life Expectency",x= "Life Expectency",y="Deaths per Million")+ 
  theme_bw()
print(scatter.lifex)

# I chose the three sets of data for this part to me the most interesting one is the HDI data since a high HDi should indicate 


#13.1 kmeans

#autoplot(kmeans(deathspm), 3) data = covid.data, label = TRUE, label.size = 3)

#covid.kmeans <-kmeans(na.omit(covid.data, centers = 3))
#covid.data$clusters <- (covid.kmeans$clusters)

#covid.kmeans.sp <- ggplot(covid.data, aes(x=lifeExp, y = deathspm, shape=clusters, color=kclusters))+
#  geom_point() + labs (title = "Kmeans - COVID", x="Average L:ife Span", "Death's Per Million") + theme_classic()
#print(covid.kmeans.sp)

data(covid.pca)
kmeans.df <- scale(na.omit(covid.pca))

#str(covid.data)
#describe(covid.data)

#13.2 create plot where points are colored accoding to cluster membership

#14 Large complex data sets are becoming more common in this ‘age of information’. Simple models of probability density, which often assume that underlying variability is due to a single process (e.g. sampling error) often can fail to describe fully the underlying ‘latent’ or unobservable aspects of this kind of data. Therefore we need to employ more sophisticated algorithms to fit models to complex data. Multimodal density distributions are often indicative of underlying latent variability. One common solution to fitting density functions to multimodal density is to use a Gaussian Mixture Model fitted (GMM) by the Expectation-Maximization (EM) algorithm as we discussed in class. In this last part of the assignment you will fit three simple probability density functions (normal, lognormal and another of your own choosing) to the distributions of one of the independent variables (or PC1 representing the most reduced form of your data if you prefer) . You will also fit a GMM as a fourth model of probability density. All models will be compared and evaluated using the Bayesian Information Criterion (BIC) as a method of multimodel inference. Your goal is to determine which model of the four (normal, lognormal, yourChoice and GMM) best fits the distribution. (Iris flower size will be demo in class)

covid.in.var <- lifeExp

#normal
covid.norm.p <- fitdistr(covid.in.var, densfun ='normal')

#lognormal
covid.lognorm.p <- fitdistr(covid.in.var, densfun ='log-normal')

#gamma distribution
#covid.gamma.p <- fitdistr(covid.in.var, densfun ='degamma')

#GMM by EM
#covid.gmm <-normalmixEM(covid.in.var)
#covid.likeli <- covid.gmm$loglik
#print("Summary of GMM")
#summary(covid.gmm)
#print(covid.likeli)


#15 Write an R script that imports or loads the data set, fits the models to your distribution, calculates likelihoods and compares BIC and plots histogram(s) of size with each model fitted to the data. Did the model testing indicate the presence of latency (via GMM having best fit?) Be sure to include verbal interpretation printed to screen and/or output files.





#output to file for all the analysis
sink(file = 'bmatzkeUA1part2.txt')

sink()
#stop writing to text file
#output of plots to file 
#need to make sure I have all plots to variables and then printing the variables.
pdf(file = 'bmatzkeUA1part2plots.pdf')


dev.off() 
#close file