### STATS PROJECT Costanza Viglianisi

# create two subfolders inside the project folder
dir.create(path = "data")
dir.create(path = "outputs")

# install packages and require library
# install.packages("vegan")
library(vegan)

# import datasets
# sep = " ", to explicit that the fields are separated by a space
# header = T, to transform the first row into column titles
mite <- read.csv("data/mite.csv")
mite_env <- read.table("data/mite_env.txt",
                       sep =" ",
                       header = T)

# count observations, species and environmental variables
observations <- nrow(mite) # number of observations = number of rows (70)
species <- ncol(mite)  # number of species = number of columns (35)
env_variables <- ncol(mite_env) # number of environmental variables = number of columns of environmental variables (5)

# check the documentation
?mite
# mite contains the data on 35 species of Oribatid mites, 
# mite.env contains environmental data in the same sampling sites

# check data structure
str(mite)
# dataframe with 70 obs of 35 variables
# integer values
str(mite_env)
# 70 obs of 5 variables
# 2 numeric data types, 3 character data types

# transform columns type as in the documentation
# check Substrate
unique(mite_env$Substrate)
# transform Substrate in factor data type
mite_env$Substrate <- factor(mite_env$Substrate,
                             levels = c("Sphagn1", "Litter", "Interface", "Sphagn3", "Sphagn4", "Sphagn2", "Barepeat", "Sphagn", NA))
# check Shrub
unique(mite_env$Shrub)
# transform Shrub in ordered factor data type
mite_env$Shrub <- factor(mite_env$Shrub,
                         levels = c("None","Few","Many", NA),
                         ordered = T)
# check Topo
unique(mite_env$Topo)
# transform Topo in factor data type
mite_env$Topo <- factor(mite_env$Topo,
                        levels = c("Blanket", "Hummock", NA))

# calculate summary stats for env variables
summary(mite_env)
# 3 NAs in WatrCont, Substrate and Topo

# to see omitted rows
options(max.print=1000000)

# check NAs
is.na(mite)  # no NAs
is.na(mite_env)  # 3 NAs: 1 in WatrCont, 1 in Substrate, 1 in Topo

# identify NAs
index_na <- which(is.na(mite_env), arr.ind = T) # to return indices
index_na
index_na <- index_na[, 1] # to choose the first coloumn that contains row numbers (45,3,43)

# remove NAs
mite_env <- na.omit(mite_env) # omit rows containing NAs in mite_env
mite <- mite[-index_na, ] # omit same rows in mite
# now there are 67 obs

# graphically explore the univariate distribution of environmental variables
# plot histograms for the numerical variables
hist(mite_env$SubsDens,
     main = "Substrate Density", # title
     xlab = "g/L", # x axis
     ylim = c(0,30)) 

hist(mite_env$WatrCont,
     main = "Water Content of the Substrate",
     xlab = "g/L",
     ylim = c(0,20))

# plot barplots for the categorical variables
# contingency table with the function table()
barplot(table(mite_env$Substrate),
        main = "Substrate Type",
        xlab = "Type")

barplot(table(mite_env$Shrub),
        main = "Shrub Density",
        xlab = "Levels")

barplot(table(mite_env$Topo),
        main = "Microtopography",
        xlab = "Levels",
        ylim = c(0,50))

# export at least one graph to the outputs subfolder
png(filename = "outputs/hist_SubsDens.png",
    width = 1800,
    height = 1800,
    res = 300)
hist(mite_env$SubsDens,
     main = "Substrate Density",
     xlab = "g/L",
     ylim = c(0,30))
dev.off()

png(filename = "outputs/bar_Shrub.png",
    width = 1800,
    height = 1800,
    res = 300)
barplot(table(mite_env$Shrub),
        main = "Shrub Density")
dev.off()

# convert community matrix in presence/absence matrix
mite <- decostand(mite, method = "pa")
mite

# calculate species richness
SpeRich <- specnumber(mite)
SpeRich

# add SpeRich into mite_env dataframe as a new column
mite_env$SpeRich <- SpeRich
mite_env


# plot the distribution of species richness in respect with the available numeric environmental variables
plot(SpeRich ~ SubsDens,
     data = mite_env,
     main = "Species Richness distribution in respect with Substrate Density",
     xlab = "Substrate Density",
     ylab = "Species Richness",
     ylim = c(0,30),
     cex = 0.5)

plot(SpeRich ~ WatrCont,
     data = mite_env,
     main = "Species Richness distribution in respect with Water Content",
     xlab = "Water Content",
     ylab = "Species Richness",
     ylim = c(0,30),
     cex = 0.5)

# test the correlation between species richness and the numerical environmental variables
# to determine whether there is a linear correlation between two numeric variables
# the null hypothesis (H0) is that there is no correlation between the variables (correlation equal to 0)
cor.test(mite_env$SpeRich, mite_env$SubsDens) 
# alternative hypothesis: true correlation is not equal to 0 (two-sided)
# p-value = 0.8179 -> not significant (p-value > 0.1 = no evidence against H0 -> no correlation)
# cor = -0.02866909  
# there is no correlation between Substrate Density and Species Richness

cor.test(mite_env$SpeRich, mite_env$WatrCont)
# alternative hypothesis: true correlation is not equal to 0
# p-value = 6.553e-11 -> very significant (p-value < 0.01 = strong evidence against H0 -> correlation)
# cor = -0.6954274 
# very significant negative correlation between Water Content and Species Richness

# run a regression model for each correlated numeric environmental variable
# linear regression is an approach to model the linear correlation between one or more variables
mod_watr <- lm(SpeRich ~ WatrCont, data = mite_env)
# inspect its summary
summary(mod_watr)
# Intercept and WatrCont are very significant because they both have a Pr(>|t|) < 0.01,
# which is the p-value associated to the estimated coefficients, indicating their significance.
# Adjusted R-squared: 0.4757, moderate value -> 47% of the variance of species richness is explained by the linear model 
# p-value: 6.553e-11, very low -> significant model

# scatterplot with the regression line
plot(SpeRich ~ WatrCont,
     data = mite_env,
     main = "Species Richness distribution in respect with Water Content",
     xlab = "Water Content of the Substrate",
     ylab = "Species Richness",
     ylim = c(0,30),
     cex = 0.5)
# to graphically represent the model 
abline(mod_watr$coefficients[1], mod_watr$coefficients[2], col = "red")

# check if residuals are normally distributed
shapiro.test(mod_watr$residuals) 
# Shapiro-Wilk normality test:
# p-value = 0.9909 -> not significant
# residuals values are normally distributed
