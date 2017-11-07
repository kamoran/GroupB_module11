## NR 995 Module 11
## Correlation and ANOVA 
## Group B, Erica Holme and Katie Moran
## Last modified: 11/07/17

## Set wd and read in ants_duke.csv
getwd() #using GitHub to complete assignment
ants <- read.table("ants_duke.csv", sep=",", header=T)

##########################################################################
## Get to know the data
##########################################################################

head(ants)
str(ants) # 54 observations of 4 variables
summary(ants)
length(unique(ants$Binom)) # 5 ant species in dataset
length(unique(ants$Delta)) # 12 unique temp treatments (Delta)
unique(is.na(ants)) # no NAs in data set

## Do the values and descriptive statistics of numerical
## variables make sense with their definitions above? 
## KM to EH: I'm unsure on answering this question. Delta has a vague definition--why are Delta values much less
## than the CTmax? Is Delta measuring an increase in temperature above the 
## ant's resting temperature? Also not sure if there are other descriptive statistics we
## should be doing... Thoughts? 

##########################################################################
## Categorize values into low, moderate, and high Delta classes
##########################################################################

ants$DeltaClass <- NA
ants$DeltaClass[ants$Delta <= 4] <- "low"
ants$DeltaClass[ants$Delta >8] <- "high"
ants$DeltaClass[ants$Delta >4 & ants$Delta <= 8] <- "moderate"
ants # yay it worked

summary(ants)
length(ants$DeltaClass[ants$DeltaClass == "low"]) # 18 low Delta observations
length(ants$DeltaClass[ants$DeltaClass == "moderate"]) # 18 moderate Delta observations
length(ants$DeltaClass[ants$DeltaClass == "high"]) # 18 high Delta observations


##########################################################################
## Run and interpret an ANOVA to test if higher temps reduce ant abundance
##########################################################################
install.packages("Hmisc")
library(Hmisc)


## check assumptions
hist(ants$Abundance) # not normally distributed, much higher frequency of low abundances
hist(ants$DeltaClass) # couldn't do histogram because not numerical

boxplot(ants$Abundance ~ ants$DeltaClass)

aov <- aov(ants$Abundance ~ ants$DeltaClass)
aov
summary(aov) #not statistically significant

reg <- lm(ants$Abundance ~ ants$DeltaClass)
summary(reg)

TukeyHSD(aov)
plot(TukeyHSD(aov))

## test assumptions
install.packages("car")
library(car)

leveneTest(ants$Abundance ~ ants$DeltaClass) # p > 0.05
bartlett.test(ants$Abundance ~ ants$DeltaClass) # p > 0.05

par(mfrow = c(2,2))
plot(aov)

shapiro.test(aov$residuals) # p value significant, not what we want

