## NR 995 Module 11
## Correlation and ANOVA 
## Group B, Erica Holm and Katie Moran
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
range(ants$Delta)
range(ants$CTmax)

## Do the values and descriptive statistics of numerical
## variables make sense with their definitions above? 

## Yes, the values and descriptive stats make sense with their definitions
## Binom - there are 5 different ant species
## Abundance - counts of individuals range from 1 to 100
## Delta - temperature treatments range from a change of 1 to a change of 12
## CTmax - lethal temperatures range from 39.2 to 45.6 degrees

##########################################################################
## Categorize values into low, moderate, and high Delta classes
##########################################################################

ants$DeltaClass <- NA
ants$DeltaClass[ants$Delta <= 4] <- "low"
ants$DeltaClass[ants$Delta >8] <- "high"
ants$DeltaClass[ants$Delta >4 & ants$Delta <= 8] <- "moderate"
ants # yay it worked

ants$Delta1 <- as.factor(ants$DeltaClass)
summary(ants)
str(ants)
# 18 low Delta observations
# 18 moderate Delta observations
# 18 high Delta observations


##########################################################################
## Run and interpret an ANOVA to test if higher temps reduce ant abundance
##########################################################################
install.packages("Hmisc")
library(Hmisc)


## check assumptions
hist(ants$Abundance) # not normally distributed, much higher frequency of low abundances
hist(ants$Delta1) # couldn't do histogram because not numerical
# would be the same across all categories because there are 18 of each

boxplot(ants$Abundance ~ ants$Delta1) 
# abundance is higher w/low delta? lots of variance within

aov <- aov(ants$Abundance ~ ants$Delta1)
aov
summary(aov) #not statistically significant

# use Poisson glm because Abundance is count data
reg <- glm(ants$Abundance ~ ants$Delta1, data = ants, family = "poisson")
summary(reg) 
## provides significant result for low delta class

# Abundance does not differ based on delta class according to ANOVA
# but Poisson p value is significant for Delta low class, meaning that low
# delta temp applications result in greater ant abundance. P values were not 
# significant for high and moderate delta classes, meaning that they cannot be
# used to predict ant abundance. 

# Tukey post-hoc
TukeyHSD(aov)
plot(TukeyHSD(aov))
# no significant p values, as expected from ANOVA findings. 

## test assumptions
install.packages("car")
library(car)

leveneTest(ants$Abundance ~ ants$Delta1) # p > 0.05
bartlett.test(ants$Abundance ~ ants$Delta1) # p > 0.05
# can accept null that variances are equal

par(mfrow = c(2,2))
plot(aov)

shapiro.test(aov$residuals) # p < 0.05
# not what we want 
# reject null hypothesis, residuals not normally distributed

##########################################
## Hypothesis: temperature and abundance are positively correlated
##########################################
par(mfrow = c(1,1))

hist(ants$Abundance)
hist(ants$Delta)

plot(ants$Abundance, ants$Delta)

# data doesn't look normal -> non-parametric tests needed

cor.test(ants$Abundance, ants$Delta, method = "spearman")
## have ties, use kendall not spearman
cor.test(ants$Abundance, ants$Delta, method = "kendall")
## not significant

kruskal.test(ants$Abundance ~ ants$Delta)
# not significant

## No, the interpretation would be the same - that ant Abundance is not
## significantly correlated with temperature changes

##########################################
## Contributions
##########################################

## Katie created the Git repo and worked on questions 1-3, Eric worked on
## 1,3,4 and reviewed/edited code.
