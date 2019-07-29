
##########################################################
# This script reads, explores, and manipulates  BCCDC radon
# data for SPPH 567
##########################################################

## load the radon data

nitrogen <- read.csv(file = "Euorpean_NO2_LUR.csv",
                  header = T,
                  stringsAsFactors = F)

### Attach the radon data to make it easier to work with

#attach(nitrogen)
class(nitrogen)
dim(nitrogen)
### The first number you see (1134) is the number of rows (observations)
# and the second number (13) is the number of columns (variables).

nrow(nitrogen)
ncol(nitrogen)
### nrow and ncol are for number of rows and columns.

object.size(nitrogen)
### object.size is for occupying memory by dataset.

names(nitrogen)
### names(radon) will return a character vector of column (i.e. variable) names

head(nitrogen)
head(nitrogen,10)
tail(nitrogen)
### The head() function allows you to preview the top of the dataset.
# The same applies for using tail() to preview the end of the dataset.
# head(radon, 10) will show you the first 10 rows of the dataset

summary(nitrogen)
### Use summary(radon) to get a better feel for how each 
# variable is distributed and how much of the dataset is missing.

table(nitrogen$Country)
table(nitrogen$Station.Type)
### Since it is a categorical/factor variable, we can see how many times
# each value actually occurs in the data with table(radon$Community).

str(nitrogen)


##########################################################
# This script reads, explores, and manipulates  BCCDC radon
# data for SPPH 567
##########################################################

### summarize nitrogen

summary(nitrogen)
mean(NO2,na.rm = T)

### create an object with the  <lod (5 μg/m3) replaced with
#   5/sqrt(2)

#   make a new object by replacing values in MainRadon
lodOverRoot2 <- replace(x = NO2,
                        list = is.na(NO2),
                        values = 5/sqrt(2))
#   check summary
summary(lodOverRoot2)

#   calculate Arithmetic mean, standard deviation, median,
#   95th percentile of the non-transformed values.
mean(lodOverRoot2)
sd(lodOverRoot2)
median(lodOverRoot2)
quantile(lodOverRoot2,0.95)

### log-transform the lodOverRoot2 and calculate summaries

#   log transformation
lodOverRoot2log <- log(lodOverRoot2)

#   calculate Arithmetic mean, standard deviation, median,
#   95th percentile of the log-transformed values.
mean(lodOverRoot2log)
sd(lodOverRoot2log)
median(lodOverRoot2log)
quantile(lodOverRoot2log,0.95)

#   calculate Arithmetic mean, standard deviation, median,
#   95th percentile of the log-transformed values.
exp(mean(lodOverRoot2log))
exp(sd(lodOverRoot2log))
exp(median(lodOverRoot2log))
exp(quantile(lodOverRoot2log,0.95))

### plot a histogram of the non-transformed values

#   base histogram
hist(lodOverRoot2)

#   base histogram with 50 bins and better labels, in blue
hist(lodOverRoot2,
     breaks = 50,
     main = "NO2 Concentrations with LOD/sqrt(2)",
     ylab = "Frequency",
     xlab = "NO2 Concentration (μg/m3)",
     col = "blue")

### plot a QQ plot of the log-transformed values

#   base QQ-plot comparing log-transformed values with theoretical normal
qqnorm(lodOverRoot2log)

#   add a thick red 1:1 line to the base plot
qqline(lodOverRoot2log,
       col = "red",
       lwd = 3)


#   update labels
qqnorm(lodOverRoot2log,
       main = "Log-Transformed NO2 Concentrations with LOD/sqrt(2)",
       ylab = "Quantiles of the Nitrogen Dioxide Sample",
       xlab = "Quantiles of the Normal Distribution")
qqline(lodOverRoot2log,
       col = "red",
       lwd = 3)
### conduct a Shapiro-Wilks test on the non-transformed values and
### log-transformed values

shapiro.test(lodOverRoot2)
shapiro.test(lodOverRoot2log)


##########################################################
# This script reads, explores, and manipulates  BCCDC radon
# data for SPPH 567
##########################################################

### replace <LOD values with 15/sqrt(2)
# $ is accessor here is extractor

nitrogen$NO2 <- replace(nitrogen$NO2,
                           is.na(nitrogen$NO2),
                           5/sqrt(2))

### create a log$Radon variable in radon

nitrogen$LogNO2 <- log(nitrogen$NO2)

### convert Station.Type to type factor and removing unknown

nitrogen$Station.Type <- factor(nitrogen$Station.Type,
                          levels = c("Background","Industrial","Traffic"))

levels(nitrogen$Station.Type)
table(nitrogen$Station.Type)

### convert Station.Loc to factor with Rural as the reference and removing unknown

nitrogen$Station.Loc <- factor(nitrogen$Station.Loc,
                     levels = c("Rural","Suburban","Urban"))
table(nitrogen$Station.Loc)
summary(nitrogen$Station.Loc)

### boxplot of log-transformed nitrogen vs. Station.Loc

boxplot(nitrogen$LogNO2 ~ nitrogen$Station.Loc,
        ylab = "log(Nitrogen Dioxide Concentrations (μg/m3)",
        main = "Log-Transformed Nitrogen Dioxide Concentrations",
        col = c("lightblue" , "orange","red","green"))

### run a linear regression model using the log-transformed values
geo.station.loc <- lm(nitrogen$LogNO2 ~ nitrogen$Station.Loc)
summary(geo.station.loc)

# calculate 95% confidence interval of intercept and slope
confint(geo.station.loc)

# calculate 95% confidence interval of geometric effect estimate
exp(confint(geo.station.loc))


##########################################################
# This script reads, explores, and manipulates  BCCDC radon
# data for SPPH 567
##########################################################

### collapse Crawl, Laundry, Recreation, and Utility into Other
#   radon$Location <- replace(radon$Location,
                          #radon$Location %in% c("Crawl","Laundry","Recreation","Utility"),
                          #"Other")

# check for category with the lowest mean
  aggregate(nitrogen$LogNO2,
            by = list(nitrogen$Station.Loc),
            FUN = mean)

### split the HomeAge1990 variable by building code

# first code in 1950, about 45 years before the radon measurements were taken
# double glazed windows in 1978, about 15 years before the measurements
#radon$BuildingCode <- cut(radon$HomeAge1990,
                          #breaks = c(0, 15, 45, max(radon$HomeAge1990)),
                          #labels = c("Newer Code","Older Code","No Code"))
# factor in a reasonable way
#radon$BuildingCode <- factor(radon$BuildingCode,
                             #levels = c("No Code","Older Code","Newer Code"))


### boxplot of building code data

#boxplot(radon$MainRadon ~ radon$BuildingCode,
 #       las = 1,
  #      log = "y",
   #     range = 10,
    #    xlab = "Building Code",
     #   ylab = "Radon Concentration (Bq/m3)",
      #  Main = "Radon Concentration By Building Code")

### run anova on the Station.Loc variable

aov.staloc <- aov(nitrogen$LogNO2 ~ nitrogen$Station.Loc)
summary(aov.staloc)
TukeyHSD(aov.staloc)

### run a linear regression model

lm.staloc <- lm(nitrogen$LogNO2 ~ nitrogen$Station.Loc)
summary(lm.staloc)  
confint(lm.staloc)  

### check for association between Station.Loc and Station.Type

table(nitrogen$Station.Loc,nitrogen$Station.Type)
mosaicplot(table(nitrogen$Station.Loc,nitrogen$Station.Type),
           col = c("red","blue","green"),
           main = "Mosaic Plot")
chisq.test(nitrogen$Station.Loc,nitrogen$Station.Type)

###############################################################
# This script loads the updated BCCDC radon data and uses the
# variables in simple and multiple linear regression analyses,
# and generates diagnostic plots.
###############################################################

### create a subset based on home age
#radon.sub <- subset(radon, HomeAge1990 <= 20)

### Pearson correlation for LogRadon and Home Age in subset
#cor.test(radon.sub$LogRadon, radon.sub$HomeAge1990)

### Regression model for LogRadon and Home Age in subset
#fit1 <- lm(radon.sub$LogRadon ~ radon.sub$HomeAge1990)

### scatter plot of MainRadon vs home age
#plot(radon.sub$LogRadon ~ radon.sub$HomeAge1990,
#     ylab = expression('Log(Radon Concentrations (µg/m'^3*'))'),
 #    xlab = "Home Age",
  #   main = "Scatter Plot for Homes <= 20 Years")

#abline(fit1, col = "red", lwd = 2)

### plot the Q-Q and Cook's D diagnostics
#plot(fit1, which = 2) #QQ
#plot(fit1, which = 4) #Cook's D

### multiple regression models
#fit2 <- lm(LogRadon ~ HomeAge1990 + Soil,
#           data = radon.sub)
#fit3 <- lm(LogRadon ~ HomeAge1990 + Soil + TecBelt,
#           data = radon.sub)

### calculate effect and CI for HomeAge1990 over 10-year interval
#exp(fit3$coefficients["HomeAge1990"]*10)
#exp(confint(fit3)["HomeAge1990",]*10)

library(leaps)
library(gmodels)

### create the Over100 variable and factorize Potential

#radon$Over100 <- ifelse(radon$MainRadon <= 100, 0, 1)

### nice cross-table and chi2 from the gmodels package

CrossTable(nitrogen$Station.Loc, nitrogen$Station.Type,
           prop.c = T, chisq = T, prop.r = F, prop.t = F, prop.chisq = F)

### run the logistic regression models

#fit1 <- glm(Over100 ~ TecBelt, data = radon, family = "binomial")
#fit2 <- glm(Over100 ~ Potential, data = radon, family = "binomial")
#fit3 <- glm(Over100 ~ TecBelt + Potential, data = radon, family = "binomial")

### get the coefficients and CIs for fit1

#exp(coef(fit1))
#exp(fit1$coef)

#exp(confint.default(fit1))    

### check the significance of fit1 using chi2

with(fit1, pchisq(null.deviance - deviance,
                  df.null - df.residual,
                  lower.tail = F))

### check for the difference between fit1 and fit2

anova(fit1, fit2, test = "Chisq")


### start modeling building by creating the data subset

high.radon <- subset(radon, Over100 == 1)

### reduce highradon to variables that will be used in
### the analyses, and complete cases. Then factorize
### categorical variables.

high.radon <- subset(high.radon,
                     select = c("LogRadon", "HomeAge1990", "AC", "Foundation", "Heating"))
high.radon$AC <- factor(high.radon$AC)
high.radon$Foundation <- factor(high.radon$Foundation)
high.radon$Heating <- factor(high.radon$Heating,
                             levels = c("Forced Air", "Electric", "Hot Water", "Other"))

### get rid of incomplete rows

high.radon <- subset(high.radon, complete.cases(high.radon) == T)

### fill out the first table

with(high.radon, cor.test(LogRadon, HomeAge1990))
with(high.radon, t.test(LogRadon ~ AC))
with(high.radon, t.test(LogRadon ~ Foundation))
with(high.radon, summary(aov(LogRadon ~ Heating)))

fit1 <- lm(LogRadon ~ HomeAge1990, data = high.radon); summary(fit1); exp(fit1$coef)
fit2 <- lm(LogRadon ~ AC, data = high.radon); summary(fit2); exp(fit2$coef)
fit3 <- lm(LogRadon ~ Foundation, data = high.radon); summary(fit3); exp(fit3$coef)
fit4 <- lm(LogRadon ~ Heating, data = high.radon); summary(fit4); exp(fit4$coef)

### fill out the second table

with(high.radon, t.test(HomeAge1990 ~ AC))
with(high.radon, t.test(HomeAge1990 ~ Foundation))
with(high.radon, summary(aov(HomeAge1990 ~ Heating)))
with(high.radon, chisq.test(AC, Foundation))
with(high.radon, chisq.test(AC, Heating))

# above throws a warning because there is not data in all cells
CrossTable(high.radon$AC, high.radon$Heating,
           prop.c = T, chisq = T, prop.r = F, prop.t = F, prop.chisq = F)

with(high.radon, chisq.test(Foundation, Heating))

### pair-wise visualisation with scatterplots

pairs(high.radon, gap = 0.2)

### fillout the third table

fit5 <- lm(LogRadon ~ ., data = high.radon)
summary(fit5); exp(fit5$coef)
fit6 <- lm(LogRadon ~ HomeAge1990 + Foundation + Heating, data = high.radon)
summary(fit6); exp(fit6$coef)
fit7 <- lm(LogRadon ~ Foundation + Heating, data = high.radon)
summary(fit7); exp(fit7$coef)

anova(fit5,fit6)
anova(fit5,fit7)
anova(fit6,fit7)

### stepwise regression

fit8 <- step(fit5, direction = "both", trace = F)
fit9 <- step(fit5, direction = "forward", trace = F)
fit10 <- step(fit5, direction = "backward", trace = F)

### use fit8 to make a prediction

exp(predict(fit8, data.frame("HomeAge1990" = 100,
                             "Foundation" = "Other",
                             "Heating" = "Electric"),
            interval = "confidence"))

### leaps regression

x <- regsubsets(LogRadon ~ HomeAge1990 + AC + Foundation + Heating,
                data = high.radon)
plot(x, scale = "adjr2")