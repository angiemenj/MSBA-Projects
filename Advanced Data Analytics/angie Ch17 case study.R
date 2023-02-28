# Prepare a report for the managers of the Carlson Department Store that summarizes your findings, forecasts, and recommendations. Include the following:

# opening the file
library(readxl)
carlsonsales <- read_excel("C:/Users/Angie Menjivar/Downloads/carlsonsales.xlsx")
View(carlsonsales)

library(fpp2)
library(TTR)
library(tidyverse)

# 1. An estimate of sales for Carlson Department Store had there been no hurricane.
carlsonsales$Month <- as.factor(carlsonsales$Month)
carlsonsales_dummy <- as.data.frame(model.matrix(~ Month -1, carlsonsales))
carlsonsales <- cbind(carlsonsales_dummy, carlsonsales)
carlsonmodel <- lm(Sales ~ MonthSeptember + MonthOctober + MonthNovember + MonthDecember + Period, data = carlsonsales)
predict(carlsonmodel, data.frame(MonthSeptember=1, MonthOctober=0, MonthNovember=0, MonthDecember=0, Period=49))
predict(carlsonmodel, data.frame(MonthSeptember=1, MonthOctober=0, MonthNovember=0, MonthDecember=0, Period=50))
predict(carlsonmodel, data.frame(MonthSeptember=1, MonthOctober=0, MonthNovember=0, MonthDecember=0, Period=51))
predict(carlsonmodel, data.frame(MonthSeptember=1, MonthOctober=0, MonthNovember=0, MonthDecember=0, Period=52))
# September = 2.244
# October = 2.256
# November = 2.267
# December = 2.279
# all in $millions

# 2. An estimate of countywide department store sales had there been no hurricane.
countysales$Month <- as.factor(countysales$Month)
countydummy <- as.data.frame(model.matrix(~ Month -1, countysales))
countysales <- cbind(countydummy, countysales)
countysales$Period <- c(1:52)
countymodel <- lm(Sales ~ MonthSeptember + MonthOctober + MonthNovember + MonthDecember + Period, data = countysales)
predict(countymodel, data.frame(MonthSeptember=1, MonthOctober=0, MonthNovember=0, MonthDecember=0, Period=49))
predict(countymodel, data.frame(MonthSeptember=1, MonthOctober=0, MonthNovember=0, MonthDecember=0, Period=50))
predict(countymodel, data.frame(MonthSeptember=1, MonthOctober=0, MonthNovember=0, MonthDecember=0, Period=51))
predict(countymodel, data.frame(MonthSeptember=1, MonthOctober=0, MonthNovember=0, MonthDecember=0, Period=52))
# September = 58.074
# October = 58.154
# November = 58.254
# December = 58.344
# all in $millions

