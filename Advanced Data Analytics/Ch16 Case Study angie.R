# Ch16 Case Study: Rating Wines from the Piedmont Region of Italy

# clear environment
rm(list = ls())

#1. Develop a table that shows the number of wines that were classified as classic, outstanding, very good, good, mediocre, and not recommended and the average price. Does there appear to be any relationship between the price of the wine and the Wine Spectator rating? Are there any other aspects of your initial summary of the data that stand out?
# Table created on Excel file. There does not appear to be a relationship between the price of the wine and the Wine spectator rating. The prices vary and there does not appear to be a pattern when looking at the price and rating. The average price of bottle of wine is $63.11.

#2. Develop a scatter diagram with price on the horizontal axis and the Wine Spectator score on the vertical axis. Does the relationship between price and score appear to be linear?
colnames(wineratings)
library(tidyverse)
wineratings %>% ggplot(aes(x = Price, y = Score)) + geom_point()
# The relationship between price and score do not appear to be linear.

#3. Using linear regression, develop an estimated regression equation that can be used to predict the score given the price of the wine.
winemodel <- lm(Score ~ Price, data = wineratings)
summary(winemodel)
# Score = 87.763226 + 0.027995*Price

#4. Using a second-order model, develop an estimated regression equation that can be used to predict the score given the price of the wine.
wineratings$Price2 <- wineratings$Price^2
winemodel2 <- lm(Score ~ Price + Price2, data = wineratings)
summary(winemodel2)
# Score = 86.1659971 + 0.0713065*Price - 0.0001133*Price^2

#5. Compare the results from fitting a linear model and fitting a second-order model.
# From looking at the two models, we can tell that the second-order model is more accurate since the p-value is significantly lower than the p-value in the linear model. We can tell that both models have significant values though.


#6. As an alternative to fitting a second-order model, fit a model using the natural logarithm of price as the independent variable. Compare the results with the second-order model.
winemodel3 <- lm(Score ~ log(Price), data = wineratings)
summary(winemodel3)
# Score = 77.7306 + 3.1559*Price
# Using the natural logarithm of the price as the independent variable provides a more accurate answer as the p-value is very low and both variables are signfifciant from the model.

#7. Based upon your analysis, would you say that spending more for a bottle of wine will provide a better wine?
# Based upon my analysis, I would say that spending more for a bottle of wine will provide a better wine because the quality will be better.


#8. Suppose that you want to spend a maximum of $30 for a bottle of wine. In this case, will spending closer to your upper limit for price result in a better wine than a much lower price?
# Spending closer to upper limit would be better