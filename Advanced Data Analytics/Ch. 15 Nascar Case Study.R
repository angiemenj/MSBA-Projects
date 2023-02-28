# calling column names
colnames(nascar)

# changing column names
colnames(nascar) <- c("driver", "points", "poles", "wins", "top5", "top10", "winnings")

#1. Suppose you wanted to predict Winnings($) using only the number of poles won (Poles), the number of wins (Wins), the number of top five finishes (Top 5), or the number of top ten finishes (Top 10). Which of these four variables provides the best single predictor of winnings?
# The best single predictor of winnings would be Top 10.

#2. Develop an estimated regression equation that can be used to predict Winnings ($) given the number of poles won (Poles), the number of wins (Wins), the number of top five finishes (Top 5), and the number of top ten (Top 10) finishes. Test for individual significance and discuss your findings and conclusions.
nascar_model <- lm(winnings ~ poles + wins + top5 + top10, data = nascar)
summary(nascar_model)
# winnings = 3140367 - 12939*poles + 13545*wins + 71629*top5 + 117071*top10
#Looking at significance, we can conclude that top10 is the most significant out of all the other variables since it has a p-value of 0.00147, less that 0.01. The least significant is poles with a p-value of 0.90474, the highest p-value from all other variables. Analyzing on R, the only marked p-value is top10 indicating that all other variables could be removed and are not marked as significant. Interesting enough, since the first prediction in my mind would have thought that top5 would have been signficant.

#3. Create two new independent variables: Top 2–5 and Top 6–10. Top 2–5 represents the number of times the driver finished between second and fifth place and Top 6–10 represents the number of times the driver finished between sixth and tenth place. Develop an estimated regression equation that can be used to predict Winnings ($) using Poles, Wins, Top 2–5, and Top 6–10. Test for individual significance and discuss your findings and conclusions.
nascar$top2_5 <- nascar$top5 - nascar$wins
nascar$top6_10 <- nascar$top10 - nascar$top5 - nascar$wins
nascar_model2 <- lm(winnings ~ poles + wins + top2_5 + top6_10, data = nascar)
summary(nascar_model2)
# winnings = 3140367 - 12939*poles + 319315*wins + 188700*top2_5 + 117071*top6_10
#Looking at significance, we can conclude that all variables except poles are significant. Top 2-5 has the lowest p-value less than 0.001, which is the most significant variable and poles has the highest p-value from all other variables.

#4. Based upon the results of your analysis, what estimated regression equation would you recommend using to predict Winnings ($)? Provide an interpretation of the estimated regression coefficients for this equation.
#I would recommend using the estimated regression equation from question 3, because they include more significant p-values, therefore the data will be analyzed more accurately than compared to the first model that was made.