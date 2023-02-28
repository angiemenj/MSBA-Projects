#a. Write the logistic regression equation relating x1 and x2 to y.
lakeland_model <- lm(Return ~ GPA + Program, data = Lakeland)
summary(lakeland_model)
#Return = 0.32121*GPA + 0.30188*Program - 0.41348

#b. What is the interpretation of E(y) when x2 = 0?
predict(lakeland_model, data.frame(GPA = 3.00, Program = 0, type = "response"))
# It interprets the probability/chance of the student returning to college. In this case, the student is likely not to return.

#c. Use both independent variables and statistical software to compute the estimated logit.
library(stats)
lakeland_model2 <- glm(Return ~ GPA + Program, data = Lakeland, family = "binomial")
exp(lakeland_model2$coefficients[-1])
p <- 1/(1+exp(0.4039))
p
#p = 0.4003757

#d. Conduct a test for overall significance using alpha = .05.
summary(lakeland_model2)
# Using alpha = 0.5, overall model is significant because all values are identified as significant and have a p-value that is less than 0.05.

#e. Use alpha = .05 to determine whether each of the independent variables is significant.
summary(lakeland_model)
# Both independent variables are signficant. 

#f. Use the estimated logit computed in part (c) to estimate the probability that students with a 2.5 grade point average who did not attend the orientation program will return to Lakeland for their sophomore year. What is the estimated probability for students with a 2.5 grade point average who attended the orientation program?
predict(lakeland_model, data.frame(GPA = 2.5, Program = 0, type = "response"))
# = 0.3895447
predict(lakeland_model, data.frame(GPA = 2.5, Program = 1, type = "response"))
# = 0.6914272

#g. What is the estimated odds ratio for the orientation program? Interpret it.
0.6914272/(1-0.6914272)
#odd ratio = 22.4%
#The odds are 22.4% that students will not return.
# 77.6% of students will return.

#h. Would you recommend making the orientation program a required activity? Why or why not?
# Yes, I would recommend making the orientation program a required activity because there is sufficient evidence that students who attend the orientation will return is greater.
