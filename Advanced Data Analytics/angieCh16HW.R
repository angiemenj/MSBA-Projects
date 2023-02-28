#Q1
rm(list = ls())
Q1model <- lm(y ~ x, data = Ch16Q1)
summary(Q1model)
Q1model$coefficients

# creating a squared variable of x
Ch16Q1$x2 <- Ch16Q1$x^2

Q1model2 <- lm(y ~ x + x2, data = Ch16Q1)
summary(Q1model2)

predict(Q1model2, data.frame(x = 25, x2 = 625))

#Q2
Q2model <- lm(y ~ x, data = CH16Q2)
summary(Q2model)
anova(Q2model)

CH16Q2$x2 <- CH16Q2$x^2
Q2model2 <- lm(y ~ x + x2, data = CH16Q2)
summary(Q2model2)
anova(Q2model2)

predict(Q2model2, data.frame(x = 14, x2 = 196))

#Q3
Q3model <- lm(traffic ~ speed, data = Ch16Q3)
summary(Q3model)

#taking out scientific notation
options(scipen = 999)

summary(Q3model)
anova(Q3model)

#Q4
rm(list = ls())
Ch16Q4$speed2 <- Ch16Q4$speed^2
Q4model <- lm(traffic ~ speed + speed2, data = Ch16Q4)
summary(Q4model)

predict(Q4model, data.frame(speed = 38, speed2 = 1444))

#Q5
rm(list = ls())
Ch16Q5$facilities2 <- Ch16Q5$facilities^2
Q4model2 <- lm(distance ~ facilities + facilities2, data = Ch16Q5)
summary(Q4model2)

Q4model3 <- lm(distance ~ facilities/100, data = Ch16Q5)
summary(Q4model3)
Q4model3$coefficients

#Q6
rm(list = ls())
Ch16Q6$rating2 <- Ch16Q6$rating^2
Q6model <- lm(price ~ rating + rating2, data = Ch16Q6)
summary(Q6model)

# linear regression with log
Q6model3 <- lm(log10(price) ~ log10(rating), data = Ch16Q6)
summary(Q6model3)

#Q9
rm(list = ls())
colnames(Ch16Q9)
Q9model <- lm(Risk ~ Age + Pressure, data = Ch16Q9)
summary(Q9model)
anova(Q9model)

rm(list = ls())
colnames(Ch16Q9)
colnames(Ch16Q9) <- c("risk", "age", "pressure", "smoker", "age2")
Q9model2 <- lm(risk ~ age + pressure + smoker + age2, data = Ch16Q9)
summary(Q9model2)

#Q13
colnames(Q13)
Q13model <- lm(Yield ~ Years + Years2, data = Q13)
summary(Q13model)
options(scipen = 999)
summary(Q13model)

# linear regression with natural log
Q13model2 <- lm(Yield ~ log(Years), data = Q13)
summary(Q13model2)

#Q14
Q14model <- lm(delay ~ industry + public + quality + finished, data = Q14)
summary(Q14model)