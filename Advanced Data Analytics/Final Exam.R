# clear environment
rm(list=ls())

#Q1
Q1$Year2 <- Q1$Year^2
Q1model <- lm(Revenue ~ Year + Year2, data = Q1)
summary(Q1model)

predict(Q1model, data.frame(Year=11, Year2=121))

#Q2
colnames(Q2)
colnames(Q2) <- c('risk', 'age', 'blood', 'smoker', 'ageblood')

Q2model <- lm(risk ~ age + blood + smoker + ageblood, data = Q2)
summary(Q2model)

#Q3
colnames(Q3) <- c('yds_att', 'int_att', 'win')
Q3model <- lm(win ~ yds_att, data = Q3)
summary(Q3model)

Q3model2 <- lm(win ~ int_att, data = Q3)
summary(Q3model2)

Q3model3 <- lm(win ~ yds_att + int_att, data = Q3)
summary(Q3model3)

predict(Q3model3, data.frame(yds_att=6.2, int_att=0.036))

#Q4
Q4model <- lm(spend ~ family + distance + member, data = Q4)
summary(Q4model)
predict(Q4model, data.frame(family=3, distance=65, member=0))

#Q6
Q6$rating2 <- Q6$rating^2
Q6model <- lm(price ~ rating + rating2, data = Q6)
summary(Q6model)

Q6model2 <- lm(log10(price) ~ log10(rating), data = Q6)
summary(Q6model2)
options(scipen=999)

#Q7
Q7model <- lm(sales ~ years, data = Q7)
summary(Q7model)

predict(Q7model, data.frame(years=7), interval = "confidence", level = .90)
predict(Q7model, data.frame(years=7), interval = "prediction", level = .90)

#Q8
Q8$quarter <- as.factor(Q8$quarter)
Q8dummy <- as.data.frame(model.matrix(~ quarter -1, Q8))
Q8 <- cbind(Q8dummy, Q8)
Q8$period <-c(1:12)
Q8model <- lm(year ~ quarter1 + quarter2 + quarter3, data = Q8)
summary(Q8model)

predict(Q8model, data.frame(quarter1=1, quarter2=0, quarter3=0, period=13))
predict(Q8model, data.frame(quarter1=0, quarter2=1, quarter3=0, period=13))
predict(Q8model, data.frame(quarter1=0, quarter2=0, quarter3=1, period=13))
predict(Q8model, data.frame(quarter1=0, quarter2=0, quarter3=0, period=13))

Q8model1 <- lm(year ~ quarter1 + quarter2 + quarter3 + period, data = Q8)
summary(Q8model1)

predict(Q8model1, data.frame(quarter1=1, quarter2=0, quarter3=0, period=13))
predict(Q8model1, data.frame(quarter1=0, quarter2=1, quarter3=0, period=14))
predict(Q8model1, data.frame(quarter1=0, quarter2=0, quarter3=1, period=15))
predict(Q8model1, data.frame(quarter1=0, quarter2=0, quarter3=0, period=16))

# Q12
rm(list=ls())

Q12model <- lm(win ~ yds_att, data = Q12)
summary(Q12model)

Q12model2 <- lm(win ~ yds_att + int_att, data = Q12)
summary(Q12model2)

# Q13
rm(list=ls())

Q13$years2 <- Q13$years^2
Q13model <- lm(yield ~ years + years2, data = Q13)
summary(Q13model)

Q13model2 <- lm(yield ~ log(years), data = Q13)
summary(Q13model2)

# Q14
rm(list=ls())

Q14model <- lm(price ~ weight, data = Q14)
summary(Q14model)

# Q15
Q14 <- data.frame(
  x = c(17,14,24,32,36,21,28,35,44,42),
  y = c(25,30,38,45,55,39,41,47,60,48))

q14m <- lm(y ~ x, data = Q14)
coefficients(q14m)

summary(q14m)

predict(q14m, data.frame(x=30))
predict(q14m, data.frame(x=30), interval = "prediction")
