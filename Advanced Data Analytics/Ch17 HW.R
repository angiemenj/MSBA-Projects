# Clear environment
rm(list=ls())

Q5model <- lm(y ~ t, data = Q5)
summary(Q5model)

predict(Q5model, data.frame(t=8, type = "response"))

colnames(netflixsubscribers) <- c("year", "period", "subs")
netflixmodel <- lm(subs ~ period, data = netflixsubscribers)
summary(netflixmodel)
anova(netflixmodel)

# calculating MSR
mean(netflixmodel$residuals^2)

netflixsubscribers$p2 <- netflixsubscribers$period^2
netflix2 <- lm(subs ~ period + p2, data = netflixsubscribers)
summary(netflix2)
anova(netflix2)
mean(netflix2$residuals^2)

predict(netflixmodel, list(period=7))
predict(netflix2, list(period=7, p2=49))

# Q7
rm(list=ls())

# creating a dummy variable
Q7$Q <- as.factor(Q7$Q)
Q7dummy <- as.data.frame(model.matrix(~ Q -1, Q7))

# combining dummy variables with data frame
Q7 <- cbind(Q7dummy, Q7)
Q7$period <- c(1:12)

Q7model <- lm(year ~ Q1 + Q2 + Q3, data = Q7)
summary(Q7model)

# predicting forecast
predict(Q7model, data.frame(Q1=1, Q2=0, Q3=0, period=13))
predict(Q7model, data.frame(Q1=0, Q2=1, Q3=0, period=13))
predict(Q7model, data.frame(Q1=0, Q2=0, Q3=1, period=13))
predict(Q7model, data.frame(Q1=0, Q2=0, Q3=0, period=13))

# Q8
rm(list=ls())
Q8$quarter <- as.factor(Q8$quarter)
Q8dummy <- as.data.frame(model.matrix(~ quarter -1, Q8))
Q8 <- cbind(Q8dummy, Q8)                         
Q8$period <- c(1:20)
Q8model <- lm(year1 ~ quarter1 + quarter2 + quarter3, data = Q8)
summary(Q8model)

# predicting quarterly forecast
predict(Q8model, data.frame(quarter1=1, quarter2=0, quarter3=0, period=21))
predict(Q8model, data.frame(quarter1=0, quarter2=1, quarter3=0, period=21))
predict(Q8model, data.frame(quarter1=0, quarter2=0, quarter3=1, period=21))
predict(Q8model, data.frame(quarter1=0, quarter2=0, quarter3=0, period=21))

# part 2
Q8model1 <- lm(year1 ~ quarter1 + quarter2 + quarter3 + period, data = Q8)
summary(Q8model1)
predict(Q8model1, data.frame(quarter1=1, quarter2=0, quarter3=0, period=21))
predict(Q8model1, data.frame(quarter1=0, quarter2=1, quarter3=0, period=22))
predict(Q8model1, data.frame(quarter1=0, quarter2=0, quarter3=1, period=23))
predict(Q8model1, data.frame(quarter1=0, quarter2=0, quarter3=0, period=24))
