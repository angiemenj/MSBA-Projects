# linear regression model
repairs_model <- lm(Time ~ Months + RepairType + person, data = Repairs)

# model statistics
summary(repairs_model)

smokers_model <- lm(Risk ~ Age + Pressure + Smoker, data = Smokers)
summary(smokers_model)

# prediction
predict(smokers_model, data.frame(Age = 68, Pressure = 175, Smoker = 1), type = "response")