# calling column names
colnames(Ships)

# linear regression model
ships_model <- lm(Overall ~ Excursions, data = Ships)
summary(ships_model)

ships_model2 <- lm(Overall ~ Excursions + Food, data = Ships)
summary(ships_model2)

# prediction
predict(ships_model2, data.frame(Excursions = 80, Food = 90), type = "response")
