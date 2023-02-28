# descriptive statistics
summary(autoresale_model)

# prediction
predict(autoresale_model, data.frame(Mileage = 40000, Age = 4), type = "response")

# confidence interval
predict(autoresale_model, data.frame(Mileage=40000, Age=4), interval = "confidence", level = .95)\

# prediction interval
predict(autoresale_model, data.frame(Mileage=40000, Age=4), interval = "prediction", level = .95)
