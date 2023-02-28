# descriptive statistics
summary(Movie_Theaters)

# calling column names
colnames(Movie_Theaters)

# changing column names
colnames(Movie_Theaters) <- c("gross", "tv", "news")

# linear regression model
movie_model <- lm(gross ~ tv, data = Movie_Theaters)
summary(movie_model)

movie_model2 <- lm(gross ~ tv + news, data = Movie_Theaters)
summary(movie_model2)

# predictions
predict(movie_model2, data.frame(tv = 3.3, news = 1.6), type = "response")

predict(movie_model2, data.frame(tv = 3.5, news = 1.8), type = "response")

# confidence level
confint(movie_model2, level = .95)

predict(movie_model2, data.frame(tv=3.5, news=1.8), interval = "confidence", level = .95)
