# renaming column names
colnames(nbastats) <- c("team", "win", "threep", "ft", "off", "def", "fg")

# part a
nbastats_model <- lm(win ~ fg, data = nbastats)
summary(nbastats_model) 
nbastats_model$coefficients

#part b
model2 <- lm(win ~ fg + threep + ft + off + def, data = nbastats)
summary(model2)

#part d
model3 <- lm(win ~ fg + threep + ft + def, data = nbastats)
summary(model3)

#part e
predict(model3, data.frame(fg = 45, threep = 35, ft = 35, def = ), type = "response")
