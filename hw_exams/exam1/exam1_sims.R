set.seed(1234)
age <- rnorm(100, 39, 8.5)
smage <- age - 30
health <- rnorm(100, 0.25, 5.60) + .1*smage
exam1 <- data.frame(age, smage, health)
summary(exam1)


lm.fit <- glm(health ~ 1 + age)
summary(lm.fit)


lm.fit.2 <- glm(health ~ 1 + smage)
summary(lm.fit.2)
