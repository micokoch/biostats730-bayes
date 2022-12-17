library(tidyverse)
library(bayesplot) 
library(brms)
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
library(plyr)

url <- "http://stat.columbia.edu/~gelman/arm/examples/arsenic/wells.dat"
wells <- read.table(url)
wells <- wells %>%
  # adding some transformed and centered variables 
  mutate(y = switch, 
         dist100 = dist / 100, 
         # rescale the dist variable (measured in meters) so that it is measured in units of 100 meters
         c_dist100 = dist100 - mean (dist100),
         c_arsenic = arsenic - mean (arsenic))


# First a look at all the data
summary(wells)
# Now, a different, simple plot to look at differences in switch
mus <- ddply(wells, "switch", summarise, grp.mean=mean(dist))
mus
ggplot(wells, aes(x=dist, color=as.factor(switch))) + 
  geom_density() + 
  geom_vline(data=mus, aes(xintercept=grp.mean, color=as.factor(switch)),
             linetype="dashed")
# Finally, I like to run a traditional glm to compare results:
hw4q.glm.fit_1 <- glm(y ~ 1 + c_dist100, data = wells, family = "binomial")
summary(hw4q.glm.fit_1)
cbind(Estimate = coef(hw4q.glm.fit_1), confint(hw4q.glm.fit_1))
exp(cbind(OR = coef(hw4q.glm.fit_1), confint(hw4q.glm.fit_1)))

# Test with non-centered parameters
hw4q.glm.fit_2 <- glm(y ~ 1 + dist100, data = wells, family = "binomial")
summary(hw4q.glm.fit_2)
cbind(Estimate = coef(hw4q.glm.fit_2), confint(hw4q.glm.fit_2))
exp(cbind(OR = coef(hw4q.glm.fit_2), confint(hw4q.glm.fit_2)))

# Find OR for d = d_bar using the second fit (should = first intercept OR)
d_bar <- mean(wells$dist100)
beta_0 <- coef(hw4q.glm.fit_2)[1]
beta_1 <- coef(hw4q.glm.fit_2)[2]
(or_d_bar <- exp(beta_0 + (d_bar * beta_1)))

## Now two predictors
hw4q2_2.glm.fit_1 <- glm(y ~ 1 + c_dist100 + c_arsenic, data = wells, family = "binomial")
summary(hw4q2_2.glm.fit_1)
cbind(Estimate = coef(hw4q2_2.glm.fit_1), confint(hw4q2_2.glm.fit_1))
exp(cbind(OR = coef(hw4q2_2.glm.fit_1), confint(hw4q2_2.glm.fit_1)))

## Not centered
hw4q2_2.glm.fit_2 <- glm(y ~ 1 + dist100 + arsenic, data = wells, family = "binomial")
summary(hw4q2_2.glm.fit_2)
cbind(Estimate = coef(hw4q2_2.glm.fit_2), confint(hw4q2_2.glm.fit_2))
exp(cbind(OR = coef(hw4q2_2.glm.fit_2), confint(hw4q2_2.glm.fit_2)))

## The intercept changes, but the coefficients for betas remain the same
hw4q2_3.glm.fit_1 <- glm(y ~ 1 + c_dist100 * c_arsenic, data = wells, family = "binomial")
summary(hw4q2_3.glm.fit_1)
cbind(Estimate = coef(hw4q2_3.glm.fit_1), confint(hw4q2_3.glm.fit_1))
exp(cbind(OR = coef(hw4q2_3.glm.fit_1), confint(hw4q2_3.glm.fit_1)))

# Not centered
hw4q2_3.glm.fit_2 <- glm(y ~ 1 + dist100 * arsenic, data = wells, family = "binomial")
summary(hw4q2_3.glm.fit_2)
cbind(Estimate = coef(hw4q2_3.glm.fit_2), confint(hw4q2_3.glm.fit_2))
exp(cbind(OR = coef(hw4q2_3.glm.fit_2), confint(hw4q2_3.glm.fit_2)))




