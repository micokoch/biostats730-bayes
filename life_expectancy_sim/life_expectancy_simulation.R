# Simulation of multilevel model for life expectancy
# Based on https://bookdown.org/roback/bookdown-BeyondMLR/ch-3level.html

library(tidyverse)
library(lme4)
# library(brms)


# re - region
# The Census uses four official regions in the US (NE, MW, S, W), and I'll add one for territories (T)
re <- c("NE", "MW", "S", "W", "T")

# st = state
# US has 50 states, one federal district and five major territories
st <- seq.int(1:56)

# Combining states and regions
# States - 9 in NE, 12 in MW, 17 in S, 13 in W, 5 in T
df.1 <- data.frame(st, "re" = c(re[rep(1,9)], re[rep(2,12)], re[rep(3,17)], re[rep(4,13)], re[rep(5,5)]), 
                   row.names = st)


# hm = expanded Medicaid (0,1), roughly 40 states have Medicaid expansion
# https://www.commonwealthfund.org/blog/2022/where-do-states-stand-medicaid-expansion
set.seed(1234)
hm <- rbinom(56, 1, 0.8)
mean(hm) # 0.857

df.2 <- data.frame(df.1,hm)


# cts = number of counties in each state
# In the 50 states here are between 3 (DE) - 254 (TX) counties, mean & median = 63, and 3,140 total
# https://simple.wikipedia.org/wiki/County_(United_States)
set.seed(1234)
cts <- c(as.integer(runif(28, 3, 62)), as.integer(runif(23, 63, 134)), c(115, 120, 134, 159, 254))
# Randomly mix them
cts <- sample(cts)
mean(cts) # mean: 68
median(cts) # 61.5
sum(cts) # 3,808 counties (close enough, and we have territories)

# Randomly assign number of counties to each state and territory
df.3 <- data.frame(df.2, cts = as.integer(cts))


# pop = person
# Average county has 100,000 people, with range from 90 (HI) to 9.8 million (LA County)
# There are probably 30 counties with population over 1'000,000
set.seed(1234)
noise1 <- sample(0:9, 3808, replace = TRUE)
noise2 <- as.integer(rnorm(3808, 0, 20))^2
# I tried many variations including exponential distributions, but tried to keep it simple
pop <- as.integer((rnorm(3808, 4, 1)^8)/2.7) + 90
# Add noise to avoid unique values, especially in small counties
pop <- pop + noise1 + noise2
head(sort(pop), 20)
# Hardly any repeated county populations
tail(sort(pop), 20)
# Just checking no repeats on the tail
summary(pop)
# min 96, median 25,348, mean 91,058, max 4'202,206
sort(pop[pop>1000000])
length(pop[pop>1000000])
# There are 41 counties with a population over 1'000,000
sum(pop)
# total 346'748,781; real US population is 331.9 million
# hist(pop, breaks = 20)
# hist(pop[pop<5000], breaks = 20)
# hist(pop[pop<50000], breaks = 20)
# hist(pop[pop>500000], breaks = 20)
# The distribution looks good


df.4 <- data.frame()
cty <- 0
for(m in 1:56){
  for(n in 1:cts[m]){
    cty <- cty + 1
    df.4[cty,1] <- cty
    df.4[cty,2] <- df.3$st[m]
    df.4[cty,3] <- df.3$re[m]
    df.4[cty,4] <- df.3$hm[m]
    df.4[cty,5] <- NA
    df.4[cty,6] <- NA #df.4$gdppc_c[m]
  }
}
colnames(df.4) <- c("cty","st","re","hm","ppl","gdppc_c")

df.4 <- df.4 %>% mutate(ppl = pop)

# gdppc_c - a centered GDP per capita (in hundred thousands)
# US 2022 GDP pc is ~ $76k, with range from $47k (MS) - $103k (NY)
# DC is at $240k, and territories are from $13K - $31K
# I will use a regional average $80k for NE, $76k for MW, $66k for S, $74k for W, $38k for T
# I will use a mean of $70k for US

# gdppc_c <- c(0.1, 0.06, -0.04, 0.04, -0.32)
df.4 <- df.4 %>% 
  mutate(gdppc_c = ifelse(df.4[,3] == "NE", 0.1, 
                          ifelse(df.4[,3] == "MW", 0.06, 
                                 ifelse(df.4[,3] == "S", -0.04, 
                                        ifelse(df.4[,3] == "W", 0.04, 
                                               ifelse(df.4[,3] == "T", -0.32, NA))))))
# Test if there are any NAs
which(is.na(df.4$gdppc_c))
# Check results
df.4 %>% group_by(re) %>% summarise(mean = mean(gdppc_c))
# A tibble: 5 ?? 2
# re     mean
# <chr> <dbl>
# 1 MW     0.06
# 2 NE     0.1 
# 3 S     -0.04
# 4 T     -0.32
# 5 W      0.04

# Introduce some noise in the measurement range with sd = (0.42/6)/4 = 0.0175
set.seed(1234)
noise3 <- rnorm(3808,0,0.0175)
df.4[,6] <- df.4[,6] + noise3

# Check results
df.4 %>% group_by(re) %>% summarise(mean = mean(gdppc_c))
# A tibble: 5 ?? 2
# re       mean
# <chr>   <dbl>
# 1 MW     0.0596
# 2 NE     0.100 
# 3 S     -0.0392
# 4 T     -0.321 
# 5 W      0.0404
mean(df.4$gdppc_c[df.4$re == "NE"]) #0.1000429 (not exactly 0.1)


# ineq = GINI index in each county [0-1], 0 is equality, 1 is inequality
# Gini index for US is 0.49, and in 2010 by county, it ranged from 0.645 - 0.207
# https://www2.census.gov/library/publications/2012/acs/acsbr10-18.pdf

# Inequality should depend strongly on county population (greater in larger counties).
# Should also depend on region - ranking (from worst to best): T, S, NE, W, MW (PR has highest)
# There might be a quadratic relationship where poorer and wealthier counties have higher GINI.
# Will use whoe numbers first to facilitate interpretation, with an intercept at 24.
# ineq = 24 + 0.01*(pop/1000) + 2T + 1.5S + 1NE - 1.5MW + (10*gdppc_c)^2 - 2hm

df.5 <- df.4 %>% 
  mutate(
    regionwts = ifelse(re == "T", 2, 
                       ifelse(re == "S", 1.5, 
                              ifelse(re == "NE", 1, 
                                     ifelse(re == "MW", -1.5, 0)))), 
    ineq = (24 + (0.01*(ppl/1000)) + (regionwts) + (10*gdppc_c)^2 - (2*hm)) / 100)

summary(df.5$ineq)
# Range is from 0.2053 to 0.6554, which is exactly what I was hoping for!
# Still, I should add some noise = 0.03/6 = 0.005:
set.seed(1234)
noise4 <- rnorm(3808,0,0.005)
df.5$ineq <- df.5$ineq - noise4
summary(df.5$ineq)
# Range is from 0.2036 to 0.6634

# sx = proportion female should have an effect on life expectancy (women live 4 years longer)
# Average percentage female in US is 51%.
# I will use a beta distribution with alpha = 100 and mean = 0.51, so that:
# 100/(100 + b) = 0.51 // 0.51b = 49 // b = 96.08 = beta
set.seed(1234)
sx <- rbeta(3808, 100, 96.08)
summary(sx) # min: 0.3815, max: 0.6460, mean: 0.5094

df.6 <- df.5 %>% add_column(sx) %>% 
  mutate(ineq_c = ineq - mean(ineq), 
         sx_c = sx - 0.5, 
         ppl_10mill_c = (ppl - mean(ppl))/10000000)

summary(df.6)

# lex = life expectancy in years
# US life expectancy (CDC) is currently 76.1 years (https://www.cdc.gov/nchs/pressroom/nchs_press_releases/2022/20220831.htm)
# Real life expectancy data from downloaded CDC data
cdclife <- read.csv("life_expectancy_sim/U.S._Life_Expectancy_at_Birth_by_State_and_Census_Tract_-_2010-2015.csv")

# By census track
summary(cdclife)
# Min: 59.60, Mean: 78.31, Max: 97.50

# By county
cdclife_cty <- cdclife %>% group_by(County) %>% 
  summarise(lex = mean(Life.Expectancy, na.rm = TRUE))
summary(cdclife_cty)
# Min: 69.05, Mean: 77.74, Max: 89.50

# By state
cdclife_state <- cdclife %>% group_by(State) %>% 
  summarise(lex = mean(Life.Expectancy, na.rm = TRUE))
summary(cdclife_state)
# Min: 74.81, Mean: 78.23, Max: 81.32


# Simple approximation to life expectancy by county
# lex = 77 + gdppc_c*10 - ineq_c*20 + sx_c*30 + hm*5 - (ppl_10mill_c*5)^2

df.7 <- df.6 %>% 
  mutate(lex = 77 + gdppc_c*10 - ineq_c*20 + sx_c*30 + hm*5 - (ppl_10mill_c*5)^2)

# Add some noise!!! Range is ~18, so noise = (18/6)/6 = 0.5
set.seed(1234)
noise5 <- rnorm(3808,0,0.5)
df.7$lex <- df.7$lex - noise5
summary(df.7$lex)

# National summary (multiply population by county)
sum(df.7$ppl*df.7$lex) / sum(df.7$ppl)
# National mean is 80.28314 - a bit high (four years higher), but okay - lower than UK

# By county
summary(df.7$lex)
# Min: 69.25, Mean: 81.67, Max: 87.61; mean is a bit high, bur range is good

# Life expectancy by state
lesbyst <- df.7 %>% group_by(st) %>% summarise (lex = mean(lex))
summary(lesbyst)
# Min: 76.50, Mean: 81.56, Max: 83.75 - a bit high but not bad



# Finally, try to model relationship with a hierarchical and non-hierarchical model
# Model only with sx and ppl, exclude other predictors but use states and regions for levels
# gdppc_c was set to vary at the regional level
# ineq varied by other variables, mainly at regional and to a lesser degree state level
# hm was set to vary at the state level
# ppl varied randomly across all counties
# sx varied randomly by all counties

# Based on: https://bookdown.org/roback/bookdown-BeyondMLR/ch-3level.html#initialmodels-3level

# y_ijk = life expectancy for people in county k, in state j, in region i
# a_ij = mean life expectancy of all people in state j, in region i
# x_ijk = proportion female in county k, in state j, in region i
# p_ijk = population in county k, in state j, in region i
# e_ijk = difference in life expectancy in a given county k from the mean life expectancy
# in a given state j, in region i, where e_ijk ~ N(0,s^2)
# s^2 = variance component (random effects model parameter) that describes county to
# county variation within a state

# a_i = mean life expectancy of all people in region i
# q_ij = state level category: meant to capture hm, part of ineq
# u_ij = difference in the mean life expectancy in a given state j from the mean
# life expectancy in region i, where u_ij ~ N(0,s_u^2)
# s_u^2 = variance component that describes state to state variation within a region

# r_i = regional category: meant to capture gdppc_c and part of ineq
# u_bar_i = difference in life expectancy in region i from overall mean across all counties, 
# states, and regions, where u_bar_i ~ N(0,s_ubar^2)
# s_ubar^2 = variance component that describes region to region variation

# y_ijk = a_ij + d * x_ijk + f * p_ijk + e_ijk
# a_ij = a_i + g * q_ij + u_ij
# a_i = a_0 + h * r_i + u_bar_i

# None of the error terms (e_ijk, u_ij, u_bar_i) are considered model parameters; they simply 
# account for differences between the observed data and expected values under our model.


# To start, select the variables I will use
df.8 <- df.7 %>% select(lex, cty, st, re, ppl_10mill_c, sx_c)
# Set county and state as factors
df.8$cty <- as.factor(df.8$cty)
df.8$st <- as.factor(df.8$st)


# Split data into train and test fractions
set.seed(1234)
split1 <- sample(c(0, rep(0, 0.7 * nrow(df.8)), rep(1, 0.3 * nrow(df.8))))
table(split1)

train1 <- df.8[split1 == 0, ]
head(train1)

test1 <- df.8[split1 == 1, ]
head(test1)

# Now, to test the models
# Model A: 1 level
model.1l <- lm(lex ~ 1 + ppl_10mill_c + sx_c, data=train1)
(model.1l.summ <- summary(model.1l))
#calculate MSE
mean(model.1l.summ$residuals^2)
# 5.996114

# Predict outcomes
model.1l.predict <- predict.glm(model.1l, test1, type = "response")

#create data frame with a column of actual values and a column of predicted values
rmse.1l.test.predict <- data.frame(index = test1$cty, pred = model.1l.predict, actual = test1$lex)
#view first six lines of data
head(rmse.1l.test.predict)
#calculate MSE
mean((rmse.1l.test.predict$actual - rmse.1l.test.predict$pred)^2)
# 6.299727

# Model B: 3 levels
model.3l <- lmer(lex ~ 1 + sx_c + ppl_10mill_c + (1|st) + (1|re), data=train1)
(model.3l.summ <- summary(model.3l))
#calculate MSE
mean(model.3l.summ$residuals^2)
# 0.9782599 - MUCH smaller MSE

# Predict outcomes
model.3l.predict <- predict(model.3l, test1, type = "response")

#create data frame with a column of actual values and a column of predicted values
rmse.3l.test.predict <- data.frame(index = test1$cty, pred = model.3l.predict, actual = test1$lex)
#view first six lines of data
head(rmse.3l.test.predict)
#calculate MSE
mean((rmse.3l.test.predict$actual - rmse.3l.test.predict$pred)^2)
# 0.05916499 - MUCH smaller MSE

# Model C: 1 level with all predictors
model2.1l <- lm(lex ~ 1 + ppl_10mill_c + sx_c + st + re, data=train1)
(model2.1l.summ <- summary(model2.1l))
#calculate MSE
mean(model2.1l.summ$residuals^2)
# 0.06138665 - actually lower than multilevel model

# Predict outcomes
model2.1l.predict <- predict.glm(model2.1l, test1, type = "response")

#create data frame with a column of actual values and a column of predicted values
rmse2.1l.test.predict <- data.frame(index = test1$cty, pred = model2.1l.predict, actual = test1$lex)
#view first six lines of data
head(rmse2.1l.test.predict)
#calculate MSE
mean((rmse2.1l.test.predict$actual - rmse2.1l.test.predict$pred)^2)
# 0.05918572 - ever so slightly bigger than multilevel model - less interpretable

### Links to review (if desired)
# 1. Household income inequality pdf (with county data):
# https://www2.census.gov/library/publications/2012/acs/acsbr10-18.pdf
# 2. Prediction and training using `caret`:
# https://cran.r-project.org/web/packages/caret/vignettes/caret.html
# https://topepo.github.io/caret/

