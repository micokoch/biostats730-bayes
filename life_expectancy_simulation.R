# Simulation of multilevel model for life expectancy
# Based on https://bookdown.org/roback/bookdown-BeyondMLR/ch-3level.html

library(tidyverse)
# library(lme4)
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
hist(pop, breaks = 20)
hist(pop[pop<5000], breaks = 20)
hist(pop[pop<50000], breaks = 20)
hist(pop[pop>500000], breaks = 20)
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
# A tibble: 5 × 2
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
# A tibble: 5 × 2
# re       mean
# <chr>   <dbl>
# 1 MW     0.0596
# 2 NE     0.100 
# 3 S     -0.0392
# 4 T     -0.321 
# 5 W      0.0404
mean(df.4$gdppc_c[df.4$re == "NE"]) #0.1000429 (not exactly 0.1)





# q = GINI index in each county [0-1], 0 is equality, 1 is inequality
# Gini index for US is 0.49, and in 2010 by county, it ranged from 0.645 - 0.207
# https://www2.census.gov/library/publications/2012/acs/acsbr10-18.pdf

# q = inequality (GINI), x = sex

# y_ijk = life expectancy of person k in county j, in state i
# x_ijk = sex of person k in county j in state i
# a_ij = mean life expectancy of all people in county j in state i

# y_ijk = a_ij + d * x_ijk

colnames(df.3) <- c("county","state","region","GDP per capita (centered)","Medicaid expansion")
