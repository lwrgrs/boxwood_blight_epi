rm(list = ls())
set.seed(21)

library(xlsx)
library(tidyverse)

setwd('~/Desktop/Cubeta Lab 2021-2022/Boxwood projects/Low Gap Field Experiments/Experiment_2/')

dat_2021 <- 
  read.xlsx('Expt2_LowGap_Combined_2021.xlsx', 1)

str(dat_2021)

dat_2021$Treatment. <- as.factor(dat_2021$Treatment.)

##
### calculate rank sums
##

dat_ordered <- 
  dat_2021 %>%
  group_by(Date, Treatment.) %>% # calculate mean severity for each cultivar at each date
  summarise(mean_pleaf = mean(pleaf)) %>%
  arrange(mean_pleaf) # order by severity values

rank <- rep(1:100, times = 1)
dat_ordered$rank <- rank

dat_rank_sums <- 
  dat_ordered %>% # calculate rank sum for each cultivar
  group_by(as.factor(mean_pleaf)) %>%
  mutate(new_rank = mean(rank)) %>%
  group_by(Treatment.) %>%
  summarise(rank_sum = sum(new_rank))

mean(dat_rank_sums$rank_sum) # what is rank sum grand mean? 505

dat_rank_sums_sd <- 
  dat_rank_sums %>% # calculate deviation of each cultivar from grand mean
  mutate(difference = rank_sum - 505) %>%
  mutate(deviation = (difference/sd(rank_sum))*2)

dat_rank_sums_full <- 
  dat_ordered %>% # test for normality
  group_by(as.factor(mean_pleaf)) %>%
  mutate(new_rank = mean(rank)) %>%
  group_by(Treatment.) %>%
  mutate(rank_sum = sum(new_rank))

hist(dat_rank_sums_full$rank_sum)

shapiro.test(dat_rank_sums_full$rank_sum)

qqnorm(dat_rank_sums$rank_sum)
qqline(dat_rank_sums$rank_sum)

##
### plots
##

x <- seq(-4, 4, length=100)
y <- dnorm(x)

plot(x, y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3", "-2", "-1", "Mean", "1", "2", "3"))
