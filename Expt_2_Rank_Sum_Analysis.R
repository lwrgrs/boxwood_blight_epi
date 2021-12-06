rm(list = ls())
set.seed(21)

library(xlsx)
library(tidyverse)

setwd('~/Desktop/Cubeta Lab 2021-2022/Boxwood projects/Low Gap Field Experiments/Experiment_2/')

dat_2021 <- 
  read.xlsx('Expt2_LowGap_Combined_2021.xlsx', 1)

str(dat_2021)

dat_2021$Treatment. <- gsub('Green Velvet2', 'Green Velvet', dat_2021$Treatment.)
dat_2021$Treatment. <- as.factor(dat_2021$Treatment.)

##
### calculate rank sums
##

dat_ordered <- 
  dat_2021 %>%
  group_by(Date, Treatment.) %>% # calculate mean severity for each cultivar at each date
  summarise(mean_pleaf = mean(pleaf)) %>%
  arrange(mean_pleaf) # order by severity values

rank <- rep(1:90, times = 1)
dat_ordered$rank <- rank

dat_rank_sums <- 
  dat_ordered %>% # calculate rank sum for each cultivar
  group_by(as.factor(mean_pleaf)) %>%
  mutate(new_rank = mean(rank)) %>%
  group_by(Treatment.) %>%
  summarise(rank_sum = sum(new_rank))

mean(dat_rank_sums$rank_sum) # what is rank sum grand mean? 455

dat_rank_sums_sd <- 
  dat_rank_sums %>% # calculate deviation of each cultivar from grand mean
  mutate(difference = rank_sum - 455) %>%
  mutate(deviation = (difference/sd(rank_sum))*2)

##
### test for normality
##

hist(dat_rank_sums_sd$deviation)

shapiro.test(dat_rank_sums_sd$deviation)

qqnorm(dat_rank_sums_sd$deviation)
qqline(dat_rank_sums_sd$deviation)

##
### plots
##

x <- seq(-4, 4, length=100)
y <- dnorm(x)

plot(x, y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3", "-2", "-1", "Mean", "1", "2", "3"))

##
### write plot to file
##

setwd('./Analysis Output_E2/')

pdf('normal_dist.pdf', 
    height = 5,
    width = 7)

plot(x, y, type = "l", lwd = 2, axes = FALSE, xlab = "", ylab = "")
axis(1, at = -3:3, labels = c("-3", "-2", "-1", "Mean", "1", "2", "3"))

dev.off()
