rm(list = ls())
set.seed(21)

library(xlsx)
library(tidyverse)
library(agricolae)

setwd('~/Desktop/Cubeta Lab 2021-2022/Boxwood projects/Low Gap Field Experiments/Experiment_2/')

dat_2021 <- 
  read.xlsx('Expt2_LowGap_Combined_2021.xlsx', 1)

str(dat_2021)

dat_2021$Treatment. <- gsub('Green Velvet2', 'Green Velvet', dat_2021$Treatment.)
dat_2021$Treatment. <- as.factor(dat_2021$Treatment.)

# audpc

dates <- unique(dat_2021$Date)
days <- c() # create string of days between ratings

for (rating in seq_along(dates)) {
  days[rating] <- dates[rating] - dates[1]
}

dat_2021$days <- rep(days, each = 78) # add 'days' to df

dat_2021 %>%
  group_by(days, Treatment.) %>%
  summarise(mean_pleaf = mean(pleaf)) %>%
  group_by(Treatment.) %>%
  summarise(AUDPC = audpc(mean_pleaf, days))