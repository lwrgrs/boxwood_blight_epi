rm(list=ls())
set.seed(22)

library(xlsx)
library(tidyverse)

setwd("~/Desktop/Cubeta Lab 2021-2022/Boxwood projects/Low Gap Field Experiments/Experiment_1/")

###
### 2021 data
###

dat_2021 <- 
  read.xlsx("Expt1_LowGap_Cultivar_AUDPC_2021.xlsx", 1) # read in data

str(dat_2021)
levels(as.factor(dat_2021$Cultivar))

dat_2021$Cultivar <- as.factor(dat_2021$Cultivar) # change data types
dat_2021$pleaf <- as.numeric(dat_2021$pleaf) 
dat_2021$pstem <- as.numeric(dat_2021$pstem)
dat_2021$pdefol <- as.numeric(dat_2021$pdefol)

### calculate rank sums

d <- dat_2021 %>%
  filter(is.na(pleaf) != T,
         is.na(pstem) != T,
         is.na(pdefol) != T) %>%
  group_by(Cultivar) %>% # calculate mean severity for each cultivar at each date
  summarise(mean_pleaf = mean(pleaf),
            mean_pstem = mean(pstem),
            mean_pdefol = mean(pdefol)) %>%
  arrange(mean_pleaf) %>%
  mutate(rank_pleaf = rep(1:17)) %>%
  arrange(mean_pstem) %>%
  mutate(rank_pstem = rep(1:17)) %>%
  arrange(mean_pdefol) %>%
  mutate(rank_pdefol = rep(1:17))

d_rank_sums <- d %>% # calculate rank sum for each cultivar
  group_by(as.factor(mean_pleaf)) %>%
  mutate(new_rank_pleaf = mean(rank_pleaf)) %>%
  group_by(as.factor(mean_pstem)) %>%
  mutate(new_rank_pstem = mean(rank_pstem)) %>%
  group_by(as.factor(mean_pdefol)) %>%
  mutate(new_rank_pdefol = mean(rank_pdefol)) %>%
  select(Cultivar, new_rank_pdefol, new_rank_pleaf, new_rank_pstem) %>%
  group_by(Cultivar) %>%
  summarise(rank_sum = new_rank_pleaf + new_rank_pstem + new_rank_pdefol)

mean(d_rank_sums$rank_sum) # grand mean: 27

d_rank_sums_sd <- d_rank_sums %>% # calculate deviation of each cultivar from grand mean
  mutate(difference = rank_sum - 27) %>%
  mutate(deviation = (difference/sd(rank_sum))*2)

shapiro.test(d_rank_sums_sd$deviation) # test for normality

###
### 2020 data
###

dat_2020 <- 
  data.frame(
    read.xlsx(
      "Expt1_LowGap_Cultivar_Ratings_2020.xlsx",
      sheetIndex = 1
    )
  ) # read in data

dat_2020$Treatment. <- as.factor(dat_2020$Treatment.)
dat_2020$Rep <- as.factor(dat_2020$Rep)
dat_2020$pleaf <- as.numeric(dat_2020$pleaf)
dat_2020$pdefol <- as.numeric(dat_2020$pdefol)
dat_2020$pstem <- as.numeric(dat_2020$pstem) # changing data types

### calculate rank sums

d <- dat_2020 %>%
  filter(is.na(pleaf) != T,
         is.na(pstem) != T,
         is.na(pdefol) != T) %>%
  group_by(Treatment.) %>% # calculate mean severity for each cultivar at each date
  summarise(mean_pleaf = mean(pleaf),
            mean_pstem = mean(pstem),
            mean_pdefol = mean(pdefol)) %>%
  arrange(mean_pleaf) %>%
  mutate(rank_pleaf = rep(1:21)) %>%
  arrange(mean_pstem) %>%
  mutate(rank_pstem = rep(1:21)) %>%
  arrange(mean_pdefol) %>%
  mutate(rank_pdefol = rep(1:21))

d_rank_sums <- d %>% # calculate rank sum for each cultivar
  group_by(as.factor(mean_pleaf)) %>%
  mutate(new_rank_pleaf = mean(rank_pleaf)) %>%
  group_by(as.factor(mean_pstem)) %>%
  mutate(new_rank_pstem = mean(rank_pstem)) %>%
  group_by(as.factor(mean_pdefol)) %>%
  mutate(new_rank_pdefol = mean(rank_pdefol)) %>%
  select(Treatment., new_rank_pdefol, new_rank_pleaf, new_rank_pstem) %>%
  group_by(Treatment.) %>%
  summarise(rank_sum = new_rank_pleaf + new_rank_pstem + new_rank_pdefol)

mean(d_rank_sums$rank_sum) # grand mean: 33

d_rank_sums_sd <- d_rank_sums %>% # calculate deviation of each cultivar from grand mean
  mutate(difference = rank_sum - 33) %>%
  mutate(deviation = (difference/sd(rank_sum))*2)

shapiro.test(d_rank_sums_sd$deviation) # test for normality

###
### combined years
###

dat_2021 <- 
  read.xlsx("Expt1_LowGap_Cultivar_AUDPC_2021.xlsx", 1) # read in data

dat_2020 <- 
  data.frame(read.xlsx("Expt1_LowGap_Cultivar_Ratings_2020.xlsx", 1)) # read in data

dat_2020$year <- rep(2020, times = 1050)
dat_2021$year <- rep(2021, times = 1050)

dat_2020 <- dat_2020 %>%
  mutate(Cultivar = Treatment.) %>%
  select(year, Date, Species, Cultivar, Rep, pleaf, pdefol, pstem)
  
dat_2021 <- dat_2021 %>%
  select(year, Date, Species, Cultivar, Rep, pleaf, pdefol, pstem)

dat_combined <- rbind(dat_2020, dat_2021)

str(dat_combined)

dat_combined$year <- as.factor(dat_combined$year)
dat_combined$Species <- as.factor(dat_combined$Species)
dat_combined$Cultivar <- as.factor(dat_combined$Cultivar)
dat_combined$Rep <- as.factor(dat_combined$Rep)
dat_combined$pleaf <- as.numeric(dat_combined$pleaf)
dat_combined$pdefol <- as.numeric(dat_combined$pdefol)
dat_combined$pstem <- as.numeric(dat_combined$pstem)

# calculate rank sums
d <- dat_combined %>%
  filter(is.na(pleaf) != T,
         is.na(pstem) != T,
         is.na(pdefol) != T) %>%
  group_by(Cultivar) %>% # calculate mean severity for each cultivar at each date
  summarise(mean_pleaf = mean(pleaf),
            mean_pstem = mean(pstem),
            mean_pdefol = mean(pdefol)) %>%
  arrange(mean_pleaf) %>%
  mutate(rank_pleaf = rep(1:21)) %>%
  arrange(mean_pstem) %>%
  mutate(rank_pstem = rep(1:21)) %>%
  arrange(mean_pdefol) %>%
  mutate(rank_pdefol = rep(1:21))

d_rank_sums <- d %>% # calculate rank sum for each cultivar
  group_by(as.factor(mean_pleaf)) %>%
  mutate(new_rank_pleaf = mean(rank_pleaf)) %>%
  group_by(as.factor(mean_pstem)) %>%
  mutate(new_rank_pstem = mean(rank_pstem)) %>%
  group_by(as.factor(mean_pdefol)) %>%
  mutate(new_rank_pdefol = mean(rank_pdefol)) %>%
  select(Cultivar, new_rank_pdefol, new_rank_pleaf, new_rank_pstem) %>%
  group_by(Cultivar) %>%
  summarise(rank_sum = new_rank_pleaf + new_rank_pstem + new_rank_pdefol)

mean(d_rank_sums$rank_sum) # grand mean: 33

d_rank_sums_sd <- d_rank_sums %>% # calculate deviation of each cultivar from grand mean
  mutate(difference = rank_sum - 33) %>%
  mutate(deviation = (difference/sd(rank_sum))*2)

shapiro.test(d_rank_sums_sd$deviation) # test for normality

###
### ranking species rather than cultivar
###

levels(dat_combined$Species)
levels(dat_combined$Cultivar)

dat_combined_2 <- dat_combined %>%
  mutate(Species = replace(as.character(Species), 
                           Cultivar == 'Sprinter' & Species == 'Bmicrophyla', 
                           'BmicroJap')) %>%
  mutate(Species = replace(as.character(Species), 
                           Cultivar == 'Franklins Gem' & Species == 'Bmicrosinica', 
                           'Bsinicainsularis')) %>%
  mutate(Species = replace(as.character(Species), 
                           Cultivar == 'Golden Dream' & Species == 'BmicroJap', 
                           'Bmicrophyla'))

dat_combined_2$Species <- as.factor(dat_combined_2$Species)

# calculate rank sums
d <- dat_combined_2 %>%
  filter(is.na(pleaf) != T,
         is.na(pstem) != T,
         is.na(pdefol) != T) %>%
  group_by(Species) %>% # calculate mean severity for each species group at each date
  summarise(mean_pleaf = mean(pleaf),
            mean_pstem = mean(pstem),
            mean_pdefol = mean(pdefol)) %>%
  arrange(mean_pleaf) %>%
  mutate(rank_pleaf = rep(1:7)) %>%
  arrange(mean_pstem) %>%
  mutate(rank_pstem = rep(1:7)) %>%
  arrange(mean_pdefol) %>%
  mutate(rank_pdefol = rep(1:7))

d_rank_sums <- d %>% # calculate rank sum for each species
  group_by(as.factor(mean_pleaf)) %>%
  mutate(new_rank_pleaf = mean(rank_pleaf)) %>%
  group_by(as.factor(mean_pstem)) %>%
  mutate(new_rank_pstem = mean(rank_pstem)) %>%
  group_by(as.factor(mean_pdefol)) %>%
  mutate(new_rank_pdefol = mean(rank_pdefol)) %>%
  select(Species, new_rank_pdefol, new_rank_pleaf, new_rank_pstem) %>%
  group_by(Species) %>%
  summarise(rank_sum = new_rank_pleaf + new_rank_pstem + new_rank_pdefol)

mean(d_rank_sums$rank_sum) # grand mean: 12

d_rank_sums_sd <- d_rank_sums %>% # calculate deviation of each cultivar from grand mean
  mutate(difference = rank_sum - 12) %>%
  mutate(deviation = (difference/sd(rank_sum))*2)

shapiro.test(d_rank_sums_sd$deviation) # test for normality