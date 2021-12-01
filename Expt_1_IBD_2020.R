rm(list=ls())
set.seed(21)

library(xlsx)
library(tidyverse)
library(agricolae)
library(car)
library(ibd)
library(crossdes)
library(lsmeans)

setwd("~/Desktop/Cubeta Lab 2021-2022/Boxwood projects/Low Gap Field Experiments/Experiment_1/")

dat_2020 <- 
  data.frame(read.xlsx("Expt1_LowGap_Cultivar_Ratings_2020.xlsx",
                       sheetIndex = 1)) # read in data

dat_2020$Treatment. <- as.factor(dat_2020$Treatment.)
dat_2020$Rep <- as.factor(dat_2020$Rep)
dat_2020$pleaf <- as.numeric(dat_2020$pleaf)
dat_2020$pdefol <- as.numeric(dat_2020$pdefol)
dat_2020$pstem <- as.numeric(dat_2020$pstem) # changing data types

##
### incomplete block anova
##

rating <-
  dat_2020 %>%
  filter(Date == "2020-11-01") # for ibd analysis for a single date

m1 <- aov.ibd(pleaf ~ Treatment. + Rep, data = rating, details = T)
m1
HSD.test(m1$lm.obj, "Treatment.", console = T)

m2 <- aov.ibd(pdefol ~ Treatment. + Rep, data = rating, details = T)
m2
HSD.test(m2$lm.obj, "Treatment.", console = T)

m3 <- aov.ibd(pstem ~ Treatment. + Rep, data = rating, details = T)
m3
HSD.test(m3$lm.obj, "Treatment.", console = T)