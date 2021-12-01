rm(list=ls())
set.seed(21)

library(xlsx)
library(tidyverse)
library(agricolae)
library(car)
library(ibd)

setwd("~/Desktop/Cubeta Lab 2021-2022/Boxwood projects/Low Gap Field Experiments/Experiment_1/")

dat_2021 <-
  read.xlsx('Expt1_LowGap_Cultivar_AUDPC_2021.xlsx',
            sheetIndex = 1)

head(dat_2021)
str(dat_2021)

dat_2021$Cultivar <- as.factor(dat_2021$Cultivar)
dat_2021$Rep <- as.factor(dat_2021$Rep)
dat_2021$pleaf <- as.numeric(dat_2021$pleaf)
dat_2021$pstem <- as.numeric(dat_2021$pstem)
dat_2021$pdefol <- as.numeric(dat_2021$pdefol) # changing data types

##
### IBD analysis
##

levels(as.factor(dat_2021$Date))

rating <-
  dat_2021 %>%
  filter(Date == '2021-06-24') # ibd analysis for a single date
  
m1_jun24 <- aov.ibd(pleaf ~ Cultivar + Rep, data = rating, details = T)
m1_jun24
HSD.test(m1_jun24$lm.obj, "Cultivar", console = T)

m2_jun24 <- aov.ibd(pdefol ~ Cultivar + Rep, data = rating, details = T)
m2_jun24
HSD.test(m2_jun24$lm.obj, "Cultivar", console = T)

m3_jun24 <- aov.ibd(pstem ~ Cultivar + Rep, data = rating, details = T)
m3_jun24
HSD.test(m3_jun24$lm.obj, "Cultivar", console = T)