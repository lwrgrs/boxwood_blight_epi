rm(list=ls())
set.seed(21)

library(xlsx)
library(tidyverse)
library(agricolae)
library(ibd)

setwd("~/Desktop/Cubeta Lab 2021-2022/Boxwood projects/Low Gap Field Experiments/Experiment_1/")

dat_2021 <- 
  read.xlsx("Expt1_LowGap_Cultivar_AUDPC_2021.xlsx", 1) # read in data

dat_2021$Cultivar <- as.factor(dat_2021$Cultivar)
dat_2021$pleaf <- as.numeric(dat_2021$pleaf) # change data types

##
### audpc
##

dates <- unique(dat_2021$Date)
days <- c() # create string of days between ratings

for (rating in seq_along(dates)) {
  days[rating] <- dates[rating] - dates[1]
}

dat_2021$days <- rep(days, each = 105) # add 'days' to df

e1_2021_audpc <- 
  dat_2021 %>%
  group_by(days, Cultivar, Rep) %>%
  summarise(mean_pleaf = mean(pleaf)) %>%
  group_by(Cultivar, Rep) %>%
  summarise(AUDPC = audpc(mean_pleaf, days)) # calculate AUDPC for each cultivar

e1_2021_audpc_species <-
dat_2021 %>%
  group_by(days, Species, Rep) %>%
  summarise(mean_pleaf = mean(pleaf)) %>%
  group_by(Species, Rep) %>%
  summarise(AUDPC = audpc(mean_pleaf, days))

e1_2021_audpc %>% # average AUDPC across reps for each cultivar
  filter(is.na(AUDPC) == F) %>%
  group_by(Cultivar) %>%
  summarise(mean_AUDPC = mean(AUDPC),
            se = sd(AUDPC)/sqrt(n())) %>%
  as.data.frame()

##
### audpc anova
##

m1_ibd <- aov.ibd(AUDPC ~ Cultivar + Rep, data = e1_2021_audpc, details = T)
m1_ibd
HSD.test(m1_ibd$lm.obj, 'Cultivar', console = T)

m2_ibd_species <- aov.ibd(AUDPC ~ Species + Rep, data = e1_2021_audpc_species, details = T)
summary(m2_ibd_species)
HSD.test(m2_ibd_species$lm.obj, 'Species', console = T)

##
### plots
##

levels(as.factor(dat_2021$Date))

e1_2021_audpc_bar <-
e1_2021_audpc %>% # bar graph of AUDPC values for each cultivar
  group_by(Cultivar) %>%
  summarise(mean_AUDPC = mean(AUDPC),
            se = sd(AUDPC)/sqrt(n())) %>%
  filter(is.na(mean_AUDPC) == F) %>%
  ggplot(aes(x = reorder(Cultivar, mean_AUDPC), y = mean_AUDPC)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = mean_AUDPC - se,
                    ymax = mean_AUDPC + se),
                width = 0.5) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1, 
                                   vjust = 1)) +
  labs(y = 'Mean AUDPC', 
       x = 'Cultivar',
       title = 'AUDPC Values for May - October 2021', 
       subtitle = 'Low Gap, NC') +
  ylim(0, 10000)

e1_2021_audpc_species %>% # bar graph of AUDPC values for each species
  group_by(Species) %>%
  summarise(mean_AUDPC = mean(AUDPC),
            se = sd(AUDPC)/sqrt(n())) #%>%
  filter(is.na(mean_AUDPC) == F) %>%
  ggplot(aes(x = reorder(Species, mean_AUDPC), y = mean_AUDPC)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = mean_AUDPC - se,
                    ymax = mean_AUDPC + se),
                width = 0.5) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1, 
                                   vjust = 1)) +
  labs(y = 'Mean AUDPC', 
       x = 'Species',
       title = 'AUDPC Values for May - September 2021', 
       subtitle = 'Low Gap, NC') 

dpc_2021_GV <- 
  dat_2021 %>% # disease progress curve for a single cultivar
  filter(is.na(pleaf) == F,
         Cultivar == 'Green Velvet') %>%
  group_by(Date, Cultivar) %>%
  summarise(mean_pleaf = mean(pleaf)) %>%
  ggplot() +
  geom_line(aes(x = Date, y = mean_pleaf),
            color = 'chartreuse4',
            size = 2) +
  geom_point(aes(x = Date, y = mean_pleaf)) +
  scale_x_date(date_labels = "%D",
               date_breaks = "1 week") +
  labs(y = "Mean % Diseased Leaf Area",
       title = "'Green Velvet' Disease Progress Curve - 2021") +
  ylim(0, 100) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

dat_2021 %>% # check ordering of cultivars
  filter(Date == "2021-09-19", is.na(pleaf) == F) %>%
  group_by(Cultivar) %>%
  summarise(mean_pleaf = mean(pleaf), se = sd(pleaf)/sqrt(n())) %>%
  ggplot(aes(x = Cultivar, y = mean_pleaf)) +
  geom_bar(stat = 'identity', fill = 'chartreuse4') +
  theme(axis.text.x = element_text(angle = 45))

bp_oct.10.2021 <- 
  dat_2021 %>% # bar graph of disease severity for each cultivar for a single rating
  filter(Date == "2021-10-10", is.na(pleaf) == F) %>%
  group_by(Cultivar) %>%
  summarise(mean_pleaf = mean(pleaf), se = sd(pleaf)/sqrt(n())) %>%
  ggplot(aes(x = reorder(Cultivar, mean_pleaf), y = mean_pleaf)) +
  geom_bar(stat = 'identity', fill = 'chartreuse4') +
  geom_errorbar(aes(ymin = mean_pleaf - se, ymax = mean_pleaf + se),
                width = 0.5) +
  geom_text(
    label = c('bc', 'bc', 'a', 'c', 'bc', 'abc', 'abc', 'ab',
              'c', 'abc', 'c', 'c', 'bc', 'bc', 'abc', 'bc', 'bc'),
    aes(y = mean_pleaf + se, x = Cultivar),
    vjust = -0.5,
    size = 4) +
  ylim(0, 75) +
  xlab("Cultivar") +
  ylab("Mean % Diseased Leaf Area") +
  labs(title = "Low Gap, NC -- October 10, 2021") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = 'none')

##
### write plots to files
##

setwd('./Analysis Output_E1/')

pdf('dpc_2021_GV.pdf',
    height = 4,
    width = 7)

dpc_2021_GV

dev.off()
