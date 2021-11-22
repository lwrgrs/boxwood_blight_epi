rm(list=ls())
set.seed(20)

library(xlsx)
library(tidyverse)
library(agricolae)
library(ibd)

setwd("~/Desktop/Cubeta Lab 2021-2022/Boxwood projects/Low Gap Field Experiments/Experiment_1/")

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

##
### audpc
##

dates <- unique(dat_2020$Date)
days <- c() # create string of days between dates

for (rating in seq_along(dates)) {
  days[rating] <- dates[rating] - dates[1]
} 

dat_2020$days <- rep(days, each = 105) 

e1_2020_audpc <-
  dat_2020 %>%
  group_by(days, Treatment., Rep) %>%
  summarise(mean_pleaf = mean(pleaf)) %>%
  group_by(Treatment., Rep) %>%
  summarise(AUDPC = audpc(mean_pleaf, days)) # calculate AUDPC for each cultivar

e1_2020_audpc_species <-
dat_2020 %>%
  group_by(days, Species, Rep) %>%
  summarise(mean_pleaf = mean(pleaf)) %>%
  group_by(Species, Rep) %>%
  summarise(AUDPC = audpc(mean_pleaf, days)) # calculate AUDPC for each species

##
### audpc anova
##

m1_2020 <- aov(AUDPC ~ Treatment. + Rep, data = e1_2020_audpc) # cultivar
summary(m1_2020)

HSD.test(m1_2020, 'Treatment.', console = T)

m2_2020 <- aov(AUDPC ~ Species + Rep, data = e1_2020_audpc_species) # species
summary(m2_2020)

HSD.test(m2_2020, 'Species', console = T)

##
### plots
##

e1_2020_audpc_bar <-
e1_2020_audpc %>% # bar graph of AUDPC values for each cultivar
  group_by(Treatment.) %>%
  summarise(mean_AUDPC = mean(AUDPC),
            se = sd(AUDPC)/sqrt(n())) %>%
  ggplot(aes(x = reorder(Treatment., mean_AUDPC), y = mean_AUDPC)) +
  geom_bar(stat = 'identity') +
  geom_errorbar(aes(ymin = mean_AUDPC - se,
                    ymax = mean_AUDPC + se),
                width = 0.5) +
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1, 
                                   vjust = 1)) +
  labs(y = 'Mean AUDPC', 
       x = 'Cultivar',
       title = 'AUDPC Values for May - September 2020', 
       subtitle = 'Low Gap, NC') 

e1_2020_audpc_species %>% # bar graph of AUDPC values for each species
  group_by(Species) %>%
  summarise(mean_AUDPC = mean(AUDPC),
            se = sd(AUDPC)/sqrt(n())) %>%
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
       title = 'AUDPC Values for May - September 2020', 
       subtitle = 'Low Gap, NC') 

dpc_2020 <- 
  dat_2020 %>% # disease progress curve for a single cultivar
  filter(is.na(pleaf) == F,
         Date != '2020-09-21',
         Date != '2020-11-01',
         Treatment. == 'Dee Runk') %>%
  group_by(Date, Treatment.) %>%
  summarise(mean_pleaf = mean(pleaf)) %>%
  ggplot() +
  geom_line(aes(x = Date, y = mean_pleaf),
            color = 'chocolate2',
            size = 2) +
  geom_point(aes(x = Date, y = mean_pleaf)) +
  scale_x_date(date_labels = "%D",
               date_breaks = "1 week") +
  labs(y = "Mean % Diseased Leaf Area",
       title = "'Dee Runk' Disease Progress Curve - 2020") +
  ylim(0, 100) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1,
                                   vjust = 1))

dat_2020 %>% # to see how cultivars are ordered
  filter(Date == "2020-06-28",
         is.na(pleaf) == F) %>%
  group_by(Treatment.) %>%
  summarise(mean_pleaf = mean(pleaf),
            se = sd(pleaf)/sqrt(n())) %>%
  ggplot(aes(x = Treatment.,
             y = mean_pleaf)) +
  geom_bar(stat = 'identity',
           fill = 'chartreuse4') +
  theme(axis.text.x = element_text(angle = 45))

bp_sep2020 <-
  dat_2020 %>% # bar graph of mean severity for each cultivar for a single date; significance letters manually added
  filter(Date == "2020-09-14",
         is.na(pleaf) == F) %>%
  group_by(Treatment.) %>%
  summarise(mean_pleaf = mean(pleaf),
            se = sd(pleaf)/sqrt(n())) %>%
  ggplot(aes(x = reorder(Treatment., mean_pleaf),
             y = mean_pleaf)) +
  geom_bar(stat = 'identity',
           fill = 'chocolate2') +
  geom_errorbar(aes(ymin = mean_pleaf - se,
                    ymax = mean_pleaf + se),
                width = 0.5) +
  geom_text(label = c('ef', 'ef', 'ab', 'f', 'bcd', 'f', 'def', 'def',
                      'cde', 'f', 'cdef', 'bc', 'ef', 'f', 'f', 'f', 'ef',
                      'a', 'ef', 'a', 'f'),
            aes(y = mean_pleaf + se, x = Treatment.),
            vjust = -0.5,
            size = 4) +
  ylim(0, 160) +
  xlab("Cultivar") +
  ylab("Mean % Diseased Leaf Area") +
  labs(title = "Low Gap, NC -- September 14, 2020") +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
        legend.position = 'none')

##
### write plots to files
##

setwd('./Analysis Output_E1/')

pdf('audpc_bar_2020.pdf', 
    height = 3,
    width = 7)

e1_2020_audpc_bar

dev.off()
