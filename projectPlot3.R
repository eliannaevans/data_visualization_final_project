# load in libraries
library(tidyverse)

# get data to analyze
data_path <- "speed_dating_data.csv"
spddat <- read.csv(data_path)
spddat <- subset(spddat, select=c(shar1_1, shar1_s, shar1_2, shar1_3, match))

spddat <- data.frame(time=c("Before", "During", "Days After", "Weeks After",
                           "Before", "During", "Days After", "Weeks After"),
                     match=c(0, 0, 0, 0,
                            1, 1, 1, 1),
                     shar_imp=c(mean(spddat[!is.na(spddat$shar1_1) & spddat$match == 0,]$shar1_1),
                                mean(spddat[!is.na(spddat$shar1_s) & spddat$match == 0,]$shar1_s),
                                mean(spddat[!is.na(spddat$shar1_2) & spddat$match == 0,]$shar1_2),
                                mean(spddat[!is.na(spddat$shar1_3) & spddat$match == 0,]$shar1_3),
                                mean(spddat[!is.na(spddat$shar1_1) & spddat$match == 1,]$shar1_1),
                                mean(spddat[!is.na(spddat$shar1_s) & spddat$match == 1,]$shar1_s),
                                mean(spddat[!is.na(spddat$shar1_2) & spddat$match == 1,]$shar1_2),
                                mean(spddat[!is.na(spddat$shar1_3) & spddat$match == 1,]$shar1_3)))

# create plot
ggplot(spddat, aes(x=time, y=shar_imp, group=match, colour=as.character(match))) +
  geom_line() +
  geom_point(aes(shape=as.character(match))) +
  scale_x_discrete(limits=c("Before", "During", "Days After", "Weeks After")) +
  scale_colour_discrete(labels=c("Did Not Match", "Matched"), type=c("#7b3494", "#008837")) +
  scale_shape_manual(labels=c("Did Not Match", "Matched"), values=c(17, 15)) +
  labs(title = "Importance of Shared Interests Grouped By Match",
       subtitle = "Are participants who value shared interests more picky?",
       colour = "Match",
       shape = "Match") +
  xlab("Time Surveyed") +
  ylab("Importance of Shared Interests (%)") +
  theme_light() +
  theme(text = element_text(family="Avenir")) +
  ylim(c(11, 13))
