# load in libraries
library(tidyverse)
library(Hmisc)

# get data to analyze
data_path <- "speed_dating_data.csv"
spd_dat <- read.csv(data_path)
spddat <- subset(spd_dat, select=c(iid, gender, attr1_1, attr3_1))

# get pearson's r and p value for data
spddat.rcorr = rcorr(as.matrix(spddat))
spddat.coeff = spddat.rcorr$r
spddat.p = spddat.rcorr$P
coeff = data.frame(spddat.coeff)
p = data.frame(spddat.p)

# print wanted correlation coefficient and p value
coeff <- subset(coeff, select=c(attr1_1))
coeff <- coeff[!(row.names(coeff) %in% c("iid", "gender")),]
p <- subset(p, select=c(attr1_1))
p <- p[!(row.names(p) %in% c("iid", "gender")),]
print(coeff)
print(p)

# preprocess data to remove nas and recode
spddat <- na.omit(spddat)
spddat <- unique(spddat)
spddat %>% mutate(gender = recode(gender,
                                  '0' = "Women",
                                  '1' = "Men")) -> spddat

# 2d contour plot
ggplot(spddat, aes(attr3_1, attr1_1, color=gender)) +
  geom_density_2d() +
  labs(title = "Physical attractiveness preference vs. self-perception") +
  scale_colour_discrete(labels=c("Men", "Women"), type=c("#d01c8b", "#4dac26")) +
  scale_x_continuous(breaks=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  labs(title = "Physical Attractiveness",
       subtitle = "Do participants' self-ratings impact their preferences?",
       colour = "Gender") +
  xlab("Self-Rating from 1-10") +
  ylab("Weight in Decision to Match (%)") +
  annotate("label", label="Pearson's r = 0.18, p < .001", x=5.5, y=40, family="Avenir") +
  theme_light() +
  theme(text = element_text(family="Avenir"))
