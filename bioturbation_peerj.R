library(tidyverse)

#read in the sediment processing data 
bioturb <- read.csv("bioturbation.csv")
head(bioturb)

# A series of five figures being made. 
# You can access them by using the quick-access tab on the right, if using RStudio 

####____A) Egestion____####

figA <- ggplot(data=bioturb, aes(x = species, y = pellets_per_hr, fill = species)) +
geom_jitter(aes(color = species), position = position_jitter(0.18), size = 1.5) +
stat_summary(aes(color = species),
    fun.data="mean_cl_normal",  fun.args = list(mult=1), 
    geom = "pointrange",  size = 0.8)+
scale_color_manual(values =  c("blue", "darkorange2")) +
  labs(y = "Egestion (pellets" ~h^-1~ ")") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.position="none") 
figA

## statistics for Fig A
#is there a difference btwn the mean number of pellets egested by each spp over a 3hr period?
mod1 <- lm(pellets_per_hr ~ species, data = bioturb)
mod1
# p val is 0.005785
summary(mod1)
library(visreg)
visreg(mod1)
anova(mod1)


####____B) Pellet Weight____####
figB <- ggplot(data=bioturb, aes(x = species, y = weight_per_pellet, fill = species)) +
  geom_jitter(aes(color = species), position = position_jitter(0.18), size = 1.5) +
  stat_summary(aes(color = species),
               fun.data="mean_cl_normal",  fun.args = list(mult=1), 
               geom = "pointrange",  size = 0.8)+
  scale_color_manual(values =  c("blue", "darkorange2")) +
  labs(y = "Pellet weight (g)") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.position="none") 
figB

## statistics for Fig B
mod2 <- lm(weight_per_pellet ~ species, data = bioturb)
mod2
summary(mod2)


####____C) Sediment Processing____####

figC <- ggplot(data=bioturb, aes(x = species, y = g_per_hr, fill = species)) +
  geom_jitter(aes(color = species), position = position_jitter(0.18), size = 1.5) +
  stat_summary(aes(color = species),
               fun.data="mean_cl_normal",  fun.args = list(mult=1), 
               geom = "pointrange",  size = 0.8)+
  scale_color_manual(values =  c("blue", "darkorange2")) +
  labs(y = "Sediment Processing (g" ~h^-1~ ")") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.position="none") 
figC

## statistics for Fig C
bioturb %>% 
  group_by(species) %>% 
  summarise(mean(speed), sd(speed))

mod3d <- t.test(g_per_hr ~ species, data = bioturb)
mod3d


## OM CONTENT
bioturb_sand <- read.csv("bioturb_om.csv")

bioturb_sand2 <- bioturb_sand %>% 
  filter(species != "sand")

####____D) % Organic Matter____####

figD <- ggplot(data=bioturb_sand2, aes(x = species, y = om, fill = species)) +
  geom_jitter(aes(color = species), position = position_jitter(0.15), size = 1.6) +
  stat_summary(aes(color = species),
               fun.data="mean_cl_normal",  fun.args = list(mult=1), 
               geom = "pointrange",  size = 0.9)+
  scale_color_manual(values =  c("blue", "darkorange2")) +
  labs(y = "% Organic Matter") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=16),
    axis.text.x = element_text(size=16),
    legend.position="none") 
figD

## statistics for Fig D
mod4 <- lm(organic_cont ~ species, data = bioturb)
mod4
summary(mod4)

####____E) Speed____####
figE <- ggplot(data=bioturb, aes(x = species, y = speed, fill = species)) +
  geom_jitter(aes(color = species), position = position_jitter(0.18), size = 1.5) +
  stat_summary(aes(color = species),
               fun.data="mean_cl_normal",  fun.args = list(mult=1), 
               geom = "pointrange",  size = 0.8)+
  scale_color_manual(values =  c("blue", "darkorange2")) +
  labs(y = "Speed (m" ~h^-1~ ")") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size=16),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.position="none") 
#scale_y_continuous(limits = c(0,0.61)) 

figE

## statistics for Fig E
#is there a differene between the mean movement speed of each species?
mod2 <- lm(speed ~ species, data = bioturb)
mod2
# p val is 0.1649
summary(mod2)


####____ 4x panel of bioturbation functions____####
library(cowplot)
citation('cowplot')
bioturbx4 <- plot_grid(figA, figB, figC, figD, figE, nrow = 1)
bioturbx4

bioturb_sand2 <- na.omit(bioturb_sand) 

bioturb_sand2 %>% 
  group_by(species) %>% 
  summarise(mean(om))

#Anova test
library(ggpubr)
library(rstatix)

levels(bioturb_sand$species)

#reorder levels
bioturb_sand <- bioturb_sand %>%
  reorder_levels(species, order = c("dd", "ft", "sand"))

#summary statistics
bioturb_sand %>%
  group_by(species) %>%
  get_summary_stats(om, type = "mean_sd")

#identify outliers
bioturb_sand %>% 
  group_by(species) %>%
  identify_outliers(om)

# Kruskal Wallis test between three groups (2 species and sediment)

kruskal.test(om ~ species, data = bioturb_sand)

pairwise.wilcox.test(bioturb_sand$om, bioturb_sand$species,
                     p.adjust.method = "BH")

# Create a QQ plot of residuals
ggqqplot(residuals(model))

#shapiro test of normality
shapiro_test(residuals(model))
# Not normal data!  p = 0.0000000316

# Need to do Kruskal Wallis test
kruskal.test(om ~ species, data = bioturb_sand)

bioturb_sand$om = factor(bioturb_sand$om,
                     levels=c("ft", "dd", "sand"))

#multiple pairwise comparison between groups  -Dunn's test-
install.packages("dunn.test")
citation('dunn.test')

dunnTest(om ~ species,
              data=bioturb_sand,
              method="bh") 

## FIG EXPLORATION
#is there a difference between organic matter content PER GRAM of dry weight in fecal pellets between species?
om_per_g <- ggplot(data=bioturb, aes(x = species, y = om_per_g_dry, fill = species)) +
  geom_boxplot(names=c(expression(italic("H. mexicana"),italic("A. agassizii")))) + 
                 geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("salmon", "cyan3")) +
  labs(x = "Species", y = "% Organic Matter / gram dry weight") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))
om_per_g

mod5 <- t.test(om_per_g_dry ~ species, data = bioturb)
mod5

## FIG EXPLORATION
#is there a difference between organic matter content PER GRAM of ash weight in fecal pellets between species?
om_ash <- ggplot(data=bioturb, aes(x = species, y = om_per_g_ash, fill = species)) +
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("salmon", "cyan3")) +
  labs(x = "Species", y = "% Organic Matter / gram AFDW") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))
om_ash

mod5 <- t.test(om_per_g_ash ~ species, data = bioturb)
mod5

## FIG EXPLORATION
#is there a difference between the sand egestion rate (dry weight/pellets per hr) between species?
vol_eg <- ggplot(data=bioturb, aes(x = species, y = sand_vol_egestion_rate, fill = species)) +
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("salmon", "cyan3")) +
  labs(x = "Species", y = "dry weight/pellets/hr") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))
vol_eg

mod6 <- t.test(sand_vol_egestion_rate ~ species, data = bioturb)
mod6

## FIG EXPLORATION
om_rate <- ggplot(data=bioturb, aes(x = species, y = om_rate, fill = species)) +
  geom_boxplot() + geom_jitter(shape=16, position=position_jitter(0.2)) +
  scale_fill_manual(values=c("salmon", "cyan3")) +
  labs(x = "Species", y = "OM/g dry weight/hr") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))
om_rate

mod7 <- t.test(om_rate ~ species, data = bioturb)
mod7

bioturb %>% 
  group_by(species) %>% 
  filter(weight_per_pellet!='NA') %>% 
  summarize(mean(weight_per_pellet), sd(weight_per_pellet), min(length), max(length), mean(girth), 
            sd(girth), min(girth), max(girth))

## FIG EXPLORATION
##### Bioturbation (grams per hr) as a function of length####
g_per_hour <- ggplot(bioturb, aes((length), (g_per_hr),  group = species, colour = species)) +
  geom_point(size = 2.5) +
  geom_smooth(method=lm,  aes(fill = species)) +
  labs(y = "Bioturbation (grams" ~hour^-1~")", x = "Length (cm)") +
    guides (col = FALSE)

g_per_hour + scale_color_manual(values=c("deepskyblue3", "orange")) +
  scale_fill_manual(values=c("deepskyblue3", "orange"), 
                    name="Species",
                    breaks=c("dd", "ft"),
                    labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
  theme (legend.position = "none",
        panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.title.x = element_text(size=18, colour = "black"),
        axis.title.y = element_text(size=18, colour = "black"),
        axis.text.y = element_text(size=18, colour = "black"),
        axis.text.x = element_text(size=18, colour = "black")) 

mod3 <- lm(g_per_hr ~ species, data = bioturb)
summary(mod3)
library(car)
Anova(mod3)

bioturb %>%
  group_by(species) %>%
  summarise(mean(g_per_hr), sd(g_per_hr)/sqrt(20))

## FIG EXPLORATION
#speed as a function of sea cucumber size index
biots <- ggplot(bioturb, aes((sizeindex), (speed),  group = species, colour = species)) +
  geom_point(size = 3) +
  #geom_line(aes(y = lwr, colour = species), linetype = "blank") +
  #geom_line(aes(y = upr, colour = species), linetype = "blank") +
  geom_smooth(method=lm,  aes(fill = species)) +
  labs(y = "Speed (metres/hour)", x = "Size Index") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))
biots

mod4 <- lm(speed ~ sizeindex * species, data = bioturb)
summary(mod4)
visreg(mod4)
anova(mod4)
drop1(mod4)


## FIG EXPLORATION
#speed as a function of sea cucumber size index
explore <- ggplot(bioturb, aes((length), (weight_per_pellet),  group = species, colour = species)) +
  geom_point(size = 3) +
  #geom_line(aes(y = lwr, colour = species), linetype = "blank") +
  #geom_line(aes(y = upr, colour = species), linetype = "blank") +
  geom_smooth(method=lm,  aes(fill = species)) +
  labs(y = "weight per pellet", x = "length") +
  theme( 
    panel.background = element_rect(fill = "transparent",colour = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"),
    axis.title.x = element_text(size=18),
    axis.title.y = element_text(size=18),
    axis.text.y = element_text(size=18),
    axis.text.x = element_text(size=18),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 16))

explore

mod5 <- lm(weight_per_pellet ~ length * species, data = bioturb)
summary(mod5)
visreg(mod5)
anova(mod5)
drop1(mod5)



