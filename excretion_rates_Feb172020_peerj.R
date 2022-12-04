# DD and FT Excretion Rates - February 17 2020
library(tidyverse)
library(ggplot2)
library(readr)
library(Rcpp)
library(backports)
theme_set(theme_cowplot()) #plot figures next to one another, example: plot_grid(plot1, plot2)

#### EXCRETION DAY 1 of 2 at CEI, July 26 2019 ####
#read in standard curve and excretion data
standard <- read_csv("2019_07_26_standardcurve.csv")
excretion <- read_csv("2019_07_26_excretion.csv") %>% 
  filter(species!='NA')

standard_f <- standard %>% 
  mutate(nh4_added_umol = nh4_vol_uL/1e6 * nh4_conc_og_umol, #amount of NH4
         total_vol_L = nh4_vol_uL/1e6 + og_vol_L, #new volume of sample + NH4
         nh4_conc_final_umol_L = nh4_added_umol / total_vol_L, #concentration
         #of NH4 in seawater sample
         mean_FLU = rowMeans(cbind(FLU1, FLU2, FLU3), na.rm = TRUE)) #mean FLU

#reading linear mod between the fluorometer reading and actual concentration of NH4
sc_mod <- lm(nh4_conc_final_umol_L ~ mean_FLU, data = standard_f)
summary(sc_mod)

#extract intercept and slope to use later to determine sample concentrations
#based on fluorometry readings
int <- coef(sc_mod)[1]
slope <- coef(sc_mod)[2]

#visualize curve to make sure it looks right 
ggplot(standard_f, aes(mean_FLU, nh4_conc_final_umol_L)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

##actual samples:
excretion_f <- excretion %>% 
  mutate(inc_time_h = as.numeric(incubation.end - incubation.start, 
                                 units = "hours")) %>% #incubation time
  mutate(bag_volume_L = vol.measured/1000 + vol.syringed/1000)  %>% #volume of water in
  #bag at the time the sample was taken in litres
  mutate(mean_preFLU = rowMeans(cbind(pre.FLU1, pre.FLU2, pre.FLU3), 
                                na.rm = TRUE), mean_postFLU = rowMeans(cbind(post.FLU1, post.FLU2, post.FLU3), 
                                 na.rm = TRUE)) %>% #mean FLU readings
  mutate(int = int, #include values for the int and slope in for every column
         slope = slope) %>% #to calculate the conversion to NH4 conc
  mutate(nh4_conc_pre = (int + slope * mean_preFLU), #this gives us the initial
         #concentration of nh4 based on our standard curve in umol/L
         nh4_umol_pre = nh4_conc_pre * bag_volume_L + 0.060, #amount of NH4 
         #in umol in the whole bag based on concentration and volume
         nh4_conc_post = (int + slope * mean_postFLU) / dilution.post.ratio, 
         #concentration of nh4 after cuke was in the bag based on standard curve
         nh4_umol_post = nh4_conc_post * bag_volume_L,#amount of NH4 based on
         #concentration and volume
         nh4_umol_diff = nh4_umol_post - nh4_umol_pre, #amount of NH4 cuke 
         #added to bag in total
         nh4_rate = nh4_umol_diff / inc_time_h, #the rate at which the cuke
         #added NH4 to the bag
         nh4_rate_per_g = nh4_rate/wetweight, #the rate at which the cuke
         #added NH4 to the bag for every gram of body weight
         inc_time_min = inc_time_h * 60,
         #we don't need season or ID here anymore because we only have 1 
         #sampling period
         mass_g = wetweight, 
         sizeindex = sqrt(length*breadth),
         sizeindex2 = length*breadth*0.01,
         cyl_vol = pi * ((breadth/(2*pi))^2) * length,
         cuke_id = ziploc.number)


#### EXCRETION DAY 2 of 2 at CEI, August 1 2019 ####
standard1 <- read_csv("2019_08_01_standardcurve.csv")
excretion1 <- read_csv("2019_08_01_excretion.csv") %>% 
  filter(species!='NA')

standard_f1 <- standard1 %>% 
  mutate(nh4_added_umol = nh4_vol_uL/1e6 * nh4_conc_og_umol, #amount of NH4
         total_vol_L = nh4_vol_uL/1e6 + og_vol_L, #new volume of sample + NH4
         nh4_conc_final_umol_L = nh4_added_umol / total_vol_L, #concentration
         #of NH4 in seawater sample
         mean_FLU = rowMeans(cbind(FLU1, FLU2, FLU3), na.rm = TRUE)) #mean FLU

#reading linear model between the fluorometer reading and actual concentration of NH4
sc_mod2 <- lm(nh4_conc_final_umol_L ~ mean_FLU, data = standard_f1)
summary(sc_mod2)

#extract intercept and slope to use later to determine sample concentrations
#based on fluorometry readings
int2 <- coef(sc_mod2)[1]
slope2 <- coef(sc_mod2)[2]
#visualize curve to make sure it looks right
ggplot(standard_f1, aes(mean_FLU, nh4_conc_final_umol_L)) +
  geom_point() +
  geom_smooth(method = lm, se = FALSE)

##actual samples:
excretion_f1 <- excretion1 %>% 
  mutate(inc_time_h = as.numeric(incubation.end - incubation.start, 
                                 units = "hours")) %>% #incubation time
  mutate(bag_volume_L = vol.measured/1000 + vol.syringed/1000)  %>% #volume of water in
  #bag at the time the sample was taken
  mutate(mean_preFLU = rowMeans(cbind(pre.FLU1, pre.FLU2, pre.FLU3), 
                                na.rm = TRUE), mean_postFLU = rowMeans(cbind(post.FLU1, post.FLU2, post.FLU3), 
                                 na.rm = TRUE)) %>% 
  mutate(int = int2, #include values for the int and slope in for every column
         slope = slope2) %>% #to calculate the coversion to NH4 conc
  mutate(nh4_conc_pre = (int + slope * mean_preFLU),
         nh4_umol_pre = nh4_conc_pre * bag_volume_L + 0.060,
         nh4_conc_post = (int + slope * mean_postFLU) / dilution.post.ratio,
         nh4_umol_post = nh4_conc_post * bag_volume_L,
         nh4_umol_diff = nh4_umol_post - nh4_umol_pre,
         nh4_rate = nh4_umol_diff / inc_time_h,
         nh4_rate_per_g = nh4_rate/wetweight,
         inc_time_min = inc_time_h * 60,
         mass_g = wetweight, 
         sizeindex = sqrt(length*breadth),
         sizeindex2 = length*breadth*0.01,
         cyl_vol = 3.1415926535897932384626433832795028841971 * ((breadth/(2*3.1415926535897932384626433832795028841971))^2) * length,
         cuke_id = ziploc.number) 


##### Cucumber corrections by RM and HW in January & February 2020 #######
  # Join excretion_f1 and excretion_f2 into one df for plotting together
  excretion_bound_df <- bind_rows(excretion_f, excretion_f1) %>% 
  #calclate water that was originally in bag (slightly less than 2L) and
  #how much went missing (i.e. likely sucked up into cuke) and another
  #60mL were collected during the pre - sampling)
  mutate(missing_water = case_when(cuke_id == "FT18" ~ 1.2 - bag_volume_L,
                                   cuke_id == "FT19" ~ 1.2 - bag_volume_L,
                                   cuke_id == "FT20" ~ 1.2 - bag_volume_L,
                                   cuke_id == "DD20" ~ 1.5 - bag_volume_L,
                                   TRUE ~ 1.84 - bag_volume_L),
         true_mass = mass_g - missing_water*1020)

  #manually change the true mass for the 4 cukes that were in a different
  #volume of water
  write.csv(excretion_bound_df, "C:\\Users\\rachelmunger\\Documents\\SFU\\MSc\\MSc_Data\\cuke_msc
                                  \\chapter1\\excretion_data_cleaned_2020_02_11.csv")
  
  
  
  
####___Prelim exploratory FIGURES####
#____Fig 1
#previous (and incorrectly) measuerd_mass
bahamas_excretion_measured_mass <- ggplot(excretion_bound_df, aes((mass_g), (nh4_rate), 
                                                                    group = species, colour = species)) +
   geom_point() +
   geom_smooth(method = 'lm',  aes(fill = species)) +
   labs(y = "Ammonium Excretion (umol/h)", x = " True Mass (g)")

   #this is looking at it with uncorrected mass on x axis
   bahamas_excretion_measured_mass

#____Fig 3
#plotting of both excretion days using true_mass
bahamas_excretion_true_mass <- ggplot(excretion_bound_df, aes((true_mass), (nh4_rate), 
                                                    group = species, colour = species)) +
  geom_point() +
  geom_smooth(method = 'lm',  aes(fill = species)) +
  labs(y = "Ammonium Excretion (umol/h)", x = " True Mass (g)") 

bahamas_excretion_true_mass


####___Correcting for total amount of water in bag------------####
#if we assume the cukes have sucked up that missing water, we should
#include that water in our total volume since we multiply the total volume by
#the concentration to get the excretion rate

corrected_df <- excretion_bound_df %>%  
  mutate(cor_vol_L = bag_volume_L + missing_water,
         nh4_conc_pre = (int + slope * mean_preFLU),
         nh4_umol_pre = nh4_conc_pre * cor_vol_L + 0.060,
         nh4_conc_post = (int + slope * mean_postFLU) / dilution.post.ratio,
         nh4_umol_post = nh4_conc_post * cor_vol_L,
         nh4_umol_diff = nh4_umol_post - nh4_umol_pre,
         nh4_rate = nh4_umol_diff / inc_time_h,
         nh4_rate_per_g = nh4_rate/wetweight,
         log_nh4_rate = log(nh4_rate))

#check to see if this changes the relationship
#removing 1 outlier (donkey dung with negative mass, row 20)
outlier_removed <- corrected_df[-c(20), ]
outlier_removed

outlier_removed2 <- outlier_removed %>% 
  mutate(log_true_mass = log(true_mass),
         log_length = log(length), 
         nh4_rate_per_g2 = nh4_rate/true_mass)

#____Fig 4A
# plotting with 1 outlier_removed, 39 sea cucumbers total now
corrected_total_water2 <- ggplot(outlier_removed, aes(x = true_mass, y = nh4_rate, group = species, colour = species)) +
  geom_point() +
  geom_smooth(method = 'lm',  aes(fill = species)) +
  labs(y = "Ammonium Excretion (umol/h)", x = " Mass (g)") 

  corrected_total_water2

##Just need to add a '+' to join the black background theme
  theme( # This part removes all the background and makes the plot transparent
    panel.background = element_rect(fill = "transparent",colour = NA), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    plot.background = element_rect(fill = "black",colour = NA)) +
  theme(legend.position = "none",
    legend.title = element_text(colour = "black", size = 11),
        axis.title = element_text(colour = "white", size = 14),
        legend.text = element_text(colour = "black", size = 8),
        axis.line.x = element_line(colour = "white"),
        axis.line.y = element_line(colour = "white"),
        axis.ticks.x = element_line(colour = "white"),
        axis.ticks.y = element_line(colour = "white"),
        axis.text.x = element_text(colour = "white"),
        axis.text.y = element_text(colour = "white"))

# Plot by day as well so that we can see if there's an "effect" of day, just looking visually 
#_____Fig 5____________________________________________________________________
corrected_df_bydate <- ggplot(outlier_removed, aes(x = true_mass, y = nh4_rate, 
                                                group = species, colour = date)) +
  geom_point() +
  geom_smooth(method = 'lm') +
  labs(y = "Ammonium Excretion (umol/h)", x = " Mass (g)") 

  corrected_df_bydate

  
  
####___BASIC MODEL_____________####

#June 12 ANCOVA 2020 using guidance from this site https://biol607.github.io/lab/11_glm_aic.html
#lm of excretion rate for two species
mod_1 <- lm(nh4_rate ~ true_mass + species,
            data=outlier_removed)
coef(mod_1)
summary(mod_1)
Anova(mod_1)

#testing asumptions of ANCOVA
par(mfrow=c(2,2))
plot(mod_1, which=c(1,2,5))
par(mfrow=c(1,1))
#histogram of residuals
hist(resid(mod_1))
#OR you can do: and it will say "Hit <Return> to see next plot:
plot(mod_1)

#And now look at residuals by group/predictors
library(car)
residualPlots(mod_1, tests=FALSE)

#test parallel presumption?
mod_2 <- lm(nh4_rate ~ true_mass * species, 
            data=outlier_removed)
summary(mod_2)
Anova(mod_2, type = "III")
drop1(mod_2, test='F')
#can see that there is no interaction so we are OK to proceed with parallel lines model (mod_1)

#F-tests using type III sum of squares
Anova(mod_1, type = "III")
#anova(mod_1)
anova(mod_1, mod_2)

mod_1$coefficients

library(emmeans)
adj_means <- emmeans(mod_1, ~species)
#adjusted means
adj_means

avg_sp_means <- emmeans(mod_1, ~species|true_mass)
avg_sp_means

#comparisons
contrast(avg_sp_means, method="tukey", adjust="none")

#no interaction
emtrends(mod_1, ~species, var = "true_mass")

##model outputs
drop1(mod_1, test='F')
summary(mod_1)
confint(mod_1, level = 0.95)
visreg(mod_1)




##__AIC - not using at this time but just exploring_##
 #null model mod_0
 mod_00 <- lm(nh4_rate ~ 1, data=outlier_removed)

 mod_11 <- lm(nh4_rate ~ true_mass, data=outlier_removed)  

 mod_22 <- lm(nh4_rate ~ sizeindex, data=outlier_removed)  

 mod_33 <- lm(nh4_rate ~ true_mass + species, data=outlier_removed)
  
 mod_44 <- lm(nh4_rate ~ true_mass * species, data=outlier_removed)

 mod_55 <- lm(nh4_rate ~ sizeindex + species, data=outlier_removed)

 mod_66 <- lm(nh4_rate ~ sizeindex * species, data=outlier_removed)

  library(survival)
  library(kimisc) # has the nlist function to create a named list
  library(AICcmodavg) # has the aictab function
  library(dplyr)
  library(ggplot2)
  library(reshape2)

  #smaller AIC value indicates better fit of several potential candidate models 
  #AIC acts as a guard against overfitting. 
  AICc(mod_00)
  AICc(mod_11)
  AICc(mod_22)
  AICc(mod_33)
  AICc(mod_44)
  AICc(mod_55)
  AICc(mod_66)

  # Put the models all together in a named list using nlist function from kimisc package
  model_list <- lst(mod_00, mod_11, mod_22, mod_33, mod_44, mod_55, mod_66)
 
  # Compare models with AIC table
  aic_table <- aictab(model_list)
  aic_table
  
  #top model was mod_3
  summary(mod_33)
  
  
####___Model + Plot with Confidence Intervals_####
  # change interval to "prediction" to create prediction intervals later if you need, 
  # and change linetype from "blank" to "dashed" in geom_line
  # using model from line 543!!
  cuke_predict <- predict(modelFeb24, interval="confidence")
  
  head(cuke_predict)
  
  cuke_ci <- cbind(outlier_removed, cuke_predict)
  
  
  #nh4_rate_per_g = nh4_rate/wetweight, the rate at which the cuke
  #added NH4 to the bag for every gram of body weight
  per_g <- ggplot(outlier_removed2, aes(x = true_mass, y = nh4_rate_per_g,  group = species, colour= species)) + 
    geom_point(size = 2.6) +
    geom_smooth(method = 'lm', aes(fill = species)) +
    labs(y = "Ammonium Excretion (umol per gram)", x = " Wet Mass (g)") +
    guides (col = FALSE) 
  per_g
  
  outlier_removed %>% 
    group_by(species) %>% 
    summarise(mean(nh4_rate_per_g))
  
  ####___Model + Plot with Prediction Intervals_####
  # 0. Build linear model (code from Jess)
  
  #center body size
  library(tidyverse)
  outlier_removed <- outlier_removed %>% mutate(true_mass.c = true_mass - (mean(true_mass)))
  outlier_removed <- outlier_removed %>% mutate(nh4_rate_per_g.c = nh4_rate_per_g - (mean(nh4_rate_per_g)))
  
  library(ggplot2)
  modelFeb24 <- lm(nh4_rate ~ true_mass.c + species, data = outlier_removed)
  summary(modelFeb24)
  
  
  # Print summary
  summary(modelFeb24)$coefficients
  
  outlier_removed %>%
    group_by(species) %>%
    summarise(max(true_mass), min(true_mass))
  
  # 1. Add predictions 
  ##prediciton intervals are around a single point, whereas a confidence interval is based around the mean
  pred.int <- predict(modelFeb24, interval = "prediction")
  rm_data <- cbind(outlier_removed, pred.int)
  
  # 2. Regression line + intervals
  pi <-  ggplot(rm_data, aes(x = true_mass.c, y  = nh4_rate,  group = species, colour = species)) +
    geom_point() +
    #geom_line(aes(y = lwr, colour = species), linetype = "dashed") +
    #geom_line(aes(y = upr, colour = species), linetype = "dashed") +
    geom_smooth(method=lm,  aes(fill = species)) +
    labs(y = "Ammonium Excretion (umol" ~hour^-1~ ")", x = " Mass (g)") +
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
  pi
  
  
  
  library(ggplot2)
  modelFeb24 <- lm(nh4_rate ~ true_mass.c + species, data = outlier_removed)
  summary(modelFeb24)
  
  
  #### FIGURES BELOW ####
  ####1A) NH4 hr ~ true_mass with Confidence Intervals ####
  # use new cuke_ci
  
  p0 <- ggplot(cuke_ci, aes(x = true_mass, y = nh4_rate,  group = species, colour= species)) + 
    geom_point(size = 2.6) +
    geom_line(aes(y = lwr, colour = species), linetype = "dashed") +
    geom_line(aes(y = upr, colour = species), linetype = "dashed") +
    geom_smooth(method = 'lm', aes(fill = species)) +
    labs(y = "Ammonium Excretion (umol" ~hour^-1~ ")", x = " Wet Mass (g)") +
    guides (col = FALSE) 
  p0
  
  #### 1B) PEERJ Supplementary Fig 2 - NH4 hr ~ true_mass with Confidence Intervals ####
  # use new cuke_ci
  ## DEC 2022 ##
 p2 <-  ggplot(cuke_ci, aes(x = true_mass, y = nh4_rate, group = species, colour = species)) + 
    geom_point(size = 2.6) +
    geom_ribbon(aes(ymin=lwr, ymax=upr, fill = species), linetype = 0, alpha=0.6) +
    geom_smooth(method = 'lm', se= F) +
    scale_fill_manual(values = c("dd"="blue", "ft"="darkorange2")) +
    guides (col = FALSE) +
    labs(y = "Ammonium Excretion (umol" ~hour^-1~ ")", x = " Wet Mass (g)") 
  
  p2 + scale_color_manual(values=c("blue", "darkorange2")) +
  theme(panel.background = element_rect(fill = "transparent",colour = NA),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
        panel.border = element_blank(),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        axis.title.x = element_text(size=18, colour = "black"),
        axis.title.y = element_text(size=18, colour = "black"),
        axis.text.y = element_text(size=18, colour = "black"),
        axis.text.x = element_text(size=18, colour = "black")) 
  
  
  
  
  ## FIG for WSN 2020
   p0 + scale_color_manual(values=c("blue", "darkorange2")) +
    scale_fill_manual(values=c("blue", "darkorange2"),
                      name="Species",
                      breaks=c("dd", "ft"),
                      labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
     theme(legend.position = "none", 
           panel.background = element_rect(fill = "transparent",colour = NA),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank(),
           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
           axis.line.x = element_line(colour = "white"),
           axis.line.y = element_line(colour = "white"),
           axis.title.x = element_text(size=18, colour = "white"),
           axis.title.y = element_text(size=18, colour = "white"),
           axis.text.y = element_text(size=18, colour = "white"),
           axis.text.x = element_text(size=18, colour = "white")) 
   
   
   p0 <- ggplot(cuke_ci, aes(x = true_mass, y = nh4_rate,  group = species, colour= species)) +
     geom_point(size = 3) +
     geom_line(aes(y = lwr, colour = species), linetype = "dashed") +
     geom_line(aes(y = upr, colour = species), linetype = "dashed") +
     geom_smooth(method=lm,  aes(fill = species)) +
     labs(y = "Ammonium Excretion (umol" ~hour^-1~ ")", x = " Wet Mass (g)") +
     guides (col = FALSE) 
   
  
   ?ggplot
   
  
   #### nh4 rate - mass 
   p0 <- ggplot(cuke_ci, aes(x = true_mass, y = nh4_rate,  group = species, colour= species)) +
   geom_point(size = 3) +
     #geom_line(aes(y = lwr, colour = species), linetype = "dashed") +
     #geom_line(aes(y = upr, colour = species), linetype = "dashed") +
     geom_smooth(method=lm,  aes(fill = species)) +
     labs(y = "Ammonium Excretion (umol" ~hour^-1~ ")", x = " Wet Mass (g)") +
     guides (col = FALSE) 
   
   
   p0 + scale_color_manual(values=c("blue", "deepskyblue")) +
     scale_fill_manual(values=c("blue", "deepskyblue"),
                       name="Species",
                       breaks=c("dd", "ft"),
                       labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
     theme(legend.position = "none", 
           panel.background = element_rect(fill = "transparent",colour = NA),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank(),
           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
           axis.line.x = element_line(colour = "white"),
           axis.line.y = element_line(colour = "white"),
           axis.title.x = element_text(size=18, colour = "white"),
           axis.title.y = element_text(size=18, colour = "white"),
           axis.text.y = element_text(size=18, colour = "white"),
           axis.text.x = element_text(size=18, colour = "white")) 
   
   #### wet mass - length
   p0 <- ggplot(outlier_removed, aes(x = length, y = true_mass,  group = species, colour= species)) +
     geom_point(size = 3) +
     geom_smooth(method=lm,  aes(fill = species), alpha = 0.7) +
     labs(y = "Wet Mass (g)", x = " Length (cm)") 
   p0
   
   
   p0 + scale_color_manual(values=c("deepskyblue", "chocolate1")) +
     scale_fill_manual(values=c("deepskyblue", "chocolate1"),
                       name="Species",
                       breaks=c("dd", "ft"),
                       labels=c(expression(italic("H. mexicana"),italic("A. agassizii")))) +
     theme(legend.position = "none", 
           panel.background = element_rect(fill = "transparent",colour = NA),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank(),
           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
           axis.line.x = element_line(colour = "white"),
           axis.line.y = element_line(colour = "white"),
           axis.title.x = element_text(size=18, colour = "white"),
           axis.title.y = element_text(size=18, colour = "white"),
           axis.text.y = element_text(size=18, colour = "white"),
           axis.text.x = element_text(size=18, colour = "white")) 

   
   ### for appendix july 26 2021
   mod1 <- lm(true_mass ~ length + species, 
              data=outlier_removed)
   summary(mod1)
   anova(mod1)
   cuke_predict2 <- predict(mod1, interval="confidence")
   head(cuke_predict2)
   
   cuke_ci2 <- cbind(outlier_removed, cuke_predict2)
   
   p0 <- ggplot(cuke_ci2, aes(x = length, y = true_mass, group = species, colour = species)) + 
     geom_point(size = 2.6) +
     geom_ribbon(aes(ymin=lwr, ymax=upr, fill = species), linetype=3, alpha=0.3) +
     geom_smooth(method = 'lm', se= F) +
     scale_fill_manual(values = c("dd"="blue", "ft"="darkorange2")) +
     guides (col = FALSE) +
     labs(y = "Wet Mass (g)", x = "Length (cm)") +
     scale_x_continuous(breaks=(seq(15, 50, 10))) +
     scale_y_continuous(breaks = scales::pretty_breaks(n = 6)) 
   
   
   
   p0 + scale_color_manual(values=c("blue", "darkorange2")) +
     theme(panel.background = element_rect(fill = "transparent",colour = NA),
           panel.grid.minor = element_blank(),
           panel.grid.major = element_blank(),
           plot.background = element_rect(fill = "transparent", color = NA), # bg of the plot
           panel.border = element_blank(),
           axis.line.x = element_line(colour = "black"),
           axis.line.y = element_line(colour = "black"),
           axis.title.x = element_text(size=18, colour = "black"),
           axis.title.y = element_text(size=18, colour = "black"),
           axis.text.y = element_text(size=14, colour = "black"),
           axis.text.x = element_text(size=14, colour = "black")) 
   

  #ggsave(p0, filename = "excretion.png", bg = "transparent", height = 7, width = 9) 
  
  ###looking at logged excretion and logged mass
  p02 <- ggplot(outlier_removed, aes(log(true_mass), log(nh4_rate),  group = species, colour= species)) +
    geom_point(size = 3) +
    geom_smooth(method=lm,  aes(fill = species)) 
  p02
  
  ###looking at excretion and length
  p03 <- ggplot(outlier_removed, aes(length, nh4_rate,  group = species, colour= species)) +
    geom_point(size = 3) +
    geom_smooth(method=lm,  aes(fill = species)) 
  p03
  
  ###looking at logged excretion and log length
  p04 <- ggplot(outlier_removed, aes(log(length), log(nh4_rate),  group = species, colour= species)) +
    geom_point(size = 3) +
    geom_smooth(method=lm,  aes(fill = species)) 
  p04
  
  
  

  
### feb 24 2021 size index  from hannahs comment
  sizei <- lm(nh4_rate ~ sizeindex + species, data = outlier_removed)
  summary(sizei)
  
  ggplot(outlier_removed, aes(x = sizeindex2, y = nh4_rate,  group = species, colour= species)) + 
    geom_point(size = 3) +
    geom_smooth(method=lm,  aes(fill = species), level = 0.95) 

  
## JULY 2 LOG length weight graphs
  #log mass ~ log length for DD
  ggplot(dd, aes(log(length), log(true_mass))) +
    geom_point(size = 2) +
    geom_smooth(method=lm)
  
  #log mass ~ log length for FT
  ggplot(ft, aes(log(length), log(true_mass))) +
    geom_point(size = 2) +
    geom_smooth(method=lm)
  
  dd_other2 <- lm(true_mass ~ sizeindex2, data=dd)
  dd_other3 <- lm(true_mass ~ length, data=dd)
  dd_other4 <- lm(true_mass ~ breadth, data=dd)
  dd_other5 <- lm(true_mass ~ cyl_vol, data=dd)
  
  ft_other2 <- lm(true_mass ~ sizeindex2, data=ft)
  ft_other3 <- lm(true_mass ~ length, data=ft)
  ft_other4 <- lm(true_mass ~ breadth, data=ft)
  ft_other5 <- lm(true_mass ~ cyl_vol, data=ft)


 