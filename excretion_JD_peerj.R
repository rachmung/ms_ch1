
## Edits made by RM on AUG 14 2021 (Section 1A and 1B), Aug 18 (Section 4, 4A, 4B)

  
# Load packages -----------------------------------------------------------
library(tidyverse)
library(broom)
library(car) # for Anova()
library(here) #package that helps with managing file paths


# Load data ---------------------------------------------------------------
excretion_bound_df <- read_csv("excretion_data_cleaned_2020_02_11.csv")

individual_cukes <- read_csv("individual_cukes.csv") %>%
  filter(treatment!='NA') %>%
  # 12 individuals we don't have data for, so remove those
  filter(species!='NA') %>%
  # only want to consider the cucumbers "kept" or "removed", ignore the ones that were "added"
  # FOR READERS: Sea cucumbers that were "kept" or "removed" represent the un-manipulated baseline 
  # sea cucumber densities at each patch. These sites were part of a separate experiment that was
  # conducted after this study, so that's why there is information regarding manipullation of the 
  # densities at each site. However, we knew what the initial densities at each site were, which is
  # represented by the "kept" and "removed" sea cucumbers.
  filter(remove_add_keep %in% c("keep", "remove")) 

#take a look at new df 
individual_cukes



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

#removing 1 outlier in row 20
outlier_removed <- corrected_df[-c(20), ]
outlier_removed




#### 1A) weight ~ length relationship (plot and model) ####
mass_length <- ggplot(outlier_removed, aes(x = length, y = true_mass, group = species, colour = species)) +
  geom_point() +
  geom_smooth(method = 'lm',  aes(fill = species)) +
  labs(y = "Wet Mass", x = "Length") 
mass_length


## mass ~ length model
mod1 <- lm(true_mass ~ length + species, 
           data=outlier_removed)
summary(mod1)
anova(mod1)


# Predict weights from lengths
#se.fit = FALSE is the default for predict.lm

nat_weights <- predict(mod1, newdata = individual_cukes, se.fit = TRUE, interval = "prediction")
nat_weights <- as.data.frame(nat_weights$fit) %>% tibble


#"fit" in pred_weights is the new weight
pred_weights <- bind_cols(individual_cukes, nat_weights)

#min and max estimates from the mean wet mass
pred_weights %>%
  group_by(patch) %>% 
  summarise(min_weight = round(min(fit, na.rm = TRUE)), max_weight = round(max(fit, na.rm = TRUE))) %>%
  view()

write.csv(predicted_weights, "C:\\Users\\rachelmunger\\Documents\\SFU\\MSc\\MSc_Data\\cuke_msc
#\\chapter1\\predicted_weights.csv")

# ^^^^^
#Do sizes from patches fall within sizes measured in lab? 
#Yes, predicted mass is 399-876 g in field, whereas mass in lab ranges from 110-1397 g. 





###  1B) Histograms of predicted weight and length distribution of both spp at all patches ####
patch_lengths <- ggplot(individual_cukes, aes(x= length, fill = species, color=species), binwidth = 0.5) +
  labs(y = "Density", x = "Length (cm)") +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.2)+
  scale_color_manual(values=c("salmon3", "goldenrod3"))+
  scale_fill_manual(values=c("salmon3", "goldenrod3"))+
  theme_classic()
patch_lengths

#separating by patch
patch_lengths2 <- ggplot(individual_cukes, aes(x= length, fill = species, color=species), binwidth = 0.5) +
  labs(y = "Density", x = "Length (cm)") +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.2)+
  scale_color_manual(values=c("salmon3", "goldenrod3"))+
  scale_fill_manual(values=c("salmon3", "goldenrod3"))+
  theme_classic() +
  facet_wrap(~patch)

patch_lengths2

# Convert length to mass at patches and make size distribution (1 graph)
masses_patch <- ggplot(pred_weights, aes(x= fit, fill = species, color=species), binwidth = 0.5) +
  labs(y = "Density", x = "Predicted Wet Mass (g)") +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.2)+
  scale_color_manual(values=c("salmon3", "goldenrod3"))+
  scale_fill_manual(values=c("salmon3", "goldenrod3"))+
  theme_classic()

masses_patch

# predicted wet mass organized by site
masses_patch2 <- ggplot(pred_weights, aes(x= fit, fill = species, color=species), binwidth = 0.5) +
  labs(y = "Density", x = "Predicted Wet Mass (g)") +
  geom_histogram(aes(y=..density..), position="identity", alpha=0.5)+
  geom_density(alpha=0.2)+
  scale_color_manual(values=c("salmon3", "goldenrod3"))+
  scale_fill_manual(values=c("salmon3", "goldenrod3"))+
  theme_classic() +
  facet_wrap(~patch)

masses_patch2

#prediction intervals around predicted mass~length relationship
pred_mass <-  ggplot(pred_weights, aes(x = length, y  = fit)) +
  geom_point() +
  geom_line(aes(y = lwr), linetype = "dashed") +
  geom_line(aes(y = upr), linetype = "dashed") +
  geom_smooth(method=lm) +
  labs(y = "Predicted Mass (g)", x = " Length (cm)") +
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
pred_mass

# I would extend your prediction lengths to match what you originally used for 
# the weight length relationships, and plot the predictions on top of the raw
# data so that you can see how well the predictions fit the data.
pred_mass + 
  geom_point(data = outlier_removed, aes(x = length, y = true_mass, colour = "red"))

#### 2) excretion ~ weight relationship (plot and model) ####

ex_mass <- ggplot(outlier_removed, aes(x = true_mass, y = nh4_rate, group = species, colour = species)) +
  geom_point() +
  geom_smooth(method = 'lm',  aes(fill = species)) +
  labs(y = "Excretion Rate (umol/h)", x = "Wet Mass") 
ex_mass

## nh4_rate ~ mass model
mod2 <- lm(nh4_rate ~ true_mass + species, 
           data=outlier_removed)

summary(mod2)

#type 3 sum of squares ANCOVA
Anova(mod2, type = "III")

#use scaled coefficients in order to avoid dismissing an effect as “small” 
          #when it is just the units of measure that are small.
summ(mod2, scale = TRUE)

#mean centered
summ(mod2, center = TRUE)

library(jtools)
library(ggstance)
plot_summs(mod2, scale = TRUE)



