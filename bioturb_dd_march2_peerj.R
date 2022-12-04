library(tidyverse)

bioturb <- read.csv("bioturbation.csv")
head(bioturb)

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

##### Grams per hour Oct 9####
#grams per hour btwn species

whole_mod <- lm(g_per_hr ~ species, data = bioturb)
summary(whole_mod)

g_per_hour <- ggplot(bioturb, aes((species), (g_per_hr),  group = species, colour = species)) +
  geom_point(size = 3) +
  geom_smooth(method=lm,  aes(fill = species)) +
  labs(y = "grams per hour", x = "Species") 

g_per_hour

#model we want to use
bioturb_mod <- lm(g_per_hr ~ species, data = bioturb)
summary(bioturb_mod)

#no significant interaction between species
drop1(bioturb_mod, test='F')

as.data.frame(bioturb)

#We have:
bt_rate_df_ddb <- 
  bioturb %>% 
  select(species, g_per_hr)

reef_data_ddb <- individual_cukes %>% 
  select(patch, species, cuke_num) %>%
  filter(species == "dd")

ddb_rate_mean <- mean(filter(bt_rate_df_ddb, species == 'dd')$g_per_hr)

ddb_rate_sd   <- sd(filter(bt_rate_df_ddb, species == 'dd')$g_per_hr)

ddb_rate_dist <- rnorm(1000, mean = ddb_rate_mean, sd = ddb_rate_sd)


hist(ddb_rate_dist)


reef_data2_ddb <- reef_data_ddb %>% 
  filter(species == "dd") %>%
  group_by(patch, species) %>% 
  summarise(count = n())



#Write a function to let you sample however many cukes you want from the 
#ft rate sampling distribution
get_ddb_rate <- function(nsamps) {
  rnorm(nsamps, mean = ddb_rate_mean, sd = ddb_rate_sd)
}

get_ddb_rate(10)

#Bootstrap samples from the sampling distribution nsamps times. 
#In this case, 10 ft are at NEW2, and we'll do this 100x.
nsamps = 10
nboot = 5000

sample_set_ddb <- 
  replicate(nboot, get_ddb_rate(nsamps), simplify = FALSE) %>% 
  enframe(name = "samp", value = "rate") %>% 
  unnest(cols = c(rate))


# Get the total excretion rate for each re-sampling.
reef_rates_ddb <- 
  sample_set_ddb %>% 
  group_by(samp) %>% 
  summarise(reef_rate = sum(rate)) %>% 
  ungroup()

#Estimate for NEW2 ft rate of NH4 with SE
# Note: it is the with SE part that is the 'bonus' we get from doing the bootstrapping

mean(reef_rates_ddb$reef_rate)
sd(reef_rates_ddb$reef_rate) 
#se
sd(reef_rates_ddb$reef_rate) / sqrt(nsamps)

hist(reef_rates_ddb$reef_rate)


## Write a for() loop - start with just 1 spp of cuke (5tooths) at all sites

#can use this vector in for loop
count_vec_ddb <- reef_data2_ddb$count

#view(count_vec)

#constant to run before looping
nboot = 5000


## ---- FUNCTION ---- ##

get_ddb_rate <- function(nsamps) {
  rnorm(nsamps, mean = ddb_rate_mean, sd = ddb_rate_sd)
}

reef_rate_function_ddb <- function(df) {
  # Change the for loop to iterate over a row in the reef dataframe, that way 
  # you get to keep row id.
  reef_row <- 1:nrow(df)
  # My b, you did need somewhere to stash stuff, in this case I'm going to use a 
  # list, I find them easier to use for stuff like this.
  reef_rate_out_list <- list()
  # Loop!
  for (i in reef_row) {
    # Get reef specific information
    # indexing in R is hard, I always trial and error it
    #str(reef_data2[i, "count"])  # look at what this does compared to:
    #str(reef_data2[[i, "count"]])
    cuke_count = df[[i, "count"]]
    patch = df[[i, "patch"]]
    
    sample_set_ddb <- 
      replicate(nboot, get_ddb_rate(nsamps = cuke_count), simplify = FALSE) %>% 
      enframe(name = "samp", value = "rate") %>% 
      unnest(cols = c(rate))
    
    reef_rates_ddb <- sample_set_ddb %>% 
      group_by(samp) %>% 
      summarise(reef_rate = sum(rate)) %>% 
      ungroup()
    #browser()  # let me peer into the for loop - my fav function after str()
    #Join each loop output with the previous loop outputs 
    reef_rates_ddb$cuke_count <- cuke_count
    reef_rates_ddb$patch <- patch
    reef_rate_out_list[[i]] <- reef_rates_ddb
  }
  # Output the final df with counts and output from each loop
  return(reef_rate_out_list)
  
  # Join each loop output with the previous loop outputs 
  reef_rates$cuke_count <- cuke_count
  reef_rates$patch <- patch
  reef_rate_out_list[[i]] <- reef_rates
}
# Output the final df with counts and output from each loop
return(reef_rate_out_list)



# Test the function
test_v2_ddb_m4 <- 
  reef_rate_function_ddb(df = reef_data2_ddb) %>% 
  bind_rows

# Add a new column to dd data frame w dd species column
test_v2_ddb_m4$species <- c("dd")
view(test_v2_ddb_m4)

hist(test_v2_ddb_m4$reef_rate)


#don't really need this
testing_v2_ddb <- test_v2_ddb %>%
  group_by(patch) %>%
  summarize(mean(test$reef_rate), median(test$reef_rate), sd(test$reef_rate), sd(test$reef_rate) / sqrt(cuke_count))

get_sd_BTdd_m4 <- test_v2_ddb_m4 %>%
  group_by(patch) %>%
  summarize(mean(reef_rate), sd(reef_rate))

#write csv of get_sd_BTdd
write.csv(get_sd_BTdd_m4, "get_sd_BTdd_march2.csv")

