library(sf)
library(dplyr)
library(tidyr)
library(faraway) #for vif

# read census data
census_data <- readRDS("../Census_data/census_data.rds")

# read AHAH index
ahah_data <- read.csv("../AHAH/allvariableslsoawdeciles.csv")

# read school ratings
school_ratings <- read.csv("buffer_byRating.csv")

# read number of bus trips by LSOA
trip_data <- read.csv("../bus_trips_stops/trips_by_LSOA.csv")

# read Interent User Classification
iuc_data <- st_read("../Internet User Classification/IUC2018.shp")

# read the lookup table between LSOAs and districts
lookupOA_LSOA <- read.csv("../BroadBandSpeed/Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District_(December_2017)_Lookup_with_Area_Classifications_in_Great_Britain.csv")
## keep only unique LSOAs
unique_lsoa <- lookupOA_LSOA %>% distinct(LSOA11CD, .keep_all = TRUE)



# join all the datasets
full_data <- left_join(iuc_data, unique_lsoa[c("LSOA11CD", "LAD17NM", "RGN11NM")], by = c("LSOA11_CD" = "LSOA11CD"))

full_data <- left_join(full_data, census_data, by = c("LSOA11_CD" = "Code"))

full_data <- left_join(full_data, ahah_data, by = c("LSOA11_CD" = "lsoa11"))

full_data <- left_join(full_data, school_ratings, by = c("LSOA11_CD" = "LSOA11_CD"))

full_data <- left_join(full_data, trip_data, by = c("LSOA11_CD" = "LSOA11_CD"))


# seect only the columns needed for modelling
cols <- c("LSOA11_CD", "GRP_LABEL", "LAD17NM", "RGN11NM", "All.usual.residents_2011", "divergence_2011", "divergence_2001",
          "LQ_mixed_2011", "LQ_asian_2011", "LQ_black_2011", "LQ_other_2011",
          "Age_20_44_per", "Age_45_64_per", "Age_65_plus_per", "Social.rented_per",
          "Economically.active..Unemployed_per",
          "Economically.inactive..Long.term.sick.or.disabled_per",
          "Density..number.of.persons.per.hectare.",
          "gpp_dist", "ed_dist", "dent_dist", "pharm_dist", "gamb_dist", "ffood_dist",
          "pubs_dist", "leis_dist", "blue_dist", "off_dist", "tobac_dist", "green_pas", "green_act", "no2_mean",
          "pm10_mean", "so2_mean", "No_qualifications_per",
          "rating1","rating2", "rating3", "rating4", "Norating", "trips_per_day", "TCITY15NM_2011"
)

full_data_subs <- full_data %>% 
  select(all_of(cols))


# calculate trips per day by population/person
full_data_subs <- full_data_subs %>% 
  mutate(trips_per_pop =  trips_per_day/All.usual.residents_2011)


#pryr::object_size(full_data_subs)


# # https://medium.com/@TheDataGyan/day-8-data-transformation-skewness-normalization-and-much-more-4c144d370e55
# full_data$z_scale <- scale(full_data$divergence_2011,center= TRUE, scale=TRUE)
# 
# full_data$roo <- full_data$divergence_2011^(1/3)
# full_data$lo <- ln(full_data$divergence_2011)
# 
# summary(full_data$roo)
# 
# library(ggplot2)
# ggplot(normalised_data, aes(divergence_2011_norm)) +
#   geom_density()

full_data_subs <- subset(full_data_subs, TCITY15NM_2011 %in% c("London",
                                                               "Birmingham",
                                                               "Liverpool", 
                                                               "Bristol" ,
                                                               "Sheffield",
                                                               "Manchester",
                                                               "Leeds",
                                                               "Leicester",
                                                               "Bradford",
                                                               "Coventry",
                                                               "Nottingham",
                                                               "Newcastle upon Tyne"))


normalize2 <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

normalize <- function(x)
{
  return((x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE))
}

replaceMissing <- function(v) replace(v, is.na(v), 0)


data_omit <- na.omit(full_data_subs)
summary(data_omit)
nrow(data_omit)
nrow(full_data_subs)
nrow(normalised_data)


cols_summary <- c("LQ_mixed_2011", "LQ_asian_2011", "LQ_black_2011", "LQ_other_2011",
          "Age_20_44_per", "Age_45_64_per", "Age_65_plus_per", 
          "Density..number.of.persons.per.hectare.",
          "Economically.active..Unemployed_per",
          "Economically.inactive..Long.term.sick.or.disabled_per",
          "Social.rented_per", "No_qualifications_per",
          "gpp_dist", "green_pas", "no2_mean",
          "rating1", "rating4", "trips_per_pop")



for_summary <- as.data.frame(data_omit) %>% 
  dplyr::select(all_of(cols_summary))
  
to_print = for_summary %>%
  summarise_all(max, na.rm = TRUE)

## this part creates descriptive plots by city and variable
cols_for_boxplot <- c("LSOA11_CD", "LQ_mixed_2011", "LQ_asian_2011", "LQ_black_2011", "LQ_other_2011",
                  "Age_20_44_per", "Age_45_64_per", "Age_65_plus_per", 
                  "Density..number.of.persons.per.hectare.",
                  "Economically.active..Unemployed_per",
                  "Economically.inactive..Long.term.sick.or.disabled_per",
                  "Social.rented_per", "No_qualifications_per",
                  "gpp_dist", "green_pas", "no2_mean",
                  "rating1", "rating4", "trips_per_pop", "TCITY15NM_2011")

for_boxplot <- as.data.frame(data_omit) %>% 
  dplyr::select(all_of(cols_for_boxplot))

rttt <- left_join(iuc_data, for_boxplot, by = c("LSOA11_CD" = "LSOA11_CD"))
st_write(rttt, "rttt.gpkg")

# reshape to long format
long_data <- gather(for_boxplot, variable, measurement, LQ_mixed_2011:trips_per_pop, factor_key=TRUE)
# rename the names
long_data = long_data %>% 
  mutate(variable = case_when(variable == 'LQ_mixed_2011' ~ 'LQ mixed',
                              variable == 'LQ_asian_2011' ~ 'LQ asian',
                              variable == 'LQ_black_2011' ~ 'LQ black',
                              variable == 'LQ_other_2011' ~ 'LQ other',
                              variable == 'Economically.active..Unemployed_per' ~ 'Unemployment',
                              variable == 'Economically.inactive..Long.term.sick.or.disabled_per' ~ 'Long-term ill',
                              variable == 'Density..number.of.persons.per.hectare.' ~ 'Population \nDensity',
                              variable == 'Social.rented_per' ~ 'Social rented',
                              variable == 'No_qualifications_per' ~ 'No Qualifications',
                              variable == 'Age_20_44_per' ~ 'Age 20-44',
                              variable == 'Age_45_64_per' ~ 'Age 45-64',
                              variable == 'Age_65_plus_per' ~ 'Age 65+',
                              variable == 'gpp_dist' ~ 'Distance to GP',
                              variable == 'green_pas' ~ 'Green space \npresence',
                              variable == 'rating1' ~ 'Best performing \nschool rating',
                              variable == 'rating4' ~ 'Worst performing \nschool rating',
                              variable == 'trips_per_pop' ~ 'Bus connectivity',
                              variable == 'no2_mean' ~ 'NO2 levels',
  ))

# then order the variable names
long_data$variable <- factor(long_data$variable ,
                          levels = c("LQ mixed", "LQ asian", "LQ black", "LQ other",
                                     "Age 20-44", "Age 45-64", "Age 65+",
                                     "Population \nDensity", "Unemployment", "Long-term ill", 
                                     "Social rented", "No Qualifications",
                                     "Distance to GP", "Green space \npresence", "NO2 levels",
                                     "Best performing \nschool rating", "Worst performing \nschool rating",
                                     "Bus connectivity"))

library(ggplot2)

descr_plot_cities = ggplot(long_data, aes(x = measurement, y =TCITY15NM_2011)) +
  geom_boxplot(outlier.size = 0.2, alpha = 0.5) +
  facet_wrap(~variable, scales="free_x", ncol = 4) +
  labs(title = "",
       x = "",
       y = "cities") +
  theme_minimal()

png("descr_plot_cities.png", units="in", width=8, height=7, res=300)
descr_plot_cities
dev.off()



library(ggridges)

ridges = ggplot(long_data, aes(x = measurement, y = TCITY15NM_2011)) +
  stat_density_ridges(quantile_lines = TRUE) +
  facet_wrap(~variable, scales="free_x", ncol = 4) +
  labs(title = "",
       x = "",
       y = "cities") +
  theme_minimal()

png("ridges_density.png", units="in", width=8, height=7, res=300)
ridges
dev.off()

# this plot is for mean and SD by city and variable

su = long_data %>% 
  group_by(TCITY15NM_2011, variable) %>% 
  summarise(mean = mean(measurement), sd = sd(measurement),  
            median = median(measurement),
            min = min(measurement),
            max = max(measurement)) %>% 
  mutate(lower = mean - sd,
       upper = mean + sd)
  # mutate(lower = min,
  #        upper = max)
  # mutate(lower = mean - sd,
  #      upper = mean + sd)


mean_sd_city = ggplot(su) +
  #geom_errorbar(aes(x = mean, y = TCITY15NM_2011, xmin=lower, xmax=upper)) +
  #geom_pointrange(aes(x = mean, y = TCITY15NM_2011, xmin=lower, xmax=upper), size = 0.3) +
  geom_point(aes(x = mean, y = TCITY15NM_2011)) +
  facet_wrap(~variable, scales="free_x", ncol = 5) +
  labs(title = "",
       x = "Mean for each variable and city",
       y = "Cities") +
  theme_bw() +
  theme(strip.background =element_rect(fill="white"),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_line(color = "grey75"),
        panel.grid.major.x = element_blank())

png("mean_city.png", units="in", width=7, height=9, res=300)
mean_sd_city
dev.off()



median_sd_city = ggplot(su) +
  #geom_errorbar(aes(x = mean, y = TCITY15NM_2011, xmin=lower, xmax=upper)) +
  #geom_pointrange(aes(x = mean, y = TCITY15NM_2011, xmin=lower, xmax=upper), size = 0.3) +
  geom_point(aes(x = median, y = TCITY15NM_2011)) +
  facet_wrap(~variable, scales="free_x", ncol = 5) +
  labs(title = "",
       x = "Median for each variable and city",
       y = "Cities") +
  theme_bw() +
  theme(strip.background =element_rect(fill="white"),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(size = 8))

png("median_city.png", units="in", width=8, height=7, res=300)
median_sd_city
dev.off()



ggplot(su) +
  #geom_errorbar(aes(x = mean, y = TCITY15NM_2011, xmin=lower, xmax=upper)) +
  #geom_pointrange(aes(x = mean, y = TCITY15NM_2011, xmin=lower, xmax=upper), size = 0.3) +
  geom_point(aes(x = mean, y = TCITY15NM_2011)) +
  facet_wrap(~variable, scales="free_x", ncol = 5) +
  labs(title = "",
       x = "Mean for each variable and city",
       y = "Cities") +
  theme_bw() +
  theme(strip.background =element_rect(fill="white"),
        axis.text.x = element_text(angle = 90),
        axis.text.y = element_text(size = 8),
        panel.grid.major.y = element_line(color = "black"))


normalised_data <- full_data_subs %>%
  select(LSOA11_CD, LAD17NM, RGN11NM, GRP_LABEL,
         divergence_2011,
         divergence_2001,
         Age_20_44_per, Age_45_64_per, Age_65_plus_per,
         Social.rented_per,
         Economically.active..Unemployed_per, Economically.inactive..Long.term.sick.or.disabled_per,
         Density..number.of.persons.per.hectare., No_qualifications_per,
         gpp_dist, green_pas,
         no2_mean, rating1,rating2, rating3, rating4, trips_per_day, trips_per_pop, TCITY15NM_2011,
         LQ_mixed_2011, LQ_asian_2011, LQ_black_2011, LQ_other_2011) %>%
  drop_na(divergence_2011,
          divergence_2001,
          Age_20_44_per, Age_45_64_per, Age_65_plus_per,
          Social.rented_per,
          Economically.active..Unemployed_per, Economically.inactive..Long.term.sick.or.disabled_per,
          Density..number.of.persons.per.hectare., No_qualifications_per,
          gpp_dist, green_pas,
          no2_mean, rating1,rating2, rating3, rating4, trips_per_day, trips_per_pop) %>% 
  mutate(divergence_2001_norm = divergence_2001^(1/3)) %>%
  mutate(divergence_2011_norm = divergence_2011^(1/3)) %>%
  mutate(Social.rented_per = normalize(Social.rented_per)) %>%
  mutate(Economically.active..Unemployed_per = normalize(Economically.active..Unemployed_per)) %>%
  mutate(Economically.inactive..Long.term.sick.or.disabled_per = normalize(Economically.inactive..Long.term.sick.or.disabled_per)) %>%
  mutate(Density..number.of.persons.per.hectare. = normalize(Density..number.of.persons.per.hectare.)) %>%
  mutate(No_qualifications_per = normalize(No_qualifications_per)) %>%
  mutate(Age_20_44_per = normalize(Age_20_44_per)) %>%
  mutate(Age_45_64_per = normalize(Age_45_64_per)) %>%
  mutate(Age_65_plus_per = normalize(Age_65_plus_per)) %>%
  mutate(gpp_dist = normalize(gpp_dist)) %>%
  mutate(green_pas = normalize(green_pas)) %>%
  mutate(no2_mean = normalize(no2_mean)) %>%
  mutate(rating1 = normalize(rating1)) %>%
  mutate(rating4 = normalize(rating4)) %>%
  mutate(trips_per_pop = normalize(trips_per_pop)) %>%
  mutate_all(funs(replaceMissing))


normalised_data$LQ_mixed_2011[normalised_data$LQ_mixed_2011==0] <- NA
normalised_data$LQ_asian_2011[normalised_data$LQ_asian_2011==0] <- NA
normalised_data$LQ_black_2011[normalised_data$LQ_black_2011==0] <- NA
normalised_data$LQ_other_2011[normalised_data$LQ_other_2011==0] <- NA





# create a subset for each ethnic group
normalised_mixed <- normalised_data %>% 
  drop_na(LQ_mixed_2011) %>% 
  mutate(log_mixed = log(LQ_mixed_2011))

normalised_asian <- normalised_data %>% 
  drop_na(LQ_asian_2011) %>% 
  mutate(log_asian = log(LQ_asian_2011))

normalised_black <- normalised_data %>% 
  drop_na(LQ_black_2011) %>% 
  mutate(log_black = log(LQ_black_2011))

normalised_other <- normalised_data %>% 
  drop_na(LQ_other_2011) %>% 
  mutate(log_other = log(LQ_other_2011))


# see how many obervations are missing for each group
nrow(normalised_mixed) - nrow(normalised_data)
nrow(normalised_asian) - nrow(normalised_data)
nrow(normalised_black) - nrow(normalised_data)
nrow(normalised_other) - nrow(normalised_data)






# specify the equations
mixed_eq1 <- log_mixed ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  (1 | TCITY15NM_2011)

mixed_eq2 <- log_mixed ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  gpp_dist + green_pas + rating1 + rating4 + trips_per_pop + no2_mean + 
  (1 | TCITY15NM_2011)



asian_eq1 <- log_asian ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  (1 | TCITY15NM_2011)

asian_eq2 <- log_asian ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  gpp_dist + green_pas + rating1 + rating4 + trips_per_pop + no2_mean + 
  (1 | TCITY15NM_2011)


black_eq1 <- log_black ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  (1 | TCITY15NM_2011)

black_eq2 <- log_black ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  gpp_dist + green_pas + rating1 + rating4 + trips_per_pop + no2_mean + 
  (1 | TCITY15NM_2011)


other_eq1 <- log_other ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  (1 | TCITY15NM_2011)

other_eq2 <- log_other ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  gpp_dist + green_pas + rating1 + rating4 + trips_per_pop + no2_mean + 
  (1 | TCITY15NM_2011)




# Fitting multilevel models
library(lme4)
# Tools for extracting information generated by lme4
library(merTools)
# for saving the model results
library(sjPlot)

mixed_mlm1 <- lmer(mixed_eq1, data = normalised_mixed)
mixed_mlm2 <- lmer(mixed_eq2, data = normalised_mixed)

asian_mlm1 <- lmer(asian_eq1, data = normalised_asian)
asian_mlm2 <- lmer(asian_eq2, data = normalised_asian)


black_mlm1 <- lmer(black_eq1, data = normalised_black)
black_mlm2 <- lmer(black_eq2, data = normalised_black)


other_mlm1 <- lmer(other_eq1, data = normalised_other)
other_mlm2 <- lmer(other_eq2, data = normalised_other)

# this converges in the random slope No_qualifications_per + Social.rented_per + no2_mean

mixed_eq3 <- log_mixed ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  gpp_dist + green_pas + rating1 + rating4 + trips_per_pop + no2_mean + 
  (1 + No_qualifications_per + Social.rented_per + no2_mean| TCITY15NM_2011)

asian_eq3 <- log_asian ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  gpp_dist + green_pas + rating1 + rating4 + trips_per_pop + no2_mean + 
  (1 + No_qualifications_per + Social.rented_per + no2_mean| TCITY15NM_2011)

black_eq3 <- log_black ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  gpp_dist + green_pas + rating1 + rating4 + trips_per_pop + no2_mean + 
  (1 + No_qualifications_per + Social.rented_per + no2_mean| TCITY15NM_2011)

other_eq3 <- log_other ~ Economically.active..Unemployed_per +
  Economically.inactive..Long.term.sick.or.disabled_per +
  Density..number.of.persons.per.hectare. + Social.rented_per +
  No_qualifications_per + Age_20_44_per + Age_45_64_per + Age_65_plus_per +
  gpp_dist + green_pas + rating1 + rating4 + trips_per_pop + no2_mean + 
  (1 + No_qualifications_per + Social.rented_per + no2_mean| TCITY15NM_2011)


mixed_mlm3 <- lmer(mixed_eq3, data = normalised_mixed)
asian_mlm3 <- lmer(asian_eq3, data = normalised_asian)
black_mlm3 <- lmer(black_eq3, data = normalised_black)
other_mlm3 <- lmer(other_eq3, data = normalised_other)

tab_model(mixed_mlm1, mixed_mlm2, mixed_mlm3, show.se = TRUE, show.loglik = TRUE, show.ci = FALSE, file = "mixed_mlm3.html")
tab_model(asian_mlm1, asian_mlm2, asian_mlm3, show.se = TRUE, show.loglik = TRUE, show.ci = FALSE, file = "asian_mlm3.html")
tab_model(black_mlm1, black_mlm2, black_mlm3, show.se = TRUE, show.loglik = TRUE, show.ci = FALSE, file = "black_mlm3.html")
tab_model(other_mlm1, other_mlm2, other_mlm3, show.se = TRUE, show.loglik = TRUE, show.ci = FALSE, file = "other_mlm3.html")


# use the summary to write up the tvalue column in the supplementary material
summary(other_mlm1)
summary(black_mlm3)


png("mixed_fe_by_model.png", units="in", width=8, height=7, res=300)
fe_plot_by_model(mixed_mlm1, mixed_mlm2, mixed_mlm3, "Mixed ethnic group")
dev.off() # Close the file


png("asian_fe_by_model.png", units="in", width=8, height=7, res=300)
fe_plot_by_model(asian_mlm1, asian_mlm2, asian_mlm3, "Asian ethnic group")
dev.off() # Close the file


png("black_fe_by_model.png", units="in", width=8, height=7, res=300)
fe_plot_by_model(black_mlm1, black_mlm2, black_mlm3, "Black ethnic group")
dev.off() # Close the file


png("other_fe_by_model.png", units="in", width=8, height=7, res=300)
fe_plot_by_model(other_mlm1, other_mlm2, other_mlm3, "Other ethnic group")
dev.off() # Close the file







#plot_model(mixed_mlm, type = "re")

# library(glmmTMB)
# 
# start.time <- Sys.time()
# mixed_testing = glmmTMB(mixed_eq, data = normalised_data, family = gaussian)
# end.time <- Sys.time()
# time.taken <- end.time - start.time
# time.taken
# 
# summary(mixed_testing)
# tab_model(mixed_testing, file = "mixed_testing.html")



# library(car)
# vif(model_testing) 
library(lattice)
plot1_mixed = qqmath(mixed_mlm3)
plot2_mixed = plot(mixed_mlm3)

png("mixed_qqplots.png", units="in", width=8, height=7, res=300)
gridExtra::grid.arrange(plot1_mixed, plot2_mixed, nrow = 1)
dev.off() # Close the file


plot1_asian = qqmath(asian_mlm2)
plot2_asian = plot(asian_mlm2)

png("asian_qqplots.png", units="in", width=8, height=7, res=300)
gridExtra::grid.arrange(plot1_asian, plot2_asian, nrow = 1)
dev.off() # Close the file

plot1_black= qqmath(black_mlm2)
plot2_black = plot(black_mlm2)

png("black_qqplots.png", units="in", width=8, height=7, res=300)
gridExtra::grid.arrange(plot1_black, plot2_black, nrow = 1)
dev.off() # Close the file

plot1_other = qqmath(other_mlm2)
plot2_other = plot(other_ml2)

png("other_qqplots.png", units="in", width=8, height=7, res=300)
gridExtra::grid.arrange(plot1_other, plot2_other, nrow = 1)
dev.off() # Close the file

#shapiro.test(resid(model_testing)[0:5000])
#library(nortest)
#ad.test(resid(model_testing))$p.value



# create a dataframe for RE
mixed_re = REsim(mixed_mlm3)
asian_re = REsim(asian_mlm3)
black_re = REsim(black_mlm3)
other_re = REsim(other_mlm3)

mixed_re = mixed_re %>% 
  mutate(term = case_when(term == '(Intercept)' ~ '(Intercept)',
                          term == 'Economically.active..Unemployed_per' ~ 'Unemployment',
                          term == 'Economically.inactive..Long.term.sick.or.disabled_per' ~ 'Long-term ill',
                          term == 'Density..number.of.persons.per.hectare.' ~ 'Population Density',
                          term == 'Social.rented_per' ~ 'Social rented',
                          term == 'No_qualifications_per' ~ 'No Qualifications',
                          term == 'Age_20_44_per' ~ 'Age 20-44',
                          term == 'Age_45_64_per' ~ 'Age 45-64',
                          term == 'Age_65_plus_per' ~ 'Age 65+',
                          term == 'gpp_dist' ~ 'Distance to GP',
                          term == 'green_pas' ~ 'Green space presence',
                          term == 'rating1' ~ 'Best performing \nschool rating',
                          term == 'rating4' ~ 'Worst performing \nschool rating',
                          term == 'trips_per_pop' ~ 'Bus connectivity',
                          term == 'no2_mean' ~ 'NO2 levels',
  ))

asian_re = asian_re %>% 
  mutate(term = case_when(term == '(Intercept)' ~ '(Intercept)',
                          term == 'Economically.active..Unemployed_per' ~ 'Unemployment',
                          term == 'Economically.inactive..Long.term.sick.or.disabled_per' ~ 'Long-term ill',
                          term == 'Density..number.of.persons.per.hectare.' ~ 'Population Density',
                          term == 'Social.rented_per' ~ 'Social rented',
                          term == 'No_qualifications_per' ~ 'No Qualifications',
                          term == 'Age_20_44_per' ~ 'Age 20-44',
                          term == 'Age_45_64_per' ~ 'Age 45-64',
                          term == 'Age_65_plus_per' ~ 'Age 65+',
                          term == 'gpp_dist' ~ 'Distance to GP',
                          term == 'green_pas' ~ 'Green space presence',
                          term == 'rating1' ~ 'Best performing \nschool rating',
                          term == 'rating4' ~ 'Worst performing \nschool rating',
                          term == 'trips_per_pop' ~ 'Bus connectivity',
                          term == 'no2_mean' ~ 'NO2 levels',
  ))


black_re = black_re %>% 
  mutate(term = case_when(term == '(Intercept)' ~ '(Intercept)',
                          term == 'Economically.active..Unemployed_per' ~ 'Unemployment',
                          term == 'Economically.inactive..Long.term.sick.or.disabled_per' ~ 'Long-term ill',
                          term == 'Density..number.of.persons.per.hectare.' ~ 'Population Density',
                          term == 'Social.rented_per' ~ 'Social rented',
                          term == 'No_qualifications_per' ~ 'No Qualifications',
                          term == 'Age_20_44_per' ~ 'Age 20-44',
                          term == 'Age_45_64_per' ~ 'Age 45-64',
                          term == 'Age_65_plus_per' ~ 'Age 65+',
                          term == 'gpp_dist' ~ 'Distance to GP',
                          term == 'green_pas' ~ 'Green space presence',
                          term == 'rating1' ~ 'Best performing \nschool rating',
                          term == 'rating4' ~ 'Worst performing \nschool rating',
                          term == 'trips_per_pop' ~ 'Bus connectivity',
                          term == 'no2_mean' ~ 'NO2 levels',
  ))


other_re = other_re %>% 
  mutate(term = case_when(term == '(Intercept)' ~ '(Intercept)',
                          term == 'Economically.active..Unemployed_per' ~ 'Unemployment',
                          term == 'Economically.inactive..Long.term.sick.or.disabled_per' ~ 'Long-term ill',
                          term == 'Density..number.of.persons.per.hectare.' ~ 'Population Density',
                          term == 'Social.rented_per' ~ 'Social rented',
                          term == 'No_qualifications_per' ~ 'No Qualifications',
                          term == 'Age_20_44_per' ~ 'Age 20-44',
                          term == 'Age_45_64_per' ~ 'Age 45-64',
                          term == 'Age_65_plus_per' ~ 'Age 65+',
                          term == 'gpp_dist' ~ 'Distance to GP',
                          term == 'green_pas' ~ 'Green space presence',
                          term == 'rating1' ~ 'Best performing \nschool rating',
                          term == 'rating4' ~ 'Worst performing \nschool rating',
                          term == 'trips_per_pop' ~ 'Bus connectivity',
                          term == 'no2_mean' ~ 'NO2 levels',
  ))


library(ggplot2)


png("mixed_re.png", units="in", width=8, height=7, res=300)
slope_plot(mixed_re, "Mixed ethnic group")
dev.off() # Close the file

png("asian_re.png", units="in", width=8, height=7, res=300)
slope_plot(asian_re, "Asian ethnic group")
dev.off() # Close the file

png("black_re.png", units="in", width=8, height=7, res=300)
slope_plot(black_re, "Black ethnic group")
dev.off() # Close the file

png("other_re.png", units="in", width=8, height=7, res=300)
slope_plot(other_re, "Other ethnic group")
dev.off() # Close the file





# random slopes function --------------------------------------------------
slope_plot <- function(slope_data, plot_title) {
  
  
  slope_data <- slope_data %>% 
    mutate(lower = median - sd *qnorm(1 - ((1 - 0.95)/2)),
           upper = median + sd * qnorm(1 - ((1 - 0.95)/2)),
           sig = lower > 0 | upper < 0)
  
  slope_data$term <- factor(slope_data$term ,
                            levels = c("(Intercept)", "Age 20-44", "Age 45-64", "Age 65+",
                                       "Population Density", "Unemployment", "Long-term ill", 
                                       "Social rented", "No Qualifications",
                                       "Distance to GP", "Green space presence", "NO2 levels",
                                       "Best performing \nschool rating", "Worst performing \nschool rating",
                                       "Bus connectivity"))
  
  p <- ggplot() +
    geom_point(data=slope_data, aes(x = median, y = groupID), color = "gray75") +
    geom_pointrange(data=subset(slope_data, sig == TRUE), aes(x = median, y = groupID, xmin=lower, xmax=upper), size = 0.3) +
    facet_wrap(~term, scales="free_x") +
    geom_vline(xintercept=0, color = "red") +
    labs(title = plot_title, y = "cities") +
    theme_minimal()
  
  
  return(p)
  
}


n2 <- n %>% 
  mutate(lower = median - sd *qnorm(1 - ((1 - 0.95)/2)),
         upper = median + sd * qnorm(1 - ((1 - 0.95)/2)),
         sig = lower > 0 | upper < 0)

library(ggplot2)
ggplot() +
  #geom_point() +
  geom_pointrange(data=n2, aes(x = median, y = groupID, xmin=lower, xmax=upper), size = 0.3, color = "gray75") +
  geom_pointrange(data=subset(n2, sig == TRUE), aes(x = median, y = groupID, xmin=lower, xmax=upper), size = 0.3) +
  facet_wrap(~term) +
  geom_vline(xintercept=0, color = "red") +
  theme_minimal()




###### here print all the local slope plots
slopes_asian = slope_plot(asian_mlm2)
slopes_black = slope_plot(black_mlm2)
slopes_mixed = slope_plot(mixed_mlm2)
slopes_other = slope_plot(other_mlm2)


png("slopes_asian.png", units="in", width=8, height=7, res=300)
slopes_asian
dev.off() # Close the file

png("slopes_black.png", units="in", width=8, height=7, res=300)
slopes_black
dev.off() # Close the file


png("slopes_mixed.png", units="in", width=8, height=7, res=300)
slopes_mixed
dev.off() # Close the file


png("slopes_other.png", units="in", width=8, height=7, res=300)
slopes_other
dev.off() # Close the file

# fixed effects slopes function --------------------------------------------------
fe_plot_by_model <- function(model1, model2, model3, plot_title) {
  
  mixed_fe1 = FEsim(model1)
  mixed_fe2 = FEsim(model2)
  mixed_fe3 = FEsim(model3)
  
  mixed_fe1$model <- "Random Intercept 1"
  mixed_fe2$model <- "Random Intercept 2"
  mixed_fe3$model <- "Random Slope"
  
  combined = rbind(mixed_fe1, mixed_fe2, mixed_fe3)
  
  combined = combined %>% 
    mutate(term = case_when(term == '(Intercept)' ~ '(Intercept)',
                            term == 'Economically.active..Unemployed_per' ~ 'Unemployment',
                            term == 'Economically.inactive..Long.term.sick.or.disabled_per' ~ 'Long-term ill',
                            term == 'Density..number.of.persons.per.hectare.' ~ 'Population Density',
                            term == 'Social.rented_per' ~ 'Social rented',
                            term == 'No_qualifications_per' ~ 'No Qualifications',
                            term == 'Age_20_44_per' ~ 'Age 20-44',
                            term == 'Age_45_64_per' ~ 'Age 45-64',
                            term == 'Age_65_plus_per' ~ 'Age 65+',
                            term == 'gpp_dist' ~ 'Distance to GP',
                            term == 'green_pas' ~ 'Green space presence',
                            term == 'rating1' ~ 'Best performing \nschool rating',
                            term == 'rating4' ~ 'Worst performing \nschool rating',
                            term == 'trips_per_pop' ~ 'Bus connectivity',
                            term == 'no2_mean' ~ 'NO2 levels',
    ))
  
  
  # 
  # combined <- combined %>%
  #   mutate(lower = median - sd *qnorm(1 - ((1 - 0.95)/2)),
  #          upper = median + sd * qnorm(1 - ((1 - 0.95)/2)),
  #          sig = lower > 0 | upper < 0)
  
  combined <- combined %>%
    mutate(lower = median - sd *qnorm(1 - ((1 - 0.95)/2)),
           upper = median + sd * qnorm(1 - ((1 - 0.95)/2)),
           tvalue = mean / sd,
           sig = tvalue > 2 | tvalue < -2)
  

  combined$term <- factor(combined$term ,
                          levels = c("(Intercept)", "Age 20-44", "Age 45-64", "Age 65+",
                                     "Population Density", "Unemployment", "Long-term ill", 
                                     "Social rented", "No Qualifications",
                                     "Distance to GP", "Green space presence", "NO2 levels",
                                     "Best performing \nschool rating", "Worst performing \nschool rating",
                                     "Bus connectivity"))
  
  
  p = ggplot() +
    geom_point(data=combined, aes(x = median, y = model), color = "gray75") +
    geom_pointrange(data=subset(combined, sig == TRUE), aes(x = median, y = model, xmin=lower, xmax=upper), size = 0.3) +
    facet_wrap(~term) +
    geom_vline(xintercept=0, color = "red") +
    labs(title = plot_title, y = "cities") +
    theme_minimal()
  
  return(p)
}
  





