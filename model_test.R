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


normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}

replaceMissing <- function(v) replace(v, is.na(v), 0)



# 
# glm_model_data <- subset(normalised_data, TCITY15NM_2011 %in% c("London",
#                                                    "Birmingham",
#                                                    "Liverpool", 
#                                                    "Bristol" ,
#                                                    "Sheffield",
#                                                    "Manchester",
#                                                    "Leeds",
#                                                    "Leicester",
#                                                    "Bradford",
#                                                    "Coventry",
#                                                    "Nottingham"))
# 
# 
# eq_glm <- divergence_2011_norm ~ Economically.active..Unemployed_per +
#   Density..number.of.persons.per.hectare. + Social.rented_per +
#   gpp_dist + green_pas + rating1 + rating4 + no2_mean
# 
# 
# library(MASS)
# summary(m1 <- glm.nb(formula = eq_glm, data = glm_model_data))





cities_vars <- c("London",
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
                 "Newcastle upon Tyne")




# create an empty list to store the plots
Data_coeff <- data.frame()
#Data_coeff <- list()

for(i in 1:length(cities_vars)){
  
  full_data2 <- subset(full_data_subs, TCITY15NM_2011 == cities_vars[i])
  
  
  normalised_data <- full_data2 %>%
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
  
  normalised_data <-sf::st_make_valid(normalised_data)
  
  
  normalised_data_wgs <- sf::st_transform(normalised_data, "epsg:4326")
  
  
  coordsW <- normalised_data%>%
    st_centroid()%>%
    st_geometry()
  
  X <- st_coordinates(coordsW)[,1]
  Y <- st_coordinates(coordsW)[,2]
  
  
  ggg <- as.data.frame(st_coordinates(coordsW))

  
  
  library(spgwr)
  
  eq1 <- divergence_2011_norm ~ Economically.active..Unemployed_per +
    Economically.inactive..Long.term.sick.or.disabled_per +
    Density..number.of.persons.per.hectare. + Social.rented_per +
    No_qualifications_per + Age_65_plus_per +
    gpp_dist + green_pas + rating1 + rating4 + trips_per_pop + no2_mean
  
  

  # find optimal kernel bandwidth using cross validation
  abw <- gwr.sel(eq1, 
                 data = normalised_data_wgs, 
                 coords = cbind(ggg$X, ggg$Y),
                 adapt = TRUE, 
                 gweight = gwr.Gauss, 
                 verbose = FALSE)
  
  # view selected bandwidth
  #abw 
  
  # fit a gwr based on adaptive bandwidth
  ab_gwr <- gwr(eq1, 
                data = normalised_data_wgs, 
                coords = cbind(ggg$X, ggg$Y),
                adapt = abw, 
                gweight = gwr.Gauss,
                hatmatrix=TRUE, 
                se.fit=TRUE)
  
  #ab_gwr
  
  
  
  
  ab_gwr_out <- as.data.frame(ab_gwr$SDF)
  
  normalised_data_wgs$amb_Unemployed <- ab_gwr_out$Economically.active..Unemployed_per
  normalised_data_wgs$amb_LTI <- ab_gwr_out$Economically.inactive..Long.term.sick.or.disabled_per
  normalised_data_wgs$amb_density <- ab_gwr_out$Density..number.of.persons.per.hectare.
  normalised_data_wgs$amb_Socrented <- ab_gwr_out$Social.rented_per
  normalised_data_wgs$amb_Noqual <- ab_gwr_out$No_qualifications_per
  normalised_data_wgs$amb_65plus <- ab_gwr_out$Age_65_plus_per
  normalised_data_wgs$amb_gpp_dist <- ab_gwr_out$gpp_dist
  normalised_data_wgs$amb_gr_pas <- ab_gwr_out$green_pas
  normalised_data_wgs$amb_sch_rating1 <- ab_gwr_out$rating1
  normalised_data_wgs$amb_sch_rating4 <- ab_gwr_out$rating4
  normalised_data_wgs$amb_trips_day <- ab_gwr_out$trips_per_pop
  normalised_data_wgs$amb_no2 <- ab_gwr_out$no2_mean
  normalised_data_wgs$amb_localR2 <- ab_gwr_out$localR2
  
  # for calculating the residuals
  normalised_data_wgs$pred <- ab_gwr_out$pred
  normalised_data_wgs$resid <- normalised_data_wgs$divergence_2011_norm - normalised_data_wgs$pred
  # plot the residuals
  # ggplot(normalised_data_wgs, aes(resid)) +
  #   geom_density()
  
  # create quartiles for divergence in 2011
  normalised_data_wgs <- normalised_data_wgs %>%
    mutate(quartile_div = ntile(divergence_2011_norm, 4),
           decile_div = ntile(divergence_2011_norm, 10)
    )
  
  
  
  
  
  # compute t statistic for 1st variable
  normalised_data_wgs$t_unempl <- ab_gwr_out$Economically.active..Unemployed_per / ab_gwr_out$Economically.active..Unemployed_per_se
  normalised_data_wgs$t_LTI <- ab_gwr_out$Economically.inactive..Long.term.sick.or.disabled_per / ab_gwr_out$Economically.inactive..Long.term.sick.or.disabled_per_se
  normalised_data_wgs$t_density <- ab_gwr_out$Density..number.of.persons.per.hectare. / ab_gwr_out$Density..number.of.persons.per.hectare._se
  normalised_data_wgs$t_Socrented <- ab_gwr_out$Social.rented_per / ab_gwr_out$Social.rented_per_se
  normalised_data_wgs$t_Noqual <- ab_gwr_out$No_qualifications_per / ab_gwr_out$No_qualifications_per_se
  normalised_data_wgs$t_65plus <- ab_gwr_out$Age_65_plus_per / ab_gwr_out$Age_65_plus_per_se
  normalised_data_wgs$t_gp_dist <- ab_gwr_out$gpp_dist / ab_gwr_out$gpp_dist_se
  normalised_data_wgs$t_gr_pas <- ab_gwr_out$green_pas / ab_gwr_out$green_pas_se
  normalised_data_wgs$t_rating1 <- ab_gwr_out$rating1 / ab_gwr_out$rating1_se
  normalised_data_wgs$t_rating4 <- ab_gwr_out$rating1 / ab_gwr_out$rating4_se
  normalised_data_wgs$t_trips_day <- ab_gwr_out$trips_per_pop / ab_gwr_out$trips_per_pop_se
  normalised_data_wgs$t_no2 <- ab_gwr_out$no2_mean / ab_gwr_out$no2_mean_se
  
  
  
  
  
  # categorise t values
  normalised_data_wgs$t_unempl_cat <- cut(normalised_data_wgs$t_unempl,
                                          breaks=c(min(normalised_data_wgs$t_unempl), -2, 2, max(normalised_data_wgs$t_unempl)),
                                          labels=c("sig","nonsig", "sig"))
  
  normalised_data_wgs$t_LTI_cat <- cut(normalised_data_wgs$t_LTI,
                                          breaks=c(min(normalised_data_wgs$t_LTI), -2, 2, max(normalised_data_wgs$t_LTI)),
                                          labels=c("sig","nonsig", "sig"))
  
  normalised_data_wgs$t_density_cat <- cut(normalised_data_wgs$t_density,
                                           breaks=c(min(normalised_data_wgs$t_density), -2, 2, max(normalised_data_wgs$t_density)),
                                           labels=c("sig","nonsig", "sig"))
  
  normalised_data_wgs$t_Socrented_cat <- cut(normalised_data_wgs$t_Socrented,
                                             breaks=c(min(normalised_data_wgs$t_Socrented), -2, 2, max(normalised_data_wgs$t_Socrented)),
                                             labels=c("sig","nonsig", "sig"))
  
  normalised_data_wgs$t_Noqual_cat <- cut(normalised_data_wgs$t_Noqual,
                                       breaks=c(min(normalised_data_wgs$t_Noqual), -2, 2, max(normalised_data_wgs$t_Noqual)),
                                       labels=c("sig","nonsig", "sig"))
  
  normalised_data_wgs$t_65plus_cat <- cut(normalised_data_wgs$t_65plus,
                                       breaks=c(min(normalised_data_wgs$t_65plus), -2, 2, max(normalised_data_wgs$t_65plus)),
                                       labels=c("sig","nonsig", "sig"))
  
  normalised_data_wgs$t_gp_dist_cat <- cut(normalised_data_wgs$t_gp_dist,
                                           breaks=c(min(normalised_data_wgs$t_gp_dist), -2, 2, max(normalised_data_wgs$t_gp_dist)),
                                           labels=c("sig","nonsig", "sig"))
  
  normalised_data_wgs$t_gr_pas_cat <- cut(normalised_data_wgs$t_gr_pas,
                                          breaks=c(min(normalised_data_wgs$t_gr_pas), -2, 2, max(normalised_data_wgs$t_gr_pas)),
                                          labels=c("sig","nonsig", "sig"))
  

  normalised_data_wgs$t_rating1_cat <- cut(normalised_data_wgs$t_rating1,
                                          breaks=c(min(normalised_data_wgs$t_rating1), -2, 2, max(normalised_data_wgs$t_rating1)),
                                          labels=c("sig","nonsig", "sig"))
  
  normalised_data_wgs$t_rating4_cat <- cut(normalised_data_wgs$t_rating4,
                                           breaks=c(min(normalised_data_wgs$t_rating4), -2, 2, max(normalised_data_wgs$t_rating4)),
                                           labels=c("sig","nonsig", "sig"))
  
  normalised_data_wgs$t_trips_day_cat <- cut(normalised_data_wgs$t_trips_day,
                                           breaks=c(min(normalised_data_wgs$t_trips_day), -2, 2, max(normalised_data_wgs$t_trips_day)),
                                           labels=c("sig","nonsig", "sig"))
  
  normalised_data_wgs$t_no2_cat <- cut(normalised_data_wgs$t_no2,
                                       breaks=c(min(normalised_data_wgs$t_no2), -2, 2, max(normalised_data_wgs$t_no2)),
                                       labels=c("sig","nonsig", "sig"))
  
 
  Data_coeff <- rbind(Data_coeff, normalised_data_wgs)
  #Data_coeff[[i]] <- normalised_data_wgs
  
}

#big_data <- do.call(rbind, Data_coeff)

st_write(Data_coeff, "Data_coeff.gpkg")
st_write(Data_coeff, "Data_coeff.csv")





