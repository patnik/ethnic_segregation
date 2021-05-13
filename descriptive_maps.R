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
cols <- c("LSOA11_CD", "GRP_LABEL", "LAD17NM", "RGN11NM", "divergence_2011", "divergence_2001",
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









library(ggplot2)
library(viridis)
library(classInt)
library(cowplot)

# this part of the code is to create maps for all cities using a f --------

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
final_maps <- list()

for(i in 1:length(cities_vars)){
  
  city_map <- subset(full_data_subs, TCITY15NM_2011 == cities_vars[i])
  
  # assign each LSOA to a quantile for every variable
  city_map <- city_map %>%
    mutate(divergence_2011_cat = as.character(ntile(divergence_2011, 5)),
           unempl_cat = as.character(ntile(Economically.active..Unemployed_per, 5)),
           noQual_cat = as.character(ntile(No_qualifications_per, 5)),
           gpdist_cat = as.character(ntile(gpp_dist, 5)),
           grsp_cat = as.character(ntile(green_pas, 5)),
           rating1_cat = as.character(ntile(rating1, 5)),
           rating4_cat = as.character(ntile(rating4, 5)),
           bustrips_cat = as.character(ntile(trips_per_day, 5))
    )
  
  
  
  
  
  m1 <- ggplot(city_map) + 
    geom_sf(aes(fill=divergence_2011_cat), lwd = 0.1) +
    # scale_fill_viridis(option="plasma", discrete=TRUE)
    scale_fill_brewer(palette = "OrRd", name = "Quantiles",
                      labels = c("1st",
                                 "2nd",
                                 "3rd",
                                 "4th",
                                 "5th")) +
    labs(title = "Divergence") +
    theme_void() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  m2 <- ggplot(city_map) + 
    geom_sf(aes(fill=unempl_cat), lwd = 0.1) +
    #scale_fill_viridis(option="plasma", discrete=TRUE)
    scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                      labels = c("1st",
                                 "2nd",
                                 "3rd",
                                 "4th",
                                 "5th")) +
    labs(title = "Unemployment") +
    theme_void() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  m3 <- ggplot(city_map) + 
    geom_sf(aes(fill=noQual_cat), lwd = 0.1) +
    #scale_fill_viridis(option="plasma", discrete=TRUE)
    scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                      labels = c("1st",
                                 "2nd",
                                 "3rd",
                                 "4th",
                                 "5th")) +
    labs(title = "No Qualifications") +
    theme_void() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  m4 <- ggplot(city_map) + 
    geom_sf(aes(fill=gpdist_cat), lwd = 0.1) +
    #scale_fill_viridis(option="plasma", discrete=TRUE)
    scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                      labels = c("1st",
                                 "2nd",
                                 "3rd",
                                 "4th",
                                 "5th")) +
    labs(title = "Access to GP") +
    theme_void() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  m5 <- ggplot(city_map) + 
    geom_sf(aes(fill=grsp_cat), lwd = 0.1) +
    #scale_fill_viridis(option="plasma", discrete=TRUE)
    scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                      labels = c("1st",
                                 "2nd",
                                 "3rd",
                                 "4th",
                                 "5th")) +
    labs(title = "Green space presence") +
    theme_void() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  m6 <- ggplot(city_map) + 
    geom_sf(aes(fill=rating1_cat), lwd = 0.1) +
    #scale_fill_viridis(option="plasma", discrete=TRUE)
    scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                      labels = c("1st",
                                 "2nd",
                                 "3rd",
                                 "4th",
                                 "5th")) +
    labs(title = "Best performing schools") +
    theme_void() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  m7 <- ggplot(city_map) + 
    geom_sf(aes(fill=rating4_cat), lwd = 0.1) +
    #scale_fill_viridis(option="plasma", discrete=TRUE)
    scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                      labels = c("1st",
                                 "2nd",
                                 "3rd",
                                 "4th",
                                 "5th")) +
    labs(title = "Worst performing schools") +
    theme_void() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  m8 <- ggplot(city_map) + 
    geom_sf(aes(fill=bustrips_cat), lwd = 0.1) +
    #scale_fill_viridis(option="plasma", discrete=TRUE)
    scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                      labels = c("1st",
                                 "2nd",
                                 "3rd",
                                 "4th",
                                 "5th")) +
    labs(title = "Bus connectivity") +
    theme_void() +
    theme(axis.title=element_blank(),
          axis.text=element_blank(),
          axis.ticks=element_blank(),
          plot.title = element_text(hjust = 0.5))
  
  
  
  plots_only <- plot_grid(
    m1 + theme(legend.position="none"),
    m2 + theme(legend.position="none"),
    m3 + theme(legend.position="none"),
    m4 + theme(legend.position="none"),
    m5 + theme(legend.position="none"),
    m6 + theme(legend.position="none"),
    m7 + theme(legend.position="none"),
    m8 + theme(legend.position="none"),
    nrow = 2
  )
  
  
  title_theme <- ggdraw() +
    draw_label(cities_vars[i], fontface = 'bold', x = 0.5, hjust = 0)
  
  maps_w_title <- plot_grid(title_theme, plots_only, ncol = 1, rel_heights = c(0.1, 1))
  
  
  legend_b <- get_legend(
    m1 + 
      guides(color = guide_legend(nrow = 1)) +
      theme(legend.position = "bottom")
  )
  
  # add the legend underneath the row we made earlier. Give it 10%
  # of the height of one plot (via rel_heights).
  final_maps[[i]] <- plot_grid(maps_w_title, legend_b, ncol = 1, rel_heights = c(1, .1))
  
  
  
  
  
}


for(i in 1:length(cities_vars)){
  
  png(paste0(cities_vars[i], "_expl_maps.png"), units="in", width=15, height=10, res=300)
  print(final_maps[[i]])
  dev.off() # Close the file
  
}


# this part of the code is to create maps for one city --------------------




city_map <- subset(full_data_subs, TCITY15NM_2011 == "Liverpool")




# assign each LSOA to a quantile for every variable
city_map <- city_map %>%
  mutate(divergence_2011_cat = as.character(ntile(divergence_2011, 5)),
         unempl_cat = as.character(ntile(Economically.active..Unemployed_per, 5)),
         noQual_cat = as.character(ntile(No_qualifications_per, 5)),
         gpdist_cat = as.character(ntile(gpp_dist, 5)),
         grsp_cat = as.character(ntile(green_pas, 5)),
         rating1_cat = as.character(ntile(rating1, 5)),
         rating4_cat = as.character(ntile(rating4, 5)),
         bustrips_cat = as.character(ntile(trips_per_day, 5))
  )





m1 <- ggplot(city_map) + 
  geom_sf(aes(fill=divergence_2011_cat), lwd = 0.1) +
  # scale_fill_viridis(option="plasma", discrete=TRUE)
  scale_fill_brewer(palette = "OrRd", name = "Quantiles",
                    labels = c("1st",
                               "2nd",
                               "3rd",
                               "4th",
                               "5th")) +
  labs(title = "Divergence") +
  theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5))

m2 <- ggplot(city_map) + 
  geom_sf(aes(fill=unempl_cat), lwd = 0.1) +
  #scale_fill_viridis(option="plasma", discrete=TRUE)
  scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                    labels = c("1st",
                               "2nd",
                               "3rd",
                               "4th",
                               "5th")) +
  labs(title = "Unemployment") +
  theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5))

m3 <- ggplot(city_map) + 
  geom_sf(aes(fill=noQual_cat), lwd = 0.1) +
  #scale_fill_viridis(option="plasma", discrete=TRUE)
  scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                    labels = c("1st",
                               "2nd",
                               "3rd",
                               "4th",
                               "5th")) +
  labs(title = "No Qualifications") +
  theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5))

m4 <- ggplot(city_map) + 
  geom_sf(aes(fill=gpdist_cat), lwd = 0.1) +
  #scale_fill_viridis(option="plasma", discrete=TRUE)
  scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                    labels = c("1st",
                               "2nd",
                               "3rd",
                               "4th",
                               "5th")) +
  labs(title = "Access to GP") +
  theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5))

m5 <- ggplot(city_map) + 
  geom_sf(aes(fill=grsp_cat), lwd = 0.1) +
  #scale_fill_viridis(option="plasma", discrete=TRUE)
  scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                    labels = c("1st",
                               "2nd",
                               "3rd",
                               "4th",
                               "5th")) +
  labs(title = "Green space presence") +
  theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5))

m6 <- ggplot(city_map) + 
  geom_sf(aes(fill=rating1_cat), lwd = 0.1) +
  #scale_fill_viridis(option="plasma", discrete=TRUE)
  scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                    labels = c("1st",
                               "2nd",
                               "3rd",
                               "4th",
                               "5th")) +
  labs(title = "Best performing schools") +
  theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5))

m7 <- ggplot(city_map) + 
  geom_sf(aes(fill=rating4_cat), lwd = 0.1) +
  #scale_fill_viridis(option="plasma", discrete=TRUE)
  scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                    labels = c("1st",
                               "2nd",
                               "3rd",
                               "4th",
                               "5th")) +
  labs(title = "Worst performing schools") +
  theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5))

m8 <- ggplot(city_map) + 
  geom_sf(aes(fill=bustrips_cat), lwd = 0.1) +
  #scale_fill_viridis(option="plasma", discrete=TRUE)
  scale_fill_brewer(palette = "OrRd", name = "Quantiles", 
                    labels = c("1st",
                               "2nd",
                               "3rd",
                               "4th",
                               "5th")) +
  labs(title = "Bus connections") +
  theme_void() +
  theme(axis.title=element_blank(),
        axis.text=element_blank(),
        axis.ticks=element_blank(),
        plot.title = element_text(hjust = 0.5))



plots_only <- plot_grid(
  m1 + theme(legend.position="none"),
  m2 + theme(legend.position="none"),
  m3 + theme(legend.position="none"),
  m4 + theme(legend.position="none"),
  m5 + theme(legend.position="none"),
  m6 + theme(legend.position="none"),
  m7 + theme(legend.position="none"),
  m8 + theme(legend.position="none"),
  nrow = 2
)
plots_only


legend_b <- get_legend(
  m1 + 
    guides(color = guide_legend(nrow = 1)) +
    theme(legend.position = "bottom")
)

# add the legend underneath the row we made earlier. Give it 10%
# of the height of one plot (via rel_heights).
printing <- plot_grid(plots_only, legend_b, ncol = 1, rel_heights = c(1, .1))


png(paste0("printing", "_plotxxx.png"), units="in", width=15, height=10, res=300)
printing
dev.off() # Close the file


# this could work if we had similar scales
# library(tidyr)
# nc2 <- city_map_subs %>% select(divergence_2011, Density..number.of.persons.per.hectare., geometry) %>% gather(VAR, SID, -geometry)
# ggplot() + 
#   geom_sf(data = nc2, aes(fill = SID)) + 
#   facet_wrap(~VAR, ncol = 1) +
#   scale_y_continuous(breaks = 34:36)




