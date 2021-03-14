library(sf)
library(dplyr)

# read census data
census_data <- readRDS("../Census_data/census_data.rds")

# read AHAH index
ahah_data <- read.csv("../AHAH/allvariableslsoawdeciles.csv")

# read Interent User Classification
iuc_data <- st_read("../Internet User Classification/IUC2018.shp")

# read in average download boradband speed by LSOA
bbspeed_data <- read.csv("../BroadBandSpeed/av_bbspeed_LSOA.csv")

# read in average price paid by LSOA
ppaid_data <- read.csv("../Properties/av_lsoa_price_dec15.csv")

# read in mode property age by LSOA
propage_data <- read.csv("../Properties/voa_property_age.csv")

# read in proportion of flats, houses and adresses with outdoor space
outdoor_space <- read.csv("../OS_ONS/EW_private_green_space_LSOA.csv")

# read the lookup table between LSOAs and districts
lookupOA_LSOA <- read.csv("../BroadBandSpeed/Output_Area_to_LSOA_to_MSOA_to_Local_Authority_District_(December_2017)_Lookup_with_Area_Classifications_in_Great_Britain.csv")
## keep only unique LSOAs
unique_lsoa <- lookupOA_LSOA %>% distinct(LSOA11CD, .keep_all = TRUE)



# join all the datasets
full_data <- left_join(iuc_data, unique_lsoa[c("LSOA11CD", "LAD17NM", "RGN11NM")], by = c("LSOA11_CD" = "LSOA11CD"))

full_data <- left_join(full_data, census_data, by = c("LSOA11_CD" = "Code"))

full_data <- left_join(full_data, ahah_data, by = c("LSOA11_CD" = "lsoa11"))

full_data <- left_join(full_data, bbspeed_data, by = c("LSOA11_CD" = "LSOA11CD"))

full_data <- left_join(full_data, ppaid_data, by = c("LSOA11_CD" = "lsoacd"))

full_data <- left_join(full_data, propage_data[c("AREA_CODE", "MODE1_TYPE")], by = c("LSOA11_CD" = "AREA_CODE"))

full_data <- left_join(full_data, outdoor_space, by = c("LSOA11_CD" = "Code"))


# seect only the columns needed for modelling
cols <- c("LSOA11_CD", "GRP_LABEL", "LAD17NM", "RGN11NM", "divergence_2011", "norm_divergence_2011",
          "divergence_2001", "norm_divergence_2001", "pred_group",
          "LQ_mixed_2011", "LQ_asian_2011", "LQ_black_2011", "LQ_other_2011",
          "LQ_mixed_2001", "LQ_asian_2001", "LQ_black_2001", "LQ_other_2001",
          "LQ_mixed_cat_2011", "LQ_asian_cat_2011", "LQ_black_cat_2011", "LQ_other_cat_2011",
          "LQ_mixed_cat_2001", "LQ_asian_cat_2001", "LQ_black_cat_2001", "LQ_other_cat_2001",
          "per_house", "per_flat", "per_outdoorspace",
          "Age_20_44_per", "Age_45_64_per", "Age_65_plus_per",
          "One.person.household..Aged.65.and.over_per", 
          "One.person.household..Other_per", "One.family.only..All.aged.65.and.over_per",
          "One.family.only..Married.or.same.sex.civil.partnership.couple_per",
          "One.family.only..Cohabiting.couple_per", "One.family.only..Lone.parent_per",
          "Other.household.types..With.dependent.children_per",
          "Owned_per", "Social.rented_per", "Private.rented_per",
          "No.cars.or.vans.in.household_per", "One.car.or.van.in.household_per",
          "Two.cars.or.vans.in.household_per", "Three.cars.or.vans.in.household_per",
          "Four.or.more.cars.or.vans.in.household_per",
          "Economically.active..Unemployed_per", "Economically.inactive..Looking.after.home.or.family_per",
          "Economically.inactive..Long.term.sick.or.disabled_per",
          "Density..number.of.persons.per.hectare.",
          "No.central.heating_per", "Gas.central.heating_per", "Electric..including.storage.heaters..central.heating_per",
          "Oil.central.heating_per", "Solid.fuel..for.example.wood..coal..central.heating_per",
          "Other.central.heating_per", "Two.or.more.types.of.central.heating_per",
          "AVG_LSOA", "dec_15", "MODE1_TYPE",
          "gpp_dist", "ed_dist", "dent_dist", "pharm_dist", "gamb_dist", "ffood_dist",
          "pubs_dist", "leis_dist", "blue_dist", "off_dist", "tobac_dist", "green_pas", "green_act", "no2_mean",
          "pm10_mean", "so2_mean"
)

full_data_subs <- full_data %>% 
  select(all_of(cols))

st_geometry(full_data_subs) <- NULL

saveRDS(full_data_subs, "all_data_2011.rds")


# treat the dataset as dataframe
st_geometry(full_data) <- NULL

# seect only the columns needed for modelling
cols <- c("LSOA11_CD", "entropy", "One.person.household..Aged.65.and.over_per", 
          "One.person.household..Other_per", "One.family.only..All.aged.65.and.over_per",
          "One.family.only..Married.or.same.sex.civil.partnership.couple_per",
          "One.family.only..Cohabiting.couple_per", "One.family.only..Lone.parent_per",
          "Other.household.types..With.dependent.children_per",
          "Owned_per", "Social.rented_per", "Private.rented_per",
          "No.cars.or.vans.in.household_per", "One.car.or.van.in.household_per",
          "Two.cars.or.vans.in.household_per", "Three.cars.or.vans.in.household_per",
          "Four.or.more.cars.or.vans.in.household_per",
          "Economically.active..Unemployed_per", "Economically.inactive..Looking.after.home.or.family_per",
          "Economically.inactive..Long.term.sick.or.disabled_per",
          "Density..number.of.persons.per.hectare.", "gpp_dist",
          "ed_dist", "dent_dist", "pharm_dist", "gamb_dist", "ffood_dist",
          "pubs_dist", "leis_dist", "blue_dist", "off_dist", "tobac_dist", "green_pas", "green_act"
          )

full_data_subs <- full_data %>% 
  select(all_of(cols)) 



  
eq1 <- entropy ~ One.person.household..Aged.65.and.over_per + 
  One.person.household..Other_per + 
  gpp_dist


model1 <- lm(formula = eq1, data = full_data_subs)

# a model that inlcudes all variables
model2 <- lm(formula = entropy ~ ., data = full_data_subs[-1])

# estimates
summary(model2)



