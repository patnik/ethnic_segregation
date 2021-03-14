library(dplyr)

# read ethnic groups by LSOA and calculate entropy ------------------------
ethn_gr <- read.csv("KS201_GB.csv")

# filter only the England and Wales rows
# wee need this as we want the total of populations across the dataset
ethn_gr <- filter(ethn_gr, grepl('E|W', Code))


# calculate percentages
ethn_gr <- ethn_gr %>% 
  mutate(White_per = White/All.usual.residents,
         Mixed.multiple.ethnic.groups_per = Mixed.multiple.ethnic.groups/All.usual.residents,
         Asian.Asian.British_per = Asian.Asian.British/All.usual.residents,
         Black.African.Caribbean.Black.British_per = Black.African.Caribbean.Black.British/All.usual.residents,
         Other.ethnic.group_per = Other.ethnic.group/All.usual.residents)

# na.zero <- function (x) {
#   x[is.na(x)] <- 0
#   return(x)
# }
# 
# 
# ent <- function(data){
#   data[data==0] <- .00001 # I use this line as the log(0) produces NA which does not help here
#   d <- data[-1]*log(data[-1])
#   d$total <- -rowSums(d)
#   d$entropy <- d$total * (1/log(ncol(data)-1))
#   
#   d <- na.zero(d)
#   d <- cbind(data[1],d$entropy)
#   colnames(d)[2] <- "entropy"
#   return(d)
#   
#   
# }



# calculate total population by group
summary <- ethn_gr %>%
  summarise_at(c("All.usual.residents", "White", "Mixed.multiple.ethnic.groups",
                 "Asian.Asian.British", "Black.African.Caribbean.Black.British",
                 "Other.ethnic.group"), sum, na.rm = TRUE)

White_av <- summary[2]/summary[1]
Mixed.multiple.ethnic.groups_av <- summary[3]/summary[1]
Asian.Asian.British_av <- summary[4]/summary[1]
Black.African.Caribbean.Black.British_av <- summary[5]/summary[1]
Other.ethnic.group_av <- summary[6]/summary[1]



ethn_gr <- ethn_gr %>% 
  mutate(white_int = White_per*log(White_per/as.numeric(White_av)),
         mixed_int = Mixed.multiple.ethnic.groups_per*log(Mixed.multiple.ethnic.groups_per/as.numeric(Mixed.multiple.ethnic.groups_av)),
         asian_int = Asian.Asian.British_per*log(Asian.Asian.British_per/as.numeric(Asian.Asian.British_av)),
         black_int = Black.African.Caribbean.Black.British_per*log(Black.African.Caribbean.Black.British_per/as.numeric(Black.African.Caribbean.Black.British_av)),
         other_int = Other.ethnic.group_per*log(Other.ethnic.group_per/as.numeric(Other.ethnic.group_av))) %>% 
  replace(is.na(.), 0)


ethn_gr <- ethn_gr %>% 
  mutate(divergence = white_int + mixed_int + asian_int + black_int + other_int)


ethn_gr <- ethn_gr %>% 
  mutate(norm_divergence = divergence/max(divergence))


# calculate Location Quotient by ethnic group
ethn_gr <- ethn_gr %>% 
  mutate(LQ_mixed = (Mixed.multiple.ethnic.groups/as.numeric(Mixed.multiple.ethnic.groups_av))/ (White/as.numeric(White_av)),
         LQ_asian = (Asian.Asian.British/as.numeric(Asian.Asian.British_av))/ (White/as.numeric(White_av)),
         LQ_black = (Black.African.Caribbean.Black.British/as.numeric(Black.African.Caribbean.Black.British_av))/ (White/as.numeric(White_av)),
         LQ_other = (Other.ethnic.group/as.numeric(Other.ethnic.group_av))/ (White/as.numeric(White_av)))


ethn_gr <- ethn_gr %>% 
  mutate(LQ_mixed_cat = case_when(LQ_mixed > 1 ~ "more_diverse",
                                   LQ_mixed < 1 ~ "less_diverse",
                                   LQ_mixed == 1 ~ "equally_diverse"),
         LQ_asian_cat = case_when(LQ_asian > 1 ~ "more_diverse",
                                  LQ_asian < 1 ~ "less_diverse",
                                  LQ_asian == 1 ~ "equally_diverse"),
         LQ_black_cat = case_when(LQ_black > 1 ~ "more_diverse",
                                  LQ_black < 1 ~ "less_diverse",
                                  LQ_black == 1 ~ "equally_diverse"),
         LQ_other_cat = case_when(LQ_other > 1 ~ "more_diverse",
                                  LQ_other < 1 ~ "less_diverse",
                                  LQ_other == 1 ~ "equally_diverse"))


# add suffix to column names
colnames(ethn_gr)[-1] <- paste(colnames(ethn_gr)[-1], "2011", sep = "_")


# cols <- c("Code", "White_per", "Mixed.multiple.ethnic.groups_per", "Asian.Asian.British_per",
#           "Black.African.Caribbean.Black.British_per", "Other.ethnic.group_per")
# 
# ethn_gr_subs <- ethn_gr %>%
#   select(all_of(cols))
# 
# 
# ethn_entr <- ent(ethn_gr_subs)
# 
# ethn_gr_full <- left_join(ethn_gr, ethn_entr, by = "Code")

# # create groups
# ethn_gr_full <- ethn_gr_full %>% 
#   mutate(segregation = case_when(entropy <= 0.4109 | White_per >= 0.8 | Mixed.multiple.ethnic.groups_per >= 0.8 | Asian.Asian.British_per >= 0.8 | Black.African.Caribbean.Black.British_per >= 0.8 | Other.ethnic.group_per >= 0.8 ~ "segregated",
#                         TRUE ~ "not_segregated")) 
# 
# ethn_gr_full <- ethn_gr_full %>% 
#   mutate(ethn_segregation = case_when(segregation == "segregated" & White_per >= 0.8 ~ "white_segregation",
#                                       segregation == "segregated" & White_per < 0.8 ~ "minority_segregation",
#                                       TRUE ~ "not_segregated"))


# may be needed in the future
# entropy > 0.6766 & (White_per <= 0.45 & Mixed.multiple.ethnic.groups_per <= 0.45 & Asian.Asian.British_per <= 0.45 & Black.African.Caribbean.Black.British_per <= 0.45 & Other.ethnic.group_per <= 0.45)  ~ "low_segregation",

# read ethnic groups by LSOA and calculate entropy ------------------------
ethn01_gr <- read.csv("KS006_EW_converted.csv")

# calculate percentages
ethn01_gr <- ethn01_gr %>% 
  mutate(White_per = White/All.usual.residents,
         Mixed.multiple.ethnic.groups_per = Mixed.multiple.ethnic.groups/All.usual.residents,
         Asian.Asian.British_per = Asian.Asian.British/All.usual.residents,
         Black.African.Caribbean.Black.British_per = Black.African.Caribbean.Black.British/All.usual.residents,
         Other.ethnic.group_per = Other.ethnic.group/All.usual.residents)




summary01 <- ethn01_gr %>%
  summarise_at(c("All.usual.residents", "White", "Mixed.multiple.ethnic.groups",
                 "Asian.Asian.British", "Black.African.Caribbean.Black.British",
                 "Other.ethnic.group"), sum, na.rm = TRUE)

White_av_01 <- summary01[2]/summary01[1]
Mixed.multiple.ethnic.groups_av_01 <- summary01[3]/summary01[1]
Asian.Asian.British_av_01 <- summary01[4]/summary01[1]
Black.African.Caribbean.Black.British_av_01 <- summary01[5]/summary01[1]
Other.ethnic.group_av_01 <- summary01[6]/summary01[1]



ethn01_gr <- ethn01_gr %>% 
  mutate(white_int = White_per*log(White_per/as.numeric(White_av_01)),
         mixed_int = Mixed.multiple.ethnic.groups_per*log(Mixed.multiple.ethnic.groups_per/as.numeric(Mixed.multiple.ethnic.groups_av_01)),
         asian_int = Asian.Asian.British_per*log(Asian.Asian.British_per/as.numeric(Asian.Asian.British_av_01)),
         black_int = Black.African.Caribbean.Black.British_per*log(Black.African.Caribbean.Black.British_per/as.numeric(Black.African.Caribbean.Black.British_av_01)),
         other_int = Other.ethnic.group_per*log(Other.ethnic.group_per/as.numeric(Other.ethnic.group_av_01))) %>% 
  replace(is.na(.), 0)


ethn01_gr <- ethn01_gr %>% 
  mutate(divergence = white_int + mixed_int + asian_int + black_int + other_int)


ethn01_gr <- ethn01_gr %>% 
  mutate(norm_divergence = divergence/max(divergence))

# calculate Location Quotient by ethnic group
ethn01_gr <- ethn01_gr %>% 
  mutate(LQ_mixed = (Mixed.multiple.ethnic.groups/as.numeric(Mixed.multiple.ethnic.groups_av))/ (White/as.numeric(White_av)),
         LQ_asian = (Asian.Asian.British/as.numeric(Asian.Asian.British_av))/ (White/as.numeric(White_av)),
         LQ_black = (Black.African.Caribbean.Black.British/as.numeric(Black.African.Caribbean.Black.British_av))/ (White/as.numeric(White_av)),
         LQ_other = (Other.ethnic.group/as.numeric(Other.ethnic.group_av))/ (White/as.numeric(White_av)))


ethn01_gr <- ethn01_gr %>% 
  mutate(LQ_mixed_cat = case_when(LQ_mixed > 1 ~ "more_diverse",
                                  LQ_mixed < 1 ~ "less_diverse",
                                  LQ_mixed == 1 ~ "equally_diverse"),
         LQ_asian_cat = case_when(LQ_asian > 1 ~ "more_diverse",
                                  LQ_asian < 1 ~ "less_diverse",
                                  LQ_asian == 1 ~ "equally_diverse"),
         LQ_black_cat = case_when(LQ_black > 1 ~ "more_diverse",
                                  LQ_black < 1 ~ "less_diverse",
                                  LQ_black == 1 ~ "equally_diverse"),
         LQ_other_cat = case_when(LQ_other > 1 ~ "more_diverse",
                                  LQ_other < 1 ~ "less_diverse",
                                  LQ_other == 1 ~ "equally_diverse"))

# add suffix to column names
colnames(ethn01_gr)[-1] <- paste(colnames(ethn01_gr)[-1], "2001", sep = "_")


# read agre groups by LSOA  ------------------------
age_group <- read.csv("KS102_EW.csv")


# calculate percentages
age_group <- age_group %>% 
  mutate(Age_0_19_per = Age_0_19/All.usual.residents,
         Age_20_44_per = Age_20_44/All.usual.residents,
         Age_45_64_per = Age_45_64/All.usual.residents,
         Age_65_plus_per = Age_65_plus/All.usual.residents,)


# read houdehold composition by LSOA  ------------------------
hhld_comp <- read.csv("KS105_GB.csv")

str(hhld_comp)

# calculate percentages
hhld_comp <- hhld_comp %>% 
  mutate(One.person.household..Aged.65.and.over_per = One.person.household..Aged.65.and.over/All.categories..Household.composition,
         One.person.household..Other_per = One.person.household..Other/All.categories..Household.composition,
         One.family.only..All.aged.65.and.over_per = One.family.only..All.aged.65.and.over/All.categories..Household.composition,
         One.family.only..Married.or.same.sex.civil.partnership.couple_per = One.family.only..Married.or.same.sex.civil.partnership.couple/All.categories..Household.composition,
         One.family.only..Cohabiting.couple_per = One.family.only..Cohabiting.couple/All.categories..Household.composition,
         One.family.only..Lone.parent_per = One.family.only..Lone.parent/All.categories..Household.composition,
         Other.household.types..With.dependent.children_per = Other.household.types..With.dependent.children/All.categories..Household.composition,
         Other.household.types..All.full.time.students_per = Other.household.types..All.full.time.students/All.categories..Household.composition,
         Other.household.types..All.aged.65.and.over_per = Other.household.types..All.aged.65.and.over/All.categories..Household.composition,
         Other.household.types..Other_per = Other.household.types..Other/All.categories..Household.composition)


# read tenure by LSOA  ------------------------
tenure <- read.csv("KS402_GB.csv")

str(tenure)

# calculate percentages
tenure <- tenure %>% 
  mutate(Owned_per = Owned/All.households,
         Shared.ownership..part.owned.and.part.rented._per = Shared.ownership..part.owned.and.part.rented./All.households,
         Social.rented_per = Social.rented/All.households,
         Private.rented_per = Private.rented/All.households,
         Living.rent.free_per = Living.rent.free/All.households)

# read car availability by LSOA  ------------------------
car_av <- read.csv("KS404_GB.csv")

str(car_av)

# calculate percentages
car_av <- car_av %>% 
  mutate(No.cars.or.vans.in.household_per = No.cars.or.vans.in.household/All.categories..Car.or.van.availability,
         One.car.or.van.in.household_per = One.car.or.van.in.household/All.categories..Car.or.van.availability,
         Two.cars.or.vans.in.household_per = Two.cars.or.vans.in.household/All.categories..Car.or.van.availability,
         Three.cars.or.vans.in.household_per = Three.cars.or.vans.in.household/All.categories..Car.or.van.availability,
         Four.or.more.cars.or.vans.in.household_per = Four.or.more.cars.or.vans.in.household/All.categories..Car.or.van.availability)

# read economic activity by LSOA  ------------------------
econ_act <- read.csv("KS601_GB.csv")

str(econ_act)

# calculate percentages
econ_act <- econ_act %>% 
  mutate(Economically.active..In.employment_per = Economically.active..In.employment/All.usual.residents.aged.16.to.74,
         Economically.active..Unemployed_per = Economically.active..Unemployed/All.usual.residents.aged.16.to.74,
         Economically.active..Full.time.student_per = Economically.active..Full.time.student/All.usual.residents.aged.16.to.74,
         Economically.inactive..Retired_per = Economically.inactive..Retired/All.usual.residents.aged.16.to.74,
         Economically.inactive..Student..including.full.time.students._per = Economically.inactive..Student..including.full.time.students./All.usual.residents.aged.16.to.74,
         Economically.inactive..Looking.after.home.or.family_per = Economically.inactive..Looking.after.home.or.family/All.usual.residents.aged.16.to.74,
         Economically.inactive..Long.term.sick.or.disabled_per = Economically.inactive..Long.term.sick.or.disabled/All.usual.residents.aged.16.to.74,
         Economically.inactive..Other_per = Economically.inactive..Other/All.usual.residents.aged.16.to.74)


# read economic activity by LSOA  ------------------------
pop_density <- read.csv("QS102_GB.csv")


# read central heating type by LSOA  ------------------------
heating <- read.csv("QS415_GB.csv")

str(heating)

# calculate percentages
heating <- heating %>% 
  mutate(No.central.heating_per = No.central.heating/All.categories..Type.of.central.heating.in.household,
         Gas.central.heating_per = Gas.central.heating/All.categories..Type.of.central.heating.in.household,
         Electric..including.storage.heaters..central.heating_per = Electric..including.storage.heaters..central.heating/All.categories..Type.of.central.heating.in.household,
         Oil.central.heating_per = Oil.central.heating/All.categories..Type.of.central.heating.in.household,
         Solid.fuel..for.example.wood..coal..central.heating_per = Solid.fuel..for.example.wood..coal..central.heating/All.categories..Type.of.central.heating.in.household,
         Other.central.heating_per = Other.central.heating/All.categories..Type.of.central.heating.in.household,
         Two.or.more.types.of.central.heating_per = Two.or.more.types.of.central.heating/All.categories..Type.of.central.heating.in.household)



# Merge all census data ---------------------------------------------------

# merge all datasets
census_data <- list(ethn_gr, ethn01_gr, hhld_comp, tenure, age_group, car_av, econ_act, pop_density, heating) %>%
  Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2, by="Code"), .)



# this is an alternative way
# library(tidyverse)
# list(x, y, z) %>% reduce(left_join, by = "Code")


## calculate the predominant group for each LSOA and attach the name of the group in a new column
pred_calc <- census_data[,c("Mixed.multiple.ethnic.groups_per_2011",
                    "Asian.Asian.British_per_2011", "Black.African.Caribbean.Black.British_per_2011",
                    "Other.ethnic.group_per_2011")]

pred_group <- colnames(pred_calc)[max.col(pred_calc,ties.method="first")]

census_data <- cbind(census_data, pred_group)

census_data %>%
  group_by(pred_group) %>% 
  tally()

write.csv(census_data, "census_data.csv")


saveRDS(census_data, "census_data.rds")



