# Helper functions
#=====================================================
# crop to Vancouver
# Demographic groups
get_groups <- function(){
  
  groups_age <- c(
    "male_0_4", "male_5_9", "male_10_14", "male_15_19", "male_20_24", "male_25_29", "male_30_34",
    "male_35_39", "male_40_44", "male_45_49", "male_50_54", "male_55_59", "male_60_64", "male_65_69",
    "male_70_74", "male_75_79", "male_80_84", "male_85", "female_0_4", "female_5_9", "female_10_14",
    "female_15_19", "female_20_24", "female_25_29", "female_30_34", "female_35_39", "female_40_44",
    "female_45_49", "female_50_54", "female_55_59", "female_60_64", "female_65_69", "female_70_74",
    "female_75_79", "female_80_84", "female_85"
  )
  
  groups_income <- c("low", "medium", "high")
  
  groups_education <- c("no_diploma", "secondary", "post_secondary")
  
  groups_minority <- c(
    "non_minority", "indigenous", "south_asian", "chinese", "black", "filipino", "arab_west_asian",
    "latin_american", "southeast_asian", "korean", "japanese", "other_minority"
  )
  
  list(income = groups_income,
       age_sex = groups_age,
       education = groups_education,
       minority = groups_minority,
       all = c(groups_income, groups_age, groups_education, groups_minority))
}

crop_to_vancouver <- function(file, raster=FALSE){
  
  e <- extent(file)
  e@xmin = -123.5
  e@xmax = -122.6
  e@ymin = 49.0
  e@ymax = 49.4
  
  if(raster){
    file <- crop(file, e)
  }else{
    file <- st_crop(file, c(xmin=e@xmin, ymin=e@ymin, xmax=e@xmax, ymax=e@ymax))
  }
  file
  
}

# crop to Canada
crop_to_canada <- function(file, raster=FALSE){
  
  e <- extent(file)
  e@xmin = -141.1
  e@xmax = -52.5
  e@ymin = 41.6
  e@ymax = 89
  
  if(raster){
    file <- crop(file, e)
  }else{
    file <- st_crop(file, c(xmin=e@xmin, ymin=e@ymin, xmax=e@xmax, ymax=e@ymax))
  }
  file
  
}