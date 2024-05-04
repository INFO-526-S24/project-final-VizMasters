library(tidyverse) 
library(janitor)
library(scales)

## Functions - Data Wrangling




fun_DataFile_01 <-function()
{
  
  # Data File : 01_Annual_Number_Of_death_by_cause ---------------------------------------------------------------------------------------------------------------------------------
  
  # Loading data
  data <- read_csv(here("data/01_annual-number-of-deaths-by-cause.csv"))
  # glimpse(data)
  
  # Data Wrangling
  data <- data |>
    clean_names() |>
    rename( country = entity,
            meningitis = deaths_meningitis_sex_both_age_all_ages_number,
            alzheimers =   deaths_alzheimers_disease_and_other_dementias_sex_both_age_all_ages_number,
            parkinsons = deaths_parkinsons_disease_sex_both_age_all_ages_number,
            nutritional_deficiencies = deaths_nutritional_deficiencies_sex_both_age_all_ages_number,
            malaria = deaths_malaria_sex_both_age_all_ages_number,
            drowning = deaths_drowning_sex_both_age_all_ages_number,
            interpersonal_violence = deaths_interpersonal_violence_sex_both_age_all_ages_number,
            maternal_disorders = deaths_maternal_disorders_sex_both_age_all_ages_number,
            hiv_aids =deaths_hiv_aids_sex_both_age_all_ages_number,
            drug_use_disorders = deaths_drug_use_disorders_sex_both_age_all_ages_number,
            tuberculosis = deaths_tuberculosis_sex_both_age_all_ages_number,
            cardiovascular_diseases = deaths_cardiovascular_diseases_sex_both_age_all_ages_number,
            lower_respiratory_infections = deaths_lower_respiratory_infections_sex_both_age_all_ages_number,
            neonatal_disorders = deaths_neonatal_disorders_sex_both_age_all_ages_number,
            alcohol_use_disorders = deaths_alcohol_use_disorders_sex_both_age_all_ages_number, self_harm =
              deaths_self_harm_sex_both_age_all_ages_number,
            exposure_to_forces_of_nature = deaths_exposure_to_forces_of_nature_sex_both_age_all_ages_number,
            diarrheal_diseases = deaths_diarrheal_diseases_sex_both_age_all_ages_number,
            environmental_heat_and_cold_exposure = deaths_environmental_heat_and_cold_exposure_sex_both_age_all_ages_number,
            Cancers = deaths_neoplasms_sex_both_age_all_ages_number,
            conflict_and_terrorism = deaths_conflict_and_terrorism_sex_both_age_all_ages_number,
            diabetes_mellitus = deaths_diabetes_mellitus_sex_both_age_all_ages_number,
            chronic_kidney_disease = deaths_chronic_kidney_disease_sex_both_age_all_ages_number, poisonings =
              deaths_poisonings_sex_both_age_all_ages_number,
            protein_energy_malnutrition = deaths_protein_energy_malnutrition_sex_both_age_all_ages_number,
            road_injuries = deaths_road_injuries_sex_both_age_all_ages_number,
            chronic_respiratory_diseases = deaths_chronic_respiratory_diseases_sex_both_age_all_ages_number,
            cirrhosis_and_other_chronic_liver_diseases = deaths_cirrhosis_and_other_chronic_liver_diseases_sex_both_age_all_ages_number,
            digestive_diseases = deaths_digestive_diseases_sex_both_age_all_ages_number,
            fire_heat_and_hot_substances = deaths_fire_heat_and_hot_substances_sex_both_age_all_ages_number,
            acute_hepatitis = deaths_acute_hepatitis_sex_both_age_all_ages_number,
            measles = deaths_measles_sex_both_age_all_ages_number
    )
  
  # glimpse(data)
  
  cleaned_data <- data |>
    select(country, year, meningitis:last_col())|>
    pivot_longer( cols = meningitis:last_col(),
                  names_to = "causes_of_death",
                  values_to = "deaths" )
  
  # missing_count <- sum(is.na(cleaned_data))
  # missing_count
  
  
  # cleaned_data_new <- cleaned_data |>
  # filter(year == 2019 & country == "World") |>
  # arrange(desc(deaths)) |>
  # mutate(causes_of_death = factor(causes_of_death,
  #                                 levels = rev(unique(causes_of_death))))
  
  # glimpse(cleaned_data_new)
  # head(cleaned_data_new)
  # print(cleaned_data_new,n = Inf)
  
  return(cleaned_data)  
}


fun_DataFile_02 <-function()
{
  # Data File : 02_total-cancer-deaths-by-type ---------------------------------------------------------------------------------------------------------------------------------
  
  data <- read_csv(here("data/02_total-cancer-deaths-by-type.csv"))
  # glimpse(data)
  
  # Data Wrangling
  
  data <- data |>
    clean_names() |>
    rename( country = entity,
            liver_cancer = deaths_liver_cancer_sex_both_age_all_ages_number,
            kidney_cancer = deaths_kidney_cancer_sex_both_age_all_ages_number
    )
  
  # glimpse(data)
  
  cleaned_data <- data |>
    select(country, year, liver_cancer:last_col())|>
    pivot_longer( cols = liver_cancer:last_col(),
                  names_to = "cancertype",
                  values_to = "deaths" )
  
  cleaned_data <- cleaned_data %>%
    mutate(cancertype = str_replace(cancertype, "_sex_both_age_all_ages_number", ""))
  cleaned_data <- cleaned_data %>%
    mutate(cancertype = str_replace(cancertype, "deaths_", ""))
  cleaned_data <- cleaned_data %>%
    mutate(cancertype = str_replace(cancertype, "_", " "))
  cleaned_data <- cleaned_data %>%
    mutate(cancertype = str_to_title(cancertype))
  
  return(cleaned_data)  
}




fun_DataFile_03 <-function()
{
library(rnaturalearthdata)
library(rnaturalearth)
library(ggplot2)
library(sf)
library(dplyr)
library(readr)
library(tidyverse)


  data <- read_csv("data/05_share-of-population-with-cancer-crude.csv")
  
  filtered_data <- data |>
    filter(Year == 2000) |>
    select(Entity, Year, Deaths_Neoplasms = 'Current number of cases of neoplasms per 100 people, in both sexes aged all ages') %>%
    mutate(Deaths_Neoplasms = round(Deaths_Neoplasms, 3))  
  world <- ne_countries(scale = "medium", returnclass = "sf")
  
  filtered_data <- filtered_data %>%
    mutate(Entity = if_else(Entity == "Democratic Republic of Congo", "Democratic Republic of the Congo", Entity),
           Entity = if_else(Entity == "Congo", "Republic of the Congo", Entity),
           Entity = if_else(Entity == "Russia", "Russian Federation", Entity))
  
  world_data <- merge(world, filtered_data , by.x = "name_long", by.y = "Entity")



return(world_data)
}

fun_DataFile_04 <- function() {
  data <- read_csv("data/04_cancer-death-rates-by-age.csv") %>%
    rename(
      country = Entity,
      year = Year,
      `70+ years` = `Deaths - Neoplasms - Sex: Both - Age: 70+ years (Rate)`,
      `50-69 years` = `Deaths - Neoplasms - Sex: Both - Age: 50-69 years (Rate)`,
      `15-49 years` = `Deaths - Neoplasms - Sex: Both - Age: 15-49 years (Rate)`,
      `5-14 years` = `Deaths - Neoplasms - Sex: Both - Age: 5-14 years (Rate)`,
      `Under 5 years` = `Deaths - Neoplasms - Sex: Both - Age: Under 5 (Rate)`  # Adjusted to match the exact column name from your dataset
    ) %>%
    select(country, year, `70+ years`, `50-69 years`, `15-49 years`, `5-14 years`, `Under 5 years`)
  
  return(data)
}
