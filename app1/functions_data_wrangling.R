library(tidyverse) 
library(janitor)
library(scales)
library(dplyr)

## Functions - Data Wrangling

to_camel_case <- function(text) {
  # Replace all underscores with blanks
  text <- gsub("_", " ", text)
  # Convert to camel case
  gsub("\\b(\\w)", "\\U\\1", text, perl = TRUE)
}

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
  
  
 
  
  cleaned_data <- cleaned_data %>%
    mutate(causes_of_death = to_camel_case(str_replace(causes_of_death, "_", " ")))
  
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
            kidney_cancer = deaths_kidney_cancer_sex_both_age_all_ages_number,
            lip_Oral_Cavity_cancer = deaths_lip_and_oral_cavity_cancer_sex_both_age_all_ages_number,
            lung_cancer = deaths_tracheal_bronchus_and_lung_cancer_sex_both_age_all_ages_number,	
            larynx_cancer = deaths_larynx_cancer_sex_both_age_all_ages_number,	
            billary_tract_cancer = deaths_gallbladder_and_biliary_tract_cancer_sex_both_age_all_ages_number,	
            skin_cancer = deaths_malignant_skin_melanoma_sex_both_age_all_ages_number,
            leukemia = deaths_leukemia_sex_both_age_all_ages_number,	
            hodgkin_lymphoma = deaths_hodgkin_lymphoma_sex_both_age_all_ages_number,	
            multiple_myeloma = deaths_multiple_myeloma_sex_both_age_all_ages_number,	
            other_neoplasms = deaths_other_neoplasms_sex_both_age_all_ages_number,	
            breast_cancer = deaths_breast_cancer_sex_both_age_all_ages_number,	
            prostate_cancer = deaths_prostate_cancer_sex_both_age_all_ages_number,	
            thyroid_cancer = deaths_thyroid_cancer_sex_both_age_all_ages_number,	
            stomach_cancer = deaths_stomach_cancer_sex_both_age_all_ages_number,	
            bladder_cancer = deaths_bladder_cancer_sex_both_age_all_ages_number,	
            uterine_cancer = deaths_uterine_cancer_sex_both_age_all_ages_number,	
            ovarian_cancer = deaths_ovarian_cancer_sex_both_age_all_ages_number,	
            cervical_cancer = deaths_cervical_cancer_sex_both_age_all_ages_number,
            brain_cancer = deaths_brain_and_central_nervous_system_cancer_sex_both_age_all_ages_number,	
            non_hodgkin_lymphoma = deaths_non_hodgkin_lymphoma_sex_both_age_all_ages_number,
            pancreatic_cancer = deaths_pancreatic_cancer_sex_both_age_all_ages_number,	
            esophageal_cancer = deaths_esophageal_cancer_sex_both_age_all_ages_number,	
            testicular_cancer = deaths_testicular_cancer_sex_both_age_all_ages_number,	
            nasopharynx_cancer = deaths_nasopharynx_cancer_sex_both_age_all_ages_number,	
            other_pharynx_cancer = deaths_other_pharynx_cancer_sex_both_age_all_ages_number,
            colon_and_rectum_cancer = deaths_colon_and_rectum_cancer_sex_both_age_all_ages_number,
            non_melanoma_skin_cancer = deaths_non_melanoma_skin_cancer_sex_both_age_all_ages_number,
            mesothelioma = deaths_mesothelioma_sex_both_age_all_ages_number
    )
  
  cleaned_data <- data |>
    select(country, year, liver_cancer:last_col())|>
    pivot_longer( cols = liver_cancer:last_col(),
                  names_to = "cancertype",
                  values_to = "deaths" )
  cleaned_data <- cleaned_data %>%
    mutate(cancertype = to_camel_case(str_replace(cancertype, "_", " ")))
  
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


fun_DataFile_05 <- function()
{
  
  # Load the data
  
  neoplasm_data <- read_csv("data/07_number-of-people-with-cancer-by-age.csv")
  
  # Filtering the data for world and pivoting the data
  
  filtered_data <- neoplasm_data %>%
    # filter(Entity == "World") %>%
    select(Entity, Year, ends_with("(Number)")) %>%
    pivot_longer(
      cols = -c(Entity, Year),  # Include the Entity column
      names_to = "age_group",
      values_to = "prevalence",
      names_pattern = "Prevalence - Neoplasms - Sex: Both - Age: (.*) \\(Number\\)"
    )
  
  filtered_data <- filtered_data |>
    mutate(country = Entity,
           year = Year
    )  
  
  
  
  return(neoplasm_data)
  
  
}