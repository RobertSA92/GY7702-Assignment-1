


# Question 1 --------------------------------------------------------------

# Question 1


survey_answers <- c(NA, 3, 4, 4, 5, 2, 4, NA, 6, 3, 5, 4, 0, 5, 7, 5, NA, 5, 2, 4, NA, 3, 3, 5, NA)


# 1 = completely disagree
# 2 = disagree
# 3 = somehow disagree
# 4 = neither agree nor disagree
# 5 = somehow agree
# 6 = agree
# 7 = completely agree
# NA = missing value


# Question 1.1 ------------------------------------------------------------


# Question 1.1: 
  
library(tidyverse)
library(knitr)

# Remove NA values from vector
cleaned_survey_answers <- survey_answers[!is.na(survey_answers)]

# Check if all survey answers completely disagree
    all(cleaned_survey_answers == "1")
    
# Check if all survey answers compleely agree
    all(cleaned_survey_answers == "7")
  
    

# Question 1.2 ------------------------------------------------------------

#
# Question 1.2: 
    

# Coerce? data into logical values, any values 5 or greater are TRUE, any values 4 or lower are FALSE
logical_survey_answers <- cleaned_survey_answers >= 5
    
 logical_original_survey_answers <- survey_answers >= 5
    

 cleaned_survey_answers
 
# Find the indexes that are TRUE 

  which(logical_original_survey_answers %in% TRUE)
    
 
## possibly use: cleaner_survey_answers <- as.numeric(cleaned_survey_answers)
  
  
  

# Question 2  -------------------------------------------------------------


# Question 2.1: -----------------------------------------------------------

# Install the library library palmerpenguins
  
  
  install.packages("palmerpenguins")
  library(palmerpenguins)
  
  

# Question 2.2: -----------------------------------------------------------


 penguins <- palmerpenguins::penguins
  raw_penguins <- palmerpenguins::penguins_raw

 newpenguins <- palmerpenguins::penguins %>%
    dplyr::select(species, island, bill_length_mm, body_mass_g) %>%
    dplyr::filter(species == "Gentoo") %>%
    dplyr::filter(!is.na(bill_length_mm)) %>%
    dplyr::arrange(desc(body_mass_g)) %>%
   dplyr::slice_head(n = 10) %>%
   knitr::kable() %>%
   print()

 
# remember to change identifier

#  Question 2.3 -----------------------------------------------------------

 averagebillnewpenguins <- palmerpenguins::penguins %>%
   dplyr::select(island, bill_length_mm, body_mass_g) %>%
   dplyr::filter(!is.na(bill_length_mm)) %>%
   dplyr::arrange(desc(bill_length_mm)) %>%
   dplyr::group_by(island) %>%
   dplyr::summarise(
     mean(bill_length_mm)
   ) %>%
   knitr::kable() %>%
   print()

# remember to add ordered by average bill length
# check body mass
 
 

# Question 2.4 ------------------------------------------------------------

 
 # doesn't work yet
 minmedmaxbillnewpenguins <- palmerpenguins::penguins %>%
   dplyr::select(species, island, bill_length_mm, bill_depth_mm) %>%
   dplyr::filter(!is.na(bill_length_mm)) %>%
   dplyr::group_by(species) %>%
   dplyr::mutate(
   proportion_between_bl_and_bd = (((bill_length_mm - bill_depth_mm)/bill_length_mm)*100),
   
   min_proportion = min(proportion_between_bl_and_bd),
   
   median_proportion = median(proportion_between_bl_and_bd),
   
   max_proportion = max(proportion_between_bl_and_bd)
  
   ) %>% 

   knitr::kable() %>%
   print()
   

# Works, but is ugly 
 minmedmaxbillnewpenguins <- palmerpenguins::penguins %>%
   dplyr::select(species, island, bill_length_mm, bill_depth_mm) %>%
   dplyr::filter(!is.na(bill_length_mm)) %>%
   dplyr::group_by(species) %>%
   dplyr::mutate(
     proportion_between_bl_and_bd = (((bill_length_mm - bill_depth_mm)/bill_length_mm)*100),
     
     min_proportion = round(min(proportion_between_bl_and_bd), 2),
     
     median_proportion = round(median(proportion_between_bl_and_bd), 2),
     
     max_proportion = round(max(proportion_between_bl_and_bd), 2)
     
   ) %>% 
   dplyr::select(-island, -proportion_between_bl_and_bd, -bill_depth_mm, -bill_length_mm) %>%
   dplyr::summarise(across(everything(), list(mean))) %>%
   dplyr::mutate(
     Species = species,
     Minimum = min_proportion_1,
     Median = median_proportion_1,
     Max = max_proportion_1
   ) %>%
   
   select(-species, -min_proportion_1, - median_proportion_1, - max_proportion_1) %>%
   knitr::kable() %>%
   print()

 
# perhaps tibble

# Question 3 --------------------------------------------------------------


# Question 3.1: -----------------------------------------------------------


 covid_data <- readr::read_csv("covid19_cases_20200301_20201017.csv")

 

# Question 3.2 ------------------------------------------------------------


powys_complete_covid_data <- covid_data %>%
   fill(newCasesBySpecimenDate) %>%
   dplyr::filter(!is.na(newCasesBySpecimenDate)) %>%
   dplyr::filter(area_name == "Powys") %>%
   dplyr::select(-area_name) %>%
   knitr::kable() %>% 
   print()
 
 

# # Question 3.3 ----------------------------------------------------------

 powys_day_before <- powys_complete_covid_data
 
 powys_day_before %>%
lubridate::as_date(day_before = specimen_data) %>%
 knitr::kable()
 
 select(-specimen_data, -cumCasesBySpecimenDate) %>%
 knitr::kable()
 
 

# Question 3.4  -----------------------------------------------------------

#  Write a short text (max 150 words) describing the development of new cases in the area
 # over time, as evidenced by the table [area]_covid_development.
 
 