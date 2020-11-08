


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
 minmedmaxbillnewpenguins2 <- palmerpenguins::penguins %>%
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
   tibble::as.tibble() 

   
powys_complete_covid_data %>%
   knitr::kable()
 
 
 ## note: Replace the remaining NA values (those that don’t have a previous value available) with zero

# # Question 3.3 ----------------------------------------------------------

 
 
 library(lubridate)
 
 powys_day_before <- powys_complete_covid_data %>%
   dplyr::mutate(day_before = ymd(specimen_date) - 1) %>%
   dplyr::select(-specimen_date, -cumCasesBySpecimenDate) %>%
   dplyr::rename("newCases_day_before" = "newCasesBySpecimenDate") %>%
   tibble::as.tibble()
 
 powys_day_before %>%
   knitr::kable()
 
 
 # full join verb
 powys_covid_development <- 
   dplyr::inner_join(
   # left table
   powys_complete_covid_data,
   # right table
   powys_day_before,
   # columns to match
   by = c( "specimen_date" ="day_before")
 ) %>%
   # reorder columns for easier reading, drop the -cumCasesBySpecimenDate colum.
   dplyr::select(specimen_date, newCases_day_before, newCasesBySpecimenDate, -cumCasesBySpecimenDate) %>%
   dplyr::mutate(
     percentage = (newCasesBySpecimenDate/ newCases_day_before)*100
     ) %>%
   tibble::as.tibble()

 powys_covid_development %>%
   knitr::kable(digits = 2)
  
## tibbles or kables?
   
   
# Question 3.4  -----------------------------------------------------------


# The number of new cases starts to rise in the second week of March but quickly lowers back to 0
  # after 4 days. 
# An increase can be observed later in March, this time with sustained percentage changes
  # often between 100 - 200%. 
# The lower figures that interrupt this sustained change tend to be weekend recordings.
# The number of new cases continues at a high rate into May, 
# reaching a percentage change high of 800% on May 10th. 
# From here, the number of new cases declines. 
# In June, July, and the first week of August there are very few new cases, 
  # although there is a notable spike of 10 new cases on July 20th. 
# New cases begin to climb again from the 2nd half of August, 
  # with percentage change often equalling between 100 - 300%.
# The figures remain similar to this through to October 9th, where this data ends.   
 
 

#  Question 4 -------------------------------------------------------------

# Load
 
lad19 <- readr::read_csv("lad19_population.csv") 
 
 

# Join

 lad19 <- readr::read_csv("lad19_population.csv") 
 
 
 lad19_covid19 <- 
   dplyr::full_join(
     lad19,
     covid_data,
     by = c("lad19_area_name" = "area_name")
   ) 
 
 
 
 # 7 Day Average Analysis
 
wide_covid <- lad19_covid19 %>%
  select(-cumCasesBySpecimenDate, -area_population, -lad19_area_code) %>%
   dplyr::mutate(
     lad19_area_name = str_replace_all(lad19_area_name, " ", "_")
   ) %>%
  filter(lad19_area_name == "Powys"|lad19_area_name == "Wychavon"| lad19_area_name == "Broadland"| 
           lad19_area_name == "Exeter" | lad19_area_name == "Epping_Forest") %>%
  pivot_wider(
  names_from = lad19_area_name,
  values_from = newCasesBySpecimenDate
  ) %>%
   dplyr::filter(
     !is.na(Broadland),
     !is.na(Powys),
     !is.na(Wychavon),
     !is.na(Exeter),
     !is.na(Epping_Forest)
     )%>%
   mutate(
     week_commencing = floor_date(specimen_date, unit = "weeks", week_start = getOption("lubridate.week.start", 1))
   ) %>%
   group_by(week_commencing) %>%
   
   # 7 Day Average of New Cases
   
   summarise(Powys = mean(Powys), Broadland = mean(Broadland), Wychavon = mean(Wychavon),
             Exeter = mean(Exeter), Epping_Forest = mean(Epping_Forest)) %>%
  tibble::as.tibble()
 


wide_covid %>%
  knitr::kable(digits = 2)


# Table will illustrate a comparison between Powys and 4 other regions that have similar populations.
# To understand the general trends and mitigate the weekend effect a 7 day average of new cases has been 
# calculated for each week. Broadland spike. Epping Forest spike.
# High finish. 



# Notes

lad19_covid19 <- 
  dplyr::full_join(
    lad19,
    covid_data,
    by = c("lad19_area_name" = "area_name")
  ) %>%
  dplyr::filter(lad19_area_name == "Powys"| lad19_area_name == "Angus")  %>%
  dplyr::select(specimen_date, -area_population, newCasesBySpecimenDate, -cumCasesBySpecimenDate, 
                lad19_area_name, -lad19_area_code) %>%
  group_by(week_number = format(specimen_date, "%W")) %>%
  summarise("New_cases_(7_day_average)" = mean(newCasesBySpecimenDate)) %>%
  
  tibble::as_tibble()


lad19_covid19 %>%
  knitr::kable (digits = 2) 


 #ISO week 
 
 # Breakdown into week summaries
 # Compare with other areas with similar population sizes
 # Can I introduce new data?
 # print width?
 # small population table?
 # possibly use pivot?
 # include densities and area?
 # wales lockdown?
 # isoweek?
 # find a way of filling na values with 0 instead of filtering them out
 
# d illustrate the development of cases over time, compared to the population, or it could
 # illustrate a comparison with other areas in the region.
 
 
 # Include a short text (max 250 words) providing a short description of the analysis and interpretation of the results.
 