library(tidyverse)
options(tibble.print_max = Inf)

getwd()
background <- read_csv('data/background-clean.csv', show_col_types = FALSE)
interest <- read_csv('data/interest-clean.csv', show_col_types = FALSE)

nrow(background)
nrow(interest)
glimpse(background)
glimpse(interest)

#51 background obs, and 49 interest obs.
#Check the two people who did not consent to sharing interest
anti_join(background, interest, by = 'response_id') |> View()
interest |> filter(response_id == '1038' | response_id == '1046')

#inner_join() background and interest to only include the 49 who consented to both
joint <- inner_join(background, interest, by = 'response_id')

#check missingness just in case
joint |> 
  filter(if_any(everything(), is.na)) |> 
  View()
  
##Separate the comma separated values, courses, domains, and areas

#Courses
courses_one_hot <- joint |> 
  select(response_id, courses) |> 
  separate_longer_delim(cols = courses, delim = ',') |> 
  mutate(courses = str_trim(courses), values = 1) |> 
  distinct(response_id, courses, .keep_all = T) |> 
  pivot_wider(
    names_from = courses,
    values_from = values,
    values_fill = 0
  )

#Domains
domains_one_hot <- joint |> 
  select(response_id, domains) |> 
  separate_longer_delim(cols = domains, delim = ',') |> 
  mutate(domains = str_trim(domains), values = 1) |> 
  distinct(response_id, domains, .keep_all = T) |> 
  pivot_wider(
    names_from = domains,
    values_from = values,
    values_fill = 0
  )


#Areas

#Some values in areas have a "x, generally" argument which shouldn't
#be separated into two values. Ex: "Predictive modeling, generally"
joint |> 
  filter(str_detect(areas, ', generally')) |> 
  pull(areas)

#Replace as empty string
joint <- joint |>
  mutate(areas = str_replace_all(areas, fixed(', generally'), ''))

#One hot encode
areas_one_hot <- joint |> 
  select(response_id, areas) |> 
  separate_longer_delim(cols = areas, delim = ',') |> 
  mutate(areas = str_trim(areas), values = 1) |> 
  distinct(response_id, areas, .keep_all = T) |> 
  pivot_wider(
    names_from = areas,
    values_from = values,
    values_fill = 0
  )

##Join the three encoded columns and remove the old ones
background_interest_processed <- joint |> 
  full_join(courses_one_hot, by = 'response_id') |> 
  full_join(domains_one_hot, by = 'response_id') |> 
  full_join(areas_one_hot, by = 'response_id') |> 
  select(-courses, -domains, -areas)

## Save to data and clean environment
write_csv(background_interest_processed, 'data/background_interest_processed.csv')
rm(list=ls())