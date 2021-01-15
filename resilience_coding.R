####10.5.5 Resilience Storymapping Project: Pilot Script####
#Molly F Jenkins 
#01/11/2021

#This script takes a database of EPIC-N projects and systematically categorizes them 
#according to an EPA-developed resilience indicator framework, 
#based on the Rockefeller City Resilience Index and other resilience indices.


####Pre-requisites: set working directory, install tidy library, and read in files####

install.packages("tidyverse")
library(tidyverse)
setwd("C:/Users/molly/OneDrive/Desktop")

case_studies <- read.csv("addresses_raw.txt") #database file
resilience_keywords <- read.csv("resilience+indicators.csv") #resilience keyword file 

#take database, comb for matches with each resilience search term, 
#assigning an indicator and domain category to each match

#I think this is the best right track right now? no loop necessary? 
mini_data = case_studies %>% 
  filter(ProjectName %in% resilience_keywords$suggestedsupplementalsearchterms |
           ProjectAbstract %in% resilience_keywords$suggestedsupplementalsearchterms | 
           ProjectDescript %in% resilience_keywords$suggestedsupplementalsearchterms) %>% 
  mutate(category == resilience_keywords$searchcategory) %>% 
  select(everything)

mini_data

######code snippets####




cases = case_studies$projectName 
for(c in cases) { 
}
#bas
tidy_epicn = case_studies %>% 
  select(contains())

df_loop2 = df_loop %>%
  group_by(stakeholder) %>%
  mutate(key_word = case_when(
    EPA_indicator == EPA_indictor ~ suggested_supplemental_search_terms,
    stakeholder == "council_mayor" ~ c("council", "mayor", "municipal"),
    TRUE ~ "everything_else"))
