####10.5.5 Resilience Storymapping Project: Pilot Script####
#Molly F Jenkins 
#01/11/2021

#This script takes a database of EPIC-N projects and systematically categorizes them 
#according to an EPA-developed resilience indicator framework, 
#based on the Rockefeller City Resilience Index and other resilience indices.


####Pre-requisites: set working directory, install tidy library, and read in files####

install.packages("tidyverse")
library(tidyverse)
setwd("C:/Users/mjenkins/OneDrive - Environmental Protection Agency (EPA)/Analyses/Resilience_EpicN")

case_studies <- read.csv("EPICN4ORD_rawdata.csv") #database file
resilience_keywords <- read.csv("EPICN_search_keywords_indicators_SDGs.csv") #resilience keyword file 

#take database, comb for matches with each resilience search term, 
#assigning an indicator and domain category to each match

mini_data = case_studies %>% 
  slice(10:20) #test subset of case studies for code dev

#need to make keyword search categories, supplemental search terms, project Names searchable strings 
#may need to use str_subset and regex, gsub replace "," with "|"
resilience_keywords$suggested.supplemental.search.terms = gsub(",", " |", resilience_keywords$suggested.supplemental.search.terms)

write.csv(resilience_keywords, "resilience_keywords_imd.csv", row.names = FALSE)
names(mini_data)
categories = resilience_keywords$Search.categories..refined..final.pass.

df_final = data.frame()

for(c in categories){
matched_categories = mini_data %>% 
  filter(resilience_keywords$Search.categories..refined..final.pass. %in% Project.Name |
           resilience_keywords$suggested.supplemental.search.terms %in% Project.Name |
           resilience_keywords$Search.categories..refined..final.pass. %in% Project.Abstract|
           resilience_keywords$suggested.supplemental.search.terms %in% Project.Abstract) %>% #add | (e.g. the OR operator when more columns about project abstracts added) 
  mutate(category = c) #%>% 
  #select(everything)
  #probably need to add line that pads with NA's for rows where no criteria are matched 
  #probably also need to add line that tells R to parse the character content since it also contains | segments
  
  df_final = rbind(matched_categories, df_final)

}

write.csv(df_final, "Categorized_case_studies_EPICN.csv", row.names = FALSE)


######code snippets####

#str_subset(, regex(a|b|c, ignore_case = TRUE)) <-function pattern I want 
#gov
dru_gov = str_subset(as.character(drupal$Email), alt("key1|key2|key3", ignore_case = TRUE)) #need to run sep times for sep column strings
dru_gov2 = str_subset(as.character(drupal$Affiliation), regex("government|govt", ignore_case = TRUE))
#check to see if worked 
dru_gov
dru_gov2

#filter data by group type 
drupal_gov = drupal %>% 
  filter(Email %in% dru_gov | Affiliation %in% dru_gov2) %>% 
  mutate(occupation_type = c("gov")) 


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