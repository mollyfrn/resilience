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
resilience_keywords <- read.csv("ResilienceKeywords_tidy.csv") #resilience keyword file 

#take database, comb for matches with each resilience search term, 
#assigning an indicator and domain category to each match

#need to make keyword search categories, supplemental search terms, project Names searchable strings 
#may need to use str_subset and regex, gsub replace "," with "|"

#actually what I should do is stretch the dataset so each keyword has its own row 
#separate_rows(table3, rate, sep = "/")

res_tidy = separate_rows(resilience_keywords, search.terms, sep = ",")

write.csv(res_tidy, "res_tidy_search_as_rows.csv", row.names = FALSE)

#think about adding just "collaboration" to keywords and then use NVivo 
#simplifying terms to encompass what people actually use e.g. "trust" and not just "institutional trust" 

####for loop assigning categories to case studies based on keyword criteria match####
mini_data = case_studies %>% 
  slice(10:20) #test subset of case studies for code dev
names(mini_data)

categories = res_tidy$EPA.Resilience.categories
df_final = data.frame(category = NULL, keyword = NULL, name = NULL, abstract = NULL)

for(c in categories){
  res_mini = res_tidy %>% 
    filter(EPA.Resilience.categories == c)
  searchterms = res_mini$search.terms
  
  for(s in searchterms){

    abstract_match = str_subset(as.character(case_studies$Project.Abstract), regex(s, ignore_case = TRUE))
    name_match = str_subset(as.character(case_studies$Project.Name), regex(s, ignore_case = TRUE))

    matched_categories = 
      case_studies %>% 
      filter(Project.Name %in% name_match | Project.Abstract %in% abstract_match) %>%
      mutate(category = c, 
          keyword = s, 
          name = Project.Name, 
          abstract = Project.Abstract) %>% 
  select(category, keyword, name, abstract)
    
  #probably need to add line that pads with NA's for rows where no criteria are matched 
  #probably also need to add line that tells R to parse the character content since it also contains | segments
  
  df_final = rbind(matched_categories, df_final)
  }
}

write.csv(df_final, "Categorized_case_studies_EPICN.csv", row.names = FALSE)

#code works but need to account for * operator for search terms to optimize search 
#need to make sure it's KEEPING everything from each iteration 

####NVivo Data Prep####
#Tidy case studies csv database to sep and save each project entry as a unique txt file so optimized for NVivo
library(tidyverse)
setwd("C:/Users/mjenkins/OneDrive - Environmental Protection Agency (EPA)/Analyses/Resilience_EpicN")

case_studies <- read.csv("EPICN4ORD_rawdata.csv") #database file
tidy_case = case_studies %>%
  select(Project.., Project.Name, Project.Abstract)

cases = tidy_case$Project..
for (c in cases) {
  mini_case = tidy_case %>% 
    filter(Project.. == c)
  write.table(mini_case, paste("mini_case", c, ".txt", sep = ""))
}

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