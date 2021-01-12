####10.5.5 Resilience Storymapping Project: Pilot Script####
#Molly F Jenkins 
#01/11/2020 

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
cases = case_studies$projectName 
for(c in cases) { 
  }
#bas
tidy_epicn = case_studies %>% 
             select(contains())

######code snippets####

df_loop2 = df_loop %>%
  group_by(stakeholder) %>%
  mutate(key_word = case_when(
    stakeholder == "teach" ~ c("teaching", "education", "learning"),
    stakeholder == "plan" ~ c("planning", "assessment", "transportation"),
    stakeholder == "policy" ~ c("policy", "regulation", "compensatory"),
    stakeholder == "health" ~ c("health", "pollution", "exercise"),
    stakeholder == "water" ~ c("water", "wetland", "hydrologic"),
    stakeholder == "restore" ~ c("restoration", "mitigation", "apply"),
    stakeholder == "market" ~ c("market", "credit", "business"),
    stakeholder == "urban" ~ c("urban", "development", "infrastructure"),
    stakeholder == "ag" ~ c("ag", "rural", "farm"),
    stakeholder == "research" ~ c("analysis", "academic", "dissertation"),
    stakeholder == "GIS" ~ c("GIS", "mapping", "technology"),
    stakeholder == "parks" ~ c("facilities", "parks", "recreation"),
    stakeholder == "utilities" ~ c("utilities", "stormwater", "management"),
    stakeholder == "council_mayor" ~ c("council", "mayor", "municipal"),
    TRUE ~ "everything_else"))

  