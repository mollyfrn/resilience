####10.5.5 Community Resilience Project: Pilot Script####
#Molly F Jenkins 
#01/11/2021

#This script takes a database of EPIC-N projects and systematically categorizes them 
#according to an EPA-developed resilience indicator framework, 
#based on the Rockefeller City Resilience Index and other resilience indices.


####Pre-requisites: set working directory, install tidy library, and read in files####
#rqda
#install.packages("tidyverse")
library(tidyverse)
library(tidytext)
library(igraph)
library(ggraph)
library(tm)
library(wordcloud)
library(SnowballC)
library(widyr)
library(ggplot2)
setwd("C:/Users/mjenkins/OneDrive - Environmental Protection Agency (EPA)/Analyses/Resilience_EpicN")

case_studies <- read.csv("EPICN4ORD_rawdata07_14_22.csv") #database file
#make case_studies a subset with the two example cities  
#case_studies2
#examplecities = c('Ramsey City')
#case_studies = case_studies2 %>% 
  #filter(City %in% examplecities)


#read in resilience keyword file 
#that maps specific words to their corresponding indicators in resilience framework
res_tidy <- read.csv("resilience_keywords_02_23_23.csv")

#take database, comb for matches with each resilience search term, 
#assigning an indicator and domain category to each match

####for loop assigning categories to case studies based on keyword criteria match####
categories = factor(unique(res_tidy$BroadCategories))
#adding layer of niche frequencies
df_final = data.frame(category = NULL, niche = NULL, keyword = NULL, name = NULL, abstract = NULL) #, sdgs = NULL)
#test: c = 'Economy & Society' 
#test: n = 'Collective ID & Cohesion'

for(c in categories){ #for each category
  res_one = res_tidy %>% #I want to subset the input keyword data
    filter(BroadCategories == c) #to just those encompassed by category c 
    niches = res_one$NicheCategories #and define niches as those contained within category c
  for(n in niches){ #for each niche
    res_mini = res_one %>% #I want to further subset the input keyword data
      filter(NicheCategories == n) #to just those encompassed by niche n 
    searchterms = res_mini$SearchTerm #and define search terms as those contained within niche n
  for(s in searchterms){ #for each search term 

    abstract_match = str_subset(as.character(case_studies$Project.Abstract), #find if search term occurs in text string
                                regex(s, ignore_case = TRUE)) #in abstract or project name
    name_match = str_subset(as.character(case_studies$Project.Name), 
                            regex(s, ignore_case = TRUE))
    
    matched_categories = #give me a subset dataframe with the case studies
      case_studies %>% #where the project name or abstract match with the search results above
      filter(Project.Name %in% name_match | Project.Abstract %in% abstract_match) %>%
      mutate(category = c, #include the broader resilience category as well as the search term
          niche = n,
          keyword = s, 
          name = Project.Name, 
          abstract = Project.Abstract) %>% #and the project name and abstract in question
  select(category, niche, keyword, name, abstract) %>% #so that we can explore these in greater depth
      distinct() #and will allow us to retroactively count word frequency within texts
    print(n) 
    
  df_final = rbind(matched_categories, df_final) #bind all of these rows together
  }
}}

df_final2 = df_final %>%
  distinct() 

write.csv(df_final2, "Cases_EPICN_Josekeywords_cleaned02_23_23.csv", row.names = FALSE)
#write this new data frame to a csv file so forloop doesn't need to be run every time