#Sdg keywords 
#based on the resilience code 

####Pre-requisites: set working directory, install tidy library, and read in files####

install.packages("tidyverse")
library(tidyverse)
setwd("C:/Users/mjenkins/OneDrive - Environmental Protection Agency (EPA)/Analyses/Resilience_EpicN")

case_studies <- read.csv("EPICN4ORD_rawdata.csv") #database file
sdg_keywords <- read.csv("testdata/EPICNSDG_test.csv", header = TRUE) #sdg keyword file 

#take database, comb for matches with each resilience search term, 
#assigning an indicator and domain category to each match


#think about adding just "collaboration" to keywords and then use NVivo 
#simplifying terms to encompass what people actually use e.g. "trust" and not just "institutional trust" 

####for loop assigning categories to case studies based on keyword criteria match####
mini_data = case_studies %>% 
  slice(10:20) #test subset of case studies for code dev
names(mini_data)

categories = unique(sdg_keywords$Sustainable.Development.Goals.for.Target.Alignment)
df_final = data.frame(category = NULL, keyword = NULL, secndkey = NULL, name = NULL, abstract = NULL)


for(c in categories){
  res_mini = sdg_keywords %>% 
    filter(Sustainable.Development.Goals.for.Target.Alignment == c)
  searchterms = res_mini$Indicators.Keywords
  
  for(s in res_mini$Indicators.Keywords){ 
    for(k in res_mini$AND.KEYWORD.1){
      if(k !is.na(k)){
  
        abstract_match1 = str_subset(as.character(case_studies$Project.Abstract), regex(s, ignore_case = TRUE)) #use str_match in new iteration
        name_match1 = str_subset(as.character(case_studies$Project.Name), regex(s, ignore_case = TRUE))
        abstract_match2 =
        name_match2 
        
        matched_categories =
          case_studies %>% 
          filter(Project.Name %in% name_match1 & Project.Name %in% name_match2 |
                 Project.Abstract %in% abstract_match1 & Project.Abstract %in% abstract_match2) %>%
          mutate(category = c, 
                 keyword = s, 
                 secndkey = k,
                 name = Project.Name, 
                 abstract = Project.Abstract) %>% 
          select(category, keyword, secndkey, name, abstract)
      } else{
        
        abstract_match1 = str_subset(as.character(case_studies$Project.Abstract), regex(s, ignore_case = TRUE)) #use str_match in new iteration
        name_match1 = str_subset(as.character(case_studies$Project.Name), regex(s, ignore_case = TRUE))
        
        matched_categories = 
          case_studies %>% 
          filter(Project.Name %in% name_match1 |
                   Project.Abstract %in% abstract_match1) %>% 
          mutate(category = c, 
                keyword = s, 
                secndkey = NA,
                name = Project.Name, 
                abstract = Project.Abstract) %>% 
          select(category, keyword, secndkey, name, abstract)
      }
      df_final = rbind(matched_categories, df_final) 
    }
  }
}


###### #may want to do a IF k != NA then, else version of this where k is printed as NA
    
   
    

write.csv(df_final, "SDG_case_studies_EPICN_test.csv", row.names = FALSE)

####Join with all data for Marshall####
case_studies = rename(case_studies, name = Project.Name)
df_full = df_final %>% 
  left_join(case_studies, by = "name")

write.csv(df_full, "EPICcases_sdgs.csv")

#code works but need to account for * operator for search terms to optimize search 
#need to make sure it's KEEPING everything from each iteration 

#add a pipe | between SDG goals when a project gets hits of more than one, instead of sep rows - each project keeps a single row (?) 