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

#test with dummy variable keywords and subset of full case study dataset, go line by line 
#mini_data = case study subset 
#line 16 of keyword df is the first to include additional and/conditional keywords 
# key_mini = sdg_keywords[16,]
# c = "01 No Poverty"
# s = "disaster*"
# k = "economic*" #dummy testing seems to work - picked out both/and when k was present in test

for(c in categories){
  res_mini = sdg_keywords %>% #key_mini %>%
    filter(Sustainable.Development.Goals.for.Target.Alignment == c)
  searchterms = res_mini$Indicators.Keywords
  
  for(s in searchterms){ 
    res_k = res_mini %>% 
      filter(Indicators.Keywords %in% s)
    for(k in res_k$AND.KEYWORD.1){
      if(!is.na(k)){
  
        abstract_match1 = str_subset(as.character(case_studies$Project.Abstract), regex(s, ignore_case = TRUE)) #use str_match in new iteration
        name_match1 = str_subset(as.character(case_studies$Project.Name), regex(s, ignore_case = TRUE))
        abstract_match2 = str_subset(as.character(case_studies$Project.Abstract), regex(k, ignore_case = TRUE))
        name_match2 = str_subset(as.character(case_studies$Project.Name), regex(k, ignore_case = TRUE))
        
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
                secndkey = 'NA',
                name = Project.Name, 
                abstract = Project.Abstract) %>% 
          select(category, keyword, secndkey, name, abstract)
      }
      df_final = rbind(matched_categories, df_final) 
    }
  }
}
#write.csv(df_final, "dummy_output_testingSDGS.csv", row.names = FALSE) 

###### #may want to do a IF k != NA then, else version of this where k is printed as NA
    
   
    

write.csv(df_final, "SDG_case_studies_EPICN_test.csv", row.names = FALSE)

####Reshape with spread and piping operators####
#Summarize all of the keywords identified into one column, separated by piping operators 
#group by project name 
df_reshaped = df_final %>% 
  group_by(name) %>%
  transmute(keywords = paste(keyword, secndkey, sep = "|"),
            name = name, 
            abstract = abstract,
            category = category) 

df_reshaped$name = as.factor(df_reshaped$name)


df_condensed = df_reshaped %>%
  group_by(name, abstract, category) %>% 
  transmute(key = str_c(keywords, collapse = "|"),
            name = name,
            abstract = abstract, 
            category = category) %>%
  distinct()

#need to make it so one project can have multiple categories at the same time

df_reshaped$objectId = seq(1:length(df_reshaped$name)) 
df_reshaped2 = df_reshaped %>%
  group_by(name)%>%
  summarise(keys = paste(keywords, sep = "|"),
                         name = name,
                         abstract = abstract,
                         category = category, 
                         objectId = objectId)

#try making additional column with "sdg1 sd2....sdg17" to use w/category

df_spread = df_reshaped %>%
  group_by(category) %>%
  mutate(sdgs = paste('sdg', category, sep = " ")) %>%
  spread(category, sdgs) #right now instead of keeping NAME column, 

#spread all of the categories into unique columns with a boolean true false cell content
#need to deduplicate, condense to unique project names, 

df_condensed = df_spread %>% 
  group_by(name) %>% 
  mutate(keys = paste(keywords, sep = "|")) #%>% 
  rename()
####Join with all data for Marshall####
case_studies = rename(case_studies, name = Project.Name)
df_full = df_final %>% 
  left_join(case_studies, by = "name")

write.csv(df_full, "EPICcases_sdgs.csv")

#code works but need to account for * operator for search terms to optimize search 
#need to make sure it's KEEPING everything from each iteration 

#add a pipe | between SDG goals when a project gets hits of more than one, instead of sep rows - each project keeps a single row (?) 