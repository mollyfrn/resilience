####Natural Language Processing of Control Community & Experimental Resilience Key Words####
#Molly F Jenkins
#07/13/2022

#This script details most of the methods used for Part 2 of 3 methods used to analyze community resilience.

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


####Experimental resilience key word rankings####
df = read.csv("Cases_EPICN_Josekeywords_cleaned07_14_2022.csv") 
df_projects = df %>% count(name)
udf_proj = unique(df_projects)

ucases = unique(case_studies$Project.Name)

df_programs = df %>% 
  left_join(case_studies, by = c("name" = "Project.Name") )
uprograms = unique(df_programs$EPIC.Program.Name)

df_hits = df %>% 
  count(keyword)

df_hitsniche = df %>% 
  count(niche) %>% 
  rename(nichecounts = n) %>%
  mutate(niche = factor(niche))

df_hitsbroad = df %>% 
  count(category)

write.csv(df_hits, "keyword_hits_dedup_07142022.csv")
write.csv(df_hitsniche, "keywordhits_niche_07142022.csv")
write.csv(df_hitsbroad, "keywordhits_broad_07142022.csv")

#look at tf-idf hit rankings for keywords too by project name 
df_hit2 = df %>% 
  count(name, keyword, sort = TRUE)

total_hit3 = df_hit2 %>%
  group_by(name) %>% 
  summarize(total = sum(n))

df_hit3 = left_join(df_hit2, total_hit3)
df_hit4 = df_hit3 %>%
  bind_tf_idf(keyword, name, n)

df_hit5 = df_hit4 %>% 
  dplyr::select(-total) %>% 
  arrange(desc(tf_idf))

write.csv(df_hit5, "experimental_keywordrankings.csv")
write.csv(df_hit3, "projects_experimentalfrequencies.csv")
#top projects ranked by keyword frequencies 
df_topprojects = df %>% 
  count(name, sort = TRUE)

####Control community key word rankings####
#bring in raw unprocessed data to look at frequency of root words, bigrams etc
#look at text organically and see what emerges  
case_words = case_studies %>% 
  unnest_tokens(word, Project.Abstract) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(Project.Name, word, sort = TRUE)

write.csv(case_words, "control_words_frequencies.csv")

case_words_topproj = case_studies %>% 
  unnest_tokens(word, Project.Abstract) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(Project.Name, sort = TRUE)
write.csv(case_words_topproj, "topprojectsasrankedbycontrolterms.csv")

#top words across projects, frequencies 
top_words = case_studies %>% 
  unnest_tokens(word, Project.Abstract) %>% 
  filter(!word %in% stop_words$word) %>% 
  count(word, sort = TRUE)
write.csv(top_words, "topwords_frequencies_control.csv")

total_words <- case_words %>% 
  group_by(Project.Name) %>% 
  summarize(total = sum(n))

case_words2 = left_join(case_words, total_words)
case_words3 = case_words2 %>%
  bind_tf_idf(word, Project.Name, n)

case_words4 = case_words3 %>% 
  dplyr::select(-total) %>% 
  arrange(desc(tf_idf))

write.csv(case_words4, "tf_idf_parsingofcasestudiestext.csv")

####Look at tf_idf x n output as compared with raw#### 
idf = read.csv("tf_idf_parsingofcasestudiestext.csv") #idf = case_words4


idf_ranked = idf %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:100)
write.csv(idf_ranked, "tf_idf_top100.csv")

#run in loop thru individual collections of cities 
#rejoin cities info by project name 
case_words4 = idf 
cases_wcities = case_words4 %>% 
  left_join(case_studies) %>% 
  dplyr::select(Project.Name, City, word, n, tf, idf, tf_idf)

selectedcommunities = read.csv("CitiestoHighlight.csv")
priorities_bycity = cases_wcities %>% 
  filter(City %in% selectedcommunities$City) %>% #about halves the data
  mutate(City = factor(City)) #need to just select like 4 cities to look at initially 

# citylist = c("Ramsey City", "Pflugerville", "Omaha", "Minneapolis")
# testcities = priorities_bycity %>% 
#   filter(City %in% citylist) 
#     
testcities %>%
  group_by(City) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>%
  mutate(word = reorder(word, tf_idf)) %>%  
  ggplot(aes(tf_idf, word, fill = City)) +
  geom_col(show.legend = FALSE) +
  labs(x = "tf-idf", y = NULL) +
  facet_wrap(~City, ncol = 2, scales = "free")



####tf_idf informed wordcloud forloop with clouds for each community####
#02/03 where I left off - need to polish and export wordclouds with city names in titles 
#also need to return to radar plots and scale if possible or index from 0-1, but that may be a follow up job for tomorrow
setwd("C:/Users/mjenkins/OneDrive - Environmental Protection Agency (EPA)/Analyses/Resilience_EpicN")
case_studies <- read.csv("EPICN4ORD_rawdata07_14_22.csv") #database file
res_updated <- read.csv("resilience_keywords_51921.csv")

res_updated = res_updated %>% 
  rename(BroadCategories = BroadCategories,
         search.terms = SearchTerm)

case_words = case_studies %>% 
  unnest_tokens(word, Project.Abstract) %>%
  mutate(word = str_extract(word, "[a-z']+"))%>%
  count(City, word, sort = TRUE)

total_words <- case_words %>% 
  group_by(City) %>% 
  summarize(total = sum(n))

case_words2 = left_join(case_words, total_words)
case_words3 = case_words2 %>%
  bind_tf_idf(word, City, n) #I want this by city 

case_words4 = case_words3 %>% 
  dplyr::select(-total) %>% 
  arrange(desc(tf_idf))%>% 
  filter(!word %in% stop_words$word)

write.csv(case_words4, "wordrankings_bycity.csv", row.names = FALSE)


df = read.csv("Cases_EPICN_Josekeywords.csv") 
#retain or re-add community name column; subset to just communities we are interested in 
data_full = read.csv("EPICN4ORD_rawdata.csv")
selectedcommunities = read.csv("CitiestoHighlight.csv") #want city names probs
case_words4 = idf
idf = case_words4
case_studies <- read.csv("EPICN4ORD_rawdata07_14_22.csv") #database file

idf_ranked = idf %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:100)
write.csv(idf_ranked, "tf_idf_top100_cities.csv")

#run in loop thru individual collections of cities 
#rejoin cities info by project name 
cases_wcities = case_words4 
#   left_join(case_studies, by = "Project.Name") %>% 
#   dplyr::select(Project.Name, City, word, n, tf, idf, tf_idf)
# write.csv(cases_wcities, "cases_wcities.csv", row.names = FALSE)

selectedcommunities = read.csv("CitiestoHighlight.csv")
priorities_bycity = cases_wcities %>% 
  filter(City %in% selectedcommunities$City) %>% #about halves the data
  mutate(City = factor(City))

# df_comnames = df %>% 
#   left_join(data_full, by = c("name" = "Project.Name")) %>% 
#   dplyr::select(category, niche, keyword, name, abstract, City, State)%>%
#   distinct()

df_topcities = priorities_bycity  %>% 
  filter(City %in% selectedcommunities$City)

cities = priorities_bycity 
#%>%
#filter(City != "Austin") 
cities = unique(factor(priorities_bycity$City))
#may need to exclude some
#with idf setting word size 
#make sure text in priorities by city normal characters - it's not, need to clean
#may be better to rerun from scratch
#I know I need to end with a) a clean vector of city names for c AND
#b) a clean df of control words by city with n freq and idf rankings
#for some reason words plotting separately maybe bc of projects? 
