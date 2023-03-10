#This script takes input data from two texts and evaluates their similarity 
#Molly F Jenkins
#07-14-2022

####3 Similarity tests between sets of key words####
library(tidyverse)
library(tidytext)
library(tidydata)
library(igraph)
library(ggraph)
library(tm)
library(wordcloud)
library(SnowballC)
library(widyr)
library(ggplot2)
library(stringdist)
library(quanteda)
library(quanteda.textstats)

setwd("C:/Users/mjenkins/OneDrive - Environmental Protection Agency (EPA)/Analyses/Resilience_EpicN")
case_studies <- read.csv("EPICN4ORD_rawdata07_14_22.csv") #database file
res_updated <- read.csv("resilience_keywords_02_23_23.csv")

res_updated = res_updated %>% 
  rename(BroadCategories = BroadCategories,
         search.terms = SearchTerm)

case_words = case_studies %>% 
  unnest_tokens(word, Project.Abstract) %>% 
  count(Project.Name, word, sort = TRUE)

total_words <- case_words %>% 
  group_by(Project.Name) %>% 
  summarize(total = sum(n))

case_words2 = left_join(case_words, total_words)
case_words3 = case_words2 %>%
  bind_tf_idf(word, Project.Name, n)

case_words4 = case_words3 %>% 
  dplyr::select(-total) %>% 
  arrange(desc(tf_idf))



experimental_resilience = res_updated %>% 
  unnest_tokens(word, search.terms) %>%
  select(word) %>% 
  distinct() 

experimental_resilience = unlist(experimental_resilience[])
control_organic = unique(case_words$word)
control_idf = unique(case_words4$word[0:500])
# experimental_resilience = tokens(unlist(experimental_resilience[]))
# 
# control_organic = tokens(unique(case_words$word)) #case text in general or just the top ranked idf ones? 
# control_idf = tokens(unique(case_words4$word[0:500])) #just the top 500 words as ranked by idf 

#until issues with the quanteda packages resolved, use stringdistr


#Jaccard's similarity 
#stringdist 
Jac = stringsim(experimental_resilience, control_idf, method = "jaccard")

# dfm = dfm(c(experimental_resilience, control_idf))
# as.matrix(dfm)
# jac_simil = textstat_simil(dfm, method = "jaccard")

#Cosine similarity 
Cos = stringsim(control_idf, experimental_resilience, method = "cosine")

#join measures together 
control_idf = as.data.frame(control_idf)
simil_stats = cbind(control_idf, Cos)
simil_stats2 = cbind(simil_stats, Jac)

write.csv(simil_stats2, "simil_stats_prelim.csv")

# cos_simil = textstat_simil(dfm, method = "cosine")

####pairwise methods#### 
cases_section_words <- case_studies %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, Project.Abstract) %>%
  filter(!word %in% stop_words$word)

cases_section_words

# count words co-occuring within sections
word_pairs <- cases_section_words %>%
  pairwise_count(word, section, sort = TRUE)

word_cors <- cases_section_words %>%
  group_by(word) %>%
  filter(n() >= 20) %>%
  pairwise_cor(word, section, sort = TRUE)

word_cors

set.seed(2017)

word_cors %>%
  filter(correlation > .5) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), show.legend = FALSE) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()

#need to break up somehow or increase the corr threshold