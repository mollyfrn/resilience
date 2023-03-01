#Molly F. Jenkins 
#02/28/2023 
#Examining category or Niche agreement between expert/academic lit lexicon and community applied lexicon 
#e.g. broad categorical agreement of priority topic areas in spite of specific language dissimilarity?
#initial inspiration in resilience_coding.R doc, but this is much more neat 

#Need to find a way to clearly visually demonstrate the similarity in categorical topics 
#use frameowkr keyword doc to roll up categorical assignments
#vs manual coding into categories using the organic language in the text 
#cluster plot/topic model with two diff shades of green, two diff shades of pink, two diff shades of blue, two shades purple 
#lighter, or triangles = resilience set 
#darker, or circles = "control" set 

#see if clusters overlap 
#maybe even repeat for niches? 
#a 4 topic model, and then a 12 topic model 

#or instead of clustering; 2 facet wrapped groups of the topic models for the control vs experimental category topics? 
library(topicmodels)
library(tidyverse)
library(tm)
library(tidytext)

setwd("C:/Users/mjenkins/OneDrive - Environmental Protection Agency (EPA)/Analyses/Resilience_EpicN")
# Read in and count data 
res_casestudies = read.csv("Cases_EPICN_Josekeywords_cleaned02_23_23.csv") 
#retain or re-add community name column; subset to just communities we are interested in 
keywords = read.csv("resilience_keywords_02_23_23.csv")

####read in control data, prep by transforming into a DTM####
data_full = read.csv("EPICN4ORD_rawdata.csv")
#filter out common words like community, city, students, and county, program, and project
#fix monroe issue 
datafull_Monroesub = data_full %>%
  filter(City == "Monroe") %>%
  mutate(State = "Wisconsin")
datafull = data_full %>%
  filter(City != "Monroe") %>% #remove the problematic Monroe entries
  full_join(datafull_Monroesub) #insert the fixed Monroe entries 


generics = c("community", "city", "students", 
             "student", "county", "program", "project",
             "report", "plan", "research", "report")

#make it control_dtm
control_dtm = datafull %>%
  unnest_tokens(word, Project.Abstract) %>%
  filter(!word %in% stop_words$word & !word %in% generics) %>%
  count(City, word) %>%
  cast_dtm(City, word, n)

#try also for cities/communities? 
#^ see if  topic model works when split that way

control_dtm

####Run topic model on control data####
# set a seed so that the output of the model is predictable
control_lda <- LDA(control_dtm, k = 4, control = list(seed = 2719))
control_lda
#> A LDA_VEM topic model with 4 topics.
control_topics <- tidy(control_lda, matrix = "beta")

control_top_terms <- control_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta)

control_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#can I manually assign topic categories? do they sync? 
####read in experimental data keywords, prep by transforming into a DTM####
exp_df = read.csv(case_studies.csv)

####See if topic model correctly allocates key words into 4 topics that correspond to the categories####

exp_lda <- LDA(Exp_DTM, k = 4, control = list(seed = 8752))
exp_lda