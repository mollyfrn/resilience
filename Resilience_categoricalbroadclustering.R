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
library(SnowballC)
library(hunspell)

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
####end cleaning, start text tokenizing etc####

####Control text processing####
generics = data.frame( word = c("community", "city", "students", 
             "student", "county", "program", "project",
             "report", "plan", "research", "reports", 
             "analysis", "develop", "design"))


#make it control_dtm
control_dtm = datafull %>%
  unnest_tokens(word, Project.Abstract) %>% #way to stem tokens?
  filter(!word %in% stop_words$word &
           !word %in% generics) %>%
  mutate(wordstem = wordStem(word))%>%
  count(City, wordstem) %>%
  cast_dtm(City, wordstem, n)
stop_df = as.data.frame(stop_words)

control_dtm = datafull %>%
  unnest_tokens(word, Project.Abstract) %>% #way to stem tokens?
  anti_join(stop_df, by = "word") %>% 
  anti_join(generics, by = "word") %>%
  #mutate(wordstem = wordStem(word))%>% consider not stemming ahead of topic modeling, plenty of lit discourages or is mixed
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
  arrange(topic, -beta) %>%
  mutate(topic = factor(topic),
         Group = "Control")
         
control_top_terms2 <- control_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 20) %>% 
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(topic = factor(topic, 
                        labels = c("public & social health",
                        "public & social health pt2", 
                        "physical & natural infra", 
                        "leadership and governance"))) 
#graph w/out labels
control_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

#graph w/labels
control_top_terms2 %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()
ggsave("controllabeled_4topicmodel_nostem.png", plot = last_plot(), width = 7, height = 7, units = c("in"))
#can I manually assign topic categories? do they sync? 
#e.g. should I use the exp categories 
#to do a supervised topic model 
#and eval how well that model fits/predicts
#the tokenized corpuses/lexicon? 
#bonus benefit of slda is that it uses likelihood
#might be more appropriate to using a 
#response variable like location 
#e.g.(coastal, west, north, south, east, etc) to predict topics
#outputs 1 and 4 most salient 
#output 1 uses stemming, output 4 ignores 
#environmental consistently its own topic; 
#health & wellbeing, socioeconomic health, 
#business and governance often 
#somewhat blended in topic models 
#but nonetheless apparent or emergent 



####read in experimental data keywords, prep by transforming into a DTM####
exp_df = read.csv(case_studies.csv)
#do outputs from topic models bear similarity to exp lexicons by category?
#how well does topic 1 correspond to public health & wellbeing etc 
#get 
####Control text processing####
generics = data.frame( word = c("community", "city", "students", 
                                "student", "county", "program", "project",
                                "report", "plan", "research", "reports", 
                                "analysis", "develop", "design"))
stop_df = as.data.frame(stop_words)
keyword_toks = keywords %>%
  unnest_tokens(word, SearchTerm)
exp_dtm = datafull %>%
  unnest_tokens(word, Project.Abstract) %>% #way to stem tokens?
  anti_join(stop_df, by = "word") %>% 
  anti_join(generics, by = "word") %>%
  filter(word %in% keyword_toks$word) %>%
  #mutate(wordstem = wordStem(word))%>% consider not stemming ahead of topic modeling, plenty of lit discourages or is mixed
  count(City, word) %>%
  cast_dtm(City, word, n) #oh shoot I probably don't want to count by City
####See if topic model correctly allocates key words into 4 topics that correspond to the categories####

exp_lda <- LDA(exp_dtm, k = 4, control = list(seed = 8705))
exp_lda

exp_topics <- tidy(exp_lda, matrix = "beta")

exp_top_terms <- exp_topics %>%
  group_by(topic) %>%
  slice_max(beta, n = 10) %>% 
  ungroup() %>%
  arrange(topic, -beta) %>%
  mutate(topic = factor(topic),
         Group = "Experimental") #topic, 
                        #labels = c("public & social health",
                                   #"public & social health pt2", 
                                   #"physical & natural infra", 
                                   #"leadership and governance"))) 

exp_top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  scale_y_reordered()

ggsave("exp_4topicmodel_nostem_nogenerics.png", plot = last_plot(), width = 7, height = 7, units = c("in"))

####Merge 2 groups of 4-set, 20 word lexicons####
#label "control" and 'experiment" 
#facet_wrap/stack for visual comparison
#do a cosine or Jaccard's similarity between THESE sets 
#is there greater similarity? 

merged_topicmods = data.frame(rbind(control_top_terms, exp_top_terms))

factor(merged_topicmods$Group)
#look at which topics correspond closest and plot 
#next to each other, or reorder 


merged_topicmods %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(beta, term, fill = topic)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(factor(Group) ~topic, scales = "free") + #add group
  scale_y_reordered()

ggsave("4topicmodel_expvscontrol.png", plot = last_plot(), width = 7, height = 7, units = c("in"))

#exp top terms vs control top terms

#####Similarity matrices####
exptop1 = merged_topicmods %>% 
  filter(topic == "1" & Group == "Experimental") %>%
  select(term)
exptop2 = merged_topicmods %>% 
  filter(topic == "2" & Group == "Experimental") %>%
  select(term)
exptop3 = merged_topicmods %>% 
  filter(topic == "3" & Group == "Experimental") %>%
  select(term)
exptop4 = merged_topicmods %>% 
  filter(topic == "4" & Group == "Experimental") %>%
  select(term)


controltop1 = merged_topicmods %>% 
  filter(topic == "1" & Group == "Control") %>%
  select(term)

controltop2= merged_topicmods %>% 
  filter(topic == "2" & Group == "Control") %>%
  select(term)

controltop3= merged_topicmods %>% 
  filter(topic == "3" & Group == "Control") %>%
  select(term)

controltop4= merged_topicmods %>% 
  filter(topic == "4" & Group == "Control") %>%
  select(term)
#yes

Jac3 = stringsim(exptop3, controltop3, method = "jaccard")
Jac4 = stringsim(exptop4, controltop3, method = "jaccard")
Jac3.b = stringsim(exptop3, controltop4, method = "jaccard")
Jac4.b = stringsim(exptop4, controltop4, method = "jaccard")

Jac3.1 = stringsim(exptop3, controltop1, method = "jaccard")
Jac4.1 = stringsim(exptop4, controltop1, method = "jaccard")
Jac2.1 = stringsim(exptop2, controltop1, method = "jaccard")
Jac1.1 = stringsim(exptop1, controltop1, method = "jaccard")

#all of the terms in the topic model groupings 
#bear high similarity regardless of exp vs control group origin 


Jacfull = stringsim(control_top_terms$term, exp_top_terms$term, method = "jaccard")
Jacfull

#maybe I should stem before calcs? 

####Naive bayes for topic classification and prediction?#### 
#can, if trained on 1/4 of the resilience framework, a model correctly assign the keywords to the right 4 categories? 
#this might be a question for another time, or at least 
#maybe would be useful for seeing the exp framework validation 
#but not necessary for the control, since the control is actually 
#pretty apparent 
#is it ok for me to compare the lda for the control to the tf and raw freq breakdowns in the exp? 
#does that help paint the picture of categorical overlap? 

