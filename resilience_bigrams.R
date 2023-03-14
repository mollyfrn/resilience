#Bigrams of organic text 
#Method part 2, NLP extensions 
#Molly Jenkins
#03/13/2023

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
#make case_studies
####bigrams and trigrams####
cases_bigrams = case_studies %>%
  unnest_tokens(bigram, Project.Abstract, token = "ngrams", n = 2)
counted_bigrams = cases_bigrams %>%
  count(bigram, sort = TRUE)


bigrams_separated <- cases_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigram_tf_idf <- bigrams_united %>%
  count(Project.Name, bigram) %>%
  bind_tf_idf(bigram, Project.Name, n) %>%
  arrange(desc(tf_idf))

write.csv(bigram_tf_idf, "project_bigrams_tfidf.csv")

#bigrams_filtered = read.csv("project_bigrams.csv")
# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)
#write 
write.csv(bigram_counts, "bigram_counts.csv")

#trigrams
trigram_cases = case_studies %>%
  unnest_tokens(trigram, Project.Abstract, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word,
         !word3 %in% stop_words$word) %>%
  count(word1, word2, word3, sort = TRUE)
write.csv(trigram_cases, "trigram_counts.csv")

#look at bigrams by city
bigram_tf_idf <- bigrams_united %>%
  count(City, bigram) %>% 
  bind_tf_idf(bigram, City, n) %>%
  arrange(desc(tf_idf))
#want to remove NA's and numbers from outputs probably 
write.csv(bigram_tf_idf, "bigramtf_idfbyCity.csv")

#look at bigrams by top words e.g. ACCESS -> replicate also w/pairwise corrs
bigram_access <- bigram_counts %>%
  filter(word1 == "access" | word2 == "access") %>% 
  arrange(desc(n))
write.csv(bigram_access, "bigramACCESS.csv")

trigram_access = trigram_cases %>% 
  filter(word1 == "access"| word2 == "access"| word3 == "access") %>%
  arrange(desc(n))
write.csv(trigram_access, "trigramACCESS.csv")

#sentiment analysis -> test out other lexicons, not just afinn
AFINN <- get_sentiments("afinn")
nrc <- get_sentiments("nrc")

not_wordsAFINN <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)




not_wordsAFINN %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")
ggsave("AFINN_notwords.png", width = 7, height = 7, units = c("in"))


negation_words <- as.factor(c("not", "no", "never", "without"))

negated_wordsAFINN <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)
#as.factor(negated_words$word1)

negated_wordsAFINN2 = negated_wordsAFINN %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution),
         word1 = as.factor(word1)) 

ggplot(data= negated_wordsAFINN2, aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")+
  facet_wrap(~word1, ncol = 2, scales = "free_y")
ggsave("AFINN_negatedbigrams.png", width = 7, height = 7, units = c("in"))

#rerun with nrc vs afinn 

not_wordsNRC <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(nrc, by = c(word2 = "word")) %>%
  count(word2, sentiment, sort = TRUE)
write.csv(not_wordsNRC, "not_wordsNRC.csv")

# not_wordsNRC %>%
#   mutate(contribution = n * sentiment) %>%
#   arrange(desc(abs(contribution))) %>%
#   head(20) %>%
#   mutate(word2 = reorder(word2, contribution)) %>%
#   ggplot(aes(n * sentiment, word2, fill = n * sentiment > 0)) +
#   geom_col(show.legend = FALSE) +
#   labs(x = "Sentiment value * number of occurrences",
#        y = "Words preceded by \"not\"")
# ggsave("NRC_notwords.png", width = 7, height = 7, units = c("in"))


negation_words <- as.factor(c("not", "no", "never", "without"))

negated_wordsNRC <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(nrc, by = c(word2 = "word")) %>%
  count(word1, word2, sentiment, sort = TRUE)
#as.factor(negated_words$word1)
write.csv(negated_wordsNRC, "negated_words_NRC.csv")


# negated_wordsNRC2 = negated_wordsNRC %>%
#   mutate(contribution = n * sentiment) %>%
#   arrange(desc(abs(contribution))) %>%
#   head(20) %>%
#   mutate(word2 = reorder(word2, contribution),
#          word1 = as.factor(word1)) 
# 
# ggplot(data= negated_wordsNRC2, aes(n * value, word2, fill = n * value > 0)) +
#   geom_col(show.legend = FALSE) +
#   labs(x = "Sentiment value * number of occurrences",
#        y = "Words preceded by \"not\"")+
#   facet_wrap(~word1, ncol = 2, scales = "free_y")
# ggsave("NRC_negatedbigrams.png", width = 7, height = 7, units = c("in"))
# 



####bigram network visualizations#### 
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

#visualize
set.seed(2098)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), hjust = 0.5, vjust = 1, position = "nudge")
ggsave("bigram_basic_0323.png", width = 12, height = 7, units = c("in"))


set.seed(5722)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()
ggsave("bigram_advanced.png", width = 7, height = 7, units = c("in"))






#visualize similarity/overlap between two sets 






####DRAFTING####


####Use top frequency words to dictate bigram analysis for context####
#e.g. access is a top 10 word in the raw ranking; access to WHAT ? 
cases_section_words = case_studies %>% 
  filter(City == "") %>%
  mutate(section = row_number() %/% 10) %>%
  filter(section > 0) %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word)

word_pairs = cases_section_words %>%
  pairwise_c0ount(word, section, sort = TRUE)

word_cors = cases_section_words %>%
  group_by(word) %>% 
  filter(n() >= 20) %>% 
  pairwise_cor(word, section, sort = TRUE)

word_cors2 = word_cors %>%
  filter(item1 == "access")

#####use top frequency IDF words to also investigate bigram analysis for context?####
#topic modeling: final analysis 

####Correlational analysis between two word frequency lists####
resilience_keywords #need the breakdown/frequency of these keywords to the text
self_reportedwords 
case_studies #the base text to compare to 

#I guess I would merge the two keyword lists into one document 
#and then have a third column
#3rd col detailing two factor variables: "self_reported" vs "resilience_assigned"
#could then do some interesting grouping 



####Wordcloud forloop for top selected communities####
setwd() 
df = c() #instead, specify file pathway where wordcloud tiffs will be stored
communities = c(df_fullepic$communityname) 

setwd() #set up empty folder for wordclouds after reading in files, 
#reset to empty folder

for(c in communities){
  df_hits = df_fullepic %>% 
    filter(communityname == c)
  
  set.seed(8002)
  w = wordcloud(words = df_hits$keyword, freq = df_hits$n, min.freq = 1,
                max.words=200, random.order=FALSE, random.color = TRUE, 
                colors=brewer.pal(8, "Dark2"), scale = c(7, 0.50))
  save.image(w, paste("wordcloud", w, ".tif"))
  
}
####Pie chart forloop for top selected communities####

for(c in communities){
  df_hits = df_fullepic %>% 
    filter(communityname == c)
  
  ggplot(df_hits)+geom
  w = wordcloud(words = df_hits$keyword, freq = df_hits$n, min.freq = 1,
                max.words=200, random.order=FALSE, random.color = TRUE, 
                colors=brewer.pal(8, "Dark2"), scale = c(7, 0.50))
  save.image(w, paste("wordcloud", w, ".tif"))
  
}

####Spider plot forloop alternative#### 
ggplot(df_hits)+geom_bar()+coord_polar("y", start = 0)

####Community size histogram####
sizes = read.csv("CitiestoHighlight.csv", header = TRUE)
ggplot(sizes, aes(x = Community.Size)) +geom_histogram()+labs(x = "Community Size", y = "Number of Communities")

####Pie charts####
#display frequency of hits relative to total hits as % of each niche and domain 
#display as a spider plot? or nested double pie chart for each of top 40 case studies

####Join with sdgs assignments####

df_sdgs = df_final %>% 
  rename(search.terms = keyword) 



df2 = df_sdgs %>%
  left_join(res_tidy, by = "search.terms")

write.csv(df2, "cases_wsdgs_resilience.csv", row.names = FALSE)

####Join with all data for Marshall####
case_studies = rename(case_studies, name = Project.Name)
df_fullepic = df2 %>% 
  left_join(case_studies, by = "name")

write.csv(df_fullepic, "EPICcases_final.csv")

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