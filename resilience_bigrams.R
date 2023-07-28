#Bigrams of organic text 
#Method part 2, continued NLP extensions 
#Molly Jenkins
#06/13/2022

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

#look at bigrams by top words e.g. ACCESS 
bigram_access <- bigram_counts %>%
  filter(word1 == "access" | word2 == "access") %>% 
  arrange(desc(n))
write.csv(bigram_access, "bigramACCESS.csv")

trigram_access = trigram_cases %>% 
  filter(word1 == "access"| word2 == "access"| word3 == "access") %>%
  arrange(desc(n))
write.csv(trigram_access, "trigramACCESS.csv")

####sentiment analysis#### -> test out other lexicons, not just afinn
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

negation_words <- as.factor(c("not", "no", "never", "without"))

negated_wordsNRC <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(nrc, by = c(word2 = "word")) %>%
  count(word1, word2, sentiment, sort = TRUE)
#as.factor(negated_words$word1)
write.csv(negated_wordsNRC, "negated_words_NRC.csv")

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
