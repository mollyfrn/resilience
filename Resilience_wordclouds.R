#wordcloud script 
#explicitly just for wordcloud image generation
#Molly F Jenkins
setwd("C:/Users/mjenkins/OneDrive - Environmental Protection Agency (EPA)/Analyses/Resilience_EpicN")

#libraries
library(tidyverse)
library(tm)
library(tidytext)
library(SnowballC)
library(hunspell)
library(wesanderson)


#################
stop_df = as.data.frame(stop_words)
data_full = read.csv("EPICN4ORD_rawdata.csv")
#filter out common words like community, city, students, and county, program, and project
#fix monroe issue 
datafull_Monroesub = data_full %>%
  filter(City == "Monroe") %>%
  mutate(State = "Wisconsin")
datafull_pre = data_full %>%
  filter(City != "Monroe") %>% #remove the problematic Monroe entries
  full_join(datafull_Monroesub) #insert the fixed Monroe entries 

datafull_StPaulsub = datafull_pre %>%
  filter(City == "St. Paul" | City == "St Paul"| City == "Saint Paul") %>%
  mutate(City = "Saint Paul") 

datafull = datafull_pre %>%
  filter(City != "St. Paul" & City != "St Paul" & City != "Saint Paul") %>%
  full_join(datafull_StPaulsub)

####end cleaning, start text tokenizing etc####

####Control text processing####
stop_df = as.data.frame(stop_words)

control_tidydata_full = datafull %>%
  unnest_tokens(word, Project.Abstract) %>% #way to stem tokens?
  anti_join(stop_df, by = "word") %>% 
  #mutate(wordstem = wordStem(word))%>% consider not stemming ahead of topic modeling, plenty of lit discourages or is mixed
  count(word) %>%
  mutate(Group = "Control") %>%
  bind_tf_idf(word, Group, n) %>%
  arrange(desc(n)) #has just n sizes bc no other grouping vars to act as 'documents'

control_tidydata_cities = datafull %>%
  unnest_tokens(word, Project.Abstract) %>% #way to stem tokens?
  anti_join(stop_df, by = "word") %>% 
  #mutate(wordstem = wordStem(word))%>% consider not stemming ahead of topic modeling, plenty of lit discourages or is mixed
  count(City, word) %>%
  mutate(Group = "Control") %>%
  bind_tf_idf(word, City, n) %>%
  arrange(desc(n)) %>%
  arrange(desc(tf_idf)) #cities as 'documents', has both n sizes and tf-idf

###########forloop wordclouds####
cities = control_tidydata_cities$City #need to subset to just selected cities

for(c in cities){
  df_mini = control_tidydata_cities %>% 
    filter(City == c) %>%
    unique()
  
  set.seed(2308) #use random num gen to set seed
  png(paste0("wordcloudn_", c,".png"), width=1200,height=1200) #add hi res specs and margins
  
  wordcloud(words = df_mini$word, freq = df_mini$n, min.freq = 2,
            max.words=200, random.order=FALSE, random.color = TRUE, 
            colors=brewer.pal(8, "Dark2")) #, scale = c(7, 0.50))
  dev.off()
  
} #right, I ran into this with the wordclouds before - debug 

#with n frequency setting word size
set.seed(8002)
wordcloud(words = idf$word, freq = idf$n, min.freq = 1,
          max.words=200, random.order=FALSE, random.color = TRUE, 
          colors=brewer.pal(8, "Dark2"), scale = c(7, 0.50))
#ggsave("Controlwordcloud_n.png", width = 7, height = 7, units = c("in"))

#with idf setting word size 
set.seed(8007)
wordcloud(words = idf$word, freq = idf$tf_idf, min.freq = 1,
          max.words=200, random.order=FALSE, random.color = TRUE, 
          colors=brewer.pal(8, "Dark2"), scale = c(7, 0.50))
#ggsave("Controlwordcloud_tf_idf.png", width = 7, height = 7, units = c("in"))