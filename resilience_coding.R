####10.5.5 Resilience Storymapping Project: Pilot Script####
#Molly F Jenkins 
#01/11/2021

#This script takes a database of EPIC-N projects and systematically categorizes them 
#according to an EPA-developed resilience indicator framework, 
#based on the Rockefeller City Resilience Index and other resilience indices.


####Pre-requisites: set working directory, install tidy library, and read in files####
#rqda
install.packages("tidyverse")
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
setwd("C:/Users/mjenkins/OneDrive - Environmental Protection Agency (EPA)/Analyses/Resilience_EpicN")

case_studies <- read.csv("EPICN4ORD_rawdata07_14_22.csv") #database file
#make case_studies a subset with the two example cities  
#case_studies2
#examplecities = c('Ramsey City')
#case_studies = case_studies2 %>% 
  #filter(City %in% examplecities)

resilience_keywords <- read.csv("ResilienceKeywords_tidy.csv") #resilience keyword file 
res_updated <- read.csv("resilience_keywords_51921.csv")

res_updated = res_updated %>% 
  rename(BroadCategories = ?..BroadCategories,
         search.terms = SearchTerm)


#take database, comb for matches with each resilience search term, 
#assigning an indicator and domain category to each match

#need to make keyword search categories, supplemental search terms, project Names searchable strings 
#may need to use str_subset and regex, gsub replace "," with "|"

#actually what I should do is stretch the dataset so each keyword has its own row 
#separate_rows(table3, rate, sep = "/")

#res_tidy = separate_rows(resilience_keywords, search.terms, sep = ",")

#write.csv(res_tidy, "res_tidy_search_as_rows.csv", row.names = FALSE)


####
res_tidy = res_updated


#think about adding just "collaboration" to keywords and then use NVivo 
#simplifying terms to encompass what people actually use e.g. "trust" and not just "institutional trust" 

####for loop assigning categories to case studies based on keyword criteria match####
categories = factor(unique(res_tidy$BroadCategories))
#adding layer of niche frequencies
df_final = data.frame(category = NULL, niche = NULL, keyword = NULL, name = NULL, abstract = NULL) #, sdgs = NULL)
#test: c = 'Economy & Society' 
#test: n = 'Collective ID & Cohesion'
for(c in categories){
  res_one = res_tidy %>% 
    filter(BroadCategories == c)
    niches = res_one$NicheCategories
  for(n in niches){
    res_mini = res_one %>% 
      filter(NicheCategories == n) 
    searchterms = res_mini$search.terms
  for(s in searchterms){ #for each search term

    abstract_match = str_subset(as.character(case_studies$Project.Abstract), #find if search term occurs
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
          abstract = Project.Abstract) %>% 
  select(category, niche, keyword, name, abstract) %>% 
      distinct()
    print(n)
    
  #matched_categories2 = cbind(matched_categories, sdgs)  
  #probably need to add line that pads with NA's for rows where no criteria are matched 
  #probably also need to add line that tells R to parse the character content since it also contains | segments
  
  df_final = rbind(matched_categories, df_final)
  }
}}

df_final2 = df_final %>%
  distinct() #deduplication 

write.csv(df_final2, "Cases_EPICN_Josekeywords_cleaned07_14_2022.csv", row.names = FALSE)

#code works but need to account for * operator for search terms to optimize search 
#need to make sure it's KEEPING everything from each iteration 

####Read in final df####
df = read.csv("Cases_EPICN_Josekeywords_cleaned07_14_2022.csv") #df = df_final2
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

####Wordclouds####
#create the word cloud!!! 
#experimental n size word cloud 
set.seed(8002)
wordcloud(words = df_hits$keyword, freq = df_hits$n, min.freq = 1,
          max.words=200, random.order=FALSE, random.color = TRUE, 
          colors=brewer.pal(8, "Dark2"), scale = c(7, 0.50))
ggsave("Experimentalwordcloud_n.png", units = c("in"))

####Wordcloud and frequency chart of top root words####
#bring in raw unprocessed data to look at frequency of root words, bigrams etc
#look at text organically and see what emerges  
####tf-idf method####
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

####tf_idf informed wordcloud forloop with clouds for each community####
#02/03 where I left off - need to polish and export wordclouds with city names in titles 
#also need to return to radar plots and scale if possible or index from 0-1, but that may be a follow up job for tomorrow

df = read.csv("Cases_EPICN_Josekeywords.csv") 
#retain or re-add community name column; subset to just communities we are interested in 
data_full = read.csv("EPICN4ORD_rawdata.csv")
selectedcommunities = read.csv("CitiestoHighlight.csv") #want city names probs
idf = read.csv("tf_idf_parsingofcasestudiestext.csv") #idf = case_words4


idf_ranked = idf %>% 
  arrange(desc(tf_idf)) %>%
  slice(1:100)
write.csv(idf_ranked, "tf_idf_top100.csv")

#run in loop thru individual collections of cities 
#rejoin cities info by project name 
cases_wcities = case_words4 %>% 
  left_join(case_studies, by = "Project.Name") %>% 
  dplyr::select(Project.Name, City, word, n, tf, idf, tf_idf)


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

cities = unique(factor(priorities_bycity$City)) #may need to exclude some
#with idf setting word size 

for(c in cities){
  df_mini = priorities_bycity %>% 
    filter(City == c) %>%
    unique()
  
set.seed(9005) #use random num gen to set seed
png(paste0("wordcloudtf_idf", c,".png"), width=1280,height=800)

wordcloud(words = df_mini$word, freq = df_mini$tf_idf, min.freq = 2,
          max.words=200, random.order=FALSE, random.color = TRUE) #, scale = c(7, 0.50))
dev.off()

}


####tf idf raw loop####
#now do just a bunch of diff graphs for each city as a loop 
cities = selectedcommunities$City
for(c in cities){
  testcities = priorities_bycity %>% 
    filter(City == c) 
  testcities %>%
    slice_max(tf_idf, n = 15) %>% 
    mutate(word = reorder(word, tf_idf)) %>%  
    ggplot(aes(tf_idf, word)) +
    geom_col(show.legend = FALSE) +
    labs(x = "tf-idf", y = NULL) 
  
  ggsave(paste0("idf_bycity", c,".png"), width = 7, height = 7, units = c("in"))
    
}

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



#bigram network visualizations 
bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph

#visualize
set.seed(2057)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)
ggsave("bigram_basic.png", width = 7, height = 7, units = c("in"))


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