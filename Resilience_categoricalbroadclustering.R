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

####read in control data, prep by transforming into a DTM####
control_df = read.csv(case_studies.csv)


####Run topic model on control data####
# set a seed so that the output of the model is predictable
control_lda <- LDA(control_DTM, k = 4, control = list(seed = 8752))
control_lda
#> A LDA_VEM topic model with 4 topics.

####read in experimental data keywords, prep by transforming into a DTM####
exp_df = read.csv(case_studies.csv)

####See if topic model correctly allocates key words into 4 topics that correspond to the categories####

exp_lda <- LDA(Exp_DTM, k = 4, control = list(seed = 8752))
exp_lda