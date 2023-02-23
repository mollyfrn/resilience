####Generating polar/circular bar charts for community resilience characterization####
####Adapted from the R Graph Gallery: https://r-graph-gallery.com/297-circular-barplot-with-groups.html
####Molly Jenkins 
####02/23/2023

#setwd 
setwd("C:/Users/mjenkins/OneDrive - Environmental Protection Agency (EPA)/Analyses/Resilience_EpicN")

#want to restructure as a normalized index on a scale of 0-1 so the scaling on the plots is again, more easily comparable 
#just because a community had a more longstanding relationship with epicn and more database entries, and therefore more text, it shouldn't 
#should 'score' higher in all categories. 

#want to do this for each keyword entry/observation *before* rolling them up and summarizing them per niche
#should be really straightforward transformation on the keyword frequencies
#compare with tf_idf rankings and see about using those or rolling those up into the niche categories since tf-idf incorporates normalization
#vs manual min-max feature scaling normalization



####Forloop to generate plots for all selected case study cities 
library(tidyverse)
library(geomtextpath)

# Read in and count data 
df = read.csv("Cases_EPICN_Josekeywords.csv") 
#retain or re-add community name column; subset to just communities we are interested in 
data_full = read.csv("EPICN4ORD_rawdata.csv")
selectedcommunities = read.csv("CitiestoHighlight.csv") #want city names probs

df_comnames = df %>% 
  left_join(data_full, by = c("name" = "Project.Name")) %>% 
  dplyr::select(category, niche, keyword, name, abstract, City, State)%>%
  distinct()
  
df_topcities = df_comnames  %>% 
  filter(City %in% selectedcommunities$City)

cities = unique(factor(df_topcities$City)) #maybe Pontotoc is problem child?

####Fixing data input error at Monroe####
topcities_Monroesub = df_topcities %>%
  filter(City == "Monroe") %>%
  mutate(State = "Wisconsin")
topcities = df_topcities %>%
  filter(City != "Monroe") %>%
  full_join(topcities_Monroesub)

####forloop summarizing resilience priorities of communities####
for(c in cities){
  df_mini = topcities %>% 
    filter(City == c) #create a subset of data for just 
  #a given particular community/city
  
  #count up keywords in subset according to niche and category rankings 
  df_hitsniche = df_mini %>% 
    mutate(niche = as.factor(niche),
           category = as.factor(category)) %>%
    add_count(niche) %>%
    select(-keyword, -name, -abstract) %>% 
    unique() %>% 
    mutate(nichecounts = as.numeric(n)) %>%
    select(-n)
  
#this block helps me ID whichever niches and categories 
#are NOT represented in a given city but i want to keep in the graph 
#will iteratively adapt based on what is absent from 
#df_hitsniche so it is important for it to be inside the loop
#includes both missing niches and categories
  data_missingfactors = df %>%
    anti_join(df_hitsniche) %>% 
    filter(category != "Misc") %>%
    mutate(category = as.factor(category), 
           niche = as.factor(niche)) %>%
    select(category, niche)%>%
    filter(niche == is.na(niche) | niche != "") %>%
    unique() %>%
    mutate(category = category, 
           niche = niche, 
           City = c, 
           State = unique(df_hitsniche$State),
           nichecounts = "0.0001") %>%
    mutate(nichecounts = as.numeric(nichecounts))
  
  data_pre = df_hitsniche %>% 
    full_join(data_missingfactors) 
  
  #add ID numbers to help with plotting structure
  #and remove the "Misc" category and assoc level
  data = data_pre %>%
    mutate(id = seq(1:dim(data_pre))) %>%
    filter(category != "Misc") %>%
    mutate(category = droplevels(category))
  
  #data now ready to be prepped for graph 
  #insert graphing code below 
  #prepping data for grid/scales 
  # Set a number of 'empty bar' to add at the end of each group
  empty_bar <- 1
  to_add <- data.frame( matrix(NA, empty_bar*nlevels(data$category), ncol(data)) )
  colnames(to_add) <- colnames(data)
  to_add$category <- rep(levels(data$category), each=empty_bar)
  data <- rbind(data, to_add)
  data <- data %>% arrange(category, nichecounts)
  data$id <- seq(1, nrow(data))
  
  # Get the name and the y position of each label
  label_data <- data
  number_of_bar <- nrow(label_data)
  angle <- 90 - 360 * (label_data$id-0.5) /number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
  label_data$hjust <- ifelse( angle < -90, 1, 0)
  label_data$angle <- ifelse(angle < -90, angle+180, angle)
  
  # prepare a data frame for base lines
  base_data <- data %>% 
    group_by(category) %>% 
    summarize(start=min(id), end=max(id) - empty_bar) %>% 
    rowwise() %>% 
    mutate(title=mean(c(start, end)))
  
  # prepare a data frame for grid (scales)
  grid_data <- base_data
  grid_data$end <- grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
  grid_data$start <- grid_data$start - 1
  grid_data <- grid_data[-1,]
  
  
  #plot 
  p <- ggplot(data, aes(x=as.factor(id), y=nichecounts, fill=category)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
    
    geom_bar(aes(x=as.factor(id), y=nichecounts, fill=category), stat="identity", alpha=0.5) 
  p
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  p+  geom_segment(data=grid_data, aes(x = end, y = 1000, xend = start, yend = 1000), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 800, xend = start, yend = 800), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 600, xend = start, yend = 600), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, linewidth=0.3 , inherit.aes = FALSE ) 
  
  # Add text showing the value of each 100/75/50/25 lines
  p+ annotate("text", x = rep(max(data$id),4), y = c(200, 600, 800, 1000), label = c("20", "60", "80", "100") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
    
    geom_bar(aes(x=as.factor(id), y=nichecounts, fill=category), stat="identity", alpha=0.5) +
    ylim(-100,1500) +
    theme_minimal() +
    theme(
      legend.position = "none",
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      plot.margin = unit(rep(-1,4), "in") 
    ) +
    coord_polar() + 
    geom_text(data=label_data, aes(x=id, y=nichecounts+10, label=niche, hjust=hjust),
              color="black", fontface="bold",alpha=0.6, size=2, angle= label_data$angle, 
              inherit.aes = FALSE) +
    #annotate(geom = "text", x = 1)
    # Add base line information
    #geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = 0), colour = "black", alpha=0.8, size=0.4 , inherit.aes = FALSE )  +
    geom_textpath(data=base_data, aes(x = title, y = 1500, label=category), 
                  hjust=c(1,1,1,1), 
                  colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE, drop = FALSE)
  ggsave(paste0("test_polarplot", c,".png"), width = 7, height = 7, units = c("in"))
  
}

#02_21 fixed, fully debugged!!! 
#next steps: normalize values for ease of interpretation
#reduce white space etc 
#move on to word clouds

####Justification for 'empty' bars in radar plots####
#join padded-negligible niches and categories 
#with real data 
#so that empty categories still are shown in the graph 
#so it is visually apparent to non-scientist users 
#that the category is still there, just their 
#community of interest does not have substantial weight
#behind that category or niche being a priority
#this will make it easier to visually compare with a 
#community where it IS a priority 
#e.g. Monroe WI may have a very very short social capital bar
#while Miami may have a comparitively large social capital bar 
#it will facilitate interpretation by non-scientist, where
#it is appropriate in this context for us to include "NA" type data, or "non-data" 
#whereas in technical contexts, this is typically discouraged
#in favor of only drawing data that IS there and maximizing 
#utility and meaning of "ink" used 