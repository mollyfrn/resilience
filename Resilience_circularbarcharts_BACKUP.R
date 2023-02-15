####Generating polar/circular bar charts for community resilience characterization####
####Adapted from the R Graph Gallery: https://r-graph-gallery.com/297-circular-barplot-with-groups.html
####Molly Jenkins 
####05/19/2022

#setwd 
setwd("C:/Users/mjenkins/OneDrive - Environmental Protection Agency (EPA)/Analyses/Resilience_EpicN")

#I want each bar to represent each resilience niche category 
#with the value coming from the frequency of keyword hits 
#and I want the bars to be GROUPED into four distinct category/colors
#these groups will be informed by the four main resilience domains 
#with a separate chart for each selected community 
#effectively rendering a quick visual characterization of that community

####02/15 notes for coding this week####
#as it stands, right now this code is NOT plotting niche bars if there are no words or hits within the niche. 
#However, the categories are still plotted and labeled 
#I need to specify that a niche category label should still be created for an "empty" bar, the empty bar should still be created and invisible
#right? is that important to me that it's created but blank? I think it's important for ease of visual comparability so someone can say 
#oh this one community ranked really high on this metric, but this other one ranked low/nothing in that niche metric 
#I think disappearing niche bars that don't let you know that they're missing, I think that loses some of the point of having these plots 

#also want to restructure as a normalized index on a scale of 0-1 so the scaling on the plots is again, more easily comparable 
#just because a community had a more longstanding relationship with epicn and more database entries, and therefore more text, it shouldn't 
#should 'score' higher in all categories. 

#want to do this for each keyword entry/observation *before* rolling them up and summarizing them per niche
#should be really starightforward transformation on the keyword frequencies
#compare with tf_idf rankings and see about using those or rolling those up into the niche categories since tf-idf incorporates normalization
#vs manual min-max feature scaling normalization


# library
library(tidyverse)
library(geomtextpath)

# Read in and count data 
df = read.csv("Cases_EPICN_Josekeywords.csv")
df_hits = df %>% 
  count(keyword)

df_hitsniche = df %>% 
  count(niche) %>% 
  rename(nichecounts = n) %>%
  mutate(niche = factor(niche))

#re-join to include broad categories
df_nichebroad = df_hitsniche %>% 
  left_join(df, by = 'niche') %>% 
  dplyr::select(-keyword, -name, -abstract) %>% 
  distinct() %>% 
  mutate(niche = factor(niche),
         category = factor(category))%>%
  slice(-(1:4))

data = df_nichebroad %>% 
  mutate(id = seq(1:dim(df_nichebroad)),
         nichecounts = as.numeric(nichecounts))

data = droplevels(data)
#now my data is prepped with the n of projects that fall into each niche and category 
#do one for the whole project and then do a loop that does this for each 
#of the top projects

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

# Make the plot
p <- ggplot(data, aes(x=as.factor(id), y=nichecounts, fill=category)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=nichecounts, fill=category), stat="identity", alpha=0.5) 
p
# Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
p+  geom_segment(data=grid_data, aes(x = end, y = 1000, xend = start, yend = 1000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 800, xend = start, yend = 800), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 600, xend = start, yend = 600), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) 

# Add text showing the value of each 100/75/50/25 lines
p+ ggplot2::annotate("text", x = rep(max(data$id),4), y = c(200, 600, 800, 1000), label = c("20", "60", "80", "100") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
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
            inherit.aes = FALSE ) +
  #annotate(geom = "text", x = 1)
  # Add base line information
  #geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = 0), colour = "black", alpha=0.8, size=0.4 , inherit.aes = FALSE )  +
  geom_textpath(data=base_data, aes(x = title, y = 1500, label=category), 
                hjust=c(1,1,1,1), 
                colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE) #+scale_x_discrete(drop=FALSE)
ggsave("test_polarplot.png", width = 7, height = 7, units = c("in"))


#need to tweak category/domain labels and scale bar 
#ideally want category labels to wrap around 
#outside or inside in nice curve 
#01/31/2023 the above code is functional and does what it's supposed to! 

#------------

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

topcities = df_topcities %>% 
  filter(City != "Bellefonte" & City != "Big Lake" & 
           City != "Austin" & City != "Glendale" & City != "Omaha" & 
           City != "Jonestown" & City != "New Bedford" & City != "Salinas" &
           City != "Chico" & City != "Milesburg" & City != "Navasota" &
           City != "West Palm Beach" & City != "Arvada" & City!= "Providence" &
           City != "Beaufort" & City != "Winthrop" & City != "Ferguson Township")

cities = unique(factor(topcities$City)) #maybe Pontotoc is problem child?

#test walkthru: c = "Saint Paul"
for(c in cities){
  df_mini = topcities %>% 
    filter(City == c)
  
  #count up keywords in subset according to niche and category rankings 
  df_hitsniche = df_mini %>% 
    count(niche) %>% 
    rename(nichecounts = n) %>%
    mutate(niche = factor(niche))
  
  #re-join to include broad categories
  df_nichebroad = df_hitsniche %>% 
    left_join(df, by = 'niche') %>% 
    dplyr::select(-keyword, -name, -abstract) %>% 
    distinct() %>% 
    mutate(niche = factor(niche),
           category = factor(category))%>%
    slice(-(1:4)) #%>% group_by(niche, .drop = FALSE)
  
  data = df_nichebroad %>% 
    mutate(id = seq(1:dim(df_nichebroad)),
           nichecounts = as.numeric(nichecounts)) %>%
    filter(category != "Misc") %>% #removing the misc in the data grid creation 1/31/23
    mutate( category = droplevels(category))#retrieve annotation from above
  
  #data = droplevels(data) #but this won't render rankings of "0" on the plot, and when it does happen
  #it causes an hjust error and short circuits the plotting. So I need to find a way to instead make sure 0's added.
  #maybe some kind of ifelse function (?)
  
  #data now ready to be prepped for graph #insert graphing code below 
  
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
  p+  geom_segment(data=grid_data, aes(x = end, y = 1000, xend = start, yend = 1000), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 800, xend = start, yend = 800), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 600, xend = start, yend = 600), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
    geom_segment(data=grid_data, aes(x = end, y = 200, xend = start, yend = 200), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) 
  
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
  
  #iowa city now where the error is happening
}

#01/31/23 St Paul test runthru works flawlessly now 
#need to adjust scaling now so plots are more visually meaningful

#Error in `geom_textpath()`:
# ! Problem while setting up geom aesthetics.
# ℹ Error occurred in the 5th layer.
# Caused by error in `check_aesthetics()`:
#   ! Aesthetics must be either length 1 or the same as the data (3) 
#(so there's an instance where a city has NO representation 
#in one of the categories and so it's missing and ends up with only 3 levels
#how to make sure it gets filled in anyway? an if or which statement? 
#this is the state of the error 
#stackoverflow says I just need to set (drop = FALSE) to all scale statements in plotting
#02/02 - trying drop = FALSE in geomtextpath argument first 
#it works! Pontotoc was indeed the problem child and so was the lack of (drop = FALSE)



# ✖ Fix the following mappings: `hjust`
# Run `rlang::last_error()` to see where the error occurred.
# There were 50 or more warnings (use warnings() to see the first 50)