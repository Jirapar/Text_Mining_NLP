
# Call the essential library
library(stringi) # String processing
#library(stringr) # More fast and correct of implementations of common string manipulations
library(mgsub) 
library(pbapply)
library(tidyverse) #collection of R package design
library(tm)
library(qdap)
library(ggplot2)
library(ggthemes)
#install.packages('dplyr')
library(dplyr)
#install.packages('ggdendro')
library(ggdendro)

# Echarts4R D3 Viz
library(fst)
library(echarts4r)
library(ggwordcloud)


# Options & Functions
Sys.setlocale('LC_ALL','C') #some tweets are in different languages so you may get an error

#-----------------------------------------------------------------------------------------------------#
# Explore the data

# Using user define function to get all files in Data folder 
# Use map_df function to combine the row
map_df_read.csv <- function(path, pattern = "*.csv") {
  list.files(path, pattern, full.names = TRUE) %>% 
    map_df(~read.csv(., stringsAsFactors = FALSE)) #text strings will not be factors of categories
}

# Put all data into df variable
df <- map_df_read.csv('C:/Users/USER/OneDrive/Desktop/Data/Text/Text-Mining-NLP/Case/Case I/Data')


# See the first of 5 row
head(df,5)

# Create new data set that drop the date of created 
data <- df[,-3]

reduce_data <- slice_sample(data, prop = 0.003)
# remove some variable to save my memory
rm(df)
rm(data)

#-------------------------------------------------------------------------------------------------------#
# Organize the data

# Using user defined function to lower the character
tryTolower <- function(x){
  y = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error'))
    y = tolower(x)
  return(y)
}

# Using user defined function to clean data
cleanCorpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, content_transformer(replace_contraction)) 
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

reduce_data$lowertext <- tryTolower(reduce_data$text)

#--------------------------------------------------------------------------------------------------------#
# I want to check the comment that how many mention to both of each athlete and each team

# Counting the number of comment that mention to each athlete
# Constraint is some people may comment only first name or last name
# To be sure that comment mention to whom I focus on only comment mention to full name

# TOP NIKE athletes
Lebron_James <- sum(grepl("Lebron James", reduce_data$lowertext,ignore.case=TRUE))
Giannis_Antetokounmpo <-sum(grepl("Giannis Antetokounmpo", reduce_data$lowertext,ignore.case=TRUE))
Kawhi_Leonard <- sum(grepl("Kawhi Leonard", reduce_data$lowertext,ignore.case=TRUE))


#	Top Adidas athletes
Damian_Lillard <- sum(grepl("Damian Lillard", reduce_data$lowertext,ignore.case=TRUE))
James_Harden <- sum(grepl("James Harden", reduce_data$lowertext,ignore.case=TRUE))
Steph_Curry <- sum(grepl("Steph Curry", reduce_data$lowertext,ignore.case=TRUE))

# Create data frame to see how many comment mention to each athletes
name_freq <- data.frame(terms = c('Lebron James','Giannis Antetokounmpo','Kawhi Leonard',
                                  'Damian Lillard','James Harden','Steph Curry'),
                        frequency = c(Lebron_James,Giannis_Antetokounmpo,Kawhi_Leonard,
                                  Damian_Lillard,James_Harden,Steph_Curry),
                        brand = c('NIKE','NIKE','NIKE',
                                  'Adidas','Adidas','Adidas'))

# Create data frame to see how many comment mention to each team
# Using table function to see the quantity on each team
team_count <- data.frame(table(reduce_data$team))
# Naming the columns name
names(team_count) <- c("team", "count")
# Sorting the decreasing data
team_count <- team_count[order(-team_count$count),]

#------------------------------------------------------------------------------------------------------------#
# Feature Extraction
# Create custom stop words
stops <- c(stopwords('english'), 'will', 'rt')

bigramTokens <-function(x){
  unlist(lapply(NLP::ngrams(words(x), 2), paste, collapse = " "), 
         use.names = FALSE)
}

# Make a volatile corpus
txtCorpus <- VCorpus(VectorSource(reduce_data$lowertext))
# Cleaning data
txtCorpus <- cleanCorpus(txtCorpus, stops)

# Structure the data
commentTDM  <- TermDocumentMatrix(txtCorpus, control=list(tokenize=bigramTokens))

# My R memory is fulled and can't convert corpus to matrix so I enhance my R memory
gc()
memory.limit(9999999999999)
# Convert into matrix
commentTDMm <- as.matrix(commentTDM)

# Sum row to see the total of each word
commentsum <- rowSums(commentTDMm)
# Create data frame
commentfreq <- data.frame(word=names(commentsum),frequency=commentsum)
# Remove row name
rownames(commentfreq) <- NULL
# Subset only word that have frequency at least 150
topWords <- subset(commentfreq, commentfreq$frequency >= 150)
# Sorting the data
topWords <- subset(topWords[order(topWords$frequency, decreasing= T),])

# Subset only top 10 words
topWords_top10 <- topWords %>%
  arrange(desc(frequency)) %>%
  slice(1:10) 


#----------------------------------------------------------------------------------------------------------------#

# Create the graph for analyzing the data
ggplot(topWords_top10, aes(x=reorder(word, frequency), y=frequency)) + 
  geom_bar(stat="identity", aes(fill = frequency)) + 
  scale_fill_gradient2(low = "#59c4e6", high = "#59c4e6",guide="none") +
  coord_flip()+theme_gdocs()+ 
  theme(plot.title=element_text(size=20, colour = 'white'),
        plot.background = element_rect(fill='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(colour = "white"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"))+
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0) + labs(title = "The frequency of word", x = "word", y = "frequency")

# Create word cloud 
name_freq %>% 
  e_color_range(frequency, color, colors = c("#59c4e6", "#f288c8")) %>% 
  e_charts() %>% 
  e_cloud(word = terms, 
          freq = frequency, 
          color = color,
          rotationRange = c(0, 0),
          sizeRange = c(8, 50)) %>% 
  e_title("Athletes") %>%
  e_tooltip() %>% e_theme("dark")

team_count %>% 
  e_color_range(count, color, colors = c("#59c4e6", "#f288c8")) %>% 
  e_charts() %>% 
  e_cloud(word = team, 
          freq = count, 
          color = color,
          rotationRange = c(0, 0),
          sizeRange = c(8, 50)) %>% 
  e_title("Team") %>%
  e_tooltip()%>% e_theme("dark")

#---------------------------------------------------------------------------------------------------#

# Feature Extraction for another data set without team name
# create vector for team name
team_name <- tryTolower(unique(reduce_data$team))

# Put team name in stop word to see what other topic that people mention
stops_withoutteam <- c(stopwords('english'), 'will','rt', team_name )

# Cleaning data
txtCorpus_withoutteam <- cleanCorpus(txtCorpus, stops_withoutteam)

# TDM
commentTDM_withoutteam  <- TermDocumentMatrix(txtCorpus_withoutteam , control=list(tokenize=bigramTokens))

# Due to limit of memory, I have to reduce size of data
reducedTDM_withoutteam <- removeSparseTerms(commentTDM_withoutteam , sparse=0.999)
gc()
memory.limit(99999999999999)
# Convert into matrix
commentTDMm_withoutteam <- as.matrix(reducedTDM_withoutteam)

# Sum row to see the total of each word
commentsum_withoutteam <- rowSums(commentTDMm_withoutteam)
# Create data frame
commentfreq_withoutteam <- data.frame(word=names(commentsum_withoutteam),frequency=commentsum_withoutteam)
# Remove row name
rownames(commentfreq_withoutteam ) <- NULL

# Subset only word that have frequency at least 60
topWords_withoutteam <- subset(commentfreq_withoutteam , commentfreq_withoutteam$frequency >= 60)
# Sorting the data
topWords_withoutteam <- subset(topWords_withoutteam[order(topWords_withoutteam$frequency, decreasing= T),])

# Subset only top 20 words
topWords_top20_withoutteam <- topWords_withoutteam %>%
  arrange(desc(frequency)) %>%
  slice(1:20) 
#----------------------------------------------------------------------------------------------------------------#

# Create the graph for analyzing the data
# Crate bar graph
ggplot(topWords_top20_withoutteam, aes(x=reorder(word, frequency), y=frequency)) + 
  geom_bar(stat="identity", aes(fill = frequency)) + 
  scale_fill_gradient2(low = "#f288c8", high = "#f288c8",guide="none") +
  coord_flip()+theme_gdocs()+ 
  theme(plot.title=element_text(size=20, colour = 'white', hjust = 0.5),
        plot.background = element_rect(fill='black'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        axis.text = element_text(colour = "white"),
        axis.title.x = element_text(colour = "white"),
        axis.title.y = element_text(colour = "white"))+
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.0) + labs(title = "The frequency of word without team name", x = "word", y = "frequency")
#--------------------------------------------------------------------------------------------------------------#

# NIKE Athlete
# Inspect word associations
assoc_lj <- findAssocs(commentTDM, 'lebron james', 0.15)
assoc_lj


# Organize the word associations
assocDF_lj <- data.frame(terms=names(assoc_lj[[1]]),
                      value=unlist(assoc_lj))
assocDF_lj$terms <- factor(assocDF_lj$terms, levels=assocDF_lj$terms)
rownames(assocDF_lj) <- NULL

# Make a dot plot
ggplot(assocDF_lj, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF_lj, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=4)+
  labs(title = "Word Asscociation with Lebron James ") +
  theme(plot.title=element_text(size=20, colour = 'black',hjust = 0.5))
  

# Inspect word associations
assoc_ga <- findAssocs(commentTDM, 'giannis antetokounmpo', 0.20)
assoc_ga


# Organize the word associations
assocDF_ga <- data.frame(terms=names(assoc_ga[[1]]),
                         value=unlist(assoc_ga))
assocDF_ga$terms <- factor(assocDF_ga$terms, levels=assocDF_ga$terms)
rownames(assocDF_ga) <- NULL

# Make a dot plot
ggplot(assocDF_ga, aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF_ga, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=4)+
  labs(title = "Word Asscociation with Giannis Antetokounmpo ") +
  theme(plot.title=element_text(size=20, colour = 'black',hjust = 0.5))



#--------------------------------------------------------------------------------------#

# Inspect word associations
assoc_jb <- findAssocs(commentTDM, 'jimmy butler', 0.20)
assoc_jb


# Organize the word associations
assocDF_jb<- data.frame(terms=names(assoc_jb[[1]]),
                        value=unlist(assoc_jb))
assocDF_jb$terms <- factor(assocDF_jb$terms, levels=assocDF_jb$terms)
rownames(assocDF_jb) <- NULL

# Make a dot plot
ggplot(assocDF_jb , aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF_jb, col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=4)+
  labs(title = "Word Asscociation with Jimmy Butler") +
  theme(plot.title=element_text(size=17, colour = 'black',hjust = 0.5))

#---------------------------------------------------------------------------------#

# Inspect word associations
assoc_miami <- findAssocs(commentTDM, 'miami heat', 0.15)
assoc_miami


# Organize the word associations
assocDF_miami <- data.frame(terms=names(assoc_miami[[1]]),
                         value=unlist(assoc_miami))
assocDF_miami$terms <- factor(assocDF_miami$terms, levels=assocDF_miami$terms)
rownames(assocDF_miami) <- NULL


# Make a dot plot
ggplot(assocDF_miami , aes(y=terms)) +
  geom_point(aes(x=value), data=assocDF_miami , col='#c00c00') +
  theme_gdocs() + 
  geom_text(aes(x=value,label=value), colour="red",hjust="inward", vjust ="inward" , size=4)+
  labs(title = "Word Asscociation with Miami Heat team") +
  theme(plot.title=element_text(size=15, colour = 'black',hjust = 0.5))


#-------------------------------------------------------------------------------------#


