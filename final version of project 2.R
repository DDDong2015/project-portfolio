data=read.csv('movies_metadata.csv')
dim(data) # 45466    21

#================================ Project Deliverable 1 ==============================#

################## I. Impute NA
sum(is.na(data))
length((which(is.na(data$runtime))))
# There are 287 missing values in 263 rows 

# Since there are more than 45,000 rows in the dataset and rows 
# with missing values only account for  0.6% of it, 
# it is reasonable to drop all missing values instead of 
# imputing with median/mean values. 

# Note: Indeed, we tried to impute all missing values using 
# mice package as shown below. 
# But there is an error warning about memory shortage.
## library(mice)
## set.seed(617)
## data_cluster = mice::complete(mice(data)) 
## Error: vector memory exhausted (limit reached)

# Remove all misssing values 
data1=na.omit(data)
# Check
sum(is.na(data1))
dim(data1) # 45203    21 


################  II. Delete meaningfulness colunms 

# Delete imdb_id, belongs_to_collection, homepage, original_language, 
# original_title, and poster_path
library(dplyr)
data2=select(data1,-c(imdb_id, homepage,belongs_to_collection, 
                      original_language,poster_path,original_title))
dim(data2) # 45203    15

################# III. Quantify Bloolean Variable
data3 <- data2 %>%
  mutate(adult=ifelse(adult=="FALSE",0,1))
data3 <- data3 %>%
  mutate(video=ifelse(video=="FALSE",0,1))


################## IV. Genres key word extraction

# Extract key words in genres column 
# Here, we want to take out the forth word in each cell for our analysis
library(stringr)
# Remove all puntcuation marks by empty spaces
subset_genres=gsub(pattern ='[^[:alpha:]]+', replacement = " ", x = data3$genres)
head(subset_genres)
word(subset_genres, 4)

key_words_genres=as.data.frame(word(subset_genres, 4))

sum(is.na(key_words_genres))
# There are 2364 null values in subset_genres (out of 45203).
# This is becuase there are rows contain solely [] in the 
# orginal genres colunm in the dataset. 
# And []s were not treated as missing values and were not deteced 
# by is.na() function.
# So when we try to extract the forth word in each cell, 
# no information is extracted from cells containing only [].

# Create common id column in key_words_genres to match with the original
# dataset. 
# This will allow us to inner join key_words_genres and data together 
key_words_genres$id <- data3$id
# key_words_genres <- tibble::rowid_to_column(key_words_genres, "id")
dim(key_words_genres)
head(key_words_genres)

# Merge data3 and key_words_genres
data4=data3 %>% inner_join(key_words_genres,
                           by=c('id'='id'))

# Delete genres colunm
data4 = subset(data4, select = -c(genres))
dim(data4) #  45265    16
summary(data4)

# Observe that 17th column is the newly added column.
# But the name is confusing
summary(data4)
head(data4[15])

# Therefore, change the name for the new column
colnames(data4)[15] <- "genres_kw"
dim(data4)
head(data4)

# Remove NA in key_words_genre
data4=na.omit(data4)

sum(is.na(data4))

#============================ Deliverable Project 2 ================================

#---------------------------- Recommendation Analysis -----------------------------#

################# Step V 

# Note this chapther didn't apply spliting data step!#
# Plot out the top 20 movies with vote_count & vote_average #
library(ggplot2)
data4  %>% top_n(20, wt=vote_count) %>%
  ggplot(aes(x=reorder(title, vote_count), y=vote_count)) +
  geom_bar(stat='identity', fill="blue") + coord_flip(y=c(0, 15000)) +
  labs(x="", y="Number of votes") +
  geom_text(aes(label=vote_count), hjust=-0.1, size=3) +
  geom_text(aes(label=vote_average), y=1000, size=3, col="white")

# Weighted vote rating#
C <- mean(data4$vote_average) # 6.09
m <- quantile(data4$vote_count, 0.75)  # 737
data4$weighted_rating <- (data4$vote_average*data4$vote_count + C*m)/(data4$vote_count + m)
head(data4)

# Plot out the top 20 movies under the command that vote_count>1000 with weighted_rating#
# plot out the top 20 under the command that vote_count>1000 with weighted_rating#
data4 %>% filter(vote_count >1000) %>% top_n(20, wt=weighted_rating) %>%
  ggplot(aes(x=reorder(title, weighted_rating), y=weighted_rating)) +
  geom_bar(stat='identity', fill="blue") + coord_flip(y=c(0,10)) +
  labs(x="", y="Weighted Rating") +
  geom_text(aes(label=round(weighted_rating, 2)), hjust=-0.1, size=3) +
  scale_y_continuous(breaks=seq(0, 10, by=1)) +
  geom_text(aes(label=paste("Votes:", vote_count, "Vote Average:", vote_average)), y=2.3, size=3, col="white")


# Plot out top 20 generes#
key_words_genres = na.omit(key_words_genres)
sum(is.na(key_words_genres))
key_words_genres %>% group_by(`word(subset_genres, 4)`) %>% count() %>%
  ggplot(aes(x=reorder(`word(subset_genres, 4)`, n), y=n)) +
  geom_col(fill="blue") + coord_flip() +labs(x="", y="Number of movies")

#facet occure some problems fixed# #using the cleaned dataset movies same as data4#
genres250 = movies %>% filter(vote_count > 250) %>% select(id, weighted_rating, vote_count, title,genres_kw)
genres250 <- genres250 %>% filter(!is.na(genres250$weighted_rating))
library(stringr)
genres250$title <- str_trunc(as.character(genres250$title), width = 50, side="right")
genres250$title <- as.factor(genres250$title)
library(ggplot2)
genres250 %>% filter(!genres_kw %in% c("Foreign", "TV")) %>%
  group_by(genres_kw) %>% arrange(desc(weighted_rating)) %>% slice(1:5) %>%
  ggplot(aes(x=reorder(title, weighted_rating), y=weighted_rating)) +
  geom_col(aes(fill=genres_kw), show.legend = FALSE) + coord_flip(y=c(6,8.5)) +
  facet_wrap(~genres_kw, scales = "free_y", ncol=2) +
  labs(x="", y="") +data=read.csv('movies_metadata.csv')
theme(axis.text.y = element_text(size=6))

#the whole no. of moives from 1800s till now#
select_year = movies %>% filter(year>1800) %>% select (id, year)
select_year$year1 = as.numeric(select_year$year)
ggplot(select_year,aes(x=year1)) +
  geom_histogram(fill="blue", binwidth = 1) +
  labs(x="Release year", y="Number of movies")

#budget top20
movies$budget = as.numeric(movies$budget)
movies  %>% top_n(20, wt=budget) %>%
  ggplot(aes(x=reorder(title, budget), y=budget)) +
  geom_bar(stat='identity', fill="gold") + coord_flip(y=c(0, 400000000)) +
  labs(x="", y="budget") +
  geom_text(aes(label=budget), hjust=-0.1, size=2) +
  geom_text(aes(label=budget), y=1000, size=2, col="white")

#revenue top20
movies  %>% top_n(20, wt=revenue) %>%
  ggplot(aes(x=reorder(title, revenue), y=revenue)) +
  geom_bar(stat='identity', fill="gold") + coord_flip(y=c(0, 3000000000)) +
  labs(x="", y="revenue") +
  geom_text(aes(label=revenue), hjust=-0.1, size=2) +
  geom_text(aes(label=revenue), y=1000, size=2, col="white")

#popularity top 20
movies  %>% top_n(20, wt=popularity) %>%
  ggplot(aes(x=reorder(title, popularity), y=popularity)) +
  geom_bar(stat='identity', fill="gold") + coord_flip(y=c(0, 600)) +
  labs(x="", y="popularity") +
  geom_text(aes(label=popularity), hjust=-0.1, size=3) +
  geom_text(aes(label=popularity), y=1000, size=3, col="white")

#runtime top 20
movies  %>% top_n(20, wt=runtime) %>%
  ggplot(aes(x=reorder(title, runtime), y=runtime)) +
  geom_bar(stat='identity', fill="gold") + coord_flip(y=c(0, 1500)) +
  labs(x="", y="runtime") +
  geom_text(aes(label=runtime), hjust=-0.1, size=3) +
  geom_text(aes(label=runtime), y=1000, size=3, col="white")

#---------------------------- Sentiment Analysis -------------------------------------#

################## Step VI. Save cleaned dataset
write.csv(data4, "cleaned_movies_metadata.csv")

#################  Step VII. Text Mining by Sentiment Analysis 
library(stringr)
library(ggplot2)

# REMINDER:
# HERE, I DID USE THE CLEANED DATA FROM PROJECT DELIVERABLE 1. BUT TRAIN/TEST DATA
# ARE NOT BEING USED.

########## 1. Prepare Data
movies = read.csv('cleaned_movies_metadata.csv',stringsAsFactors = F)
data4$weighted_rating
movies_genres=subset(movies,select=c('genres_kw', 'overview', 'weighted_rating'))
# This data has three columns: movie id, overview and weighted_rating
# where overview is an overall description of the movie, 
# and weighted_rating is weighted voting score recieved by each movie based
# on its overview

# Double check that there should be no missing values 
sum(is.na(movies_genres)) # 0


########## 2. Explore Data

str(movies_genres)

########## 3. overview lengths and average voting scores

## Lengths in characters 
cor(nchar(movies_genres$overview),movies_genres$weighted_rating)
# Cannot derive a conclusion on the relationship between voting score and the length
# of the overview

############# 4. Tokenize

#### 4.1. The number of words in each overview
library(dplyr); library(tidytext)
movies_genres%>%
  select(genres_kw,overview)%>%
  unnest_tokens(output = word,input=overview)%>%
  group_by(genres_kw)%>%
  summarize(count = n())
# unnest_tokens break all the words and put each word into a row
# count = n() shows the total number of words in each genres 

movies_genres %>% 
  select(genres_kw, overview) %>%
  unnest_tokens(output=word, input=overview)

#### 4.2. The total number of words in all overviews
movies_genres%>%
  select(genres_kw,overview)%>%
  unnest_tokens(output = word,input=overview)%>%
  count() # 2,362,017

#### 4.3. The top 20 common words
library(tidytext)

# Visualize the list of top 20 frequent words after removing stop words 
# (e.g. the, a, and, an etc.)
movies_genres%>%
  unnest_tokens(input = overview, output = word)%>%
  select(word)%>%
  anti_join(stop_words)%>%   # remove stop words by anti_join
  group_by(word)%>%
  summarize(count = n())%>%
  ungroup()%>%
  arrange(desc(count))%>%
  top_n(20)%>%
  ggplot(aes(x=reorder(word,count), y=count, fill=count))+
  geom_col()+
  xlab('words')+
  coord_flip()
# Notice that among listed movies, most of the movies themes are around life, 
# love and family. It is obvious that directors assume the public prefer peaceful
# movies and therefore shoot a lot of them. 
# Therefore, we expect movies with positive/joy keywords to gain higher
# ratings, since they dominate the market.

summary(as.data.frame(movies$genres_kw))
# We can also see from genres that crime and horror movies are the least 
# popular themes that directors would like to shoot.

########## 5. Lexicon

## Overviews of films barely contain offensive words, since they were published
# by official sites and already went through investigation. 
## And categorization of [-1,1] or [negative, positive] on overview
# seems to be too vague. 
## Overviews should be classified more precisely based on emotional sense
# since there are different genres of movies.

## Therefore, Emotion NRC Lexicon is chosen. That is, words will be categorized 
# based on the emotion reflected. 

library(tidytext)
nrc = get_sentiments('nrc')

## Total number of words in each type of emotion
library(ggthemes)
movies_genres%>%
  group_by(genres_kw)%>%
  unnest_tokens(output=word, input=overview)%>%
  inner_join(nrc)%>%
  group_by(sentiment)%>%
  count() %>% 
  ggplot(aes(x=reorder(sentiment, X=n), y=n, fill=sentiment))+
  geom_col()+guides(fill=F)+coord_flip()+theme_wsj()

## Number of words in each type of emotion for each genres and rating
movies_genres%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(nrc)%>%
  group_by(genres_kw,sentiment,weighted_rating)%>%
  count()

## Ratings of all overviews based on Emotion Expressed
ratings_of_all_overviews=movies_genres%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(nrc)%>%
  group_by(genres_kw,sentiment,weighted_rating)%>%
  count()%>%
  group_by(sentiment, weighted_rating)%>%
  summarize(n = mean(n))%>%
  data.frame()

ratings_of_all_overviews %>%
  group_by(sentiment) %>%
  summarise(mean(weighted_rating))
# Since peaceful and positive movie themes are the majority ones circulated in 
# the film industry, it is reasonable to see, as we expected, that movies with 
# positive and joy key words received higher weighted rating from the public than 
# the movies which have an overview containing fear, sadness, and disgust words.

## Correlation between n and weighted rating
# Note: Here, n presents the number of times a sentiment word with a specific voting
# score occurs. 
movies_genres%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(nrc)%>%
  group_by(genres_kw,sentiment,weighted_rating)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  summarize(correlation = cor(n,weighted_rating))
# Correlations are all close to zero. Therefore, no conclusion can be drawn on the 
# relationship between weighted voting scores and n.


########### 6. WordCloud for each genres

# Drama
movies_drama=movies_genres[movies_genres$genres_kw=='Drama',]

wordcloudData = 
  movies_drama%>%
  unnest_tokens(output=word,input=overview)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame() 

library(wordcloud)
set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.3),
          max.words = 100,colors=brewer.pal(9,"Spectral"))

# Comedy

movies_comedy=movies_genres[movies_genres$genres_kw=='Comedy',]

wordcloudData = 
  movies_comedy%>%
  unnest_tokens(output=word,input=overview)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame() 

set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.3),
          max.words = 100,colors=brewer.pal(9,"Spectral"))

# Action 

movies_action=movies_genres[movies_genres$genres_kw=='Action',]

wordcloudData = 
  movies_action%>%
  unnest_tokens(output=word,input=overview)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame() 

set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.1),
          max.words = 100,colors=brewer.pal(9,"Spectral"))

# Documentary

movies_documentary=movies_genres[movies_genres$genres_kw=='Documentary',]

wordcloudData = 
  movies_documentary%>%
  unnest_tokens(output=word,input=overview)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame() 

set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.1),
          max.words = 100,colors=brewer.pal(9,"Spectral"))

# Horror

movies_horror=movies_genres[movies_genres$genres_kw=='Horror',]

wordcloudData = 
  movies_horror%>%
  unnest_tokens(output=word,input=overview)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame() 

set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.1),
          max.words = 100,colors=brewer.pal(9,"Spectral"))

# Crime

movies_Crime=movies_genres[movies_genres$genres_kw=='Crime',]

wordcloudData = 
  movies_Crime%>%
  unnest_tokens(output=word,input=overview)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame() 

set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.1),
          max.words = 100,colors=brewer.pal(9,"Spectral"))


# ==================================================================================

###### This is part of Deliverable 1, but as we mentioned before,
###### we don't use sampling in Deliverable 2.

################### 5.  Split the dataset 

library(caret)
set.seed(617)
split=createDataPartition(y=data4$vote_average, 
                          p=0.05, 
                          list=FALSE, 
                          group=50)
sample_data4=data4[split,]
population_data4=data4[-split,]

# Test if data is splitted into the right proportion
nrow(sample_data4)+nrow(population_data4)==nrow(data4) # True
round(nrow(sample_data4)/nrow(data4),2)==0.05 # True


