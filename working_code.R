# Analysis of overview
data = read.csv('/Users/Jerry/Desktop/tmdb-5000-movie-dataset/cleaned_tmdb.csv',
                stringsAsFactors = F)

## Explore Ratings of Reviews
str(data)
median(data$vote_average) # median review rating
## Using dplyr
library(dplyr)
data%>%
  summarize(average_rating = mean(vote_average), median_rating = median(vote_average))

library(ggplot2); library(ggthemes)
ggplot(data=data,aes(x=vote_average))+
  geom_histogram(fill='sienna')+
  theme_economist()

data$overview[100]
nchar(data$overview[100])

## Number of words, sentences in overview 100
library(stringr)
str_count(string = data$overview[100], pattern = '\\S+')
str_count(string = data$overview[100],pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]")

## Character, Words and Sentences for all Reviews
mean_char = mean(nchar(data$overview)); mean_char
median_char = median(nchar(data$overview)); median_char

## Words across all reviews
mean_words = mean(str_count(string = data$overview,pattern = '\\S+')); mean_words
median_words = median(str_count(string = data$overview,pattern = '\\S+')); median_words

## Sentences across all reviews
mean_sentences = mean(str_count(string = data$overview,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]")); mean_sentences
median_sentences = median(str_count(string = data$overview,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]")); median_sentences

## Longest overview and Shortest Review
summary(nchar(data$overview))
summary(str_count(string = data$overview,pattern = '\\S+'))
summary(str_count(string = data$overview,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"))

## Longest review
longest_review_index = which.max(str_count(string = data$overview,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"))
data$title[longest_review_index]
data$overview[longest_review_index]

## length in characters
cor(nchar(data$overview),data$vote_average)
cor.test(nchar(data$overview),data$vote_average)

## length in words
cor(str_count(string = data$overview,pattern = '\\S+'),data$vote_average)
cor.test(str_count(string = data$overview,pattern = '\\S+'),data$vote_average)

## length in sentences
cor(str_count(string = data$overview,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),data$vote_average)
cor.test(str_count(string = data$overview,pattern = "[A-Za-z,;'\"\\s]+[^.!?]*[.?!]"),data$vote_average)

## Upper case overview
proportionUpper = str_count(data$overview,pattern='[A-Z]')/nchar(data$overview)

cor(proportionUpper,data$vote_average)
cor.test(proportionUpper,data$vote_average)

## Exclamation marks
summary(str_count(data$overview,pattern='!'))

proportionExclamation = str_count(data$overview,pattern='!')/nchar(data$overview)
cor(proportionExclamation,data$vote_average)
cor.test(proportionUpper,data$vote_average)

## Most Common Words
library(qdap)
freq_terms(text.var = data$overview, top = 25)
plot(freq_terms(text.var = data$overview, top = 25))

## look at the top 25 list after removing the stopwords
freq_terms(text.var=data$overview,top=25,stopwords = Top200Words)
plot(freq_terms(text.var=data$overview,top=25,stopwords = Top200Words))

words_by_rating = word_list(data$overview,
                            grouping.var=tolower(data$overview),
                            stopwords = c(Top200Words,"while","he's","soon"),
                            cut.n = 25)

## Sentiment Analysis
## create index column named 'id'
data$id = seq.int(nrow(data))

## word counts in all overview
library(dplyr); library(tidytext)
data %>%
  select(id, overview)%>%
  group_by(id)%>%
  unnest_tokens(output = word,input=overview)%>%
  count()

## Bing Lexicon
as.data.frame(get_sentiments('bing'))[1:50,]

get_sentiments('bing')%>%
  group_by(sentiment)%>%
  count()

data%>%
  select(id, overview)%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)

## total number of positive and negative
data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()

data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=sentiment,y=n,fill=sentiment))+geom_col()+theme_economist()+guides(fill=F)

## Proportion of Positive words in overviews
data %>%
  select(id,overview)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=overview)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

data%>%
  select(id,overview,vote_average)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=overview)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(vote_average,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))

data%>%
  select(id,overview,vote_average)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=overview)%>%
  ungroup()%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(vote_average,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=vote_average,y=proportion,fill=sentiment))+geom_col()+theme_economist()

data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(id,vote_average)%>%
  summarize(positivity = sum(sentiment=='positive')/n())%>%
  ungroup()%>%
  summarize(correlation = cor(positivity,vote_average))

## nrc lexicon
get_sentiments('nrc')%>%
  group_by(sentiment)%>%
  count()

## examine the emotions expressed in the overviews
data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()

data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()+theme_wsj()

## Ratings of all Reviews based on Emotion Expressed
## For each emotion, let us examine the relationship between number of words and vote_average. What is the nature of relationship for each emotion? Examine the trend.
data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(id,sentiment,vote_average)%>%
  count()%>%
  group_by(sentiment, vote_average)%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=vote_average,y=n,fill=vote_average))+
  geom_col()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()

## Correlation between emotion expressed and review rating
data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(id,sentiment,vote_average)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  summarize(correlation = cor(n,vote_average))

## Scatterplot of relationship
data%>%
  group_by(id)%>%
  unnest_tokens(output = word, input = overview)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(id,sentiment,vote_average)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  ggplot(aes(x=vote_average,y=n))+geom_point()+facet_wrap(~sentiment)+geom_smooth(method='lm',se=F)

## afinn Lexicon
## examine the sentiment of all reviews.
data %>%
  select(id,overview)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=overview)%>%
  inner_join(get_sentiments('afinn'))%>%
  summarize(reviewSentiment = mean(score))%>%
  ungroup()%>%
  summarize(min=min(reviewSentiment),max=max(reviewSentiment),median=median(reviewSentiment),mean=mean(reviewSentiment))

data %>%
  select(id,overview)%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=overview)%>%
  inner_join(get_sentiments('afinn'))%>%
  summarize(reviewSentiment = mean(score))%>%
  ungroup()%>%
  ggplot(aes(x=reviewSentiment,fill=reviewSentiment>0))+
  geom_histogram(binwidth = 0.1)+
  scale_x_continuous(breaks=seq(-5,5,1))+scale_fill_manual(values=c('tomato','seagreen'))+
  guides(fill=F)+
  theme_wsj()

library(wordcloud)
wordcloudData = 
  data%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=overview)%>%
  anti_join(stop_words)%>%
  group_by(word)%>%
  summarize(freq = n())%>%
  arrange(desc(freq))%>%
  ungroup()%>%
  data.frame()

set.seed(617)
wordcloud(words = wordcloudData$word,wordcloudData$freq,scale=c(2,0.5),max.words = 100,colors=brewer.pal(9,"Spectral"))

## Comparison Cloud
library(tidyr)
wordcloudData = 
  data%>%
  group_by(id)%>%
  unnest_tokens(output=word,input=overview)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  ungroup()%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.5),max.words = 200, rot.per=0)



# Analysis of genre and budget
## Import the clean dataset
data=read.csv("cleaned_tmdb.csv")

## I want to find how many movies are there in each genre. First，parse the column 'genres' and get rid of the square brackets.Seperate it so each row only contains one genre. 
library(dplyr)
library('gsubfn')
library(tidyr)
genres=gsub("\\[|\\]", "\\\\", data$genres) 
data=cbind(data,genres)
data=data[,-c(2)]
data=separate_rows(data,genres,sep = "[^[:alnum:].]+", convert = FALSE)
summary(data$genres)

## Observe how many movies there are under some major genres
as.character(data$genres)
class(data$genres)
str(data$genres)
summary(data$genres)
sum(data$genres=="Action",na.rm=FALSE)
sum(data$genres=="Adventure",na.rm=FALSE)
sum(data$genres=="Fantasy",na.rm=FALSE)
sum(data$genres=="Science",na.rm=FALSE)
sum(data$genres=="Fiction",na.rm=FALSE)
sum(data$genres=="Crime",na.rm=FALSE)
sum(data$genres=="Drama",na.rm=FALSE)
sum(data$genres=="Thriller",na.rm=FALSE)
sum(data$genres=="Animation",na.rm=FALSE)
sum(data$genres=="Western",na.rm=FALSE)
sum(data$genres=="Family",na.rm=FALSE)
sum(data$genres=="Mystery",na.rm=FALSE)
sum(data$genres=="Comedy",na.rm=FALSE)
sum(data$genres=="Romance",na.rm=FALSE)
sum(data$genres=="Horror",na.rm=FALSE)
sum(data$genres=="",na.rm=FALSE)

## Seperate rows so that each row contains one genre name, remove empty cells and some small genres.
Action=c("Action")
Adventure=c("Adventure")
Fantasy=c("Fantasy")
Science=c("Science")
Fiction=c("Fiction")
Crime=c("Crime")
Drama=c("Drama")
Thriller=c("Thriller")
Animation=c("Animation")
Western=c("Western")
Family=c("Family")
Mystery=c("Mystery")
Comedy=c("Comedy")
Romance=c("Romance")
Horror=c("Horror")
data$type=
  ifelse(data$genres %in% Action, "Action", 
         ifelse(data$genres %in% Adventure, "Adventure", 
                ifelse(data$genres %in% Fantasy, "Fantasy", 
                       ifelse(data$genres %in% Science, "Science",
                              ifelse(data$genres %in% Fiction, 'Fiction',
                                     ifelse(data$genres %in% Crime, 'Crime',      
                                            ifelse(data$genres %in% Drama, 'Drama',
                                                   ifelse(data$genres %in% Thriller, 'Thriller',
                                                          ifelse(data$genres %in% Animation, 'Animation',
                                                                 ifelse(data$genres %in% Western, 'Western',
                                                                        ifelse(data$genres %in% Family, 'Family',
                                                                               ifelse(data$genres %in% Mystery, 'Mystery',
                                                                                      ifelse(data$genres %in% Comedy, 'Comedy',
                                                                                             ifelse(data$genres %in% Romance, 'Romance',
                                                                                                    ifelse(data$genres %in% Horror, 'Horror',
                                                                                                           "NA")))))))))))))))
summary(data$type)
data=data[which(data$type!="NA"),]
head(data)
data = subset(data, select = -c(type) )
## Now we have one genre in each row.

## Convert genres from character to factor datatype. 
library(dplyr)
data <- mutate_if(data, is.character, as.factor)
## Genre Analysis
install.packages('NLP')
## Genre Distribution ___number of movies per genre
library(tm)
library(dplyr)
library(ggplot2)
library(wordcloud)
genre <- Corpus(VectorSource(data$genres))
genre_dtm <- DocumentTermMatrix(genre)
genre_freq <- colSums(as.matrix(genre_dtm))
freq <- sort(colSums(as.matrix(genre_dtm)), decreasing=TRUE) 
genre_wf <- data.frame(word=names(genre_freq), freq=genre_freq)

ggplot(genre_wf, aes(x=reorder(word,-freq), y=freq))+ 
  geom_bar(stat="identity")+
  theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="Black",face="bold"),legend.position="none")+
  #theme(axis.text.x=element_text(angle=45, hjust=1))+
  ggtitle("Distribution of Movies by Genre")+
  xlab("Genre")+
  ylab("No of Movies")

## sentiment analysis of movie genre
set.seed(1)
pal2 <- brewer.pal(8,"Dark2")
wordcloud(genre_wf$word,genre_wf$freq,random.order=TRUE,
          rot.per=.15, colors=pal2,scale=c(4,.9))

## Overall, density plots of all numeric variables
## put all numeric variables into a new dataframe
data2=data[, c("budget","popularity", "revenue","runtime", "vote_average","vote_count","profit")]

## Density plot
par(mfrow=c(3, 3))
colnames <- dimnames(data2)[[2]]
for (i in 1:7) 
{ d <- density(data2[,i])
plot(d, type="n", main=colnames[i])
polygon(d, col="red", border="gray")}

## Distribution of factors among genres
ggplot(data,aes(y=revenue/1000000,fill=genres))+geom_boxplot() 
ggplot(data,aes(y=budget/1000000,fill=genres))+geom_boxplot() 
ggplot(data,aes(y=profit,fill=genres))+geom_boxplot() 
ggplot(data,aes(y=vote_average,fill=genres))+geom_boxplot() 
ggplot(data,aes(y=vote_count,fill=genres))+geom_boxplot() 
ggplot(data,aes(y=popularity,fill=genres))+geom_boxplot() 
ggplot(data,aes(y=runtime,fill=genres))+geom_boxplot() 
median(data$vote_average[data$genres=="Western"])
median(data$vote_average[data$genres=="Drama"])
median(data$vote_average[data$genres=="Horror"])
median(data$vote_average[data$genres=="Comedy"])

## Relation between vote_average, Revenue & Budget
library(plotly)
plot_ly(data, x = ~budget/1000000,y = ~revenue/1000000,mode = "markers", marker=list( size=3 , opacity=0.5), color = ~vote_average, colors=c("black","green") )

## Relation between genre, Revenue & Budget
library(plotly)
plot_ly(data, x = ~budget/1000000, y = ~revenue/1000000, mode = "markers", marker=list( size=3 , opacity=0.5), color = ~genres,colors = rainbow(15))

## correlation among important movie factors
library(corrgram)
corrgram_data <- data %>% 
  dplyr::select(., budget,popularity, revenue,runtime, vote_average,vote_count,profit)

corrgram(corrgram_data)

## insert the code from Vincent here
## k-mean clustering
library(ggplot2)
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)
set.seed(123)
## Generate movie clusters and plot
mydataCluster <- kmeans(na.omit(scale(data[, c("budget","popularity", "revenue","runtime", "vote_average","vote_count","profit")])), 2, nstart = 20)
data$cluster <- as.factor(mydataCluster$cluster)
ggplot(data, aes(revenue, vote_average, color = data$cluster)) + geom_point()+scale_colour_manual(values=c("green", "blue")) + xlab("revenue") + ylab("vote_average")

## some other plots
library(tidyverse)
library(scales)
library(ggplot2)

## a dot-plot of vote_average on revenue
plot_revenue <- ggplot(data, aes(x = vote_average, y = revenue)) +geom_point()
plot_revenue

## a heatmap of vote_average on revenue to observe the distribution of movies in the plot more clearly
plot_votecount_revenue <- ggplot(data, aes(x = vote_average, y = revenue)) +
  geom_bin2d() +
  scale_x_log10(labels = comma) +
  scale_y_continuous(breaks = 1:10) +
  scale_fill_viridis_c(labels = comma)
plot_votecount_revenue

## a heatmap of vote_count on vote_average 
plot_votecount_avg <- ggplot(data, aes(x = vote_count, y = vote_average)) +
  geom_bin2d() +
  scale_x_log10(labels = comma) +
  scale_y_continuous(breaks = 1:10) +
  scale_fill_viridis_c(labels = comma)
plot_votecount_avg

## a heatmap of vote_count on revenue
plot_voteavg_revenue <- ggplot(data, aes(x = vote_count, y = revenue)) +
  geom_bin2d() +
  scale_x_log10(labels = comma) +
  scale_y_continuous(breaks = 1:10) +
  scale_fill_viridis_c(labels = comma)
plot_voteavg_revenue




# Analysis of runtime, tagline and keywords
data = read.csv('cleaned_tmdb.csv')
summary(data)

## Explore correlation between runtime and vote_average & profit
cor(data$runtime, data$vote_average)
cor(data$runtime, data$revenue)

plot(data$runtime, data$vote_average)
plot(data$runtime, data$revenue)

## Text mining on tagline
## Average number of characters and words in the tagline
library(stringr)
data$tagline = as.character(data$tagline)
mean_char = mean(nchar(data$tagline)); mean_char
mean_words = mean(str_count(string = data$tagline,pattern = '\\S+')); mean_words

## Longest and shortest tagline
summary(str_count(string = data$tagline,pattern = '\\S+'))
shortest_tagline = which.min(str_count(string = data$tagline,pattern = '\\S+')); data$tagline[shortest_tagline]
longest_tagline = which.max(str_count(string = data$tagline,pattern = '\\S+')); data$tagline[longest_tagline]

## Correlation between tagline length and vote_average & profit
cor(nchar(data$tagline), data$vote_average)
cor(nchar(data$tagline), data$profit)

cor((str_count(string = data$tagline,pattern = '\\S+')), data$vote_average)
cor((str_count(string = data$tagline,pattern = '\\S+')), data$profit)

## Common words used in taglines
library(qdap)
freq_terms(text.var = data$tagline,top = 25, stopwords = Top200Words)
plot(freq_terms(text.var=data$tagline,top=25,stopwords = Top200Words))

words_by_rating = word_list(data$tagline,
                            grouping.var=tolower(data$vote_average),
                            stopwords = c(Top200Words,"it's","games","they're","you're","he's"),
                            cut.n = 25)


## Sentiment Analysis
library(dplyr); library(tidytext)
## Positive and negtive words in taglines
data%>%
  unnest_tokens(output = word, input = tagline)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()

## Sentiment and vote_average
data %>%
  select(tagline,vote_average)%>%
  unnest_tokens(output=word,input=tagline)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(vote_average,sentiment)%>%
  summarize(n = n())%>%
  mutate(proportion = n/sum(n))%>%
  ggplot(aes(x=vote_average,y=proportion,fill=sentiment))+geom_col()

## Correlation between positive words and vote_average & profit
data%>%
  unnest_tokens(output = word, input = tagline)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(word,vote_average)%>%
  summarize(positivity = sum(sentiment=='positive')/n())%>%
  ungroup()%>%
  summarize(correlation = cor(positivity,vote_average))

data%>%
  unnest_tokens(output = word, input = tagline)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(word,profit)%>%
  summarize(positivity = sum(sentiment=='positive')/n())%>%
  ungroup()%>%
  summarize(correlation = cor(positivity,profit))

## Emotions in taglines
data%>%
  unnest_tokens(output = word, input = tagline)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()

## Vote_average based on tagline emotion
data%>%
  unnest_tokens(output = word, input = tagline)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment,vote_average)%>%
  count()%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=vote_average,y=n,fill=vote_average))+
  geom_line()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()

## Profit and tagline emotion
data%>%
  unnest_tokens(output = word, input = tagline)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment,profit)%>%
  count()%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=profit,y=n,fill=profit))+
  geom_line()+
  facet_wrap(~sentiment)+
  guides(fill=F)


## Correlation between emotion expressed and vote ratings
data%>%
  unnest_tokens(output = word, input = tagline)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment, vote_average)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  summarize(correlation = cor(n,vote_average))



data%>%
  unnest_tokens(output = word, input = tagline)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment,vote_average)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  ggplot(aes(x=vote_average,y=n))+geom_point()+facet_wrap(~sentiment)+geom_smooth(method='lm',se=F)


## Wordcloud
library(tidyr)
library(wordcloud)
wordcloudData = 
  data%>%
  unnest_tokens(output=word,input=tagline)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()
rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.7),max.words = 100, rot.per=0)

## Top performance movies (top 20%)
data_top_vote = subset(data, vote_average > quantile(vote_average, prob = 1 - 20/100))
str(data_top_vote)
data_top_profit = subset(data, profit > quantile(profit, prob = 1 - 20/100))

## Check budget difference for top vote and top profit
vote_com = cbind(mean(data_top_vote$budget), mean(data$budget))
barplot(vote_com, names.arg=c('top_vote_budget_average','budget_average'))

profit_com = cbind(mean(data_top_profit$budget), mean(data$budget))
barplot(profit_com, names.arg=c('top_profit_budget_average','budget_average'))

## Check runtime difference for top vote and top profit
run_vote = cbind(mean(data_top_vote$runtime), mean(data$runtime))
barplot(run_vote, names.arg=c('top_vote_runtime_average','budget_average'))

## Budget and ratings
cor(data$budget, data$vote_average)
plot(data$popularity, data$vote_average)

## Clean out keywords variable from JSON object
library(tidyverse)
data_raw = read_csv('tmdb_raw.csv', guess_max = 3000)

library(stringr)
library(jsonlite)
keywords <- data_raw %>%    
  filter(nchar(keywords)>2) %>%     # fiter out blank keywords field
  mutate(                           # create a new field 
    js = lapply(keywords, fromJSON) #   containing a LIST of keyword and value pairs
  ) %>%                             #   called id and name
  unnest(js) %>%                    # turn each keyword/value pairs in the LIST into a row
  select(id, title,vote_average,revenue, keyword=name)   # select the columns we want

str(keywords)

## calculate frequencies
tab <- table(keywords$keyword)
## sort
tab_s <- sort(tab)
## extract 10 most frequent nationalities
top15 <- tail(names(tab_s), 15)
## subset of data frame
d_s <- subset(keywords, keyword %in% top15)
## order factor levels
d_s$keyword <- factor(d_s$keyword, levels = rev(top15))

## plot
library(ggplot2)
ggplot(d_s, aes(y = revenue, fill = keyword)) + geom_boxplot()
ggplot(d_s, aes(y = vote_average, fill = keyword)) + geom_boxplot()

## Check median values for the best % worst vote_average movie keywords
median(d_s$vote_average[d_s$keyword == "based on novel"])
median(d_s$vote_average[d_s$keyword == "biography"])
median(d_s$vote_average[d_s$keyword == "teenager"])

## Check median values for the highest % lowest revenue movie keywords
median(d_s$revenue[d_s$keyword == "duringcreditsstinger"])
median(d_s$revenue[d_s$keyword == "aftercreditsstinger"])
median(d_s$revenue[d_s$keyword == "dystopia"])
median(d_s$revenue[d_s$keyword == "independent film"])

## Text mining on keywords
## Average number of characters and words in the keywords
library(stringr)
data$keywords = as.character(data$keywords)
mean_char = mean(nchar(data$keywords))
mean_char #121.8944
mean_words=mean(str_count(string = data$keywords,pattern = '\\S+'))
mean_words #5.718834

## Longest and shortest keywords
summary(str_count(string = data$keywords,pattern = '\\S+')) 
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#1.000   3.000   5.000   5.719   8.000  94.000 
shortest_keywords = which.min(str_count(string = data$keywords,pattern = '\\S+'))
data$keywords[shortest_keywords]
#"[\"egypt\",\"sun\",\"chaos\",\"symbol\",\"artifact\",\"transformers\",\"tank\",\"robot\",\"imax\",\"duringcreditsstinger\"]"
longest_keywords = which.max(str_count(string = data$keywords,pattern = '\\S+'));
data$keywords[longest_keywords]
#"[\"male nudity\",\"female nudity\",\"tattoo\",\"gambling\",\"corruption\",\"father son relationship\",\"prostitute\",\"robbery\",\"detective\",\"police brutality\",\"wife husband relationship\",\"card game\",\"desperation\",\"undercover\",\"kidnapping\",\"drug dealer\",\"retirement\",\"death of a friend\",\"asthma\",\"police\",\"fellatio\",\"blood splatter\",\"revenge\",\"deception\",\"murder\",\"betrayal\",\"priest\",\"breast\",\"bag of money\",\"shootout\",\"blood on shirt\",\"shot in the back\",\"undercover cop\",\"blood\",\"suicidal\",\"f word\",\"shot to death\",\"punched in the face\",\"redemption\",\"police officer killed\",\"hospital\",\"dirty cop\",\"police corruption\",\"pistol\",\"new york city\",\"church\",\"violence\",\"strangulation\",\"foot chase\",\"police detective\",\"father daughter relationship\",\"police station\",\"held at gunpoint\",\"catholic\",\"guilt\",\"brooklyn new york city\",\"narcotics cop\",\"nypd\",\"missing person\",\"racial slur\",\"swat team\",\"ensemble cast\",\"pregnant wife\",\"confessional\",\"drive by shooting\",\"shot in the shoulder\",\"gun in mouth\",\"police raid\",\"reference to god\",\"razor blade\",\"shot through a door\",\"stealing money\",\"police officer shot\",\"expecting twins\",\"lens flare\",\"flashback\",\"money problems\",\"family man\",\"neo-noir\",\"bmw\",\"apathy\",\"confession booth\",\"thoughts of retirement\",\"hail mary\",\"working in the nude\",\"drug raid\",\"ironing money\",\"bedtime prayer\",\"briefing\",\"shot point blank\",\"reference to gregory hines\",\"police badge\",\"sex slave\",\"trail of blood\",\"gold watch\",\"man slaps a woman\",\"mold\"]"

## Correlation between keywords length and popularity,profit,budget,revenue
cor(nchar(data$keywords), data$popularity) #0.2052186
cor(nchar(data$keywords), data$profit) #0.1889385
cor(nchar(data$keywords), data$budget) #0.08857026
cor(nchar(data$keywords), data$revenue) #0.1817585

cor((str_count(string = data$keywords,pattern = '\\S+')), data$popularity) #0.1694745
cor((str_count(string = data$keywords,pattern = '\\S+')), data$profit) #0.1555276
cor((str_count(string = data$keywords,pattern = '\\S+')), data$budget) #0.07458499
cor((str_count(string = data$keywords,pattern = '\\S+')), data$revenue) #0.150013
cor((str_count(string = data$keywords,pattern = '\\S+')), data$vote_average) #0.2119504


## Common words used in keywords
library(qdap)
freq_terms(text.var = data$keywords,top = 25, stopwords = Top200Words)
plot(freq_terms(text.var=data$keywords,top=25,stopwords = Top200Words))

## Sentiment Analysis
library(dplyr); library(tidytext)
## Positive and negtive words in keywords
data%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(sentiment)%>%
  count()
#negative 4402, positive 1479

## Correlation between positive words and popularity & revenue
data%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(word,popularity)%>%
  summarize(positivity = sum(sentiment=='positive')/n())%>%
  ungroup()%>%
  summarize(correlation = cor(positivity,popularity))
#correlation 0.0582

data%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('bing'))%>%
  group_by(word,revenue)%>%
  summarize(positivity = sum(sentiment=='positive')/n())%>%
  ungroup()%>%
  summarize(correlation = cor(positivity,revenue))
#correlation 0.0904

## Emotions in keywords
data%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment)%>%
  count()%>%
  ggplot(aes(x=reorder(sentiment,X = n),y=n,fill=sentiment))+geom_col()+guides(fill=F)+coord_flip()

## Revenue based on keywords emotion
data%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment,revenue)%>%
  count()%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=revenue,y=n,fill=revenue))+
  geom_line()+
  facet_wrap(~sentiment)+
  guides(fill=F)+coord_flip()

## Profit and keywords emotion
data%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment,revenue)%>%
  count()%>%
  summarize(n = mean(n))%>%
  ungroup()%>%
  ggplot(aes(x=revenue,y=n,fill=revenue))+
  geom_line()+
  facet_wrap(~sentiment)+
  guides(fill=F)


## Correlation between emotion expressed and popularity
data%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment, popularity)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  summarize(correlation = cor(n,popularity))
# A tibble: 10 x 2
#sentiment    correlation
#<chr>              <dbl>
#1 anger          0.0724   
#2 anticipation   0.0916   
#3 disgust        0.0422   
#4 fear           0.0434   
#5 joy            0.00223  
#6 negative       0.0456   
#7 positive       0.0746   
#8 sadness        0.0000267
#9 surprise       0.0779   
#10 trust          0.0692   

data%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment, revenue)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  summarize(correlation = cor(n,revenue))

## Correlation between emotion expressed and revenue
data%>%
  unnest_tokens(output = word, input = keywords)%>%
  inner_join(get_sentiments('nrc'))%>%
  group_by(sentiment,revenue)%>%
  count()%>%
  ungroup()%>%
  group_by(sentiment)%>%
  ggplot(aes(x=revenue,y=n))+geom_point()+facet_wrap(~sentiment)+geom_smooth(method='lm',se=F)


## Wordcloud
library(tidyr)
library(wordcloud)
wordcloudData = 
  data%>%
  unnest_tokens(output=word,input=keywords)%>%
  anti_join(stop_words)%>%
  inner_join(get_sentiments('bing'))%>%
  count(sentiment,word,sort=T)%>%
  spread(key=sentiment,value = n,fill=0)%>%
  data.frame()

rownames(wordcloudData) = wordcloudData[,'word']
wordcloudData = wordcloudData[,c('positive','negative')]
set.seed(617)
comparison.cloud(term.matrix = wordcloudData,scale = c(2,0.7),max.words = 100, rot.per=0)

## Explore correlation between popularity and budget,revenue,profit,vote_average
cor(data$popularity,data$revenue) #0.5931425
cor(data$popularity,data$budget) #0.4203454
cor(data$popularity,data$profit) #0.5801792
cor(data$popularity,data$vote_average) #0.2973832

plot(data$popularity,data$revenue)
plot(data$popularity,data$vote_average)

## Top popularity movies (top 20%)
data_top_popularity = subset(data, popularity > quantile(popularity, prob = 1 - 20/100))
str(data_top_popularity)
data_top_revenue = subset(data, revenue > quantile(profit, prob = 1 - 20/100))

## Check budget difference for top popularity and top revenue
popularity_com = cbind(mean(data_top_popularity$budget), mean(data$budget))
barplot(popularity_com, names.arg=c('top_vote_popularity_average','budget_average'))

revenue_com = cbind(mean(data_top_revenue$budget), mean(data$budget))
barplot(revenue_com, names.arg=c('top_revenue_budget_average','budget_average'))






# Analysis of Production Companies & Release Data
library(ggplot2)
library(lubridate)
library(mice)
library(cluster)
library(psych)
library(dplyr)
library(tidyr)
library(RColorBrewer)
library(plyr)

## input the data
setwd("C:/Users/Xu Ning/Desktop/Columbia/2ndSemester/5205Applied Analytics in Frameworks & Methods/GroupProject")
data = read.csv('cleaned_tmdb.csv')
head(data)
summary(data)

## data overview
str(data$production_companies)
plot(data$production_companies)
str(data$production_countries)
plot(data$production_countries)
str(data$release_date)
plot(data$release_date)

## Clustering
library(rpart)
library(rattle)
library(rpart.plot)
library(RColorBrewer)
library(caret)

#data_cluster = data[17:18]
data_cluster = scale(data[, c("budget","popularity", "revenue","runtime", "vote_average","vote_count","profit")])

## Determination of segmentations number
within_ss = sapply(1:10,FUN = function(x) kmeans(x = data_cluster,centers = x)$tot.withinss)
ggplot(data=data.frame(cluster = 1:10,within_ss),aes(x=cluster,y=within_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

ratio_ss = sapply(1:10,FUN = function(x) {km = kmeans(x = data_cluster,centers = x)
km$betweenss/km$totss} )
ggplot(data=data.frame(cluster = 1:10,ratio_ss),aes(x=cluster,y=ratio_ss))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(1,10,1))

silhoette_width = sapply(2:10,FUN = function(x) pam(x = data_cluster,k = x)$silinfo$avg.width)
ggplot(data=data.frame(cluster = 2:10,silhoette_width),aes(x=cluster,y=silhoette_width))+
  geom_line(col='steelblue',size=1.2)+
  geom_point()+
  scale_x_continuous(breaks=seq(2,10,1))

## K-mean clustering

#set.seed(428)
#km = kmeans(x = data_cluster,centers = 2,iter.max=10000,nstart=25)
#k_segments = km$cluster

km = kmeans(na.omit(data_cluster), 2, nstart = 20)
k_segments = as.factor(km$cluster)
table(k_segments)
data = cbind(data,k_segments)

ggplot(data, aes(revenue, vote_average, color = k_segments)) + 
  geom_point()+scale_colour_manual(values=c("green","red")) + 
  xlab("revenue") + ylab("vote_average")


#temp = data.frame(cluster = factor(k_segments),
#                  factor1 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,1],
#                  factor2 = fa(data_cluster,nfactors = 2,rotate = 'varimax')$scores[,2])
#ggplot(temp,aes(x=factor1,y=factor2,col=cluster))+
#  geom_point()

##Release Month and Release Year Analysis
##Add columns of release year and month
data$release_date = as.Date(data$release_date)
data$releaseyear = year(data$release_date)
data$releasemonth = month(data$release_date)

#release month
# prop.table(table(data$k_segments,data[,22]),1)
# 
# lapply(22,function(x) {
#   dat = round(prop.table(table(data$k_segments,data[,x]),1),2)*100
#   dat = data.frame(dat)
#   ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
#     geom_tile()+
#     geom_text(aes(label=Freq),size=6)+
#     xlab(label = '')+
#     ylab(label = '')+
#     scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))
# })

# ggplot(data, aes(releasemonth, vote_average, color = k_segments)) + 
#   geom_point()+scale_colour_manual(values=c("green","red")) + 
#   xlab("release_month") + ylab("vote_average")
# 
# ggplot(data, aes(releasemonth, revenue, color = k_segments)) + 
#   geom_point()+scale_colour_manual(values=c("green","red")) + 
#   xlab("release_month") + ylab("revenue")


# #Remove the number of release year less than 10
# sum_releaseyear = count(data$releaseyear)
# sum_releaseyear$freq
# rm = filter(sum_releaseyear, freq >= 50)
# data$check = data$releaseyear %in% rm$x
# 
# rm_releaseyear = filter(data, check == 1)
# 
# ggplot(rm_releaseyear, aes(releaseyear, vote_average, color = k_segments)) + 
#   geom_point()+scale_colour_manual(values=c("green","red")) + 
#   xlab("release_year") + ylab("vote_average")
# 
# ggplot(rm_releaseyear, aes(releaseyear, revenue, color = k_segments)) + 
#   geom_point()+scale_colour_manual(values=c("green","red")) + 
#   xlab("release_year") + ylab("revenue")
# 
# lapply(19,function(x) {
#   dat = round(prop.table(table(rm_releaseyear$k_segments,rm_releaseyear[,x]),1),2)*100
#   dat = data.frame(dat)
#   ggplot(data=dat,aes(x=Var2,y=Var1,fill=Freq))+
#     geom_tile()+
#     geom_text(aes(label=Freq),size=6)+
#     xlab(label = '')+
#     ylab(label = '')+
#     scale_fill_gradientn(colors=brewer.pal(n=9,name = 'Greens'))
# })
# 
# 
# data2 %>%
#   select(vote_average,k_segments)%>%
#   group_by(k_segments)%>%
#   summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
#   data.frame()
# 
# data2 %>%
#   select(profit,k_segments)%>%
#   group_by(k_segments)%>%
#   summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
#   data.frame()


## Plot Avarage Year Revenue and Month Revenue
mean_month = aggregate(data[, c(11,17)], list(data$releasemonth), mean)

ggplot(mean_month, aes(x = Group.1, y = revenue)) + 
  geom_bar(stat = "identity") +
  xlab("Month") + ylab("Revenue")

ggplot(mean_month, aes(x = Group.1, y = vote_average)) + 
  geom_line(stat = "identity") +
  xlab("Month") + ylab("Vote_Average")

mean_year = aggregate(data[, c(11,17)], list(data$releaseyear), mean)

ggplot(mean_year, aes(x = Group.1, y = revenue)) + 
  geom_bar(stat = "identity") +
  xlab("Year") + ylab("Revenue")

ggplot(mean_year, aes(x = Group.1, y = vote_average)) + 
  geom_line(stat = "identity") +
  xlab("Year") + ylab("Vote_Average")

# data2 %>%
#   select(releaseyear,k_segments)%>%
#   group_by(k_segments)%>%
#   summarize_all(function(x) round(mean(x,na.rm=T),2))%>%
#   gather(key = var,value = value,releaseyear)%>%
#   ggplot(aes(x=var,y=value,fill=factor(k_segments)))+
#   geom_col(position='dodge')+
#   coord_flip()
# 
# data_omit = na.omit(data2)


##Production Coompanies Analysis

# prod_countries = gsub("\\[|\\]", "\\\\", data$production_countries) 
# data = cbind(data,prod_countries)
# data = separate_rows(data,prod_countries,sep = "[^[:alnum:].]+", convert = FALSE)
# summary(data$genres)

library(tidyverse)
data_raw = read_csv('tmdb_5000_movies.csv', guess_max = 3000)

library(stringr)
library(jsonlite)

production_com <- data_raw %>%    
  filter(nchar(production_companies)>2) %>%
  mutate(                            
    js = lapply(production_companies, fromJSON)
  ) %>%                             
  unnest(js) %>%                    
  select(id, title, vote_average,revenue, production_companies=name)  

str(production_com)

## calculate frequencies
t <- table(production_com$production_companies)
## sort
sort_t <- sort(t)
## extract 10 most frequent nationalities
p_top15 <- tail(names(sort_t), 15)
## subset of data frame
p_s <- subset(production_com, production_companies %in% p_top15)
## order factor levels
p_s$production_companies <- factor(p_s$production_companies, levels = rev(p_top15))

## Boxplot of Production Companies
library(ggplot2)
ggplot(p_s, aes(y = revenue, fill = production_companies)) + geom_boxplot()
ggplot(p_s, aes(y = vote_average, fill = production_companies)) + geom_boxplot()

## Number of Movies per Production Companies
plot(table(p_s$production_companies))

# ggplot(prod_comp_wf, aes(x=reorder(word,-freq), y=freq))+ 
#   geom_bar(stat="identity")+
#   theme(axis.text.x = element_text(angle=90),plot.title=element_text(color="Black",face="bold"),legend.position="none")+
#   ggtitle("Distribution of Movies by Production Companies")+
#   xlab("Production Companies")+
#   ylab("Number of Movies")

## Relationship between Vote Average and Revenue depends on Production Companies
library(plotly)
plot_ly(p_s, x = ~vote_average, y = ~revenue, mode = "markers", marker=list( size=3 , opacity=0.5), color = ~production_companies,colors = rainbow(15))


## Production Countries
production_cty <- data_raw %>%    
  filter(nchar(production_countries)>2) %>%     
  mutate(                          
    js = lapply(production_countries, fromJSON) 
  ) %>%                             
  unnest(js) %>%                    
  select(id, title, vote_average,revenue, production_countries=name)  

## Calculate Frequencies
t <- table(production_cty$production_countries)
## sort
sort_t <- sort(t)
## extract 10 most frequent nationalities
p_top15 <- tail(names(sort_t), 15)
## subset of data frame
p_s <- subset(production_cty, production_countries %in% p_top15)
## order factor levels
p_s$production_countries <- factor(p_s$production_countries, levels = rev(p_top15))

## Boxplot of Production Companies
library(ggplot2)
ggplot(p_s, aes(y = revenue, fill = production_countries)) + geom_boxplot()
ggplot(p_s, aes(y = vote_average, fill = production_countries)) + geom_boxplot()

## Number of Movies per Production Companies
plot(table(p_s$production_countries))

## Relationship between Vote Average and Revenue depends on Production Companies
library(plotly)
plot_ly(p_s, x = ~vote_average, y = ~revenue, mode = "markers", marker=list( size=3 , opacity=0.5), color = ~production_countries,colors = rainbow(15))
