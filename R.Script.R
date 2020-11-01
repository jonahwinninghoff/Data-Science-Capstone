## Obtain and unzip the database
download.file("https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip",
  destfile = "files", method = "curl")
paste(getwd(),"/Raw-and-Relational-Datasets/files",sep= "")->lookat
unzip(lookat)

## Separate raw data storing in three raw datasets from blogs to news to twitter
twitter <- paste(getwd(),"/Raw-and-Relational-Datasets/final/en_US/en_US.twitter.txt",sep="")
news <- paste(getwd(),"/Raw-and-Relational-Datasets/final/en_US/en_US.news.txt",sep="")
blogs <- paste(getwd(),"/Raw-and-Relational-Datasets/final/en_US/en_US.blogs.txt",sep="")

readLines(twitter,length(readLines(twitter,skipNul = T))*.1,
  skipNul = T)->doc1
readLines(news,length(readLines(news,skipNul = T))*.25, skipNul = T)->doc2
readLines(blogs,length(readLines(blogs,skipNul = T))*.25,skipNul = T)->doc3

## Create the logical function for identifying English words 
library(lexicon)
available_data('grady_augmented')
is.word <-function(x) x %in% grady_augmented

## Transform unstructured datasets into structured datasets
library(tidytext)
library(dplyr)

## Look at https://www.tidytextmining.com/ngrams.html
## Processing the relationship between two or more words can be found in chapter 4
text_df1 <- tibble(text = doc1)
text_df2 <- tibble(text = doc2)
text_df3 <- tibble(text = doc3)

## Process unstructured into unigram, bigram, and trigram structured datasets
ngram1 <- text_df1%>%unnest_tokens(gram,text,token = "ngrams",n=1)
bigram1 <- text_df1%>% unnest_tokens(bigram,text,token = "ngrams", n = 2)
trigram1 <- text_df1%>%unnest_tokens(trigram,text,token="ngrams",n=3)

twitter1 <- paste(getwd(),"/Raw-and-Relational-Datasets/twittergram", sep="")
twitter2 <- paste(getwd(),"/Raw-and-Relational-Datasets/twitterbigram", sep="")
twitter3 <- paste(getwd(),"/Raw-and-Relational-Datasets/twittertrigram", sep="")

write.csv(ngram1,twitter1)
write.csv(bigram1,twitter2)
write.csv(trigram1,twitter3)

ngram2 <- text_df2%>%unnest_tokens(gram,text,token = "ngrams",n=1)
bigram2 <- text_df2%>% unnest_tokens(bigram,text,token = "ngrams", n = 2)
trigram2 <- text_df2%>%unnest_tokens(trigram,text,token="ngrams",n=3)

news1 <- paste(getwd(),"/Raw-and-Relational-Datasets/newsgram", sep="")
news2 <- paste(getwd(),"/Raw-and-Relational-Datasets/newsbigram", sep="")
news3 <- paste(getwd(),"/Raw-and-Relational-Datasets/newstrigram", sep="")

write.csv(ngram2,news1)
write.csv(bigram2,news2)
write.csv(trigram2,news3)

ngram3 <- text_df3%>%unnest_tokens(gram,text,token = "ngrams",n=1)
bigram3 <- text_df3%>% unnest_tokens(bigram,text,token = "ngrams", n = 2)
trigram3 <- text_df3%>%unnest_tokens(trigram,text,token="ngrams",n=3)

blog1 <- paste(getwd(),"/Raw-and-Relational-Datasets/bloggram", sep="")
blog2 <- paste(getwd(),"/Raw-and-Relational-Datasets/blogbigram", sep="")
blog3 <- paste(getwd(),"/Raw-and-Relational-Datasets/blogtrigram", sep="")

write.csv(ngram3,blog1)
write.csv(bigram3,blog2)
write.csv(trigram3,blog3)

## Bigram datasets are in use for force graphs
library(tidyr)

bigram_counted1 <- bigram1 %>%
  separate(bigram, c("word1", "word2"),sep=" ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  count(word1, word2, sort = TRUE)

bigram_counted2 <- bigram2 %>%
  separate(bigram, c("word1", "word2"),sep=" ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>%
  count(word1, word2, sort = TRUE)

bigram_counted3 <- bigram3 %>%
  separate(bigram, c("word1", "word2"),sep=" ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>% 
  count(word1, word2, sort = TRUE)

## Remove the gibberish language
isaword1 <- c()
isaword2 <- c()
for(i in 1:length(unlist(bigram_counted1[,1]))){
  is.word(bigram_counted1[i,1])->isaword1[i]
  is.word(bigram_counted1[i,2])->isaword2[i]
}
tibble(isaword1)->isaword1; tibble(isaword2)->isaword2

neatbigram_counted1<-bind_cols(bigram_counted1[,1],isaword1,
  bigram_counted1[,2],isaword2, bigram_counted1[,3])%>%
  filter(isaword1==TRUE)%>%filter(isaword2==TRUE)%>%
  select(-isaword1, -isaword2)%>%slice(1:200000)

isaword3 <- c()
isaword4 <- c()
for(i in 1:length(unlist(bigram_counted2[,1]))){
  is.word(bigram_counted2[i,1])->isaword3[i]
  is.word(bigram_counted2[i,2])->isaword4[i]
}
tibble(isaword3)->isaword3; tibble(isaword4)->isaword4

neatbigram_counted2<-bind_cols(bigram_counted2[,1],isaword3,
  bigram_counted2[,2],isaword4, bigram_counted2[,3])%>%
  filter(isaword3==TRUE)%>%filter(isaword4==TRUE)%>%
  select(-isaword3, -isaword4)

isaword5 <- c()
isaword6 <- c()
for(i in 1:length(unlist(bigram_counted3[,1]))){
  is.word(bigram_counted3[i,1])->isaword5[i]
  is.word(bigram_counted3[i,2])->isaword6[i]
}
tibble(isaword5)->isaword5; tibble(isaword6)->isaword6

neatbigram_counted3<-bind_cols(bigram_counted3[,1],isaword5,
  bigram_counted3[,2],isaword6, bigram_counted3[,3])%>%
  filter(isaword5==TRUE)%>%filter(isaword6==TRUE)%>%
  select(-isaword5, -isaword6)

## Write the neat datasets in csv form
twitter1 <- paste(getwd(),"/Raw-and-Relational-Datasets/twitter", sep="")
news1 <- paste(getwd(),"/Raw-and-Relational-Datasets/news", sep="")
blog1 <- paste(getwd(),"/Raw-and-Relational-Datasets/blog", sep="")

write.csv(neatbigram_counted1,twitter1)
write.csv(neatbigram_counted2,news1)
write.csv(neatbigram_counted3,blog1)

## Set up three informative force graphs
## Look at https://kateto.net/sunbelt2019 for how to make r script
library(networkD3)
library(tidyr)
library(tidytext)
library(dplyr)
twitter <- read.csv(paste(getwd(),"/Raw-and-Relational-Datasets/twitter",sep=""))
news <- read.csv(paste(getwd(),"/Raw-and-Relational-Datasets/news",sep=""))
blog <- read.csv(paste(getwd(),"/Raw-and-Relational-Datasets/blog",sep=""))

as.character(blog$word1)->blog$word1
as.character(blog$word2)->blog$word2
smallblog<-filter(blog, n > 35)%>%select(word1,word2,n)

as.character(twitter$word1)->twitter$word1
as.character(twitter$word2)->twitter$word2
smalltwitter<-filter(twitter, n > 18)%>%select(word1,
  word2,n)

as.character(news$word1)->news$word1
as.character(news$word2)->news$word2
smallnews<-filter(news, n > 69)%>%select(word1,word2,n)

## Build nodes and links
g <- graph.data.frame(smallblog[,c(1,2)],directed=F)
blognodes<-data.frame(
  name = V(g)$name,
  betweeness = (betweenness(g,directed=F,normalized=T)*115)+0.1
) 
smallblog[,c(1,2)]->bloglink; names(bloglink)<-c("source","target")
bloglink$source.index = match(bloglink$source, blognodes$name)-1
bloglink$target.index = match(bloglink$target, blognodes$name)-1

g <- graph.data.frame(smalltwitter[,c(1,2)],directed=F)
twitternodes<-data.frame(
  name = V(g)$name,
  betweeness = (betweenness(g,directed=F,normalized=T)*115)+0.1
) 
smalltwitter[,c(1,2)]->twitterlink; names(twitterlink)<-c("source","target")
twitterlink$source.index = match(twitterlink$source, twitternodes$name)-1
twitterlink$target.index = match(twitterlink$target, twitternodes$name)-1

g <- graph.data.frame(smallnews[,c(1,2)],directed=F)
newsnodes<-data.frame(
  name = V(g)$name,
  betweeness = (betweenness(g,directed=F,normalized=T)*115)+0.1
) 
smallnews[,c(1,2)]->newslink; names(newslink)<-c("source","target")
newslink$source.index = match(newslink$source, newsnodes$name)-1
newslink$target.index = match(newslink$target, newsnodes$name)-1


## Install the positive and negative connotations in these nodes
ne<-function(x) x %in% unlist(filter(get_sentiments("bing"),
  sentiment=="negative"))
po<-function(x) x %in% unlist(filter(get_sentiments("bing"),
  sentiment=="positive"))

negative<-c();positive<-c()
for(i in 1:length(unlist(blognodes$name))){
  ne(blognodes[i,1])->negative[i]
  po(blognodes[i,1])->positive[i]
}
tibble(negative)->negative
tibble(positive)->positive

blognodes<-bind_cols(blognodes,positive,negative)%>%mutate(group = ifelse(
  positive == TRUE, "positive", ifelse(negative == TRUE, "negative", 
  "undefined")))

negative<-c();positive<-c()
for(i in 1:length(unlist(twitternodes$name))){
  ne(twitternodes[i,1])->negative[i]
  po(twitternodes[i,1])->positive[i]
}
tibble(negative)->negative
tibble(positive)->positive

twitternodes<-bind_cols(twitternodes,positive,negative)%>%mutate(group = ifelse(
  positive == TRUE, "positive", ifelse(negative == TRUE, "negative", 
  "undefined")))

negative<-c();positive<-c()
for(i in 1:length(unlist(newsnodes$name))){
  ne(newsnodes[i,1])->negative[i]
  po(newsnodes[i,1])->positive[i]
}
tibble(negative)->negative
tibble(positive)->positive

newsnodes<-bind_cols(newsnodes,positive,negative)%>%mutate(group = ifelse(
  positive == TRUE, "positive", ifelse(negative == TRUE, "negative", 
  "undefined")))

## Brew colors for each forcenetwork
ColourScale1 <- 'd3.scaleOrdinal()
  .domain(["negative", "positive","undefined"])
  .range(["#FF1717", "#E5B81B","#880000"]);'

ColourScale1 <- 'd3.scaleOrdinal()
  .domain(["negative", "positive","undefined"])
  .range(["#FF1717", "#E5B81B","#880000"]);'

ColourScale2 <- 'd3.scaleOrdinal()
  .domain(["negative", "positive","undefined"])
  .range(["#24CF41", "#24CFC4","#006411"]);'

ColourScale3 <- 'd3.scaleOrdinal()
  .domain(["negative", "positive","undefined"])
  .range(["#7E78E3", "#D578E3","#070082"]);'

## Write csv datasets in neatdatasets file
blogl <- paste(getwd(),"/NeatDatasets/bloglink", sep="")
blogn <- paste(getwd(),"/NeatDatasets/blognodes", sep="")

newsl <- paste(getwd(),"/NeatDatasets/newslink", sep="")
newsn <- paste(getwd(),"/NeatDatasets/newsnodes", sep="")

twitterl <- paste(getwd(),"/NeatDatasets/twitterlink", sep="")
twittern <- paste(getwd(),"/NeatDatasets/twitternodes", sep="")

write.csv(bloglink,blogl)
write.csv(blognodes,blogn)
write.csv(newslink,newsl)
write.csv(newsnodes,newsn)
write.csv(twitterlink,twitterl)
write.csv(twitternodes,twittern)

## Before plots create, datasets need to clean
newsgram<-paste(getwd(),"/Raw-and-Relational-Datasets/newsgram",sep="")
bloggram<-paste(getwd(),"/Raw-and-Relational-Datasets/bloggram",sep="")
twittergram<-paste(getwd(),"/Raw-and-Relational-Datasets/twittergram",sep="")

newsgram<-read.csv(newsgram);bloggram<-read.csv(bloggram)
twittergram<-read.csv(twittergram)

bloggram_counted<-bloggram%>%count(gram)
newsgram_counted<-newsgram%>%count(gram)
twittergram_counted<-twittergram%>%count(gram)

## Remove gibberish language
library(lexicon)
available_data('grady_augmented')
is.word <-function(x) x %in% grady_augmented

as.character(bloggram_counted$gram)->bloggram_counted$gram
as.character(newsgram_counted$gram)->newsgram_counted$gram
as.character(twittergram_counted$gram)->twittergram_counted$gram

isaword1 <- c()
isaword2 <- c()
isaword3 <- c()
for(i in 1:length(unlist(bloggram_counted[,1]))){
  is.word(bloggram_counted[i,1])->isaword1[i]
}
for(i in 1:length(unlist(newsgram_counted[,1]))){
  is.word(newsgram_counted[i,1])->isaword2[i]
}
for(i in 1:length(unlist(twittergram_counted[,1]))){
  is.word(twittergram_counted[i,1])->isaword3[i]
}
tibble(isaword1)->isaword1; tibble(isaword2)->isaword2
tibble(isaword3)->isaword3

neatbloggram_counted<-bind_cols(bloggram_counted,isaword1)%>%
  filter(isaword1==TRUE)%>%select(-isaword1)
neatnewsgram_counted<-bind_cols(newsgram_counted,isaword2)%>%
  filter(isaword2==TRUE)%>%select(-isaword2)
neattwittergram_counted<-bind_cols(twittergram_counted,isaword3)%>%
  filter(isaword3==TRUE)%>%select(-isaword3)

## Rank the 7 top angry words and joy words based relative frequency
library(tidyr)
library(dplyr)

an<-function(x) x %in% unlist(filter(get_sentiments("nrc"),sentiment=="anger")[,1])
jo<-function(x) x %in% unlist(filter(get_sentiments("nrc"),sentiment=="joy")[,1])

smallneatbloggram_counted<-filter(neatbloggram_counted, n > 200)
smallneatnewsgram_counted<-filter(neatnewsgram_counted, n > 200)
smallneattwittergram_counted<-filter(neattwittergram_counted, n > 50)

anger1<-c(); joy1<-c()
anger2<-c(); joy2<-c()
anger3<-c(); joy3<-c()
for(i in 1:length(unlist(smallneatbloggram_counted[,1]))){
  an(smallneatbloggram_counted[i,1])->anger1[i]
  jo(smallneatbloggram_counted[i,1])->joy1[i]
}
for(i in 1:length(unlist(smallneatnewsgram_counted[,1]))){
  an(smallneatnewsgram_counted[i,1])->anger2[i]
  jo(smallneatnewsgram_counted[i,1])->joy2[i]
}
for(i in 1:length(unlist(smallneattwittergram_counted[,1]))){
  an(smallneattwittergram_counted[i,1])->anger3[i]
  jo(smallneattwittergram_counted[i,1])->joy3[i]
}
tibble(anger1)->anger1; tibble(joy1)->joy1
tibble(anger2)->anger2; tibble(joy2)->joy2
tibble(anger3)->anger3; tibble(joy3)->joy3

bloganger<-bind_cols(smallneatbloggram_counted,anger1,joy1)%>%
  filter(anger1==TRUE)%>%arrange(desc(n))%>%slice(1:10)
newsanger<-bind_cols(smallneatnewsgram_counted,anger2,joy2)%>%
  filter(anger2==TRUE)%>%arrange(desc(n))%>%slice(1:10)
twitteranger<-bind_cols(smallneattwittergram_counted,anger3,joy3)%>%
  filter(anger3==TRUE)%>%arrange(desc(n))%>%slice(1:10)

blogjoy<-bind_cols(smallneatbloggram_counted,anger1,joy1)%>%
  filter(joy1==TRUE)%>%arrange(desc(n))%>%slice(1:10)
newsjoy<-bind_cols(smallneatnewsgram_counted,anger2,joy2)%>%
  filter(joy2==TRUE)%>%arrange(desc(n))%>%slice(1:10)
twitterjoy<-bind_cols(smallneattwittergram_counted,anger3,joy3)%>%
  filter(joy3==TRUE)%>%arrange(desc(n))%>%slice(1:10)

blogn<-sum(filter(bind_cols(smallneatbloggram_counted,anger1,joy1),
  anger1==TRUE)$n)+sum(filter(bind_cols(smallneatbloggram_counted,anger1,
  joy1),joy1==TRUE)$n)

newsn<-sum(filter(bind_cols(smallneatnewsgram_counted,anger2,joy2),
  anger2==TRUE)$n)+sum(filter(bind_cols(smallneatnewsgram_counted,
  anger2,joy2),joy2==TRUE)$n)

twittern<-sum(filter(bind_cols(smallneattwittergram_counted,anger3,joy3),
  anger3==TRUE)$n)+sum(filter(bind_cols(smallneattwittergram_counted,anger3,
  joy3),joy3==TRUE)$n)

bloganger<-mutate(bloganger,connotation=ifelse(anger1==TRUE,"angry",NULL))
bloganger<-mutate(bloganger,relative_frequency=n/blogn)%>%select(-n,
  -anger1,-joy1)
blogjoy<-mutate(blogjoy,connotation=ifelse(joy1==TRUE,"happy",NULL))
blogjoy<-mutate(blogjoy,relative_frequency=n/blogn)%>%select(-n,
  -anger1,-joy1)
blogrank<-bind_rows(bloganger,blogjoy)

newsanger<-mutate(newsanger,connotation=ifelse(anger2==TRUE,"angry",NULL))
newsanger<-mutate(newsanger,relative_frequency=n/newsn)%>%select(-n,
  -anger2,-joy2)
newsjoy<-mutate(newsjoy,connotation=ifelse(joy2==TRUE,"happy",NULL))
newsjoy<-mutate(newsjoy,relative_frequency=n/newsn)%>%select(-n,
  -anger2,-joy2)
newsjoy[2,1]<-"money_1"; newsanger[1,1]<-"money_2"
newsrank<-bind_rows(newsanger,newsjoy)

twitteranger<-mutate(twitteranger,connotation=ifelse(anger3==TRUE,"angry",
  NULL))
twitteranger<-mutate(twitteranger,relative_frequency=n/twittern)%>%
  select(-n,-anger3,-joy3)
twitterjoy<-mutate(twitterjoy,connotation=ifelse(joy3==TRUE,"happy",NULL))
twitterjoy<-mutate(twitterjoy,relative_frequency=n/twittern)%>%select(-n,
  -anger3,-joy3)
twitterrank<-bind_rows(twitteranger,twitterjoy)

round(blogrank$relative_frequency,3)->blogrank$relative_frequency
round(newsrank$relative_frequency,3)->newsrank$relative_frequency
round(twitterrank$relative_frequency,3)->twitterrank$relative_frequency

## Write the neat datasets into csv form
blogwrite<-paste(getwd(),"/NeatDatasets/blogrank", sep="")
newswrite<-paste(getwd(),"/NeatDatasets/newsrank", sep="")
twitterwrite<-paste(getwd(),"/NeatDatasets/twitterrank", sep="")

write.csv(blogrank,blogwrite)
write.csv(newsrank,newswrite)
write.csv(twitterrank,twitterwrite)

## Build the bar charts using plotly
library(plotly)
blogplotly<-plot_ly(blogrank,y=~gram, x=~relative_frequency, 
  name = ~connotation, type="bar", color=~connotation, 
  colors = c("#DE0D0D", "#7B0000"))%>%
  layout(title = "Highest percentage of each word usage group
  based on angry and happy connotations",
  titlefont = list(family = "Courier",size = 20),
  margin = 10, font = list(family = "Courier", size = 14),
  yaxis = list(categoryarray = ~connotation, 
  categoryorder = "array",title=""), xaxis = list(
  title = "Percentage in decimal form"), showlegend = T,
  plot_bgcolor='#FFC100',paper_bgcolor='#FFC100')

newsplotly<-plot_ly(newsrank,y=~gram, x=~relative_frequency, 
  name = ~connotation, type="bar", color=~connotation, 
  colors = c("#27D838", "#006409"))%>%
  layout(title = "Highest percentage of each word usage group 
  based on angry and happy connotations",
  titlefont = list(family = "Courier",size = 20),
  margin = 10, font = list(family = "Courier", size = 14),
  yaxis = list(categoryarray = ~connotation, 
  categoryorder = "array",title=""), xaxis = list(
  title = "Percentage in decimal form"), showlegend = T,
  plot_bgcolor='#00FFD1',paper_bgcolor='#00FFD1')

twitterplotly<-plot_ly(twitterrank,y=~gram, x=~relative_frequency, 
  name = ~connotation, type="bar", color=~connotation, 
  colors = c("#FFA3F5", "#A3BFFF"))%>%
  layout(title = "Highest percentage of each word usages group
  based on angry and happy connotations",
  titlefont = list(family = "Courier",size = 20, color = "FFFFFF"),
  margin = 10, font = list(family = "Courier", size = 14, 
  color = "FFFFFF"), yaxis = list(categoryarray = ~gram, 
  categoryorder = "array",title="", color = '#ffffff'), xaxis = list(
  title = "Percentage in decimal form", color = '#ffffff'), 
  showlegend = T,plot_bgcolor='#8A47BA',paper_bgcolor='#8A47BA')

## Create data table by merging three different gram datasets at each group
## But at first, I need to clean each bigram_counted and create trigram datasets
library(tidytext)
library(dplyr)
library(tidyr)

blogtrigram<-paste(getwd(),"/Raw-and-Relational-Datasets/blogtrigram", sep="")
newstrigram<-paste(getwd(),"/Raw-and-Relational-Datasets/newstrigram", sep="")
twittertrigram<-paste(getwd(),"/Raw-and-Relational-Datasets/twittertrigram", sep="")

read.csv(blogtrigram)->blogtrigram
read.csv(newstrigram)->newstrigram
read.csv(twittertrigram)->twittertrigram

blogtrigram_counted <- blogtrigram %>%
  separate(trigram, c("word1", "word2","word3"),sep=" ")%>%
  filter(!word1 %in% stop_words$word,
  !word2 %in% stop_words$word,
  !word2 %in% stop_words$word)%>%
  count(word1, word2, word3, sort = TRUE)

newstrigram_counted <- newstrigram %>%
  separate(trigram, c("word1", "word2","word3"),sep=" ")%>%
  filter(!word1 %in% stop_words$word,
  !word2 %in% stop_words$word,
  !word2 %in% stop_words$word)%>%
  count(word1, word2, word3, sort = TRUE)

twittertrigram_counted <- twittertrigram %>%
  separate(trigram, c("word1", "word2","word3"),sep=" ")%>%
  filter(!word1 %in% stop_words$word,
  !word2 %in% stop_words$word,
  !word2 %in% stop_words$word)%>%
  count(word1, word2, word3, sort = TRUE)

smallblog<-arrange(blogtrigram_counted,desc(n))%>%slice(1:200)
smallnews<-arrange(newstrigram_counted,desc(n))%>%slice(1:200)
smalltwitter<-arrange(twittertrigram_counted,desc(n))%>%slice(1:200)

## Remove the gibberish language
library(lexicon)
is.word<-function(x) x %in% grady_augmented

bisaword1<-c()
nisaword1<-c()
tisaword1<-c()
bisaword2<-c()
nisaword2<-c()
tisaword2<-c()
bisaword3<-c()
nisaword3<-c()
tisaword3<-c()

for(i in 1:length(unlist(smallblog[,1]))){
  is.word(smallblog[i,1])->bisaword1[i]
  is.word(smallblog[i,2])->bisaword2[i]
  is.word(smallblog[i,3])->bisaword3[i]
  
}
for(i in 1:length(unlist(smallnews[,1]))){
  is.word(smallnews[i,1])->nisaword1[i]
  is.word(smallnews[i,2])->nisaword2[i]
  is.word(smallnews[i,3])->nisaword3[i]
}
for(i in 1:length(unlist(smalltwitter[,1]))){
  is.word(smalltwitter[i,1])->tisaword1[i]
  is.word(smalltwitter[i,2])->tisaword2[i]
  is.word(smalltwitter[i,3])->tisaword3[i]
}
tibble(bisaword1)->bisaword1;tibble(bisaword2)->bisaword2
tibble(bisaword3)->bisaword3;tibble(nisaword1)->nisaword1
tibble(nisaword2)->nisaword2;tibble(nisaword3)->nisaword3
tibble(tisaword1)->tisaword1;tibble(tisaword2)->tisaword2
tibble(tisaword3)->tisaword3

neatblogtrigram<-bind_cols(smallblog,bisaword1,
  bisaword2,bisaword3)%>%
  filter(bisaword1==T)%>%filter(bisaword2==T)%>%
  filter(bisaword3==T)%>%select(-bisaword1,-bisaword2,
  -bisaword3)

neatnewstrigram<-bind_cols(smallnews,nisaword1,
  nisaword2,nisaword3)%>%
  filter(nisaword1==T)%>%filter(nisaword2==T)%>%
  filter(nisaword3==T)%>%select(-nisaword1,-nisaword2,
  -nisaword3)

neattwittertrigram<-bind_cols(smalltwitter,tisaword1,
  tisaword2,tisaword3)%>%
  filter(tisaword1==T)%>%filter(tisaword2==T)%>%
  filter(tisaword3==T)%>%select(-tisaword1,-tisaword2,
  -tisaword3)

neatblogtrigram1<-unite(neatblogtrigram,"trigram",c(word1,word2,word3),
  sep = " ")%>%arrange(desc(n))%>%slice(1:50)
names(neatblogtrigram1)[2]<-"n_3"
neatnewstrigram1<-unite(neatnewstrigram,"trigram",c(word1,word2,word3),
  sep = " ")%>%arrange(desc(n))%>%slice(1:50)
names(neatnewstrigram1)[2]<-"n_3"
neattwittertrigram1<-unite(neattwittertrigram,"trigram",c(word1,word2,word3),
  sep = " ")%>%arrange(desc(n))%>%slice(1:50)
names(neattwittertrigram1)[2]<-"n_3"

## Clean bigram and ngram datasets
newsgram<-paste(getwd(),"/Raw-and-Relational-Datasets/newsgram",sep="")
bloggram<-paste(getwd(),"/Raw-and-Relational-Datasets/bloggram",sep="")
twittergram<-paste(getwd(),"/Raw-and-Relational-Datasets/twittergram",sep="")

newsgram<-read.csv(newsgram);bloggram<-read.csv(bloggram)
twittergram<-read.csv(twittergram)

bloggram_counted<-bloggram%>%filter(!gram %in% stop_words$word)%>%count(gram)
newsgram_counted<-newsgram%>%filter(!gram %in% stop_words$word)%>%count(gram)
twittergram_counted<-twittergram%>%filter(!gram %in% stop_words$word)%>%count(gram)

as.character(bloggram_counted$gram)->bloggram_counted$gram
as.character(newsgram_counted$gram)->newsgram_counted$gram
as.character(twittergram_counted$gram)->twittergram_counted$gram

bloggram_counted<-arrange(bloggram_counted,desc(n))%>%
  slice(1:200)
newsgram_counted<-arrange(newsgram_counted,desc(n))%>%
  slice(1:200)
twittergram_counted<-arrange(twittergram_counted,desc(n))%>%
  slice(1:200)
  
isaword1 <- c()
isaword2 <- c()
isaword3 <- c()
for(i in 1:length(unlist(bloggram_counted[,1]))){
  is.word(bloggram_counted[i,1])->isaword1[i]
}
for(i in 1:length(unlist(newsgram_counted[,1]))){
  is.word(newsgram_counted[i,1])->isaword2[i]
}
for(i in 1:length(unlist(twittergram_counted[,1]))){
  is.word(twittergram_counted[i,1])->isaword3[i]
}
tibble(isaword1)->isaword1; tibble(isaword2)->isaword2
tibble(isaword3)->isaword3

neatbloggram_counted<-bind_cols(bloggram_counted,isaword1)%>%
  filter(isaword1==TRUE)%>%select(-isaword1)%>%arrange(desc(n))%>%
  slice(1:50)
neatnewsgram_counted<-bind_cols(newsgram_counted,isaword2)%>%
  filter(isaword2==TRUE)%>%select(-isaword2)%>%arrange(desc(n))%>%
  slice(1:50)
neattwittergram_counted<-bind_cols(twittergram_counted,isaword3)%>%
  filter(isaword3==TRUE)%>%select(-isaword3)%>%arrange(desc(n))%>%
  slice(1:50)

names(neatbloggram_counted)[1]<-"unigram"
names(neatbloggram_counted)[2]<-"n_1"
names(neatnewsgram_counted)[1]<-"unigram"
names(neatnewsgram_counted)[2]<-"n_1"
names(neattwittergram_counted)[1]<-"unigram"
names(neattwittergram_counted)[2]<-"n_1"

newsbigram<-paste(getwd(),"/Raw-and-Relational-Datasets/newsbigram",sep="")
blogbigram<-paste(getwd(),"/Raw-and-Relational-Datasets/blogbigram",sep="")
twitterbigram<-paste(getwd(),"/Raw-and-Relational-Datasets/twitterbigram",sep="")

newsbigram<-read.csv(newsbigram);blogbigram<-read.csv(blogbigram)
twitterbigram<-read.csv(twitterbigram)

blogbigram_counted <- blogbigram %>%
  separate(bigram, c("word1", "word2"),sep=" ")%>%
  filter(!word1 %in% stop_words$word)%>%
  filter(!word2 %in% stop_words$word)%>%
  count(word1, word2, sort = TRUE)

newsbigram_counted <- newsbigram %>%
  separate(bigram, c("word1", "word2"),sep=" ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>%
  count(word1, word2, sort = TRUE)

twitterbigram_counted <- twitterbigram %>%
  separate(bigram, c("word1", "word2"),sep=" ")%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)%>% 
  count(word1, word2, sort = TRUE)

blogbigram_counted1<-arrange(blogbigram_counted,desc(n))%>%
  slice(1:200)
newsbigram_counted1<-arrange(newsbigram_counted,desc(n))%>%
  slice(1:200)
twitterbigram_counted1<-arrange(twitterbigram_counted,desc(n))%>%
  slice(1:200)

isaword1 <- c()
isaword2 <- c()
for(i in 1:length(unlist(blogbigram_counted1[,1]))){
  is.word(blogbigram_counted[i,1])->isaword1[i]
  is.word(blogbigram_counted[i,2])->isaword2[i]
}
tibble(isaword1)->isaword1; tibble(isaword2)->isaword2

neatblogbigram_counted<-bind_cols(blogbigram_counted1,
  isaword1,isaword2)%>%filter(isaword1==TRUE)%>%
  filter(isaword2==TRUE)%>%select(-isaword1, -isaword2)

isaword1 <- c()
isaword2 <- c()
for(i in 1:length(unlist(newsbigram_counted1[,1]))){
  is.word(newsbigram_counted[i,1])->isaword1[i]
  is.word(newsbigram_counted[i,2])->isaword2[i]
}
tibble(isaword1)->isaword1; tibble(isaword2)->isaword2

neatnewsbigram_counted<-bind_cols(newsbigram_counted1,
  isaword1,isaword2)%>%filter(isaword1==TRUE)%>%
  filter(isaword2==TRUE)%>%select(-isaword1, -isaword2)

isaword1 <- c()
isaword2 <- c()
for(i in 1:length(unlist(twitterbigram_counted1[,1]))){
  is.word(twitterbigram_counted[i,1])->isaword1[i]
  is.word(twitterbigram_counted[i,2])->isaword2[i]
}
tibble(isaword1)->isaword1; tibble(isaword2)->isaword2

neattwitterbigram_counted<-bind_cols(twitterbigram_counted1,
  isaword1,isaword2)%>%filter(isaword1==TRUE)%>%
  filter(isaword2==TRUE)%>%select(-isaword1, -isaword2)

neatblogbigram1<-unite(neatblogbigram_counted,"bigram",
  c(word1,word2),sep=" ")%>%arrange(desc(n))%>%slice(1:50)
names(neatblogbigram1)[2]<-"n_2"
neatnewsbigram1<-unite(neatnewsbigram_counted,"bigram",
  c(word1,word2),sep=" ")%>%arrange(desc(n))%>%slice(1:50)
names(neatnewsbigram1)[2]<-"n_2"
neattwitterbigram1<-unite(neattwitterbigram_counted,
  "bigram",c(word1,word2),sep=" ")%>%arrange(desc(n))%>%slice(1:50)
names(neattwitterbigram1)[2]<-"n_2"

## Bind ngram, bigram, and trigram datasets
ngramblog<-bind_cols(neatbloggram_counted,neatblogbigram1,neatblogtrigram1)
ngramnews<-bind_cols(neatnewsgram_counted,neatnewsbigram1,neatnewstrigram1)
ngramtwitter<-bind_cols(neattwittergram_counted,neattwitterbigram1,neattwittertrigram1)

ngramblog<-select(ngramblog,-X)
ngramnews<-select(ngramnews,-X)
ngramtwitter<-select(ngramtwitter,-X)

round(ngramblog$n_1/sum(ngramblog$n_1),3)->ngramblog$n_1
round(ngramblog$n_2/sum(ngramblog$n_2),3)->ngramblog$n_2
round(ngramblog$n_3/sum(ngramblog$n_3),3)->ngramblog$n_3

round(ngramnews$n_1/sum(ngramnews$n_1),3)->ngramnews$n_1
round(ngramnews$n_2/sum(ngramnews$n_2),3)->ngramnews$n_2
round(ngramnews$n_2/sum(ngramnews$n_2),3)->ngramnews$n_3

round(ngramtwitter$n_1/sum(ngramtwitter$n_1),3)->ngramtwitter$n_1
round(ngramtwitter$n_2/sum(ngramtwitter$n_2),3)->ngramtwitter$n_2
round(ngramtwitter$n_3/sum(ngramtwitter$n_3),3)->ngramtwitter$n_3

blog<-paste(getwd(),"/NeatDatasets/ngramblog",sep="")
news<-paste(getwd(),"/NeatDatasets/ngramnews",sep="")
twitter<-paste(getwd(),"/NeatDatasets/ngramtwitter",sep="")

write.csv(ngramblog,blog)
write.csv(ngramnews,news)
write.csv(ngramtwitter,twitter)

## Neat list of necessary functions for R shiny
library(lexicon)
library(dplyr)
library(plotly)
library(networkD3)
library(tidyr)
library(formattable)

blogrank<-paste(getwd(),"/NeatDatasets/blogrank", sep="")
newsrank<-paste(getwd(),"/NeatDatasets/newsrank", sep="")
twitterrank<-paste(getwd(),"/NeatDatasets/twitterrank", sep="")

blogrank<-read.csv(blogrank);newsrank<-read.csv(newsrank)
twitterrank<-read.csv(twitterrank)

blogplotly<-plot_ly(blogrank,y=~gram, x=~relative_frequency, 
  name = ~connotation, type="bar", color=~connotation, 
  colors = c("#DE0D0D", "#7B0000"))%>%
  layout(title = "Highest percentage of each word usage group
  based on angry and happy connotations",
  titlefont = list(family = "Courier",size = 16),
  margin = 10, font = list(family = "Courier", size = 14),
  yaxis = list(categoryarray = ~connotation, 
  categoryorder = "array",title=""), xaxis = list(
  title = "Percentage in decimal form"), showlegend = T,
  plot_bgcolor='#FFC100',paper_bgcolor='#FFC100')

newsplotly<-plot_ly(newsrank,y=~gram, x=~relative_frequency, 
  name = ~connotation, type="bar", color=~connotation, 
  colors = c("#27D838", "#006409"))%>%
  layout(title = "Highest percentage of each word usage group 
  based on angry and happy connotations",
  titlefont = list(family = "Courier",size = 16),
  margin = 10, font = list(family = "Courier", size = 14),
  yaxis = list(categoryarray = ~connotation, 
  categoryorder = "array",title=""), xaxis = list(
  title = "Percentage in decimal form"), showlegend = T,
  plot_bgcolor='#00FFD1',paper_bgcolor='#00FFD1')

twitterplotly<-plot_ly(twitterrank,y=~gram, x=~relative_frequency, 
  name = ~connotation, type="bar", color=~connotation, 
  colors = c("#FFA3F5", "#A3BFFF"))%>%
  layout(title = "Highest percentage of each word usages group
  based on angry and happy connotations",
  titlefont = list(family = "Courier",size = 16, color = "FFFFFF"),
  margin = 10, font = list(family = "Courier", size = 14, 
  color = "FFFFFF"), yaxis = list(categoryarray = ~gram, 
  categoryorder = "array",title="", color = '#ffffff'), xaxis = list(
  title = "Percentage in decimal form", color = '#ffffff'), 
  showlegend = T,plot_bgcolor='#8A47BA',paper_bgcolor='#8A47BA')

bloglink<-paste(getwd(),"/NeatDatasets/bloglink", sep="")
newslink<-paste(getwd(),"/NeatDatasets/newslink", sep="")
twitterlink<-paste(getwd(),"/NeatDatasets/twitterlink", sep="")

blognodes<-paste(getwd(),"/NeatDatasets/blognodes", sep="")
newsnodes<-paste(getwd(),"/NeatDatasets/newsnodes", sep="")
twitternodes<-paste(getwd(),"/NeatDatasets/twitternodes", sep="")

bloglink<-read.csv(bloglink);newslink<-read.csv(newslink)
twitterlink<-read.csv(twitterlink);blognodes<-read.csv(blognodes)
newsnodes<-read.csv(newsnodes);twitternodes<-read.csv(twitternodes)

ColourScale1 <- 'd3.scaleOrdinal()
  .domain(["negative", "positive","undefined"])
  .range(["#FF1717", "#E5B81B","#880000"]);'

ColourScale1 <- 'd3.scaleOrdinal()
  .domain(["negative", "positive","undefined"])
  .range(["#FF1717", "#E5B81B","#880000"]);'

ColourScale2 <- 'd3.scaleOrdinal()
  .domain(["negative", "positive","undefined"])
  .range(["#24CF41", "#24CFC4","#006411"]);'

ColourScale3 <- 'd3.scaleOrdinal()
  .domain(["negative", "positive","undefined"])
  .range(["#7E78E3", "#D578E3","#070082"]);'

library(formattable)

ngramblog<-paste(getwd(),"/NeatDatasets/ngramblog", sep="")
ngramnews<-paste(getwd(),"/NeatDatasets/ngramnews", sep="")
ngramtwitter<-paste(getwd(),"/NeatDatasets/ngramtwitter", sep="")

ngramblog<-read.csv(ngramblog);ngramnews<-read.csv(ngramnews)
ngramtwitter<-read.csv(ngramtwitter)

ngramblog<-select(ngramblog,-X)
ngramnews<-select(ngramnews,-X)
ngramtwitter<-select(ngramtwitter,-X)

ngramblog<-as.datatable(formattable(ngramblog,
  list(area(col = c(n_1, n_2, n_3)) ~ normalize_bar("#FFC3C3", 0.2))))
ngramnews<-as.datatable(formattable(ngramnews,
  list(area(col = c(n_1, n_2, n_3)) ~ normalize_bar("#9EFF9E", 0.2))))
ngramtwitter<-as.datatable(formattable(ngramtwitter,
  list(area(col = c(n_1, n_2, n_3)) ~ normalize_bar("#E8C3FF", 0.2))))
