## Retrieve the dataset from computer file
blogbigram<-paste(getwd(),"/Raw-and-Relational-Datasets/blogbigram",sep="") ## This should be in dataset file, not Raw-and-Relational-Datasets
newsbigram<-paste(getwd(),"/Raw-and-Relational-Datasets/newsbigram",sep="")
twitterbigram<-paste(getwd(),"/Raw-and-Relational-Datasets/twitterbigram",sep="")
read.csv(blogbigram)->blogbigram
read.csv(newsbigram)->newsbigram
read.csv(twitterbigram)->twitterbigram

## Clean up the dataset
library(lexicon)
library(dplyr)
library(tidyr)
library(tidytext)

blogbigram_counted1 <- blogbigram %>%
  separate(bigram, c("word1", "word2"),sep=" ")%>%
  count(word1, word2, sort = TRUE)%>%slice(1:1000000)
newsbigram_counted1 <- newsbigram %>%
  separate(bigram, c("word1", "word2"),sep=" ")%>%
  count(word1, word2, sort = TRUE)%>%slice(1:1000000)
twitterbigram_counted1 <- twitterbigram %>%
  separate(bigram, c("word1", "word2"),sep=" ")%>%
  count(word1, word2, sort = TRUE)%>%slice(1:1000000)

## Create the function to remove gibberish language
is.word<-function(x) x %in% grady_augmented

## Clean up the blog
isaword1<-c()
isaword2<-c()
for(i in 1:length(unlist(blogbigram_counted1[,1]))){
  is.word(blogbigram_counted1[i,1])->isaword1[i]
  is.word(blogbigram_counted1[i,2])->isaword2[i]
}
tibble(isaword1)->isaword1;tibble(isaword2)->isaword2

nblog<-bind_cols(blogbigram_counted1,isaword1,isaword2)%>%
  filter(isaword1==TRUE)%>%
  filter(isaword2==TRUE)%>%select(-isaword1,-isaword2)
nblog<-filter(nblog,!word1 %in% c(0:10000))%>%
  filter(!word2 %in% c(0:10000))%>%filter(!word1 %in%
  profanity_alvarez)%>%filter(!word2 %in%
  profanity_alvarez)
neatblog<-arrange(nblog,desc(n))%>%
  slice(1:80000)

## Clean up the news
isaword1<-c()
isaword2<-c()
for(i in 1:length(unlist(newsbigram_counted1[,1]))){
  is.word(newsbigram_counted1[i,1])->isaword1[i]
  is.word(newsbigram_counted1[i,2])->isaword2[i]
}
tibble(isaword1)->isaword1;tibble(isaword2)->isaword2

nnews<-bind_cols(newsbigram_counted1,isaword1,isaword2)%>%
  filter(isaword1==TRUE)%>%
  filter(isaword2==TRUE)%>%select(-isaword1,-isaword2)
nnews<-filter(nnews,!word1 %in% c(0:10000))%>%
  filter(!word2 %in% c(0:10000))%>%filter(!word1 %in%
  profanity_alvarez)%>%filter(!word2 %in%
  profanity_alvarez)
neatnews<-arrange(nnews,desc(n))%>%
  slice(1:80000)

## Clean up the twitter
isaword1<-c()
isaword2<-c()
for(i in 1:length(unlist(twitterbigram_counted1[,1]))){
  is.word(twitterbigram_counted1[i,1])->isaword1[i]
  is.word(twitterbigram_counted1[i,2])->isaword2[i]
}
tibble(isaword1)->isaword1;tibble(isaword2)->isaword2

ntwitter<-bind_cols(twitterbigram_counted1,isaword1,isaword2)%>%
  filter(isaword1==TRUE)%>%
  filter(isaword2==TRUE)%>%select(-isaword1,-isaword2)
ntwitter<-filter(ntwitter,!word1 %in% c(0:10000))%>%
  filter(!word2 %in% c(0:10000))%>%filter(!word1 %in%
  profanity_alvarez)%>%filter(!word2 %in%
  profanity_alvarez)
neattwitter<-arrange(ntwitter,desc(n))%>%
  slice(1:80000)

stopblog<-filter(neatblog,word1 %in% stop_words$word)%>%
  filter(word2 %in% stop_words$word)
stopnews<-filter(neatnews,word1 %in% stop_words$word)%>%
  filter(word2 %in% stop_words$word)
stoptwitter<-filter(neattwitter,word1 %in% stop_words$word)%>%
  filter(word2 %in% stop_words$word)

## All neat datasets bind row by row and write the data in csv form
neatblog1<-paste(getwd(),"/NeatDatasets/neatblog",sep="")
neatnews1<-paste(getwd(),"/NeatDatasets/neatnews",sep="")
neattwitter1<-paste(getwd(),"/NeatDatasets/neattwitter",sep="")

stopblog1<-paste(getwd(),"/NeatDatasets/stopblog",sep="")
stopnews1<-paste(getwd(),"/NeatDatasets/stopnews",sep="")
stoptwitter1<-paste(getwd(),"/NeatDatasets/stoptwitter",sep="")

write.csv(neatblog,neatblog1)
write.csv(neatnews,neatnews1)
write.csv(neattwitter,neattwitter1)

write.csv(stopblog,stopblog1)
write.csv(stopnews,stopnews1)
write.csv(stoptwitter,stoptwitter1)

## Read this file
neatblog<-paste(getwd(),"/NeatDatasets/neatblog",sep="")
neatnews<-paste(getwd(),"/NeatDatasets/neatnews",sep="")
neattwitter<-paste(getwd(),"/NeatDatasets/neattwitter",sep="")

stopblog<-paste(getwd(),"/NeatDatasets/stopblog",sep="")
stopnews<-paste(getwd(),"/NeatDatasets/stopnews",sep="")
stoptwitter<-paste(getwd(),"/NeatDatasets/stoptwitter",sep="")

read.csv(neatblog)->neatblog
read.csv(neatnews)->neatnews
read.csv(neattwitter)->neattwitter

read.csv(stopblog)->stopblog
read.csv(stopnews)->stopnews
read.csv(stoptwitter)->stoptwitter

## Machine Learning
blog<-function(x){
  if(x == ""){
    ""
  }else{
    library(dplyr)
    wordsplit<-strsplit(x, " ")%>%data.frame()
    result<-wordsplit[as.numeric(count(wordsplit)),]%>%
      as.character()
    lowerit<-tolower(result)
    if(lowerit %in% stop_words$word){
      filter(stopblog, word1==lowerit)%>%
        arrange(desc(n))%>%select(word2)%>%
        unique()%>%unlist%>%as.character()
    }else{
      filter(neatblog,word1==lowerit)%>%
        arrange(desc(n))%>%select(word2)%>%
        unique()%>%unlist()%>%as.character()
    }
  }
}
news<-function(x){
  if(x == ""){
    ""
  }else{
    library(dplyr)
    wordsplit<-strsplit(x, " ")%>%data.frame()
    result<-wordsplit[as.numeric(count(wordsplit)),]%>%
      as.character()
    lowerit<-tolower(result)
    if(lowerit %in% stop_words$word){
      filter(stopnews, word1==lowerit)%>%
        arrange(desc(n))%>%select(word2)%>%
        unique()%>%unlist%>%as.character()
    }else{
      filter(neatnews,word1==lowerit)%>%
        arrange(desc(n))%>%select(word2)%>%
        unique()%>%unlist()%>%as.character()
    }
  }
}
twitter<-function(x){
  if(x == ""){
    ""
  }else{
    library(dplyr)
    wordsplit<-strsplit(x, " ")%>%data.frame()
    result<-wordsplit[as.numeric(count(wordsplit)),]%>%
      as.character()
    lowerit<-tolower(result)
    if(lowerit %in% stop_words$word){
      filter(stoptwitter, word1==lowerit)%>%
        arrange(desc(n))%>%select(word2)%>%
        unique()%>%unlist%>%as.character()
    }else{
      filter(neattwitter,word1==lowerit)%>%
        arrange(desc(n))%>%select(word2)%>%
        unique()%>%unlist()%>%as.character()
    }
  }
}


