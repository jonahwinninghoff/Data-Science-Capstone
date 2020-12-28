library(shiny)

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

blogrank<-paste(getwd(),"/NeatDatasets/blogrank", sep="")
newsrank<-paste(getwd(),"/NeatDatasets/newsrank", sep="")
twitterrank<-paste(getwd(),"/NeatDatasets/twitterrank", sep="")

bloglink<-paste(getwd(),"//NeatDatasets/bloglink", sep="")
newslink<-paste(getwd(),"/NeatDatasets/newslink", sep="")
twitterlink<-paste(getwd(),"/NeatDatasets/twitterlink", sep="")

blognodes<-paste(getwd(),"/NeatDatasets/blognodes", sep="")
newsnodes<-paste(getwd(),"/NeatDatasets/newsnodes", sep="")
twitternodes<-paste(getwd(),"/NeatDatasets/twitternodes", sep="")

ngramblog<-paste(getwd(),"/NeatDatasets/ngramblog", sep="")
ngramnews<-paste(getwd(),"/NeatDatasets/ngramnews", sep="")
ngramtwitter<-paste(getwd(),"/NeatDatasets/ngramtwitter", sep="")

ngramblog<-read.csv(ngramblog);ngramnews<-read.csv(ngramnews)
ngramtwitter<-read.csv(ngramtwitter)

bloglink<-read.csv(bloglink);newslink<-read.csv(newslink)
twitterlink<-read.csv(twitterlink);blognodes<-read.csv(blognodes)
newsnodes<-read.csv(newsnodes);twitternodes<-read.csv(twitternodes)

blogrank<-read.csv(blogrank);newsrank<-read.csv(newsrank)
twitterrank<-read.csv(twitterrank)

blogplotly<-plot_ly(blogrank,x=~gram, y=~relative_frequency, 
  name = ~connotation, type="bar", color=~connotation, 
  colors = c("#DE0D0D", "#7B0000"))%>%
  layout(title = "Highest percentage of each word usage group
  based on angry and happy connotations",
  titlefont = list(family = "Courier",size = 16),
  margin = 10, font = list(family = "Courier", size = 14),
  xaxis = list(categoryarray = ~connotation, 
  categoryorder = "array",tickangle = -90 ,title=""), yaxis = list(
  title = "Percentage in decimal form"), showlegend = T,
  plot_bgcolor='#FFC100',paper_bgcolor='#FFC100')

newsplotly<-plot_ly(newsrank,x=~gram, y=~relative_frequency, 
  name = ~connotation, type="bar", color=~connotation, 
  colors = c("#27D838", "#006409"))%>%
  layout(title = "Highest percentage of each word usage group 
  based on angry and happy connotations",
  titlefont = list(family = "Courier",size = 16),
  margin = 10, font = list(family = "Courier", size = 14),
  xaxis = list(categoryarray = ~connotation, 
  categoryorder = "array",title="", tickangle = -90), yaxis = list(
  title = "Percentage in decimal form"), showlegend = T,
  plot_bgcolor='#00FFD1',paper_bgcolor='#00FFD1')

twitterplotly<-plot_ly(twitterrank,x=~gram, y=~relative_frequency, 
  name = ~connotation, type="bar", color=~connotation, 
  colors = c("#FFA3F5", "#A3BFFF"))%>%
  layout(title = "Highest percentage of each word usages group
  based on angry and happy connotations",
  titlefont = list(family = "Courier",size = 16, color = "FFFFFF"),
  margin = 10, font = list(family = "Courier", size = 14, 
  color = "FFFFFF"), xaxis = list(categoryarray = ~gram, 
  categoryorder = "array",title="", color = '#ffffff', tickangle = -90), yaxis = list(
  title = "Percentage in decimal form", color = '#ffffff'), 
  showlegend = T,plot_bgcolor='#8A47BA',paper_bgcolor='#8A47BA')

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

ngramblog<-select(ngramblog,-X)
ngramnews<-select(ngramnews,-X)
ngramtwitter<-select(ngramtwitter,-X)

ngramblog<-as.datatable(formattable(ngramblog,
  list(area(col = c(n_1, n_2, n_3)) ~ normalize_bar("#FFC3C3", 0.2))))
ngramnews<-as.datatable(formattable(ngramnews,
  list(area(col = c(n_1, n_2, n_3)) ~ normalize_bar("#9EFF9E", 0.2))))
ngramtwitter<-as.datatable(formattable(ngramtwitter,
  list(area(col = c(n_1, n_2, n_3)) ~ normalize_bar("#E8C3FF", 0.2))))

shinyServer(function(input, output) {
    output$BlogsForce <- renderForceNetwork({
      forceNetwork(Links = bloglink, Nodes = blognodes, 
        Source = "source.index", Target = "target.index", 
        NodeID = "name", Group = "group", opacity = 1,
        Nodesize="betweeness", colourScale = ColourScale1,
        charge = -10, zoom = T, fontSize = 30, legend = F)
    })
    output$NewsForce <- renderForceNetwork({
      forceNetwork(Links = newslink, Nodes = newsnodes,
        Source = "source.index", Target = "target.index",
        NodeID = "name", Group = "group", opacity = 1,
        Nodesize="betweeness", colourScale = ColourScale2,
        charge = -10, zoom = T, fontSize = 30, legend = F)
    })
    output$TwitterForce <- renderForceNetwork({
      forceNetwork(Links = twitterlink, Nodes = twitternodes, 
        Source = "source.index", Target = "target.index", 
        NodeID = "name", Group = "group", opacity = 1,
        Nodesize="betweeness", colourScale = ColourScale3,
        charge = -10, zoom = T, fontSize = 30, legend = F)
    })
    observeEvent(input$explain1,{
      shinyalert("What are these nodes and links?", "The unstructured text data retrieved from blog processes and transforms into nodes and links, which are called the word networking. This network is from the authors who would make a linkage between one word and another. The color nodes of words represent negative (red), positive (orange), and undefined (dark) connotations. Do you know that you can zoom out or zoom in if you want to?",
      confirmButtonText = "Got It!",confirmButtonCol = "#FF7E00",type="info",
      closeOnEsc = TRUE,closeOnClickOutside = T)
    })
    observeEvent(input$explain2,{
      shinyalert("What are these nodes and links?", "Here is another network retrieved from news. This network is from the tweeters who would make a linkage between one word and another. The color nodes of words represent negative (bluish purple), positive (purplish pink), and undefined (dark blue) connotations.",
      confirmButtonText = "Uh Huh.",confirmButtonCol = "#FF7E00",type="info",
      closeOnEsc = TRUE,closeOnClickOutside = T)
    })
    observeEvent(input$explain3,{
      shinyalert("What are these nodes and links?", "The final network is from news. This network is from the journalists who would make a linkage between one word and another. The color nodes of words represent negative (green), positive (bluish green), and undefined (dark green) connotations.",
      confirmButtonText = "Alright.",confirmButtonCol = "#FF7E00",type="info",
      closeOnEsc = TRUE,closeOnClickOutside = T)
    })
    observeEvent(input$explain4,{
      shinyalert("Bar Chart?", "The angry and happy connotations are being identified using the NRC Word-Emotion Association Lexicon. The unstructured data retrieved from blog has near 900 thousand long strings and this text string is reduced by 75%. When these angry and happy connotations are identified, its sampling size further reduced by 99.97%, which is the sampling of interest. Each of top ten happy and top ten angry connotations is a percentage of this aggregated sampling.",
      confirmButtonText = "Oh, Ok.",confirmButtonCol = "#FF7E00",type="info",closeOnEsc = TRUE,
      closeOnClickOutside = T)
    })
    output$BlogPlotly<-renderPlotly({blogplotly})
    observeEvent(input$explain5,{
      shinyalert("Bar Chart?", "This procedure is in similar way, however, what is different is that this data retrieved from news has 1.01 million long strings. It is reduced by 75% and then further reduced by 99.98% when the NRC lexicon identifies these connotations. Thus, each connotation group is a percentage of that. If you see two money ('money_1'' and 'money_2') words in each group, this means this word holds two different connotations at once.",
      confirmButtonText = "Let's go",confirmButtonCol = "#FF7E00",type="info",
      closeOnEsc = TRUE,closeOnClickOutside = T)
    })
    output$NewsPlotly<-renderPlotly({newsplotly})
    observeEvent(input$explain6,{
      shinyalert("Bar Chart?", "As mentioned earlier, this procedure is not much different from previous results. But especially, the Twitter data is twice size of news and thus, it needs to be reduced by 90% in order to make text mining manageable. In the result of how many connotations this text string has, this sampling is reduced by 99.97%. Each connotation group is a percentage of this aggregated sampling.",
      confirmButtonText = "Ok.",confirmButtonCol = "#FF7E00",type="info",
      closeOnEsc = TRUE,closeOnClickOutside = T)
    })
    output$TwitterPlotly<-renderPlotly({twitterplotly})
    output$Ngramblog<-renderDT({ngramblog})
    output$Ngramnews<-renderDT({ngramnews})
    output$Ngramtwitter<-renderDT({ngramtwitter})
    observeEvent(input$explain7,{
      shinyalert("What is this table?", "This table is retrieved from the blog. Three groups are based on unigram, bigram, and trigram. This term ‘unigram’ is meant to account how many same words the authors use. The term ‘bigram’ is meant to account how many pairs of words they use. This table ranks these words and sentences based on how often they use and is limited to fifty rows. The terms ‘n_1’, ‘n_2’, and ‘n_3’ represent the relative frequency that is a percentage of total frequency in this table.",
      confirmButtonText = "Alright.",confirmButtonCol = "#FF7E00",type="info",
      closeOnEsc = TRUE,closeOnClickOutside = T)
      })
    observeEvent(input$explain8,{
      shinyalert("What is this table?", "This table is little different from the previous one because it is retrieved from the news. If you want to understand more about how relative frequency calculates, the number of particular word or sentence the journalists use is a numerator and total number of these is a denominator. This factor turns into a percentage.",
      confirmButtonText = "Alright.",confirmButtonCol = "#FF7E00",type="info",
      closeOnEsc = TRUE,closeOnClickOutside = T)
    })
    observeEvent(input$explain9,{
      shinyalert("What is this table?", "As usual, this table is little different from the previous two because it is retrieved from the twitter. If you want to understand more about how the ranking of these works, this R programming uses two different packages (Tidytext and Dplyr). Both are responsible for counting same word or same sentence the tweeters repeatedly use, descending the order of counts, and setting limit to fifty rows.",
      confirmButtonText = "Alright.",confirmButtonCol = "#FF7E00",type="info",
      closeOnEsc = TRUE,closeOnClickOutside = T)
    })
    output$wordP1 <- renderText({
      head(blog(input$blogtext),input$number1)
    })
    output$wordP2 <- renderText({
      head(news(input$newstext),input$number1)
    })
    output$wordP3 <- renderText({
      head(twitter(input$twittertext),input$number1)
    })
    output$word1<-renderText({
      if(input$number1 == 1){
      paste(input$number1, "Word Suggests:", 
      sep = " ") 
      }else{
      paste(input$number1, "Words Suggest:", 
      sep = " ")
      }
    })
    output$word2<-renderText({
      if(input$number1 == 1){
      paste(input$number1, "Word Suggests:", 
      sep = " ") 
      }else{
      paste(input$number1, "Words Suggest:", 
      sep = " ")
      }
    })
    output$word3<-renderText({
      if(input$number1 == 1){
      paste(input$number1,"Word Suggests:", 
      sep = " ") 
      }else{
      paste(input$number1,"Words Suggest:", 
      sep = " ")
      }
    })
    observeEvent(input$explain10,{
      shinyalert("What is that?", "The word predictor is a machine learning that predicts what the next words would be.",
        confirmButtonText = "Oh!",confirmButtonCol = "#FF7E00",type="info",
        closeOnEsc = TRUE,closeOnClickOutside = T)
    })
    observeEvent(input$explain11,{
      shinyalert("What is that?", "The word predictor is a machine learning that predicts what the next words would be.",
      confirmButtonText = "Oh!",confirmButtonCol = "#FF7E00",type="info",
      closeOnEsc = TRUE,closeOnClickOutside = T)
    })
    observeEvent(input$explain12,{
      shinyalert("What is that?", "The word predictor is a machine learning that predicts what the next words would be.",
      confirmButtonText = "Oh!",confirmButtonCol = "#FF7E00",type="info",
      closeOnEsc = TRUE,closeOnClickOutside = T)
    })
})
