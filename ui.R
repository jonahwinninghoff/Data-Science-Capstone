library(shinydashboard)
library(shinyalert)
library(r2d3)
library(rsconnect)
library(networkD3)
library(plotly)
library(tidyr)
library(dplyr)
library(formattable)
library(DT)
library(readr)
library(tidytext)

dashboardPage(skin = "red",
    dashboardHeader(title = "NLP Analysis",
        dropdownMenu(type = "messages",
          messageItem(
             from = "Developer",
             message = "Jonah Winninghoff",
             icon("glasses")
          ),
          messageItem(
             from = "Address",
             message = "Tacoma, WA",
             icon("map-marker-alt")
          ),
          messageItem(
             from = "Email Address",
             message = "jonah.winninghoff@gmail.com",
             icon("envelope")
          ),
          messageItem(
             from = "Phone Number",
             message = "(478)200-8116",
             icon("american-sign-language-interpreting")
          )
          )),
    dashboardSidebar(sidebarMenu(
        menuItem("Prediction", tabName = "prediction1", icon = icon("brain")),
        menuItem(sliderInput("number1","Number of Word Suggestion",1,10,step = 1, value = 3)),
        menuItem("Exploratory Data Analysis", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Charts", tabName = "charts1", icon = icon("chart-bar"),
                 menuSubItem("Bar Chart", tabName = "chart1"),
                 menuSubItem("Table", tabName = "chart2"))
    )),
    dashboardBody(
        tabItems(
            tabItem("dashboard",
                    fluidPage(
                        tabsetPanel(
                            # Blog nodes
                            tabPanel("Blog",useShinyalert(),actionButton("explain1",
                            "What are these nodes and links?"),
                            forceNetworkOutput("BlogsForce", height = "700px")
                            ## Develop beautiful table https://github.com/renkun-ken/formattable
                            ),
                            # Twitter nodes
                            tabPanel("Twitter",useShinyalert(),actionButton("explain2",
                            "What are these nodes and links?"),
                            forceNetworkOutput("TwitterForce", height = "700px")),
                            # News nodes
                            tabPanel("News",useShinyalert(),actionButton("explain3",
                            "What are these nodes and links?"),
                            forceNetworkOutput("NewsForce", height = "700px"))
                        )
                    )),
            tabItem("chart1",
                    tabsetPanel(
                    # Blog bar
                    tabPanel("Blog",
                            plotlyOutput("BlogPlotly", height = "600px"),
                            useShinyalert(), 
                            actionButton("explain4","Bar Chart?")
                        ),
                    # News bar
                    tabPanel("News",
                            plotlyOutput("NewsPlotly",height = "600px"),
                            useShinyalert(), 
                            actionButton("explain5","Bar Chart?")),
                    # Twitter bar
                    tabPanel("Twitter",
                            plotlyOutput("TwitterPlotly",height = "600px"),
                            useShinyalert(), actionButton("explain6",
                            "Bar Chart?")))),
            tabItem("chart2",
                    tabsetPanel(
                    tabPanel("Blog",DTOutput("Ngramblog"),
                    useShinyalert(), actionButton("explain7",
                    "What is this table?")),
                    tabPanel("News",DTOutput("Ngramnews"),
                    useShinyalert(), actionButton("explain8",
                    "What is this table?")),
                    tabPanel("Table",DTOutput("Ngramtwitter"),
                    useShinyalert(), actionButton("explain9",
                    "What is this table?"))
            )),
            tabItem("prediction1",
                    fluidPage(
                    tabsetPanel(
                    tabPanel("Blog",box(title = strong("Word Predictor"), background = "red", solidHeader = TRUE,
                    textInput("blogtext",
                    label = "",value = ""),
                    h4(strong(textOutput("word1")),align="left"),
                    h5(textOutput("wordP1"), align="left"),
                    useShinyalert(),actionButton("explain11",
                    "What is that?")
                    ),column(h3(strong("What is the Machine Learning")),p(style="text-align: justify;","There are several types of machine learnings with many different purposes. For this one, it is an algorithm capable of processing natural language and making prediction of what is next words the user might choose. To make this possible requires undergoing the text mining responsible for structuring dataset and systematically identifying subjective information in order to make this algorithm more effective and predictable. That is, this data is retrieved from", strong("Blog."),"As shown by the sidebar, there are descriptive and interactive data that you can click, zoom in and out, and drag in exploratory data analysis and charts. This algorithm is successful to eliminate the majority of profane languages, but it is just a stepping stone toward the user-friendly interface."),
                    width = 6)),
                    tabPanel("News",box(title = strong("Word Predictor"), background = "red", solidHeader = TRUE,
                    textInput("newstext",
                    label = "", value = ""),
                    h4(strong(textOutput("word2")),align="left"),
                    h5(textOutput("wordP2"), align="left"),
                    useShinyalert(),actionButton("explain10",
                    "What is that?")
                    ),column(h3(strong("What is the Machine Learning"),align="left"),p(style="text-align: justify;","There are several types of machine learnings with many different purposes. For this one, it is an algorithm capable of processing natural language and making prediction of what is next words the user might choose. To make this possible requires undergoing the text mining responsible for structuring dataset and systematically identifying subjective information in order to make this algorithm more effective and predictable. That is, this data is retrieved from" , strong("news."), "As shown by the sidebar, there are descriptive and interactive data that you can click, zoom in and out, and drag in exploratory data analysis and charts. This algorithm is successful to eliminate the majority of profane languages, but it is just a stepping stone toward the user-friendly interface."),
                    width = 6)),
                    tabPanel("Twitter",
                    box(title = strong("Word Predictor"), background = "red", solidHeader = TRUE,
                    textInput("twittertext",
                    label = "", value = ""),h4(strong(textOutput("word3")),align="left"),
                    h5(textOutput("wordP3"), align="left"),
                    useShinyalert(),actionButton("explain12",
                    "What is that?"))
                    ,column(h3(strong("What is the Machine Learning")),p(style="text-align: justify;","There are several types of machine learnings with many different purposes. For this one, it is an algorithm capable of processing natural language and making prediction of what is next words the user might choose. To make this possible requires undergoing the text mining responsible for structuring dataset and systematically identifying subjective information in order to make this algorithm more effective and predictable. That is, this data is retrieved from", strong("Twitter."), "As shown by the sidebar, there are descriptive and interactive data that you can click, zoom in and out, and drag in exploratory data analysis and charts. This algorithm is successful to eliminate the majority of profane languages, but it is just a stepping stone toward the user-friendly interface."),
                    width = 6))
                    )
                    )))
))
