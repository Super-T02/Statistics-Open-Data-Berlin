#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list = ls())
setwd("C:/Users/tomfr/OneDrive/Studium/UNI/5. Semester/Data Sience/Prüfung")
library(shiny)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(reshape2)

# Read the data from csv with the ";" as separator
data <- read.csv("./open-data_berlin_Nutzerdaten.csv", sep = ";")
# str(data)
# View(data)

# After looking into the data:
# 1. The Names of the pages with ä,ö,ü,ß are escaped --> First step is to remove the escape
# 2. Every Month has two columns: pi (Page impressions), v (Page Visits)
# 3. Many NA's

####### REMOVE ESCAPED LETTERS START ####### 
# Next: Start with 1. step and remove escaped letters
# - √º = ü
# - √∂ = ö
# - √§ = ä
# - √ü = ß

# ü <- data[grep('√º', data$page), ]
# ö <- data[grep('√∂', data$page), ]
# ä <- data[grep('√§', data$page), ]
ü <- grep('√º', data$page)
ö <- grep('√∂', data$page)
ä <- grep('√§', data$page)

# Search for 'Straße' --> √ü = ß
# View(data[grep('stra', data$page), ])
ß <- grep('√ü', data$page)

# Thesis: Every escaped char starts with '√'. Search for not found escaped chars:
# escaped <- data[grep('√', data$page)]
escaped <- grep('√', data$page)
others <- setdiff(setdiff(setdiff(setdiff(escaped, ß), ü), ö), ä)
# View(data[others, ])
# There exist more:
# √É≈∏ = ß
# √É∆í√Ü‚Äô√É‚Ä¶√Ç¬∏ = ß

# Ö:
# √É¬∂ = ö
# √É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨ √É¬¢√¢‚Äö¬¨√¢‚Äû¬¢√É∆í√Ü‚Äô√É¬¢√¢‚Äö¬¨√Ö¬°√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬∂ = ö
# √É∆í√Ü‚Äô√É‚Äö√Ç¬∂ = ö

# Ü:
# √É¬º = ü
# √É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨ √É¬¢√¢‚Äö¬¨√¢‚Äû¬¢√É∆í√Ü‚Äô√É¬¢√¢‚Äö¬¨√Ö¬°√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬º = ü
# √É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬º = ü
# √É∆í√Ü‚Äô√É‚Äö√Ç¬º = ü
# √É¬É¬º = ü

# Ä:
# √É¬§A4 = ä
# √É∆í√Ü‚Äô√É‚Äö√Ç¬§ = ä
# √É¬§ = ä
# √É∆í√Ç¬§ = ä

others <- setdiff(others, grep('√É≈∏', data$page))
others <- setdiff(others, grep('√É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨ √É¬¢√¢‚Äö¬¨√¢‚Äû¬¢√É∆í√Ü‚Äô√É¬¢√¢‚Äö¬¨√Ö¬°√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬', data$page))
others <- setdiff(others, grep('√É¬∂', data$page))
others <- setdiff(others, grep('√É∆í√Ü‚Äô√É‚Äö√Ç¬∂', data$page))
others <- setdiff(others, grep('√É¬º', data$page))
others <- setdiff(others, grep('√É∆í√Ü‚Äô√É‚Äö√Ç¬º', data$page))
others <- setdiff(others, grep('√É¬§A4', data$page))
others <- setdiff(others, grep('√É∆í√Ü‚Äô√É‚Äö√Ç¬§', data$page))
others <- setdiff(others, grep('√É¬§', data$page))
others <- setdiff(others, grep('√É∆í√Ç¬§', data$page))
others <- setdiff(others, grep('√É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬º', data$page))
others <- setdiff(others, grep('√É¬É¬º', data$page))
# View(data[others,])

# Thesis is a pattern for the most escaped letters:
# - √(something)º = ü Problem:
#                     √É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬º because 2 times °
#                     √É∆í√Ü‚Äô√É‚Ä†√¢‚Ç¨‚Ñ¢√É∆í√¢‚Ç¨ √É¬¢√¢‚Äö¬¨√¢‚Äû¬¢√É∆í√Ü‚Äô√É¬¢√¢‚Äö¬¨√Ö¬°√É∆í√¢‚Ç¨≈°√É‚Äö√Ç¬º
#                     Solution: ° is different to º and º only one time at the end!
# - %C3%BC = ü
# - √(something)∂ = ö Problem: none
# - %C3%B6 = ö
# - √(something)§ = ä Problem: √É¬§A4 and because one more letter
# - %C3%A4 = ä
# - A4 = ä
# - "¬® " = ä
# - √(something)ü or √(something)∏ = ß Problem: should be parsed before ü because it uses ü
# - %C3%9F = ß
#
# Replace letters:
temp <- data[others, "page"]
data[ß, "page"]
data_sub <- data
data_sub$page <- gsub("(√[^A-Za-z]*?[ü∏])|%C3%9F", "ß", data_sub$page) # ß
data_sub$page <- gsub("(√[^A-Za-z]*?º)|%C3%BC", "ü", data_sub$page) # ü
data_sub$page <- gsub("(√[^A-Za-z]*?∂)|%C3%B6", "ö", data_sub$page) # ö
data_sub$page <- gsub("(√[^A-Za-z]*?§(A4)?)|(A4)|¬® ", "ä", data_sub$page) # ä
# View(data_sub)
# Unsolved Problem: ¬ is sometimes in the data but don't know what it does

####### REMOVE ESCAPED LETTERS END ####### 




####### FIND DUPLICATES START #######
# Find the duplicates
data_dup <- data_sub[(duplicated(data_sub$page) | duplicated(data_sub$page, fromLast = T)),]

# Sum the amount of visits and replace the 0's with NA
data_agg <- aggregate(x = data_sub[ , colnames(data_sub) != "page"],             # Mean by group
                      by = list(data_sub$page),
                      FUN = sum,
                      na.rm = TRUE
)
colnames(data_agg)[colnames(data_agg) == 'Group.1'] <- 'page'
data_agg[data_agg == 0] <- NA
####### FIND DUPLICATES END #######





####### Define functions for the app START #######
# Delete the time of a date
monthStart <- function(string) {
  string <- as.POSIXlt(string)
  string$mday <- 1
  string <- as.character(as.Date(string))
  string <- str_split(string, "-")
  paste(string[[1]][1], string[[1]][2], sep = "-")
}

# Function for generating the column names
generateColumnNames <- function(date) {
  string <- str_split(date, "-")
  pi <- paste( "X" , paste(string[[1]][1], string[[1]][2], "pi", sep = "."), sep = "")
  v <- paste( "X" , paste(string[[1]][1], string[[1]][2], "v", sep = "."), sep = "")
  return(c(pi, v))
}

# Function for chart
fun_bar_chart <- function(data_enr_temp, number_to_display, col, month = "", decreasing = T, head_by_v = T) {
  # Order by sum of visits
  if(head_by_v) {
    data_enr_temp <- data_enr_temp[order(eval(parse(text = paste("data_enr_temp", col[1], sep = "$"))), decreasing = decreasing), ]
  } else {
    data_enr_temp <- data_enr_temp[order(eval(parse(text = paste("data_enr_temp", col[2], sep = "$"))), decreasing = decreasing), ]
  }
  
  # Get the 10 highest/lowest per class
  top10 <- data_enr_temp[which(data_enr_temp$page %in% head(data_enr_temp, number_to_display)$page),]
  
  # Make two fields for every variable of each page
  melted <- melt(top10[,c("page", col)], id="page")
  # melted <- melted[order(melted$variable, -melted$value, decreasing = F), ]
  
  # Make a factor to order the data by the number of visits
  melted$page <- factor(melted$page, levels = unique(melted$page[order(melted$variable, melted$value, decreasing = F)]))
  
  # Define Title
  title <- ""
  if(decreasing) {
    title <- paste("Sum of page impressions and visits of the", number_to_display ,"most visited pages in", month, sep = " ")
  } else {
    title <- paste("Sum of page impressions and visits of the", number_to_display ,"least visited pages", month, sep = " ")
  }
  
  # Make plot
  plot <- ggplot(melted, aes(value, page)) +   
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle(title) +
    ylab("Sum") + xlab("Page") +
    scale_fill_discrete(labels=c('Visits', 'Page impressions')) +
    labs(fill='') +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5, size=18),
      axis.title=element_text(size=14,face="bold"),
      text = element_text(size = 20)
      )
  return(plot)
}

####### Define functions for the app END #######






####### APP START #######

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Mothly visits and page impressions of open data pages in Berlin"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider",
                  "Time",
                  min = as.Date("2019-02-01"),
                  max =as.Date("2022-10-01"),
                  value=as.Date("2022-10-01"),
                  timeFormat="%b %Y"
                  ),
      sliderInput("num_of_elements",
                  "Number of elements",
                  min = 5,
                  max = 100,
                  value= 10,
                  step = 1.0
      ),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("most"),
      #tags$br(),
      #tags$br(),
      #tags$br(),
      #tags$br(),
      #tags$br(),
      #tags$br(),
      #tags$br(),
      # plotOutput("least")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  sliderMonth <- reactiveValues()
  num_of_elements <- reactiveValues()
  
  observe({
    full.date <- as.POSIXct(input$slider, tz="GMT")
    sliderMonth$Col <- generateColumnNames(monthStart(full.date))
    sliderMonth$Name <- monthStart(full.date)
  })
  
  observe({
    num_of_elements$num <- input$num_of_elements
    num_of_elements$size <- (input$num_of_elements * 50) + 200
  })
  
  output$most <- renderPlot({
    fun_bar_chart(data_agg, num_of_elements$num, sliderMonth$Col, sliderMonth$Name, T, T)
  }, height = function(){num_of_elements$size})
  
  
  #output$least <- renderPlot({
  #  fun_bar_chart(data_agg, 10, sliderMonth$Col, sliderMonth$Name, F, T)
  #}, height = 500)
}

# Run the application 
shinyApp(ui = ui, server = server)
####### APP END #######

