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


####### PREPARE SUMS START####### 
data_enr <- data_agg

# Add the sum of visits | sum of impressions
month_cols_pi <- names(data_enr)[grep("X[0-9]*[.][0-9][0-9][.]pi", names(data_enr))]
month_cols_v <- names(data_enr)[grep("X[0-9]*[.][0-9][0-9][.]v", names(data_enr))]

# Calculate the sums
sum_pi <- apply(data_enr[,month_cols_pi], c(1), function(x) {sum(x, na.rm = T)})
sum_v <- apply(data_enr[,month_cols_v], c(1), function(x) {sum(x, na.rm = T)})
data_enr['sum_pi'] <- sum_pi
data_enr['sum_v'] <- sum_v

# View(data_enr[,c('page','sum_v', 'sum_pi')])
####### PREPARE SUMS END ####### 


####### Define functions for the app START #######

# Function for generating the column names
generateColumnNames <- function(names) {
  names <- names[grep("X[0-9]*[.][0-9]*[.]pi", names)]
  names <- str_split(names, "X")
  result <- c()
  for(name in names) {
    parts <- str_split(name[2], "[.]")
    date <- paste(parts[[1]][1], parts[[1]][2], sep = "-")
    result <- c(result, date)
  }
  return(result)
}

# Function generates the data of the new data frame for displaing it later
# in the plot
generateData <- function(months, dataPage, mode = "pi"){
  vals <- c()
  for (month in months) {
    month <- str_split(month, "-")
    col <- paste("X", paste(month[[1]][1], month[[1]][2], mode, sep = "."), sep = "")
    vals <- c(vals, dataPage[,col])
  }
  return(vals)
}
  

# Function for chart: It will display a dodged bar chart for visits and page impressions
fun_bar_chart <- function(data_enr_temp, page) {
  data_enr_temp <- data_enr_temp[which(data_enr_temp == page),]
  
  # Build the new data frame
  page_data <- data.frame(
    month = unique(generateColumnNames(names(data_enr_temp)))
  )
  page_data['Page impressions'] <- generateData(page_data$month, data_enr_temp, "pi")
  page_data['Visits'] <- generateData(page_data$month, data_enr_temp, "v")
  
  # Remove rows with na
  page_data <- na.omit(page_data)
  
  # Melt the data
  melted <- melt(page_data[,c("month", c("Page impressions", "Visits"))], id="month")
  
  title <- paste("Page impressions and visits of", page, "per month", sep = " ")
  
  # Make plot
  plot <- ggplot(melted, aes(month, value)) +   
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    ggtitle(title) +
    ylab("Amount") + xlab("Month") +
    scale_fill_discrete(labels=c('Page impressions', 'Visits')) +
    labs(fill='') +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5, size=15),
      axis.title=element_text(size=15,face="bold"),
      text = element_text(size = 20),
      axis.text.x = element_text(size= 12, angle = 45, vjust = 1, hjust = 1)
    )
  return(plot)
}

####### Define functions for the app END #######






####### APP START #######

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Mothly visits and page impressions of the 10 most used open data pages in Berlin"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("select", "Page", choices = data_enr[which(data_enr$page %in% head(data_enr[order(data_enr$sum_v, decreasing = T), ], 10)$page),]$page)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("most"),
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$most <- renderPlot({
    fun_bar_chart(data_enr, input$select)
  }, height = 800)
}

# Run the application 
shinyApp(ui = ui, server = server)
####### APP END #######

