#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

source("../Open-Data-Berlin.R")
library(shiny)


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
fun_bar_chart <- function(data_enr_temp, number_to_display, col, month = "", decreasing = T) {
  # Order by sum of visits and page impressions
  data_enr_temp <- data_enr_temp[order(data_enr_temp[col[1]], decreasing = decreasing), ]
  
  # Get the 10 highest/lowest per class
  top10 <- data_enr_temp[which(data_enr_temp$page %in% head(data_enr_temp, number_to_display)$page),]
  top10 <- top10[order(top10[col[1]], decreasing = T), ]
  # Make two fields for every variable of each page
  melted <- melt(top10[,c("page", col[1], col[2])], id="page")
  
  # melted <- melted[order(-melted$variable, -melted$value, decreasing = F), ]
  
  # Make a factor to order the data
  melted$page <- factor(melted$page, levels = unique(melted$page),ordered = T)
  
  # Define Title
  title <- ""
  if(decreasing) {
    title <- paste("Die", number_to_display ,"meist genutzten Seiten nach Page Imrpessions in", month, sep = " ")
  } else {
    title <- paste("Die", number_to_display ,"am wenigsten genutzten Seiten nach Page Imrpessions in", month, sep = " ")
  }
  
  # Make plot
  plot <- ggplot(melted, aes(value, page, label=value)) +
    geom_bar(aes(fill = variable), position = "dodge", stat="identity") +
    geom_text( size = 4, hjust=1.3, position=position_dodge2(0.9), color = "white") +
    ggtitle(title) +
    ylab("Seite") + xlab("Summe") +
    scale_fill_manual(values=c("#824f8c", "#e64823"), labels=c('Impressions', 'Visits'))+
    theme_linedraw()+
    labs(fill='') +
    theme(
      legend.position = "top",
      plot.title = element_text(hjust = 0.5, size=18),
      axis.title=element_text(size=14,face="bold"),
      text = element_text(size = 16)
      )
  return(plot)
}
####### Define functions for the app END #######

####### GET ALL DATA START #######
data <- loadData()
data_sub <- substituteData(data)
data_agg <- aggregateData(data_sub)
data_enr <- enrichData(data_agg)
####### GET ALL DATA END #######


####### APP START #######
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Visits und Impressions der pro Monat meist benutzten Seiten"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("slider",
                  "Monat",
                  min = as.Date("2019-02-01"),
                  max =as.Date("2022-10-01"),
                  value=as.Date("2022-10-01"),
                  timeFormat="%b %Y"
                  ),
      sliderInput("num_of_elements",
                  "Anzahl an dargestellten Seiten",
                  min = 5,
                  max = 100,
                  value= 10,
                  step = 1.0
      ),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("most"), #alt: least
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
    fun_bar_chart(data_agg, num_of_elements$num, sliderMonth$Col, sliderMonth$Name, T)
  }, height = function(){num_of_elements$size})
  
  
  output$least <- renderPlot({
    fun_bar_chart(data_agg, num_of_elements$num, sliderMonth$Col, sliderMonth$Name, F)
  }, height = function(){num_of_elements$size})
}

# Run the application 
shinyApp(ui = ui, server = server)
####### APP END #######

