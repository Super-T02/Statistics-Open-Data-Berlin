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
  page_data['Impressions'] <- generateData(page_data$month, data_enr_temp, "pi")
  page_data['Visits'] <- generateData(page_data$month, data_enr_temp, "v")
  page_data['Page impressions - visits'] <- page_data['Impressions'] - page_data['Visits']
  
  # Remove rows with na
  #page_data <- na.omit(page_data)
  
  # Melt the data
  melted <- melt(page_data[,c("month", c("Page impressions - visits", "Visits"))], id="month")
  
  title <- paste("Page impressions und visits von", page, "pro Monat", sep = " ")
  # Make plot
  plot <- ggplot(melted, aes(month, value, fill = variable, label = value)) +   
    geom_col() + 
    geom_text( size = 3, position = position_stack( vjust = 0.5 ), color = "white") +
    ggtitle(title) +
    ylab("Anzahl") + xlab("Monat") +
    scale_fill_manual(values=c("#824f8c", "#e64823"), labels=c('Page impressions - visits', 'Visits'))+
    theme_linedraw()+
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




####### GET ALL DATA START #######
data <- loadData()
data_sub <- substituteData(data)
data_agg <- aggregateData(data_sub)
data_enr <- enrichData(data_agg)
data_enr_sorted <- data_enr[order(data_enr['sum_pi'], decreasing = T), ]
####### GET ALL DATA END #######




####### APP START #######

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Monatlichen Page Visits und Impressions pro Seite von Open Data Berlin"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("select", "Seite", choices = data_enr_sorted$page)
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

