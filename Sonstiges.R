####### TASK A) 10 most used pages START#######
# Add the sum of visits | sum of impressions
month_cols <- names(page_impressions)[grep("[0-9]*[.][0-9]", names(page_impressions))]

# Both tables have the same month col names!
month_cols == names(page_visits)[grep("[0-9]*[.][0-9]", names(page_visits))]

# Calculate the sums
sum_pi <- apply(page_impressions[,month_cols], c(1), function(x) {sum(x, na.rm = T)})
sum_v <- apply(page_visits[,month_cols], c(1), function(x) {sum(x, na.rm = T)})
page_impressions['sum'] <- sum_pi
page_visits['sum'] <- sum_v

# Order decreasing by sum
page_impressions <- page_impressions[order(page_impressions$sum, decreasing = T), ]
head(page_impressions$sum)
page_visits <- page_visits[order(page_visits$sum, decreasing = T), ]
head(page_visits$sum)

# Get the 10 highest per class
top10_imperssions <- page_impressions[which(page_impressions$page %in% head(page_impressions, 10)$page),]
top10_visits <- page_visits[which(page_visits$page %in% head(page_visits, 10)$page),]

ggplot(data=top10_imperssions) +
  geom_bar(mapping = aes(top10_imperssions$page))

####### TASK A) 10 most used pages END#######









####### DIVIDE IN TWO TABLES START #######
# For the shiny app the Month must be detected by itself. Goal: two data frames
# One for the page impressions and one for the page visits
cols <- colnames(data_agg)
cols_pi <- cols[grep('pi', cols)]
cols_v <- cols[grep('v', cols)]

page_impressions <- data_agg[ , c('page', cols_pi)]
page_visits <- data_agg[ , c('page', cols_v)]

# Rename the columns
names(page_impressions) <- gsub("X|([.]pi)", "", names(page_impressions))
names(page_visits) <- gsub("X|[.]v", "", names(page_visits))

####### DIVIDE IN TWO TABLES END #######
































# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Mothly visits and page impressions of open data pages in Berlin"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("Month",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',
         xlab = 'Waiting time to next eruption (in mins)',
         main = 'Histogram of waiting times')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



