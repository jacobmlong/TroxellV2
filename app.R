
library(alphavantager)
library(shiny)
library(tidyverse)
library(readxl)
Troxell0 <- read_excel("TroxellReal2.xls", sheet = "Core Eq Plus")
Troxell <- Troxell0 %>% filter(is.na(`DELETED\nDATE`))
all_tickers <- sort(unique(Troxell$SYMBOL)) 
av_api_key("0PERGX4JVLVXIKUO")


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel(""),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId="ticker",
                  label="Select a stock.",
                  choices = all_tickers,
                  selectize = TRUE)

      ),
    

    
    # Show a plot of the generated distribution
    mainPanel(
      fluidRow(
        column(htmlOutput(outputId = "category_info"), width=4),
        column(htmlOutput(outputId = "star_info"), width=4),
        column(htmlOutput(outputId = "date_info"), width=4)
      ),
      fluidRow(
        column(htmlOutput(outputId = "price_info"), width=4), 
        column(htmlOutput(outputId = "change_info"), width=4),
        column(htmlOutput(outputId = "project_info"), width=4)
      ),
      br(),
      br(),
      br(),
      plotOutput("Priceplot"),
      br(),
      br(),
      br(),
      htmlOutput(outputId = "mean"),
      htmlOutput(outputId = "std_dev")

    )
  )
)



server <- function(input, output) {
  
  T1 <- reactive({
    req(input$ticker)
    Troxell %>%
      filter(`SYMBOL`==input$ticker) %>%
      summarise(stock_ticker=`SYMBOL`, category=`LIST`, name=`COMPANY NAME`, 
                current_stars=`CURRENT\nSTARS`, entered_stars=`ENTRY\nSTARS`,
                entry_date=`ENTRY\nDATE`, target= `12 MO.\nTARGET\nPRICE`) 
  })
  
  T2 <- reactive({
    req(input$ticker) 
     av_get(symbol=input$ticker, av_fun = "TIME_SERIES_DAILY", outputsize="full") %>% 
      filter(timestamp > as.Date(T1()$entry_date))  
  })
  
  T3 <- reactive ({
    (diff(T2()$close)/T2()$close)*100
  })

  output$category_info <- renderUI({
    HTML(paste("<h1>", "<i>", max(T1()$category), "<i/>", "</h1>")
    )
  })
  
  output$star_info <- renderUI ({
    HTML(paste("<h5>", mean(T1()$entered_stars), "Entered Stars", "</h5>"),
    paste("<h5>", mean(T1()$current_stars), "Current Stars", "</h5>")
    )
  })
  
  output$Priceplot <- renderPlot({
    T2() %>%  ggplot(mapping=aes(timestamp,close)) + geom_line(color="darkblue") +
      labs(x="Date", y="Price ($)")  + 
      ggtitle((max(T1()$name))) +
      theme(panel.background = element_blank(),
            panel.grid= element_blank(), axis.ticks= element_blank(),
            plot.title = element_text(face="bold", size= rel(2.5)),
            axis.title= element_blank(), axis.text= element_text(size= rel(1)))
  })
  
  output$price_info <- renderUI({
    HTML(paste("<h5>", "Entered Price:",  (first(T2()$close)), "</h5>"),
         paste("<h5>",  "Current Price:", (last(T2()$close)), "</h5>")
    )
  })
  
  output$change_info <- renderUI ({
    HTML(paste( "<h5>","Price Change:", (last(T2()$close)-first(T2()$close)) %>% round(2), "</h5>"),
        paste( "<h5>","Perc. Change:", ((last(T2()$close)-first(T2()$close))/first(T2()$close)*100) %>% round(2), "%", "</h5>" )
                  )
  })
  
  output$project_info <- renderUI ({
    HTML( paste( "<h5>", "Target Price:", "$", mean(T1()$target)),
          paste( "<h5>", "P. Earnings:",  "$", (mean(T1()$target) -(last(T2()$close))) %>% round(2) )
          )
  })
  
  output$date_info <- renderUI({
    HTML(paste("<h5>", "Date Entered:", first(T2()$timestamp), "</h5>"),
         paste("<h5>", "Last Updated:", last(T2()$timestamp), "</h5>")
    )
  })
  
  output$std_dev <- renderUI ({
    HTML(paste("<h5>", "Standard Deviation:", sd(T3()) %>% round(2), "%", "<h5/>"))
  })
  
  output$mean <- renderUI ({
    HTML(paste("<h5>", "The average daily price movement is", mean(T3()) %>% round(2), "%.", "<h5/>"))
  })
  
}
# Run the application 
shinyApp(ui = ui, server = server)
