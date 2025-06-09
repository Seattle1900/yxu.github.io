library(shiny)
library(plotly)
library(quantmod) 
library(reshape2)

ui <- fluidPage(
  titlePanel("Healthcare Inustry Stocks"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Choose a Large-scale Comprehensive Pharmaceutical Company from the list bellow:"),
      selectInput("symbol1", "Large-scale Pharmaceutical Stocks:",selected = NULL, multiple = TRUE,
                  choices = c("PFE", "MRK", "JNJ", "ABT", "TMO", "LLY", "BMY", "GILD", "AMGN", "ABBV", "REGN", "BIIB")),
      
      helpText("Choose a Biotechnology Company from the list bellow:"),
      selectInput("symbol2", "Biotechnology Stocks:", 
                  selected = NULL, multiple = TRUE,
                  choices = c("VRTX", "MRNA", "BNTX", "ILMN", "ALNY", "INCY", "NBIX", "NKTR", "IONS")),
      
      helpText("Choose a Generic and Specialty Drug Company from the list bellow:"),
      selectInput("symbol3", "Generic and Specialty Drug Stocks:", 
                  selected = NULL, multiple = TRUE,
                  choices = c("TEVA", "PRGO", "CTLT", "BHC", "JAZZ", "VTRS")),
      
      helpText("Choose a Small and Medium-sized Biotech Company from the list bellow:"),
      selectInput("symbol4", "Small and Medium-sized Biotech Stocks:", 
                  selected = NULL, multiple = TRUE,
                  choices = c("SAGE","EXEL", "UTHR", "ACAD", "LGND", "FOLD")),
    ),
    
    mainPanel(
      plotlyOutput("pharmaceutical"),
      plotlyOutput("biotechnology"),
      plotlyOutput("drug"),
      plotlyOutput("smallbiotech")
    )
  )
)




server <- function(input, output) {
  
  output$pharmaceutical <- renderPlotly({
    symbols <- input$symbol1
    
    if (length(symbols) > 0) {
      stock_list <- lapply(symbols, function(symbol) {
        data <- getSymbols(symbol, auto.assign = FALSE)
        data <- data.frame(Date = index(data), Price = data[, 6])
        data
      })
      merged <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), stock_list)
      plot1 <- melt(merged, id.vars = "Date", variable.name = "Symbol", value.name = "Price")
      plot_ly(data = plot1, x = ~Date, y = ~Price, color = ~Symbol, type = 'scatter', mode = 'lines')
    }
  })
  
  output$biotechnology <- renderPlotly({
    symbols <- input$symbol2
    
    if (length(symbols) > 0) {
      stock_list2 <- lapply(symbols, function(symbol) {
        data <- getSymbols(symbol, auto.assign = FALSE)
        data <- data.frame(Date = index(data), Price = data[, 6])
        data
      })
      merged <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), stock_list2)
      plot2 <- melt(merged, id.vars = "Date", variable.name = "Symbol", value.name = "Price")
      plot_ly(data = plot2, x = ~Date, y = ~Price, color = ~Symbol, type = 'scatter', mode = 'lines')
    }
  })
  
  output$drug <- renderPlotly({
    symbols <- input$symbol3
    
    if (length(symbols) > 0) {
      stock_list3 <- lapply(symbols, function(symbol) {
        data <- getSymbols(symbol, auto.assign = FALSE)
        data <- data.frame(Date = index(data), Price = data[, 6])
        data
      })
      merged <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), stock_list3)
      plot3 <- melt(merged, id.vars = "Date", variable.name = "Symbol", value.name = "Price")
      plot_ly(data = plot3, x = ~Date, y = ~Price, color = ~Symbol, type = 'scatter', mode = 'lines')
    }
  })
  
  output$smallbiotech <- renderPlotly({
    symbols <- input$symbol4
    
    if (length(symbols) > 0) {
      stock_list4 <- lapply(symbols, function(symbol) {
        data <- getSymbols(symbol, auto.assign = FALSE)
        data <- data.frame(Date = index(data), Price = data[, 6])
        data
      })
      merged <- Reduce(function(x, y) merge(x, y, by = "Date", all = TRUE), stock_list4)
      plot4 <- melt(merged, id.vars = "Date", variable.name = "Symbol", value.name = "Price")
      plot_ly(data = plot4, x = ~Date, y = ~Price, color = ~Symbol, type = 'scatter', mode = 'lines')
    }
  })
}

shinyApp(ui = ui, server = server)