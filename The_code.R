library(shiny)
library(ggplot2)
library(dplyr)
library(viridis)
library(magrittr)
library(gridExtra)
library(arules)
library(arulesViz)

ui <- fluidPage(
  titlePanel("Combined Shiny App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("path", "Enter the path of the file"),
      h3("Choose Your Chart"),
      selectInput("chart", "Select a chart:", 
                  choices = c("Pie Chart", "Scatter Line Chart", "Bar Chart", "Boxplot", "All Dashboards")),
      hr(),
      p("This dashboard visualizes data interactively."),
      numericInput("clusters", "Number of Clusters:", 3, min = 2, max = 4),
      numericInput("min_support", "Enter Min Support", value = 0.01, min = 0.001, max = 1),
      numericInput("min_confidence", "Enter Min Confidence", value = 0.5, min = 0.001, max = 1),
      actionButton("run_apriori", "Run Apriori"),
      textOutput("validationMessage"),
      textOutput("fileErrorMessage")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Chart", plotOutput("plot")),
        tabPanel("Summary", tableOutput("summary")),
        tabPanel("K-means Clustering", plotOutput("kmeansPlot"), tableOutput("clusterTable")),
        tabPanel("Apriori Algorithm", verbatimTextOutput("rules_output"), plotOutput("rules_plot"))
      )
    )
  )
)

server <- function(input, output) {
  
  observe({
    print(input$chart)
  })
  
  data <- reactive({
    req(input$path)
    read.csv(input$path)
  })
  
  output$plot <- renderPlot({
    req(data())
    df <- data()
    
    if (input$chart == "Pie Chart") { 
      x <- table(df$paymentType)
      pie(x, labels = c("Cash", "Credit"), main = "Pie Chart",
          col = c("steelblue4", "peachpuff1"))
    } else if (input$chart == "Scatter Line Chart") {
      age_total <- df %>%
        group_by(age) %>%
        summarise(total_spending1 = sum(total))
      
      ggplot(age_total, aes(x = age, y = total_spending1)) +
        geom_line(linetype = "solid", linewidth = 0.95) +
        geom_jitter(color = "darkorchid", fill = "orchid", size = 2.3, shape = 21) +
        labs(title = "Scatter Line Chart")
    } else if (input$chart == "Bar Chart") {
      city_total <- df %>%
        group_by(city) %>%
        summarise(total_spending = sum(total)) %>%
        arrange(desc(total_spending))
      
      ggplot(city_total, aes(x = city, y = total_spending)) +
        geom_col(aes(fill = city), show.legend = FALSE) +
        scale_fill_viridis_d("city") +
        labs(x = "City", y = "Total", title = "City vs Total Spending")
    } else if (input$chart == "Boxplot") {
      boxplot(c(500, 1500, 2500), main = "Boxplot", col = "salmon")
    } else if (input$chart == "All Dashboards") {
      x <- table(df$paymentType)
      pie_chart <- ggplotGrob(ggplot(data.frame(x), aes(x = "", y = x, fill = factor(names(x)))) +
                                geom_bar(stat = "identity") +
                                coord_polar(theta = "y") +
                                theme_void() +
                                scale_fill_manual(values = c("steelblue4", "peachpuff1")) +
                                labs(title = "Pie Chart"))
      
      age_total <- df %>%
        group_by(age) %>%
        summarise(total_spending1 = sum(total))
      
      scatter_line_chart <- ggplotGrob(ggplot(age_total, aes(x = age, y = total_spending1)) +
                                         geom_line(linetype = "solid", linewidth = 0.95) +
                                         geom_jitter(color = "darkorchid", fill = "orchid", size = 2.3, shape = 21) +
                                         labs(title = "Scatter Line Chart"))
      
      city_total <- df %>%
        group_by(city) %>%
        summarise(total_spending = sum(total)) %>%
        arrange(desc(total_spending))
      
      bar_chart <- ggplotGrob(ggplot(city_total, aes(x = city, y = total_spending)) +
                                geom_col(aes(fill = city), show.legend = FALSE) +
                                scale_fill_viridis_d("city") +
                                labs(x = "City", y = "Total", title = "City vs Total Spending"))
      
      boxplot_chart <- ggplotGrob(ggplot(data.frame(values = c(500, 1500, 2500)), aes(x = "", y = values)) +
                                    geom_boxplot(fill = "salmon") +
                                    theme_void() +
                                    labs(title = "Boxplot"))
      
      grid.arrange(pie_chart, scatter_line_chart, bar_chart, boxplot_chart, ncol = 2)
    }
  })
  
  output$summary <- renderTable({
    data.frame(
      Chart = c("Pie Chart", "Scatter Line Chart", "Bar Chart", "Boxplot", "All Dashboards"),
      Description = c(
        "Compares two categories",
        "Shows trends over time",
        "Compares categories using bars",
        "Displays data distribution",
        "Displays all charts"
      )
    )
  })
  
  output$validationMessage <- renderText({
    if (input$clusters < 2 || input$clusters > 4) {
      return("Please enter a number between 2 and 4.")
    } else {
      return("")
    }
  })
  
  output$fileErrorMessage <- renderText({
    if (input$path != "") {
      if (!file.exists(input$path)) {
        return("The file path is incorrect or the file does not exist.")
      } else {
        return("")
      }
    }
  })
  
  output$kmeansPlot <- renderPlot({
    if (input$clusters >= 2 && input$clusters <= 4 && input$path != "" && file.exists(input$path)) {
      data <- read.csv(input$path)
      
      customer_data <- data %>%
        group_by(customer, age) %>%
        summarise(total_spending = sum(total))
      
      kmeansResult <- kmeans(customer_data[, c("total_spending", "age")], centers = input$clusters)
      
      customer_data$cluster <- kmeansResult$cluster
      
      plot_data <- customer_data
      
      ggplot(plot_data, aes(x = age, y = total_spending, color = as.factor(cluster))) +
        geom_point(size = 3) +
        labs(color = "Cluster") +
        theme_minimal()
    }
  })
  
  output$clusterTable <- renderTable({
    if (input$clusters >= 2 && input$clusters <= 4 && input$path != "" && file.exists(input$path)) {
      data <- read.csv(input$path)
      
      customer_data <- data %>%
        group_by(customer, age) %>%
        summarise(total_spending = sum(total))
      
      kmeansResult <- kmeans(customer_data[, c("total_spending", "age")], centers = input$clusters)
      
      customer_data$cluster <- kmeansResult$cluster
      
      customer_data
    }
  })
  
  observeEvent(input$run_apriori, {
    if (input$path != "" && file.exists(input$path)) {
      zoro11 <- read.csv(input$path)
      
      if (!"items" %in% colnames(zoro11)) {
        output$rules_output <- renderPrint({ "The file must contain an 'items' column." })
        return()
      }
      
      transection_list <- strsplit(zoro11$items, ",") 
      transactions <- as(transection_list, "transactions")
      
      item_rules <- apriori(transactions, parameter = list(support = input$min_support, confidence = input$min_confidence))
      
      if (length(item_rules) > 0) {
        output$rules_output <- renderPrint({
          inspect(head(item_rules, 10))
        })
        
        output$rules_plot <- renderPlot({
          plot(item_rules, method = "graph", interactive = TRUE)
        })
      } else {
        output$rules_output <- renderPrint({ "No rules found based on the given support and confidence." })
      }
      
    } else {
      output$rules_output <- renderPrint({ "The file path is incorrect or the file does not exist." })
    }
  })
}

shinyApp(ui = ui, server = server)
