library(shiny)
library(dplyr)
library(ggplot2)
library(formattable)
library(showtext)
library(htmltools)

# Load the data
df <- read.csv("All_Clusters.csv")

# Define the user interface

font_add_google(name = "Montserrat", family = "Montserrat")
showtext_auto()

main_page <- tabPanel(
  title="",
  titlePanel("Select Filters"),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      selectInput("x_axis", "Select X-Axis Variable:",
                  choices = colnames(df),
                  selected = "BOM_Dependency"),
      selectInput("year", "Select Year:",
                  choices = unique(df$year),
                  selected = 2019),
      checkboxGroupInput("abcd_vars", "Select ABCD Filters:",
                         choices = c("A", "B", "C", "D"),
                         selected = c("A", "B", "C", "D")),
      checkboxGroupInput("wxyz_vars", "Select WXYZ Filters:",
                         choices = c("W", "X", "Y", "Z"),
                         selected = c("W", "X", "Y", "Z")),
      selectInput("cluster", "Select Cluster:",
                  choices = 1:10,
                  selected = 1)
    ),
    mainPanel(
      img(src='ThermoFisher-Logo-Site.jpg', align = "right"),
      tabsetPanel(
        tabPanel(
          title = "Cluster Graphs",
          plotOutput("scatterplot"),
        ),
        tabPanel(
          title="Outlier Table",
          tableOutput("outlier_table"),
          
        )
      )
    )
  ),
  collapsible = TRUE  # Set collapsible to TRUE
  )

ui <- navbarPage(
  title = "Thermo Fisher Scrap Identification",
  main_page,
)

# Define the server logic

server <- function(input, output) {
  
  # Filter the data based on user inputs
  filtered_data <- reactive({
    df %>% 
      filter(year == input$year,
             cluster == input$cluster,
             Sales_Rank_Code %in% input$abcd_vars,
             WXYZ %in% input$wxyz_vars) %>% 
      select(.data[[input$x_axis]], Scrap, outlier, sku_number, unit_cost)
  })
  
  
  # Create the scatter plot
  output$scatterplot <- renderPlot({
    ggplot(filtered_data(), aes(x = .data[[input$x_axis]], y = Scrap, color = factor(outlier))) +
      geom_point() +
      scale_color_manual(values = c("0" = "black", "1" = "red"),labels = c("Normal Scrap Quantity","High Scrap Quantity")) +
      xlab(input$x_axis) +
      ylab("Scrap Quantity") +
      ggtitle(paste0("Identifying Outliers for Cluster ", input$cluster, " in ", input$year))+
      theme(axis.text = element_text(face="bold"),axis.title.x = element_text(size=12, face="bold", colour = "black"),axis.title.y = element_text(size=12, face="bold", colour = "black"),legend.title = element_text(face = "bold"))+
      labs(color = "Outlier Key")+ theme(plot.title = element_text(hjust = 0.5, face = "bold"))+ 
      theme(text=element_text(family="Montserrat")) + theme(legend.title.align=0.5) 
  })
  
  output$outlier_table <- renderTable({
    outliers <- filtered_data() %>% filter(outlier == 1)
    normal <- filtered_data() %>% filter(outlier == 0)
    value <- quantile(normal$Scrap, 0.95)
    outliers %>%
      mutate(Delta = Scrap - value,
             Potential_Savings = currency(Delta * unit_cost)) %>%
      select(sku_number, unit_cost, Delta, Potential_Savings) %>%
      arrange(desc(Potential_Savings))
  })
  
  
  
}


# Run the app
shinyApp(ui, server)


