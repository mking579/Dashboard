library(shiny)
library(shinythemes)
library(shinyWidgets)
library(shinycssloaders)
library(ggplot2)
library(dplyr)
library(tidyr)
library(viridis)

# Load the CSV files
inequality_data <- read.csv("Inequality.csv")
gini_data <- read.csv("Gini.csv")
income_shares_data <- read.csv("Income_Shares.csv")
healthcare_data <- read.csv("Healthcare_Expenditures.csv")
voter_turnout_data <- read.csv("Voter_Turnout_by_State.csv")
incarceration_data <- read.csv("Incarceration_Counts_Rates.csv")
sug_migration_data <- read.csv("SUG_Migration.csv")

# Define UI
ui <- navbarPage(
  theme = shinytheme("sandstone"),  # Apply Sandstone theme
  title = "Dashboard",
  
  # About Tab
  tabPanel("About",
           icon = icon("info-circle"),
           fluidPage(
             h3("About"),
             p("Welcome to Dr. King's Dashboard. This is a work in progress. It features an introduction to different data on inequality, voting, healthcare, and more.             for various topics, including inequality, income shares, healthcare expenditures, voter turnout, 
               and more. Use the tabs above to navigate through the different sections.")
                     )
  ),
  
  # Classes Tab
  tabPanel("Classes",
           icon = icon("book"),
           fluidPage(
             h3("Classes"),
             p("Resources"),
             tags$ul(
               tags$li(tags$a(href = "https://static1.squarespace.com/static/6175920475097a732cd0ae90/t/6644eab7fe72bb2d5b644b3b/1715792567078/APSA+Style+Guide.pdf", "Guide to the APSA Citation Format")),
             )
           )
  ),
  
  # Tab 1: Inequality
  tabPanel("Inequality",
           icon = icon("chart-bar"),
           sidebarLayout(
             sidebarPanel(
               h4("Customize Plot"),
               selectInput("x_variable", "Select X-axis Variable:",
                           choices = c("Disenfranchisement_Rank", "Environmental_Rank", "Arts_Dollars_Rank",
                                       "Total_Incarceration_Rate", "Black_Homeownership_Rate", 
                                       "Legislative_Professionalism", "Union_Membership_Rate_2018", 
                                       "White_Homeownership_Rate", "Black_Pop_Percent_2018", 
                                       "Gini_2018", "PerCap_Educ_Spending_2018", "GDP_2023_Growth_Rank", 
                                       "Black_BA%_Rank", "Cost_of_Voting_Index_Rank", "GDP_Capita_Rank", 
                                       "Labor_Force_Participation_Rate_Rank", 
                                       "Denial_Rates_Black_Homeowner_Applicants"),
                           selected = "GDP_Capita_Rank"),
               selectInput("y_variable", "Select Y-axis Variable:",
                           choices = c("Disenfranchisement_Rank", "Environmental_Rank", "Arts_Dollars_Rank",
                                       "Total_Incarceration_Rate", "Black_Homeownership_Rate", 
                                       "Legislative_Professionalism", "Union_Membership_Rate_2018", 
                                       "White_Homeownership_Rate", "Black_Pop_Percent_2018", 
                                       "Gini_2018", "PerCap_Educ_Spending_2018", "GDP_2023_Growth_Rank", 
                                       "Black_BA%_Rank", "Cost_of_Voting_Index_Rank", "GDP_Capita_Rank", 
                                       "Labor_Force_Participation_Rate_Rank", 
                                       "Denial_Rates_Black_Homeowner_Applicants"),
                           selected = "GDP_2023_Growth_Rank")
             ),
             mainPanel(
               plotOutput("inequality_plot") %>% withSpinner(color = "#007bff")
             )
           )
  ),
  
  # Tab 2: Gini
  tabPanel("Gini",
           icon = icon("balance-scale"),
           sidebarLayout(
             sidebarPanel(
               selectInput("gini_states", "Select States:",
                           choices = unique(gini_data$State),
                           selected = unique(gini_data$State)[1:3],
                           multiple = TRUE),
               selectInput("gini_variable", "Select Variable:",
                           choices = c("Atkin05", "Gini", "RMeanDev", "Theil"),
                           selected = "Gini")
             ),
             mainPanel(
               plotOutput("gini_plot") %>% withSpinner(color = "#007bff")
             )
           )
  ),
  
  # Tab 3: Income Shares
  tabPanel("Income Shares",
           icon = icon("dollar-sign"),
           sidebarLayout(
             sidebarPanel(
               selectInput("income_states", "Select States:",
                           choices = unique(income_shares_data$State),
                           selected = unique(income_shares_data$State)[1:3],
                           multiple = TRUE),
               selectInput("income_variable", "Select Income Variable:",
                           choices = c("Top10_adj", "Top5_adj", "Top1_adj", "Top05_adj", "Top01_adj", "Top001_adj"),
                           selected = "Top10_adj")
             ),
             mainPanel(
               plotOutput("income_shares_plot") %>% withSpinner(color = "#007bff")
             )
           )
  ),
  
  # Tab 4: Healthcare Expenditures
  tabPanel("Healthcare Expenditures",
           icon = icon("heartbeat"),
           sidebarLayout(
             sidebarPanel(
               h4("Healthcare Expenditures Overview"),
               p("This chart displays trends in out-of-pocket expenditures and year-over-year percentage changes.")
             ),
             mainPanel(
               plotOutput("healthcare_plot") %>% withSpinner(color = "#007bff")
             )
           )
  ),
  
  # Tab 5: Voter Turnout by State
  tabPanel("Voter Turnout by State",
           icon = icon("users"),
           sidebarLayout(
             sidebarPanel(
               selectInput("voter_states", "Select States:",
                           choices = sort(unique(voter_turnout_data$State)),
                           selected = sort(unique(voter_turnout_data$State))[1:3],
                           multiple = TRUE),
               selectInput("voter_variable", "Select Variable:",
                           choices = colnames(voter_turnout_data)[!(colnames(voter_turnout_data) %in% c("Year", "State"))],
                           selected = colnames(voter_turnout_data)[3])
             ),
             mainPanel(
               plotOutput("voter_turnout_plot") %>% withSpinner(color = "#007bff")
             )
           )
  ),
  
  # Tab 6: Incarceration Counts & Rates
  tabPanel("Incarceration Counts & Rates",
           icon = icon("gavel"),
           sidebarLayout(
             sidebarPanel(
               selectInput("incarceration_variable", "Select Variable:",
                           choices = c("State prisons", "Federal prisons", "Local jails",
                                       "State_Prison_Rate_100k", "Federal_Prison_Rate_100k",
                                       "Local_Jail_Rate_100k"),
                           selected = "State prisons")
             ),
             mainPanel(
               plotOutput("incarceration_plot") %>% withSpinner(color = "#007bff")
             )
           )
  ),
  
  # Tab 7: SUG Migration
  tabPanel("SUG Migration",
           icon = icon("university"),
           sidebarLayout(
             sidebarPanel(
               selectInput("university_select", "Select University:",
                           choices = colnames(sug_migration_data)[-1],
                           selected = "University_of_Mississippi",
                           multiple = TRUE)
             ),
             mainPanel(
               plotOutput("sug_migration_plot") %>% withSpinner(color = "#007bff")
             )
           )
  ),
  
  # Footer
  tags$footer(
    style = "background-color: #343a40; color: #ffffff; text-align: center; padding: 10px; position: fixed; bottom: 0; width: 100%;",
    HTML("© 2025 Dr. King's Dashboard | Built with Shiny")
  )
)

# Define Server
server <- function(input, output, session) {
  output$inequality_plot <- renderPlot({
    ggplot(inequality_data, aes_string(x = input$x_variable, y = input$y_variable)) +
      geom_point(color = "#007bff", size = 3) +
      labs(title = paste(input$y_variable, "vs", input$x_variable),
           x = input$x_variable,
           y = input$y_variable) +
      theme_minimal(base_size = 14)
  })
  
  filtered_gini <- reactive({
    req(input$gini_states, input$gini_variable)
    gini_data %>%
      filter(State %in% input$gini_states) %>%
      select(Year, State, all_of(input$gini_variable))
  })
  
  output$gini_plot <- renderPlot({
    dataset <- filtered_gini()
    req(dataset)
    ggplot(dataset, aes(x = Year, y = .data[[input$gini_variable]], color = State, group = State)) +
      geom_line(size = 1.2) +
      scale_color_viridis_d() +
      labs(title = "Gini Over Time", x = "Year", y = input$gini_variable, color = "State") +
      theme_minimal(base_size = 14)
  })
  
  # Add similar plot enhancements for other tabs...
}

# Run the App
shinyApp(ui, server)


