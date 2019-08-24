library(tidyverse)
library(readxl)
library(lubridate)
library(viridis)
library(shiny)
library(scales)


budget <- read_excel(choose.files(),sheet = "Expense Entry")


budget <- budget %>% 
  mutate(Date = as_date(Date)) %>% 
  janitor::clean_names("snake") %>% 
  select(-month,-check_cleared,-charged_at) %>% 
  mutate(actual_cost = str_remove(string = actual_cost,"\\$"),
         actual_cost = as.numeric(actual_cost))


## this is a 3 month average of all the expenses
recent_avg <- budget %>% 
  mutate(beg_date = floor_date(date,unit = 'month')) %>% 
  filter(year == 2019,
         beg_date >= floor_date(date(Sys.Date() %m+% months(-2) ),unit = "months")) %>% 
  arrange(beg_date) %>% 
  select(-year,-notes,-description) %>% 
  group_by(category) %>% 
  summarise(avg = sum(actual_cost)/3) %>% 
  ungroup() %>% 
  arrange(category)


categories <- budget %>% distinct(category) %>% arrange(category) %>% pull()



## lookup function to get the avg cost per category
source("~/Budget-App/R/cost_lookup.R")


## helper function for monthly income
source("~/Budget-App/R/monthly_income.R")


## updated UI 

ui <- fluidPage(title = "Poulsen Montly Budget",
  fluidRow(
    column(2,
           numericInput('charity',categories[1],value = cost_lookup("Charity")),
           numericInput("clothes",categories[2],value = cost_lookup(categories[2])),
           numericInput("dine",categories[3],value = cost_lookup(categories[3])),
           numericInput("education",categories[4],value = cost_lookup(categories[4])),
           numericInput("emergency",categories[5],value = cost_lookup(categories[5]))),  ## end first column
    column(2,
           numericInput("entertainment",categories[6],value = cost_lookup(categories[6])),
           numericInput("gifts",categories[7],value = cost_lookup(categories[7])),
           numericInput("grad",categories[8],value = cost_lookup(categories[8])),
           numericInput("groceries",categories[9],value = cost_lookup(categories[9])),
           numericInput("health",categories[10],value = cost_lookup(categories[10]))),
    column(2,
           numericInput("homeimprove",categories[11],value = cost_lookup(categories[11])),
           numericInput("house",categories[12],value = cost_lookup(categories[12])),
           numericInput("immigration",categories[13],value = cost_lookup(categories[13])),
           numericInput("insurance",categories[14],value = cost_lookup(categories[14])),
           numericInput("liquor",categories[15],value = cost_lookup(categories[15]))),
    column(2,
           numericInput("mortgage",categories[16],value = cost_lookup(categories[16])),
           numericInput("nr",categories[17],value = cost_lookup(categories[17])),
           numericInput("other",categories[18],value = cost_lookup(categories[18])),
           numericInput("personalcare",categories[19],value = cost_lookup(categories[19])),
           numericInput("pets",categories[20],value = cost_lookup(categories[20]))),
    column(2,
           numericInput("recreation",categories[21],value = cost_lookup(categories[21])),
           numericInput("taxes",categories[22],value = cost_lookup(categories[22])),
           numericInput("transportation",categories[23],value = cost_lookup(categories[23])),
           numericInput("utilities",categories[24],value = cost_lookup(categories[24])),
           numericInput("vacation",categories[25],value = cost_lookup(categories[25])))
  ),
br(),
  fluidRow(
           numericInput("wes","Take Home Pay for Wes",value = 1700),
           numericInput("dany","Take Home Pay for Dany",value = 595.58)),
  fluidRow(
    mainPanel(
      plotOutput("budgplot")
    )
  )

  )

server <- function(input, output, session) {

  observe({
    my_colors <- c("#8EF75E", "#BA3636", "#CC8EF5")
    
    income <- monthly_income(input$wes,input$dany)
    
    cost <- sum(input$charity,input$clothes,input$dine,input$education,input$emergency,input$entertainment,input$gifts,input$grad,input$groceries,input$health,
                  input$homeimprove,input$house,input$immigration,input$insurance,input$liquor,input$mortgage,input$nr,input$other,input$personalcare,input$pets,
                  input$recreation,input$taxes,input$transportation,input$utilities,input$vacation)
    
    df <- tibble(net_income = income,
           expenses = cost) %>% 
      mutate(balance = net_income - expenses)
    
    output$budgplot <- renderPlot(
      ggplot(df) +
        geom_col(aes(x = factor(1), y = net_income),fill = my_colors[1] ) +
        geom_col(aes(x = factor(2), y = expenses),fill = my_colors[2]) + 
        geom_col(aes(x = factor(3), y = balance), fill = my_colors[3]) + 
        scale_y_continuous(label = dollar) + 
        ggthemes::theme_clean()
    )
    
  })
  

}

shinyApp(ui, server)



