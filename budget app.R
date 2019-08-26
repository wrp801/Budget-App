library(tidyverse)
library(readxl)
library(lubridate)
library(viridis)
library(shiny)
library(scales)
library(DT)

## lookup function to get the avg cost per category
source("~/Budget-App/cost_lookup.R")


## helper function for monthly income
source("~/Budget-App/monthly_income.R")


## updated UI 

ui <- fluidPage(title = "Poulsen Montly Budget",

sidebarLayout(
  sidebarPanel(
    fileInput('fileload',label = 'Choose a file to upload')),
    mainPanel(
      dataTableOutput("moneytab")
    ) ## end mainpanel

)



  )

server <- function(input, output, session) {

 
  
  output$moneytab <- renderDataTable({
    
  req(input$fileload)
    
  tryCatch(
    {
  budget <- read_excel(input$fileload$datapath,sheet = "Expense Entry")
  
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
    arrange(category) %>% 
    replace_na(list(avg = 0)) %>% 
    mutate(avg = round(avg, digits = 0))
  
  
  categories <- budget %>% distinct(category) %>% arrange(category) %>% pull()
  
  recent_avg %>% 
    datatable(options = list(pageLength = nrow(recent_avg)),editable = TRUE)
    },
  
  error = function(e) {
    stop(safeError(e))
    
    }
  ) ## end tryCatch
  
  }) ## end render DataTable


}

shinyApp(ui, server)



