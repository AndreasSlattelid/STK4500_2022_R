library(shiny)
library(shinydashboard)
library(tidyverse)
library(highcharter)
library(scales)


ui <- dashboardPage(
  dashboardHeader(title = "Endownment insurance dashboard"),
  dashboardSidebar(
    numericInput("age", "Age (x)", value = 30, min = 0, max = 120), 
    numericInput("length_contract", "Length of contract (T)", value = 25, min = 0, max = 60), 
    numericInput("int_rate", "Interest rate (r)", value = 0.03, min = -0.05, max = 1),
    numericInput("benefit", "Benefit (B)", value = 200000, min = 0, max = 1e6), 
    numericInput("endownment", "Endownment (E)", value = 125000, min = 0, max = 1e6), 
    actionButton("action1", "Submit")
  ),
  dashboardBody(
    fluidRow(box(textOutput("yearly_premium"))), 
    fluidRow(box(tableOutput("reserve")), box(highchartOutput("reserve_plt")))
  )
)


server <- function(input, output){
  #GLOBAL VARIABLES: 
  
  #0: alive, 1:dead
  mu01 <- function(t){
    return(0.0015 + 0.0004*(t-35))
  }
  
  p_surv <- function(t,s){
    f <- Vectorize(mu01)
    integral <- integrate(f, lower = t, upper = s)$value
    return(exp((-1)*integral))
  }
  
  yearly_prem <- eventReactive(input$action1, {
    
    x <- input$age
    T <- input$length_contract
    r <- input$int_rate
    B <- input$benefit 
    E <- input$endownment
    
    v <- function(t){
      return(exp(-r*t))
    }
    
    
    upper_summand <- function(n){
      v(n+1)*p_surv(x, x + n)*(1-p_surv(x + n, x + n + 1))
    }
    
    lower_summand <- function(n){
      v(n)*p_surv(x, x + n)
    }
    
    upper_sum <- sum(map_dbl(0:(T-1), upper_summand))
    lower_sum <- sum(map_dbl(0:(T-1), lower_summand))
    
    upper_expression <- v(T)*p_surv(x, x + T)*E + B*upper_sum
    yearly_prem <- upper_expression/lower_sum
    yearly_prem
  }, ignoreNULL = FALSE)
  
  output$yearly_premium <- renderText({
    sprintf("According to the equivalence principle, the yearly premium should be: NOK %s", round(yearly_prem(), 2)
    )
  }) 
  
  reserve1 <- eventReactive(input$action1, {
    
    x <- input$age
    T <- input$length_contract
    r <- input$int_rate
    B <- input$benefit 
    E <- input$endownment
    
    v <- function(t){
      return(exp(-r*t))
    }
    
    V_star <- function(t){
      
      summand_1 <- function(t, n){
        (v(n)/v(t))*p_surv(x + t, x + n)
      }
      
      summand_2 <- function(t, n){
        (v(n+1)/v(t))*p_surv(t + x, n + x)*(1 - p_surv(x + n, x + n +1))
      }
      
      ans <- (v(T)/v(t))*p_surv(x + t, x + T)*E - 
        yearly_prem()*sum(map_dbl(t:(T-1),summand_1, t = t)) + B*sum(
          map_dbl(t:(T-1), summand_2, t = t))
      
      
      return(ans)
    }
    
    length_contract <- 0:T
    reserve <- map_dbl(length_contract, V_star)
    df <- data.frame(length_contract, reserve)
    colnames(df) <- c("length_contract", "reserve")
    
    df
    
  }, ignoreNULL = FALSE)
  
  output$reserve <- renderTable({
    reserve1()
  })
  
  plt <- eventReactive(input$action1, {
    fig <- reserve1() %>% 
      hchart("line", hcaes(x = length_contract, y = reserve)
             , color = "steelblue")
    
    fig
    
  }, ignoreNULL = FALSE)
  
  output$reserve_plt <- renderHighchart({
    plt()
  })
  
}




shinyApp(ui, server)