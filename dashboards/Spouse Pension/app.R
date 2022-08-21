#--------------------------#
library(shiny)
library(shinydashboard)

#wrangeling
library(tidyverse)
library(data.table)

#plotting and formatting
library(highcharter)
library(scales)

#speed 
library(memoise)
#--------------------------#

ui <- dashboardPage(
  dashboardHeader(title = "Spouse Pension"),
  dashboardSidebar(
    numericInput("int_rate", "Interest rate (r)", value = 0.03, min = -0.05, max = 1, step = 0.01),
    numericInput("length_contract", "Length of contract (T)", value = 60, min = 0, max = 100),
    numericInput("pension", "Pension (P)", value = 100000, min = 0, max = 1e9, step = 10000),
    actionButton("action1", "Submit")
  ),
  dashboardBody(
    fluidRow(
      box(radioButtons("gender_x", "Person 1 aged x:", 
                       c("Male" = "M", "Female" =   "F")),
          numericInput("age_x", "Age (x)", value = 25, min = 16, max = 120))
      , 
      box(radioButtons("gender_y", "Person 2 aged y:", 
                       c("Male" = "M", "Female" = "F"), selected = "F"), 
          numericInput("age_y", "Age (y)", value = 24, min = 0, max = 120)
          )
      ), 
    fluidRow(box(textOutput("yearly_premium")),box(textOutput("mnt_premium")) ), 
    fluidRow(box(tableOutput("reserve")), box(highchartOutput("reserve_plt")))
  ) 
)


server <- function(input, output) {
  #States: 
  #p1: person aged x
  #p2: person aged y
  #0: p1, p2 alive
  #1: p1 dead, p2 alive
  #2: p1 alive, p2 dead 
  #3: p1, p2 dead
  
  #GLOBAL VARIABLES:
  #The weights given by finanstilsynet
  w <- function(x, G){
    #male: G = "M"
    #female: G = "F"
    #x: age in calender year Y,
    if (G == "M"){
      return (min(2.671548-0.172480*x + 0.001485*x**2, 0))
    } 
    else {
      return(min(1.287968-0.101090*x+ 0.000814*x**2,0))
    }
  }
  
  mu_kol_2013 <- function(x, G){
    #male
    if (G == "M"){
      return((0.241752+0.004536*10**(0.051*x))/1000)
    }
    #female
    else {
      return((0.085411+0.003114*10**(0.051*x))/1000)
    }
  } 
  
  #turning mu into a function of u, so that we can use integrate 
  mu <- function(u, x, G, Y = 2022){
    return(mu_kol_2013(x+u, G)*(1 + w(x+u, G)/100)^{(Y+u-2013)})
  }
  
  p_surv <- function(x, G, Y, t, s){
    
    if (t == s){
      return(1)
    }
    
    f <- Vectorize(mu)
    integral <- integrate(f, lower = t, upper = s, x=x, Y=Y, G=G)$value
    
    ans <- exp((-1)*integral)
    return(ans)
  }
  
  #works a bit like caching i think
  p_surv <- memoise::memoise(p_surv)
  
  x <- reactive({
    if (input$age_x < 16){
      stop("The insured needs to be 16 or above")
    }
    input$age_x
  })
  
  y <- reactive({
    if (input$age_y < 16){
      stop("The insured needs to be 16 or above")
    }
    input$age_y
  }) 
    
  
  G_x <-reactive({
    input$gender_x
  })
  
  G_y <- reactive({
    input$gender_y
  })
    
  r <- reactive({
    input$int_rate
  }) 
  
  T <- reactive({
    if (input$length_contract <= 0){
      stop("The contract length cannot be negative!")
    }
    input$length_contract
  })
  
  P <- reactive({
    if (input$pension <= 0){
      stop("Please choose a positive pension!")
    }
    input$pension
  })
  
  v <- function(r, t){
    return(exp(-(r*t)))
  } 
  
  v <- memoise::memoise(v)
  #PROBABILITES: 
  #both survive:
  p_00 <- function(t, n){
    p_surv(x(), G= G_x(), Y=2022, t=t,s = n)*p_surv(y(), G= G_y(), Y=2022, t=t, s=n)
  }
  
  #p1 dies, p2 survive:
  p_01 <- function(t,n){
    (1 - p_surv(x(), G=G_x(), Y = 2022, t=t,s = n ))*p_surv(y(), G = G_y(), Y=2022, t=t, s = n)
  }
  
  #p1 survive, p2 die:
  p_02 <- function(t,n){
    p_surv(x(), G=G_x(), Y=2022, t=t,s = n )*(1 - p_surv(y(), G=G_y(), Y=2022, t=t, s = n))
  }
  
  #p2 remains alive:
  p_11 <- function(t,n){
    p_surv(y(), G = G_y(), Y=2022, t=t, s = n)
  } 
  
  #p1 remains alive: 
  p_22 <- function(t,n){
    p_surv(x(), G = G_x(), Y=2022, t=t, s = n)
  }
  
  prem <- eventReactive(input$action1, {
    
    upper_summand <- function(n){
      prob <- p_01(0,n) + p_02(0,n)
      return(v(r(),n)*prob)
    }
    
    lower_summand <- function(n){
      prob <- p_00(0,n)
      return(v(r(), n)*prob)
    }
    
    ans <- P()*sum(map_dbl(0:(T()-1), upper_summand))/sum(map_dbl(0:(T()-1), lower_summand))
    ans
  }, ignoreNULL = FALSE)
  
  output$yearly_premium <- renderText({
    sprintf("The yearly premium should be: NOK %s", scales::label_comma(accuracy = .1)(prem()))
  })
  
  output$mnt_premium <- renderText({
    sprintf("The monthly premium should be: NOK %s", scales::label_comma(accuracy = .1)(prem()/12))
  })
  
  reserves <- eventReactive(input$action1, {
    
    V_0 <- function(t){
      
      #avoid computing multiple times:
      v_rt <- v(r(), t)
      
      summand_1 <- function(t, n){
        (v(r(), n)/v_rt)*p_00(t, n)
      }
      
      summand_2 <- function(t, n){
        (v(r(), n)/v_rt)*(p_01(t,n) + p_02(t,n))
      }
      
      ans <- (-1)*prem()*sum(map_dbl(t:(T()-1),summand_1, t = t)) + P()*sum(
        map_dbl(t:(T()-1), summand_2, t = t))
      
      return(ans)
    }
    
    #reseve in state 1: 
    V_1 <- function(t){
      
      v_rt <- v(r(), t)
      summand <- function(t, n){
        (v(r(), n)/v_rt)*p_11(t,n)
      }
      
      ans <- P()*sum(map_dbl(t:(T()-1), summand, t = t))
      
      return(ans)
    }
    
    #reserve in state 2: 
    V_2 <- function(t){
      
      summand <- function(t,n){
        (v(r(), n)/v(r(), t))*p_22(t,n)
      }
      
      ans <- P()*sum(map_dbl(t:(T()-1), summand, t = t))
      
      return(ans)
    }
    
    
    
    length_contract <- 0:(T())
    state0 <- round(map_dbl(length_contract, V_0),2)
    
    #data
    # df <- data.frame(length_contract, state0) %>% 
    #   mutate(state1 = round(map_dbl(length_contract, V_1), 2)) %>% 
    #   mutate(state2 = round(map_dbl(length_contract, V_2), 2))
    # 
    # df
    
    dt <- as.data.table(data.frame(length_contract, state0))
    dt_reserves <- dt[, `:=`(state1 = map_dbl(length_contract, ..V_1))][, 
                        `:=`(state2 = map_dbl(length_contract, ..V_2))]
    dt_reserves
  }, ignoreNULL = FALSE)
  
  output$reserve <- renderTable({
    reserves()
  })
  
  plt <- eventReactive(input$action1, {
    
    df_plt <- reserves() %>% 
      as_tibble() %>% 
      pivot_longer(!length_contract, names_to = "state", values_to = "reserve")
    
    fig <- df_plt %>% 
      hchart("line", hcaes(x = length_contract, y = reserve, group = state))
    
    fig
    
  }, ignoreNULL = FALSE)
  
  
  output$reserve_plt <- renderHighchart({
    plt()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
