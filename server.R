
#load packages
library(shiny)
library(rjson)
library(dplyr)
library(DescTools)
library(purrr)
library(profvis)
library(compiler) # compile is a base package installed by default
library(multcomp)

options(scipen = 100000000)

#================================================================================================================================================================
#### 1. function to get stock price from Yahoo Finance over a period of time ####

# input:
#   @symbol: stock ticker
#   @start_date: the date to start holding the portfolio
#   @end_date: the date to stop holding the portfolio

source('get_data_Yahoo 2.R')

#================================================================================================================================================================
#### 2. function to filter stocks ####

# input:
#   @ind: industry name
#         a vector with values:
#     "Health Care"           "Finance"              
#     "Consumer Services"     "Technology"           
#     "Capital Goods"         "Energy"               
#     "Public Utilities"      "Basic Industries"     
#     "Transportation"        "Consumer Non-Durables"
#     "Consumer Durables"     "Miscellaneous"  
#   @s_date: the date to start holding the portfolio
#   @cap: minimum market cap (current) threshold
#   @vol: minimum daily trading volume threshold over period of M months
#   @n_month: Number of months to hold stocks after the start date
#   @N: take top N stocks ranked by growth rate from start_date to end_date

source('stock_pick.R')

#================================================================================================================================================================
#### 3. Set up price-weighted portfolio ####

# input:
#   @stocks: the stocks in the portfolio
#   @s_date: the date to start holding the portfolio
#   @n_month: Number of months to hold stocks after the start date
#   @rebalance: the number of months after that the portfolio is rebalanced
#   @init_quantity: the volume invested for stocks in the portfolio at the beginning. 
#   @rf: the risk-free rate

source('portfolio_measure.R')

#================================================================================================================================================================
# Define server logic 
shinyServer(function(input, output) {
    
    #to select the stocks every time the submit button is clicked
    stock_symbol <- eventReactive(input$submit_loc,{
        stock_pick(c(input$sector1), 
                   vol = input$vol,
                   cap = input$cap, 
                   s_date = input$sdate, 
                   n_month = input$nmonth, 
                   N = input$N)
    })
    
    #render table of stocks
    output$stocks = renderTable({
        invest_portfolio <- stock_symbol()
        invest_portfolio = as.data.frame(invest_portfolio)
        colnames(invest_portfolio) = c("Ticker", "Sector", "Average volume", "Growth")
        invest_portfolio
    })
    
    
    # draw the cummulative PnL
    output$portfolioPlot <- renderPlot({
        
        portfolio (stocks = stock_symbol()$symbol_f, #c("RP", "SANM"), 
                   s_date = input$p_sdate, 
                   n_month = input$p_nmonth,
                   rebalance = input$rebalance, #yearly
                   init_quantity = input$init_quantity,
                   rf = input$rf) 
        
    })
    
})
