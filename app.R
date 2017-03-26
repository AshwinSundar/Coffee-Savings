# Coffee Saver
# Ashwin Sundar
# March 19, 2017

require(shiny)
require(plotly)

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
   HTML("<br>"), # just gives some space from the top of the page
   # Application title
   # titlePanel("[Title]"),
   
   # Sidebar with a slider input
   sidebarLayout(
      sidebarPanel(
         sliderInput("cupsPerWeek",
                     "How many cups of coffee do you drink each week?",
                     min = 1,
                     max = 49,
                     value = 7),
         HTML("<b>Assumptions</b><br>"),
         HTML("Price per cup: $2.50<br>"),
         HTML("Market return: 4%<br><br>"),
         HTML("<hr>"),
         
         conditionalPanel(condition = "input.cupsPerWeek <= 7",
                          HTML("<font color='grey'>This is a normal, human amount of coffee to drink per week.")),
         
         conditionalPanel(condition = "(input.cupsPerWeek > 7) & (input.cupsPerWeek <= 21)",
                          HTML("While I personally try not to drink this much coffee, I could understand why some of you might.")),
         
         conditionalPanel(condition = "(input.cupsPerWeek > 21) & (input.cupsPerWeek < 27)",
                          HTML("This is quite a bit of coffee - you should consider cutting back. It's not only bad for your wallet, but bad for your heart.")),
         
         conditionalPanel(condition = "input.cupsPerWeek == 27",
                          HTML("This is a really weirdly specific amount of coffee to drink every week. Do you mean to tell me you drink 4 cups of coffee per day, EXCEPT Sunday, when you <i>only</i> drink 3? Weirdo. Anyway, look at all the money you're wasting!")),
         
         conditionalPanel(condition = "(input.cupsPerWeek) >= 28 & (input.cupsPerWeek < 48)",
                          HTML("Yikes. If you go to the doctor and ask the question, 'How many cups of coffee is it safe to drink in a week?', the answer is probably not 'Your age plus 5',")),
         
         conditionalPanel(condition = "input.cupsPerWeek == 48",
                          HTML("You did it! Congrats! I think it's fair to say I <a target='_blank' href='https://www.paypal.me/ASundar/2.50'>earned</a> that extra $2.50 per week you are now saving! P.S. If you are seeing this and wondering why I randomly extended my upturned palm to you...well, you're the weirdo for telling me that you drink precisely 48 cups of coffee per week. You mean to say that you drink 7 cups of coffee every day EXCEPT for Sunday, when you drink 6? Seriously? By the way, if you cut down to 47 cups of coffee, you'll save $2.50 per week! Just an idea.")),
                          
         conditionalPanel(condition = "input.cupsPerWeek == 49",
                          HTML("<p align='justify'>Alright dude, you're drinking too much coffee. Like at least 4 or 5 cups too much per day. There's a reason I set a max to how much coffee you can have. You're not gonna even survive to retirement if you keep this up. Or maybe that's your strategy for having enough money for the rest of your life, however short that life may be. Anyway, you gotta cut back - and that's coming from a guy who used to work in a coffee shop. I would recommend starting by cutting back to 48 cups of coffee per week - hey, and while you're at it why don't you celebrate your first week of relative sobriety by clicking that <b><a target='_blank' href='https://www.paypal.me/ASundar/2.50'>donate</a></b> button and sending some good cheer my way? I gave you some apparently invaluable financial advice, because clearly someone who purchases 7 cups of coffee per day has no concept of financial planning...or maybe they are an investment banker, and know way more about finance than I ever will? *<i>welp</i>* If you are, then still please click that <b><a target='_blank' href='https://www.paypal.me/ASundar/2.50'>donate</a></b> button and maybe add a few extra zeros to the end? :D Unfortunately I doubt you are an investment banker, because very few of those guys are (t)rolling through /r/financialindependence or /r/personalfinance in search of personal finance tips. By the way, if you got here through some other means, send me an <b><a href='mailto:ashiundar@gmail.com?subject=Your Stupid Sassy Coffee Calculator Saved me a Bajillion Dollars'>email</a></b> and let me know what unfortunate series of events caused you to visit my R Shiny depot (which is what this web application was written in, in case anyone was interested. In case anyone has even read this far.)! Also, this was my own fault, but I totally spent way too long last night coding this up and now I'm tired at work (where I'm supposed to write much better programs than this one that do really important things like support business decisions and save the company a bajillion dollars or something) and could sure use a cup of joe! Cheers! ~ <b><a href='mailto:ashiundar@gmail.com?subject=Your Stupid Sassy Coffee Calculator Saved me a Bajillion Dollars'>Ashwin</a></b>"))
      ),

      
      # Show a bar plot of your savings over time
      mainPanel(
         plotlyOutput("coffeePlot")
      )
   )
))

# Define server logic
server <- shinyServer(function(input, output) {
   
   output$coffeePlot <- renderPlotly({
     numDaysInYear = 365
     pricePerCup = 2.50 # assume a venti hot coffee
     moneySaved = 0 # starting value
     moneySavedArray = array(c(moneySaved))
     savingsRate = 0.04 # The safe withdrawal rate
     currentIter = 1 # a counter
     
     for(each in 1:(365*40)) {
       moneySaved = moneySaved*(1 + savingsRate/365) + pricePerCup*(input$cupsPerWeek/7) # daily compounding
       
       if (currentIter %% 30 == 0) { # get a new data every month or so, to speed up computation by a lot
         moneySavedArray = c(moneySavedArray, round(moneySaved, 2)) 
       }
       currentIter = currentIter + 1
     }
     
     yearsArray = array(c(1:length(moneySavedArray)))
     yearsArray = yearsArray/12
     
     plot_ly(x = ~yearsArray,
             y = ~moneySavedArray,
             type = "bar") %>% layout(title = "<b>How much your daily coffee is costing you</b>",
                                     xaxis = list(title = "<b>Years</b>",
                                                  dtick = 2), # show a tick every other year
                                     yaxis = list(title = "<b>Money you're not saving</b>",
                                                  range = c(0, 250000)))
   }) 
})

# Run the application 
shinyApp(ui = ui, server = server)