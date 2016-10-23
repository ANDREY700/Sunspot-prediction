# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
# http://shiny.rstudio.com/articles/dynamic-ui.html

library(shiny)

SSNSourceNumber <- 0 # start flag

shinyUI(fluidPage(
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      h3("Course Project"),
      p("This is the Course Project: Shiny Application and Reproducible Pitch at Developing Data Products Course by Johns Hopkins University"),
      br(),
      p("The goal of this project is to  create the Shiny application for prediction of Sun Activities based on two different data source."),
      p("The first data source is an internal R data set."),
      p("The second one is the live data source at the Belgium World Data Center server."),
      p("External data will be downloaded automatically."),
      p("I will not compare two sets of data during the calculations."),
      h3("Choose main data set"),
      br(),
      selectInput("SSNSource", label = "Main SSN Source for prediction", 
                  choices = list("Internal RStudio SSN Data Set" = 1, 
                                 "Belgium World Data Center" = 2
                  ), selected = 1),
      br(),
      p("By choosing a main data set you are changing the base for following prediction of the Sun activities."),
      p("Main data set will be market by red color at Income Data Plot."),
      br(),
      p("Parameters for model calculation: "),
      sliderInput(
        inputId = "aure",
        label = "Autoregressive",
        min = 0,
        max = 10,
        value = 4),
      sliderInput(
        inputId = "diff",
        label = "Differencing",
        value = 0,
        min = 0,
        max = 3),
      sliderInput(
        inputId = "moav",
        label = "Moving-Average",
        min = 0,
        max = 10,
        value = 4),
      br(),
      p("Andrey ABRAMOV"),
      p("abramov.andre@yandex.ru")
    ),
    
    mainPanel(
      h2("Sunspot Number (SSN) Prediction"),
      tabsetPanel(
        id = "tab",
        tabPanel(
          title = "income Data Plot",
          p("The idea of computing sunspot numbers was originated by Rudolf Wolf in 1848[1] in Zurich, Switzerland and, thus, the procedure he initiated bears his name (or place). The combination of sunspots and their grouping is used because it compensates for variations in observing small sunspots."),
          p("This number has been collected and tabulated by researchers for over 150 years.[2] They have found that sunspot activity is cyclical and reaches its maximum around every 9.5 to 11 years. This cycle was first noted by Heinrich Schwabe in 1843."),
          p("Due to weather and researcher unavailability, the sunspot count is actually an average of observations by multiple people in multiple locations with different equipment, with a scaling factor k assigned to each observer to compensate for their differing ability to resolve small sunspots and their subjective division of groups of sunspots."),
          
          p("The relative sunspot number R is computed using the formula (collected as a daily index of sunspot activity):"),
            
          p("R=k(10g+s),"),
          p("where"),
          
          p(" s is the number of individual spots,"),
          p(" g is the number of sunspot groups, and"),
          p(" k is a factor that varies with location and instrumentation (also known as the observatory factor or the personal reduction coefficient K)."),
          h4("Two SSN data set source"),
          p("You can change base data set for prediction at the left panel."),
          plotOutput('plot.Compare'),
          br(),
          p("There are some different between two data source of SunSpot number (SSN)."),
          p("Sunspots are temporary phenomena on the photosphere of the Sun that appear as dark spots compared to surrounding regions."),
          p("It's a calculated value. "),
          p("Sunspot activity cycles about every eleven years. The point of highest sunspot activity during a cycle is known as solar maximum, and the point of lowest activity as solar minimum. This period is also observed in most other solar activity and is linked to a variation in the solar magnetic field that changes polarity with this period."),
          p("Early in the cycle, sunspots appear in the higher latitudes and then move towards the equator as the cycle approaches maximum, following Sporer's law. Spots from two adjacent cycles can co-exist for some time. Spots from adjacent cycles can be distinguished by direction of their magnetic field."),
          p("The Wolf number sunspot index counts the average number of sunspots during specific intervals. The 11-year solar cycles are numbered sequentially, starting with the observations made in the 1750s.")
        ),
        
        tabPanel(
          title = "Prediction",
          h3("Summary of prediction model"),
          textOutput('model.description'),
          
          
          p("Full SSN curve and last year prediction"),
          plotOutput('plot1'),
          
          p("Prediction for the last years including SSN confidence interval."),
          plotOutput('plot2')
        ),
        tabPanel(
          title = "Data",
          h3("Main SSN data for prediction"),
          p("Full main data set choosed at the left panel."),
          p("You can change base data set for prediction at the left panel."),
          dataTableOutput('data.visual.table')
        )
        
        
      )
    ))))

