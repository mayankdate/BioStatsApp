#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # App title ----
    titlePanel("Sample Size Calculator"),
    
    # Sidebar layout with input and output definitions ----
    sidebarLayout(
        
        # Sidebar panel for inputs ----
        sidebarPanel(
            numericInput("sig", label = h3("Significance Level"), value = 0.95,min=0, max=1, step=0.01),
            br(),
            numericInput("power", label = h3("Power"), value = 0.80, min=0, max=1, step=0.01),
         
            ),
        
        # Main panel for displaying outputs ----
        mainPanel(
            
            # Output: Tabset w/ plot, summary, and table ----
            tabsetPanel(type = "tabs",
                        tabPanel("Event Rates",img(src='event.png', align="left",height = '120px', width = '600px'),
                                 fluidRow(
                                     column(3,
                                            h4("True Event Rates"),
                                            numericInput("presence", label = h5(withMathJax("$$\\lambda_{1}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01),
                                            numericInput("absence", label = h5(withMathJax("$$\\lambda_{0}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01)
                                     ),
                                     column(4, offset = 1,
                                            h4("Coefficient of Variation"),
                                            numericInput("coeffcontrol", label = h5(withMathJax("$$k_{0}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01),
                                            numericInput("coeffintervention", label = h5(withMathJax("$$k_{1}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01)
                                     ),
                                     column(4,
                                            h4("Person Years"),
                                            numericInput("personyear", label = h5(withMathJax("$$y$$")), value=200,min = 0, max = 10000,
                                                         step = 10))
                                 ),
                                 
                                 verbatimTextOutput("samplesize")
                                 ),
                        tabPanel("Proportion",
                                 img(src='proportion.png', align="left",height = '120px', width = '600px'),
                                 fluidRow(
                                     column(3,
                                            h4("True Proportions"),
                                            numericInput("pipresence", label = h5(withMathJax("$$\\pi_{1}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01),
                                            numericInput("piabsence", label = h5(withMathJax("$$\\pi_{0}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01)
                                     ),
                                     column(4, offset = 1,
                                            h4("Coefficient of Variation"),
                                            numericInput("picoeffcontrol", label = h5(withMathJax("$$k_{0}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01),
                                            numericInput("picoeffintervention", label = h5(withMathJax("$$k_{1}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01)
                                     ),
                                     column(4,
                                            h4("Cluster Sample"),
                                            numericInput("clustsample", label = h5(withMathJax("$$m$$")), value=200,min = 0, max = 10000,
                                                         step = 10))
                                 ),
                                 
                                 verbatimTextOutput("samplesize2")),
                        tabPanel("Means", 
                                 img(src='means.png', align="left",height = '120px', width = '600px'),
                                 fluidRow(
                                     column(3,
                                            h4("True Means & Within Cluster Standard Deviation"),
                                            numericInput("meanpresence", label = h5(withMathJax("$$\\mu_{1}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01),
                                            numericInput("meanabsence", label = h5(withMathJax("$$\\mu_{0}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01),
                                            numericInput("sdpresence", label = h5(withMathJax("$$\\sigma_{W1}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01),
                                            numericInput("sdabsence", label = h5(withMathJax("$$\\sigma_{W0}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01)
                                     ),
                                    
                                     column(4, offset = 1,
                                            h4("Coefficient of Variation"),
                                            numericInput("meancoeffcontrol", label = h5(withMathJax("$$k_{0}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01),
                                            numericInput("meancoeffintervention", label = h5(withMathJax("$$k_{1}$$")), value=0.05,min = 0, max = 100,
                                                         step = 0.01)
                                     ),
                                     column(4,
                                            h4("Cluster Sample"),
                                            numericInput("clustsample2", label = h5(withMathJax("$$m$$")), value=200,min = 0, max = 10000,
                                                         step = 10))
                                 ),
                                 
                                 verbatimTextOutput("samplesize3"))
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
    output$samplesize <- renderText({ 
        zalpha=abs(round(qnorm(1-((1-input$sig)/2)),2))
        zbeta = abs(round(qnorm(input$power),2))
        a=(input$presence+input$absence)/input$personyear
        b=(input$coeffintervention^2)*(input$presence^2)+(input$coeffcontrol^2)*(input$absence^2)
        c=(input$absence-input$presence)^2
        paste("The result is =", 1+((zalpha+zbeta)^2)*((a+b)/c))
        })
    
    output$samplesize2 <- renderText({ 
        zalpha=abs(round(qnorm(1-((1-input$sig)/2)),2))
        zbeta = abs(round(qnorm(input$power),2))
        e = (input$piabsence*(1-input$piabsence)/input$clustsample)
        f =(input$pipresence*(1-input$pipresence)/input$clustsample)
        g = (input$picoeffintervention^2)*(input$pipresence^2)+(input$picoeffcontrol^2)*(input$piabsence^2)
        h = (input$piabsence-input$pipresence)^2
        paste("The result is =", 1+((zalpha+zbeta)^2)*((e+f+g)/h))
    })
 
    output$samplesize3 <- renderText({ 
        zalpha1=abs(round(qnorm(1-((1-input$sig)/2)),2))
        zbeta1 = abs(round(qnorm(input$power),2))
        i = (input$sdabsence^2+input$sdpresence^2)/input$clustsample2
        j = (input$meancoeffintervention^2)*(input$meanpresence^2)+(input$meancoeffcontrol^2)*(input$meanabsence^2)
        k = (input$meanabsence-input$meanpresence)^2
        paste("The result is =", 1+((zalpha1+zbeta1)^2)*((i+j)/k))
    })
   
}

# Run the application 
shinyApp(ui = ui, server = server)
