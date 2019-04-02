

library(shiny)
library(ggplot2)
library(rsconnect)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
    # Application title
   titlePanel("Predict Diamond Carat from Dimensions"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        
        h3("Model will estimte carats given dimensions"),
        h3("Select dimensions in millimetres "),
        sliderInput("sliderx", "Diamond width mm?", 0, 10, value = 5),
        sliderInput("slidery", "Diamond heightmm?", 0, 10, value = 10),
        sliderInput("sliderz", "Diamond depth mm?", 0, 10, value = 10),
        
        submitButton("Submit!")
      ),
        
       # Show a plot of the generated distribution
      mainPanel(
        plotOutput("plot",height=600),
        h3("Predicted Carat from Model:"),
        textOutput("pred")
        )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
    dsub <- diamonds
    dsub$vol <- dsub$x * dsub$y * dsub$z
    model <- lm(carat ~ vol, data = dsub, na.action=na.exclude)
    
    modelpred <- reactive({
    
    volInput <- input$sliderx*input$slidery*input$sliderz
    predict(model, newdata = data.frame(vol = volInput))
      })
    
    output$pred <- renderText({modelpred()})
  
    output$plot <- renderPlot({
  
      
    volInput <- input$sliderx*input$slidery*input$sliderz    
    plot(dsub$vol, dsub$carat, xlab = "Volume", 
    ylab = "Carat", bty = "n", pch = 16,
    xlim = c(0, 2000), ylim = c(0, 6))
    abline(model,
           col = "red", lwd = 2)      
    points(volInput, modelpred(), col = "red", pch = 16, cex = 2)
    
   
      })
  
   }
   
# Run the application 
shinyApp(ui = ui, server = server)

