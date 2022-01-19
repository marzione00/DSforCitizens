# ISTRUZIONI GENERALI
# File delle funzioni: fare un file esterno dove mettere tutte quelle porzioni 
# di codice ripetute molte volte. I nomi delle funzioni devono essere scritti
# in minuscolo.
# Cercare di tenere il codice pulito (senza codice commentato), e con spiegazioni
# di cosa si fa se non è chiaro. 



# LIBRERIE
library(shiny)
library(ineq)
library(ggpubr)

# VARIABILI GLOBALI
# Istruzione: dare il nome alle variabili:
# il nome delle variabili deve essere scritto in minuscolo e in inglese
# se il nome è composto da più parole, dividere le parole con "_"
# esempio: hello_world

# IMPORT DI FILE ESTERNI
# ad esempio, quello con dentro le funzioni


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Variability indices"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 100,
                  value = 30),
      sliderInput("mean_x_s",
                  "mean x:",
                  min = -10,
                  max = 10,
                  value = 0),
      sliderInput("sd_x_s",
                  "sd x:",
                  min = 0.5,
                  max = 5,
                  value = 1),
      sliderInput("n_x_s",
                  "number x:",
                  min = 1,
                  max = 1000,
                  value = 1),
      sliderInput("mean_y_s",
                  "mean y:",
                  min = -10,
                  max = 10,
                  value = 0),
      sliderInput("sd_y_s",
                  "sd y:",
                  min = 0.5,
                  max = 5,
                  value = 1),
      sliderInput("n_y_s",
                  "number x:",
                  min = 1,
                  max = 1000,
                  value = 1),
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot0"),
      plotOutput("distPlot1"),
      plotOutput("distPlot2"),
      textOutput("gini"),
      textOutput("entropy"),
      plotOutput("levels")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    bins_x <- reactive(sample(1:100,input$bins)/100)
    mean_x <- reactive(input$mean_x_s)
    mean_y <- reactive(input$mean_y_s)
    sd_x<- reactive(input$sd_x_s)
    sd_y<- reactive(input$sd_y_s)
    n_x_s<- reactive(input$n_x_s)
    n_y_s<- reactive(input$n_y_s)
  
    x_group<-reactive(rnorm( n_x_s() ,mean_x(),sd_x()))
    y_group<-reactive(rnorm( n_y_s() ,mean_y(),sd_y()))

    
     output$distPlot0 <- renderPlot({
        # generate bins based on input$bins from ui.R
        #x    <- faithful[, 2]
        #y    <- faithful[, 2]
      #bins_x <- runif(input$bins, min=0, max=1)
        
        #df <- data.frame(bins_x)
        # draw the histogram with the specified number of bins
        #bins_x <- runif(input$bins, min=0, max=1)
        hist(x_group(), col = 'darkgray', border = 'white')
         
    })
     
     output$distPlot1 <- renderPlot({
       # generate bins based on input$bins from ui.R
       #x    <- faithful[, 2]
       #y    <- faithful[, 2]
       #bins_x <- runif(input$bins, min=0, max=1)
       
       #df <- data.frame(bins_x)
       # draw the histogram with the specified number of bins
       #bins_x <- runif(input$bins, min=0, max=1)
       hist(y_group(), col = 'darkgray', border = 'white')
       
     })
    
    output$distPlot2 <- renderPlot({
      #bins_x <- runif(input$bins, min=0, max=1)
      #df <- data.frame(bins_x)
      ineq(bins_x(),type="Gini")  
      plot(Lc(bins_x()),col="darkred",lwd=2)
      
      
      
      
    })
    
    output$gini<- renderText({ 
      ineq(x_group(),type="Gini") 
    })
    
    output$entropy<- renderText({ 
      ineq(x_group(),type="entropy") 
    })
    
    output$levels <- renderPlot({ 
      #bins_x <- runif(input$bins, min=0, max=1)
      #df <- data.frame(bins_x)
      
      
      
      
      data_x<-data.frame(x_group(),"1")
      data_y<-data.frame(y_group(),"2")
      colnames(data_x)<-c("values","group")
      colnames(data_y)<-c("values","group")
      data_fin<-rbind(data_x,data_y)
      
      p<-ggboxplot(data_fin, x = "group", y = "values", color = "group")
      
      p
      
    })
    
    
    
    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
