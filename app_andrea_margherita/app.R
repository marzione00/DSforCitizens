# LIBRERIE
library(shiny)
library(ggplot2)
data <- read.csv("/Users/andreacutrera/Desktop/stage DataScience4Citizens/dataset.csv",
                 sep=';')
data <- data[,-c(1,2,9,18,19,20,21,22,26)]

data$gender <- as.factor(data$gender)
data$statistical_knowledge <- as.factor(data$statistical_knowledge)
data$software_knowledge <- as.factor(data$software_knowledge)
data$post_diploma_1uni_0bo<- as.factor(data$post_diploma_1uni_0bo)
data$pm_2_5 <- as.factor(data$pm_2_5)

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("App Andrea-Margherita"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("variable_one",
                        "First Variable",
                        c("study_path","gender","statistical_knowledge","software_knowledge",
                        "data_science_relevance","data_science_difficulty","smartphone_gg_ore",
                        "post_diploma_1uni_0bo","drive_license_grandmothers","study_math_hours",
                        "women_degree_ICT","rispetto_ambiente","pm_2_5","pm_10","Expected_salary_0_10",
                        "delta_T_C","deltapred_T_C","new_tecnology","orientation","female_emancipation",  
                        "environment_pollution","labour_market","freedom","climate_change"),
                        ),
            selectInput("variable_two",
                        "Second Variable",
                        c("study_path","gender","statistical_knowledge","software_knowledge",
                          "data_science_relevance","data_science_difficulty","smartphone_gg_ore",
                          "post_diploma_1uni_0bo","drive_license_grandmothers","study_math_hours",
                          "women_degree_ICT","rispetto_ambiente","pm_2_5","pm_10","Expected_salary_0_10",
                          "delta_T_C","deltapred_T_C","new_tecnology","orientation","female_emancipation",  
                          "environment_pollution","labour_market","freedom","climate_change"), selected = "gender"
            )
        ),

        mainPanel("Bar charts",
            fluidRow(
                splitLayout(cellWidths = c("50%", "50%"),
                            plotOutput("bar_chart_one", width='100%'),
                            plotOutput("bar_chart_two", width='100%'))
            )
          ,
           tableOutput("table_1"),
           tableOutput('chisq')
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    data_reactive <- reactive({
        data_to_plot <- data[,c(input$variable_one, input$variable_two)]
        colnames(data_to_plot) <- c("x", "y")
        return(data_to_plot)
    })
    
    variables_name_reactive <- reactive({
        variable_names <- c(input$variable_one, input$variable_two)
        return(variable_names)
    })
    
    tab_reactive <- reactive({
        tab_1 <- table(data_reactive()$x, data_reactive()$y)
        tab_1 <- addmargins(tab_1)
        return(tab_1)
    })
    
    chi_reactive <- reactive({
        chi <- chisq.test(tab_reactive())
        return(chi)
    })
    
    output$bar_chart_one <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(data_reactive(), aes(x=x)) + geom_bar() + xlab(variables_name_reactive()[1])
    })    
    output$bar_chart_two <- renderPlot({
        # generate bins based on input$bins from ui.R
        ggplot(data_reactive(), aes(x=y)) + geom_bar() + xlab(variables_name_reactive()[2])
    })
    
    output$table_1 <- renderTable({
        tab_1 <- table(data_reactive()$x, data_reactive()$y)
        tab_1 <- addmargins(tab_1)
        as.data.frame.matrix(tab_1)
        }, striped = TRUE, bordered = TRUE , rownames = TRUE
    )
    
    output$chisq <- renderTable({
        result <- data.frame(statistic = chi_reactive()[1],
                             df = chi_reactive()[2],
                             p_value = chi_reactive()[3])
        as.data.frame.matrix(result)
    }, striped = TRUE, bordered = TRUE , rownames = TRUE
        
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
