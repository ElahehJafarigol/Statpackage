library(shiny)
#library(Statpackage)
library(ggplot2)

#Start the app
ui <- fluidPage(

  #App title
  titlePanel("Descriptive and Inferential Statistics"),

  fluidRow(
    #App inputs
    column (width = 5,
            h5("Input sample data: x"),
            wellPanel(

              #Slidebar for input
              sliderInput( inputId = "numx", label = "Choose the sample size:",
                           value = 25, min = 20, max = 100),

              sliderInput( inputId = "mux", label = "Choose the sample mean:",
                           value = 5, min = 0, max = 10),

              sliderInput( inputId = "sdx", label = "Choose the sample standard deviation:",
                           value = 5, min = 1, max = 20)

            ),

            h5("Input samplle data: y"),
            wellPanel(

              #Slidebar for input
              sliderInput( inputId = "numy", label = "Choose the sample size:",
                           value = 25, min = 20, max = 100),

              sliderInput( inputId = "muy", label = "Choose the sample mean:",
                           value = 5, min = 0, max = 10),

              sliderInput( inputId = "sdy", label = "Choose the sample standard deviation:",
                           value = 5, min = 1, max = 20)
            )),
    #App outputs
    column (width = 5,
            h5("Box plot"),
            plotOutput(outputId = "boxplot1")
            ),

    column (width = 5,
            h5("Box plot"),
            plotOutput(outputId = "boxplot2")
    ),

    h5("T-test results"),
    textOutput(outputId = "results")
  )

)



server <- function(input,output)
{
         output$boxplot1 <- renderPlot({
           set.seed(32)
           x = rnorm (input$numx, mean = input$mux, sd = input$sdx )

           title <- ("Sample data of population 1")
           boxplot(x, main = title, col = "dark blue")

         })

          output$boxplot2 <- renderPlot({
            set.seed(32)
            y = rnorm (input$numy, mean = input$muy, sd = input$sdy )

            title <- ("Sample data of population 2")
            boxplot(y, main = title, col = "dark red")
         })

          output$results <- renderText({
            #set.seed(32)
            #l1 <- is.numeric( rnorm (input$numx, mean = input$mux, sd = input$sdx ))
            #l2 <- is.numeric( rnorm (input$numy, mean = input$muy, sd = input$sdy ))
            l1 = is.numeric_version(x)
            l2 = is.numeric_version(y)
            myT <- ttest(l1, l2, paired = FALSE, alpha=0.05)


            mydf <- list("t test" = myT$ttype, "p value" = round(myT$ttest$p.value,4),
                         "alpha" = round(1-attr(myT$ttest$conf.int, "conf.level"),4), "reject" = myT$reject)
          })


}


shinyApp(ui=ui, server=server)
