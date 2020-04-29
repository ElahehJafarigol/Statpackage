library(shiny)
#library(Statpackage)
library(ggplot2)

#Start the app
ui <- fluidPage(

  #App title
  titlePanel("Descriptive and Inferential Statistics"),

  fluidRow(
    #App inputs
    column(width = 8,
           strong("For paired T-test the sample sizes must be equal."),
           checkboxInput(inputId = "pairing", label = "Is the data paired?",
                         value = FALSE)),


    column (width = 5,
            strong("Input sample data: x"),
            wellPanel(

              #Slidebar for input
              sliderInput( inputId = "numx", label = "Choose the sample size:",
                           value = 25, min = 20, max = 100),

              sliderInput( inputId = "mux", label = "Choose the sample mean:",
                           value = 5, min = 0, max = 10),

              sliderInput( inputId = "sdx", label = "Choose the sample standard deviation:",
                           value = 5, min = 1, max = 20)

            ),

            strong("Input samplle data: y"),
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

    column (width = 3,
            h4("T-test results"),

    h5("Statistics:"),
    textOutput(outputId = "statistic"),

    h5("Parameter:"),
    textOutput(outputId = "parameter"),

    h5("P value:"),
    textOutput(outputId = "pvalue"),

    h5("Condifence interval:"),
    textOutput(outputId = "conf"),

    h5("Estimate:"),
    textOutput(outputId = "estimate"),


    h5("Alternative mathod:"),
    textOutput(outputId = "alternative"),

    h5("Method:"),
    textOutput(outputId = "method"),

    )


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

          output$statistic <- renderText({
            set.seed(32)
            l1 = rnorm (input$numx, mean = input$mux, sd = input$sdx )
            l2 = rnorm (input$numy, mean = input$muy, sd = input$sdy )

            t_test <- t.test(l1, l2, paired = input$pairing, alpha=0.05)

            t_test$statistic

          })

          output$parameter <- renderText({
            set.seed(32)
            l1 = rnorm (input$numx, mean = input$mux, sd = input$sdx )
            l2 = rnorm (input$numy, mean = input$muy, sd = input$sdy )

            t_test <- t.test(l1, l2, paired = input$pairing, alpha=0.05)

            t_test$parameter

          })

          output$pvalue <- renderText({
            set.seed(32)
            l1 = rnorm (input$numx, mean = input$mux, sd = input$sdx )
            l2 = rnorm (input$numy, mean = input$muy, sd = input$sdy )

            t_test <- t.test(l1, l2, paired = input$pairing, alpha=0.05)

            t_test$p.value

          })

          output$conf <- renderText({
            set.seed(32)
            l1 = rnorm (input$numx, mean = input$mux, sd = input$sdx )
            l2 = rnorm (input$numy, mean = input$muy, sd = input$sdy )

            t_test <- t.test(l1, l2, paired = input$pairing, alpha=0.05)

            t_test$conf.int

          })

          output$estimate <- renderText({
            set.seed(32)
            l1 = rnorm (input$numx, mean = input$mux, sd = input$sdx )
            l2 = rnorm (input$numy, mean = input$muy, sd = input$sdy )

            t_test <- t.test(l1, l2, paired = input$pairing, alpha=0.05)

            t_test$estimate

          })


          output$alternative <- renderText({
            set.seed(32)
            l1 = rnorm (input$numx, mean = input$mux, sd = input$sdx )
            l2 = rnorm (input$numy, mean = input$muy, sd = input$sdy )

            t_test <- t.test(l1, l2, paired = input$pairing, alpha=0.05)

            t_test$alternative
          })

          output$method <- renderText({
            set.seed(32)
            l1 = rnorm (input$numx, mean = input$mux, sd = input$sdx )
            l2 = rnorm (input$numy, mean = input$muy, sd = input$sdy )

            t_test <- t.test(l1, l2, paired = input$pairing, alpha=0.05)

            t_test$method
          })



}


shinyApp(ui=ui, server=server)
