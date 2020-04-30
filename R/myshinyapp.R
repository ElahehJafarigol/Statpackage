library(shiny)
library(ggplot2)
library(vioplot)

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
            strong("Input Sample Data: x"),
            wellPanel(

              #Slidebar for input
              sliderInput( inputId = "numx", label = "Choose the sample size:",
                           value = 25, min = 20, max = 100),

              sliderInput( inputId = "mux", label = "Choose the sample mean:",
                           value = 5, min = 0, max = 10),

              sliderInput( inputId = "sdx", label = "Choose the sample standard deviation:",
                           value = 5, min = 1, max = 20)

            ),

            strong("Input Sample Data: y"),
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

    column (width = 5,
            h4("T-test results"),

    strong("Statistics:"),
    textOutput(outputId = "statistic"),

    strong("Parameter:"),
    textOutput(outputId = "parameter"),

    strong("P-value:"),
    textOutput(outputId = "pvalue"),

    strong("Confidence Interval:"),
    textOutput(outputId = "conf"),

    strong("Estimate:"),
    textOutput(outputId = "estimate"),

    strong("Method:"),
    textOutput(outputId = "method"),

    strong("Alternative Method:"),
    textOutput(outputId = "alternative"),

    ),

    column (width = 5,
            h5("Violin Plot"),
            plotOutput(outputId = "violin")
    )


  )

)



server <- function(input,output)
{
         output$violin <- renderPlot({
          set.seed(32)
          x = rnorm (input$numx, mean = input$mux, sd = input$sdx )
          y = rnorm (input$numy, mean = input$muy, sd = input$sdy )

          title <- ("Violin plot of sample x and y")
          vioplot(x, y, names=c("Sample x", "Sample y"), main = title, col="gold")

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

}


shinyApp(ui=ui, server=server)
