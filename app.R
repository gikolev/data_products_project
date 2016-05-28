library(shiny)

ui <- fluidPage(
        
        navbarPage("Altman Z-score Calculator",
                   
                tabPanel("Calculator",
        
                        sidebarPanel(
                                helpText("Enter numbers only (no commas or brackets), and precede negative values with a minus sign."), 
                                br(),
                                numericInput(inputId = "WC",
                                             label = "Working Capital",
                                             value = 0),
                                numericInput(inputId = "TA",
                                             label = "Total Assets",
                                             value = 0),
                                numericInput(inputId = "RE",
                                             label = "Retained Earnings",
                                             value = 0),
                                numericInput(inputId = "EBIT",
                                             label = "Earnings before Interest and Taxes",
                                             value = 0),
                                numericInput(inputId = "MVE",
                                             label = "Market Value of Equity",
                                             value = 0),
                                numericInput(inputId = "TL",
                                             label = "Book Value of Total Liabilities",
                                             value = 0),
                                numericInput(inputId = "S",
                                             label = "Sales",
                                             value = 0),
                                actionButton(inputId = "Calculate", label = "Calculate")
                        ),
                
                        mainPanel(
                                tabsetPanel(
                                        tabPanel("Score",
                                                 br(),
                                                 h5(textOutput("text0")),
                                                 br(),
                                                 h2(textOutput("textZ")),
                                                 h2(br()),
                                                 textOutput("interpretation")
                                                 ), 
                                        tabPanel("Explanation",
                                                 br(),
                                                 HTML("The Z-score is calculated as a linear combination of five business ratios, weighted by coefficients:
                                                        <br> <br>
                                                        <b> Z = 1.2*X1 + 1.4*X2 + 3.3*X3 + 0.6*X4 + 1.0*X5 </b>
                                                        <br> <br>
                                                        where: <br> <br>
                                                        X1 = Working Capital / Total Assets <br
                                                        X2 = Retained Earnings / Total Assets <br>
                                                        X3 = Earnings Before Interest and Taxes / Total Assets <br>
                                                        X4 = Market Value of Equity / Book Value of Total Liabilities <br>
                                                        X5 = Sales / Total Assets <br>"),
                                                 br(),
                                                 HTML("The level of financial distress is estimated, based on the range or 'zones' the score falls into: <br> <br>
                                                        <b> Z > 2.99 - Safe Zone. </b> <br>
                                                        The company is in good financial health and not likely to experience distress or to go bankrupt within the next two years. <br> <br>
                                                        <b> 1.81 < Z < 2.99 - Grey Zone. </b> <br>
                                                        The score is inconclusive, so caution is warranted. <br> <br>
                                                        <b> Z < 1.81 - Distress Zone. </b> <br>
                                                        There is a high probability of financial distress or bankruptcy within the next two years. <br>"
                                                 )
                                        )
                                )
                        )
                ),
                
                tabPanel("About",
                         includeMarkdown("about.Rmd")),
                tabPanel("Sample Data",
                         includeMarkdown("sampledata.Rmd"))
                
        )
)

server <- function(input, output) {
                X1 <- eventReactive(input$Calculate, { input$WC / input$TA })
                X2 <- eventReactive(input$Calculate, { input$RE / input$TA })
                X3 <- eventReactive(input$Calculate, { input$EBIT / input$TA })
                X4 <- eventReactive(input$Calculate, { input$MVE / input$TL })
                X5 <- eventReactive(input$Calculate, { input$S / input$TA })
                Z <- eventReactive(input$Calculate, { round(1.2*X1() + 1.4*X2() + 3.3*X3() + 0.6*X4() + 1.0*X5(), 2) })
                Intro <- eventReactive(input$Calculate, { "The Z-score for this company is:" })
                output$text0 <- renderText({ paste(Intro()) })
                output$textZ <- renderText({ Z() })
                output$interpretation <- renderText({ 
                        if(Z() > 2.99) { paste("The company is in good financial health and not likely to experience distress or to go bankrupt within the next two years.") }
                        else {
                                if(Z() > 1.88) { paste("This score is inconclusive, so caution is warranted.") }
                                else { paste("There is a high probability of financial distress or bankruptcy within the next two years.") }
                        }
                })
}


shinyApp(ui = ui, server = server)