library(shiny)
library(ggvis)
source("../../code/functions.R")
dat <- read.csv('../../data/cleandata/cleanscores.csv', stringsAsFactors = FALSE)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Grade Visualizer"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(condition = "input.tabselected==1", h4("Grade Distribution"),
                       tableOutput("Grdist")
      ),
      
      conditionalPanel(condition = "input.tabselected==2", h4("Histogram"), 
                       selectInput('var', 'X-axis variable', c("HW1", "HW2", "HW3", 
                                                               "HW4", "HW5", "HW6", "HW7", "HW8", "HW9",
                                                               "ATT", "QZ1", "QZ2", "QZ3", "QZ4", 
                                                               "Test1", "Test2", "Homework", "Quiz", 
                                                               "Lab", "Overall")),
                       
                       sliderInput("width",
                                   "Bin width",
                                   value = 10,
                                   min = 1,
                                   max = 10)
      ),
      conditionalPanel(condition = "input.tabselected==3", selectInput('xcol', 'X-axis variable', c("HW1", "HW2", "HW3", 
                                                                                                                 "HW4", "HW5", "HW6", "HW7", "HW8", "HW9",
                                                                                                                 "ATT", "QZ1", "QZ2", "QZ3", "QZ4", 
                                                                                                                 "Test1", "Test2", "Homework", "Quiz", 
                                                                                                                 "Lab", "Overall")),
                       selectInput('ycol', 'Y-axis variable', c("HW1", "HW2", "HW3", 
                                                                "HW4", "HW5", "HW6", "HW7", "HW8", "HW9",
                                                                "ATT", "QZ1", "QZ2", "QZ3", "QZ4", 
                                                                "Test1", "Test2", "Homework", "Quiz", 
                                                                "Lab", "Overall")),
                       sliderInput("op",
                                   "Opacity",
                                   value = 0.5,
                                   min = 0,
                                   max = 1),
                       
                       radioButtons("line", "Show line",
                                    c("none",
                                      "lm",
                                      "loess"))
      ),
      width = 2),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Barchart", value = 1, ggvisOutput("grades")),
                  tabPanel("Histogram", value = 2, ggvisOutput("histTab2"), h4("Summary Statistics"), verbatimTextOutput("summary")),
                  tabPanel("Scatterplot", value = 3, ggvisOutput("scatterTab3"), "Correlation:", verbatimTextOutput("corr")),
                  id = "tabselected"
      ))
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$Grdist <- renderTable({
    t <- table(factor(dat$Grade, levels <- c('A+', 'A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D', 'F')))
    out1 <- as.data.frame(t)
    colnames(out1) <- c('Grade', 'Freq')
    out1$Prop <- round(out1$Freq/sum(out1$Freq), 2)
    out1
  })
  
  vis_barchart <- reactive({
    dat$Grade <- factor(dat$Grade, levels <- c('A+', 'A', 'A-', 'B+', 'B', 'B-', 'C+', 'C', 'C-', 'D', 'F'))
    bar <- layer_bars(ggvis(dat, ~Grade), fill:="skyblue", opacity := 0.5)
    add_axis(bar, "y", title = "frequency")
  })
  
  vis_barchart %>% bind_shiny("grades")
  
  vis_histogram <- reactive({
    title <- input$var
    vec <- unlist(dat[input$var])
    histog <- layer_histograms(ggvis(dat, ~vec), fill:="lightgrey", width = input$width)
    add_axis(histog, "x", title = title)
  })
  
  vis_histogram %>% bind_shiny("histTab2")

  
  output$summary <- renderPrint({
     cat(print_stats(dat[,input$var]))
  })
  
  vis_scatterplot <- reactive({
    title1 <- input$xcol
    title2 <- input$ycol
    vec1 <- unlist(dat[input$xcol])  
    vec2 <- unlist(dat[input$ycol])
 if (input$line == "none") {
    
    scatter <- layer_points(ggvis(dat, ~vec1, ~vec2), opacity:=input$op)
    scatter <- add_axis(scatter, "x", title = input$xcol)
    scatter <- add_axis(scatter, "y", title = input$ycol)
    scatter
 } else {
   vec1 <- prop("x", as.symbol(input$xcol))
   vec2 <- prop("y", as.symbol(input$ycol))
   scatter <- dat%>%
     ggvis(x=vec1, y=vec2) %>%
     layer_points(opacity:= input$op) %>%
     layer_model_predictions(model = input$line)
   scatter <- add_axis(scatter, "x", title = input$xcol)
   scatter <- add_axis(scatter, "y", title = input$ycol)
   scatter
   
 }
  })
  vis_scatterplot %>% bind_shiny("scatterTab3")
  
  output$corr <- renderPrint({
    cat(cor(unlist(dat[input$xcol]), unlist(dat[input$ycol])))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)