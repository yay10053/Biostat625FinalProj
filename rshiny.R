library(shiny)
library(shinythemes)
library(plotly)

library(maps)
library(mapproj)

df <- read.csv("C:/Users/cszet/Desktop/Fall 2022/BIOSTAT 625/Final Proj/merge_df.csv")

#### define function
conditional <- function(condition, success) {
  if (condition) success else TRUE
}

#Map function; source: https://shiny.rstudio.com/tutorial/written-tutorial/lesson5/
percent_map <- function(var, color, legend.title, min = 0, max = 100) {
  
  # generate vector of fill colors for map
  shades <- colorRampPalette(c("white", color))(100)
  
  # constrain gradient to percents that occur between min and max
  var <- pmax(var, min)
  var <- pmin(var, max)
  percents <- as.integer(cut(var, 100, 
                             include.lowest = TRUE, ordered = TRUE))
  fills <- shades[percents]
  
  # plot choropleth map
  map("county", fill = TRUE, col = fills, 
      resolution = 0, lty = 0, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  # overlay state borders
  map("state", col = "white", fill = FALSE, add = TRUE,
      lty = 1, lwd = 1, projection = "polyconic", 
      myborder = 0, mar = c(0,0,0,0))
  
  # add a legend
  inc <- (max - min) / 4
  legend.text <- c(paste0("Very little"),
                   paste0("Little"),
                   paste0("Moderate"),
                   paste0("High"),
                   paste0("Very high"))
  
  legend("bottomleft", 
         legend = legend.text, 
         fill = shades[c(1, 25, 50, 75, 100)], 
         title = legend.title)
}

# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  # theme = "cerulean",  # <--- To use a theme, uncomment this
                  "Diabetes Infomation Page",
                  tabPanel("Diabetes Data Table",
                           sidebarPanel(
                             selectInput("statelv", "State", choices = df$State),
                             selectInput("yearlv", "Year", choices = df$Year)
                             
                           ),
                           # sidebarPanel
                           mainPanel(
                             tableOutput("my_table")
                           ) # mainPanel
                           
                  ), # Navbar 1, tabPanel
                  
                  
                  tabPanel("Map", 
                           sidebarPanel(
                             helpText("Create demographic maps with 
        Diabetes Infomation"),
                             selectInput("var", "Variable", choices = c("Diagnosed Diabetes", "Overall SVI",
                                                                        "Food Insecurity", "No Health Insurance"))
                             
                           ),
                           mainPanel(plotOutput("map")) 
                           
                  ), 
                  
                  
                  
                  tabPanel("Health Self-Evalution Center", 
                           sidebarPanel(
                             HTML("<h3>Input values</h3>"),
                             sliderInput("height", 
                                         label = "Height (inches)", 
                                         value = 65, 
                                         min = 40, 
                                         max = 85),
                             sliderInput("weight", 
                                         label = "Weight (lb)", 
                                         value = 130, 
                                         min = 20, 
                                         max = 400),
                             sliderInput("a1c", 
                                         label = "A1c (%)", 
                                         value = 5, 
                                         min = 0, 
                                         max = 10),
                             actionButton("submitbutton", 
                                          "Submit", 
                                          class = "btn btn-primary")
                           ),
                           
                           mainPanel(
                             tags$label(h3('Result')), # Status/Output Text Box
                             verbatimTextOutput('contents'),
                             tableOutput('tabledata') # Results table
                           )       
                           
                           )
                  
                  
                  
                  
                 # navbarPage
)) # fluidPage


# Define server function  
server <- function(input, output) {
  ##data table
  table <- reactive({
    df[,-2] %>% 
      filter(
        conditional(input$statelv != "", State == input$statelv),
        conditional(input$yearlv != "", Year == input$yearlv))
  })
  
  output$my_table <- renderTable({
    table()
  })
  
  ###
  datasetInput <- reactive({  
    
    bmi <- 703*(input$weight/(input$height)^2 )
    bmi.comment <- ifelse(bmi < 18.5 || bmi > 24.9, "Look out for your diet!", "Healthy!")
    a1c <- input$a1c
    a1c.comment <- if(a1c < 5.7){"Healthy!"}else if(a1c < 6.4){"Pre Diabetes"}else{"Diabetes"}
    res <- data.frame(Indicator = c("BMI", "A1c"), value = c(bmi, a1c), result = c(bmi.comment, a1c.comment))
    print(res)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation completed.") 
    } else {
      return("Please enter your weight, height, and A1c information on the left.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
  output$map <- renderPlot({
    num <- which(input$var == c("Diagnosed Diabetes", "Overall SVI",
                         "Food Insecurity", "No Health Insurance"))
    percent_map(df[,(4+num)], "red", paste0("Density of ", input$var))
  })
  
  
} # server


# Create Shiny object
shinyApp(ui = ui, server = server)
