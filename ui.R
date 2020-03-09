###############################
### Messy Data - ui.R ###
###############################

library(shiny) 

shinyUI(pageWithSidebar( 
  
  headerPanel("Messy Data"), 
  
  sidebarPanel( 
    
    checkboxInput(inputId = "square",
                  label = "Change shapes to squares in corrplot?",
                  value = FALSE),
    
    checkboxInput(inputId = "bg_colour",
                  label = "Add yellow background colour to corrplot?",
                  value = FALSE)
  ),
  mainPanel(
    tabsetPanel( 
      tabPanel("Task 1 corrplot", plotOutput("Task1corrplot"), 
               "Summary 1.1", verbatimTextOutput("textDisplay1_1")), 
      tabPanel("Task 1 balloon plot", plotOutput("Task1balloonplot"), "Summary 1.2", 
               verbatimTextOutput("textDisplay1_2")),
      tabPanel("Task 2 corrplot", plotOutput("Task2corrplot"), "Summary 2.1", 
               verbatimTextOutput("textDisplay2_1")), 
      tabPanel("Task 2 balloon plot", plotOutput("Task2balloonplot"), "Summary 2.2", 
               verbatimTextOutput("textDisplay2_2")),
      tabPanel("Task 3 Logistic Regression", 
               tableOutput("Evaluation"),
               verbatimTextOutput("textDisplay3_1")),
      tabPanel("Task 3 AUC - training and test sets", plotOutput("Task3AUCplot"), 
               "AUC comment - training and test sets", verbatimTextOutput("textDisplay3_2"))
          )
  )
))

