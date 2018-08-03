library(shiny)
library(shinyjs)
library(shinythemes)
library(googlesheets)
library(dplyr)
library(stringr)
library(DT)

# Define UI for application
ui <- navbarPage(
 
  
  # Theme
  theme = shinytheme("cerulean"),
   
   # Application title
   title = "ICHS Detention System",
  
  # navbarPage ID
  id = "tabs",
  
  # Tab for login
  tabPanel(
    "Login",
    
    wellPanel(
      useShinyjs(),
      textInput("email", "Email"),
      textInput("password", "Password"),
      actionButton("submit","Submit"),
      textOutput("error")
    )
  ),
   
   # Tab for issuance
   tabPanel(
     
     "Detentions Issued",
     
     wellPanel(
       
       htmlOutput(
         "issue",
         container = tags$iframe,
         src = "https://docs.google.com/forms/d/e/1FAIpQLSfYQnGM67fs34TTBeU5XEOuxfZx0_iQ9cEWaIyrmiHTXexppA/viewform?embedded=true",
         width = 800,
         height = 525,
         frameborder = 0,
         marginheight = 0
       )
     )
   ),

   # Tab for Detention served
   tabPanel(
     "Detentions Served",
     
     wellPanel(
       htmlOutput(
         "serve",
         container = tags$iframe,
         src = "https://docs.google.com/forms/d/e/1FAIpQLSc_L7aNSBdJ4UE7DpsX2NNdIKkOYt9qTg1KiCk4lWzHkiWvWw/viewform?embedded=true",
         width = 800,
         height = 425,
         frameborder = 0,
         marginheight = 0
       )
     )
     ),
  # Tab for report 
  tabPanel(   
    "Report",
  
  sidebarLayout(
    sidebarPanel(
      radioButtons(
        "choice",
        "Choose one",
        choices = c(
          "Detentions Issued" = 1,
          "Detentions Served" = 2
        )
      ),
      actionButton("display","Display")
    ),
    mainPanel(
      fluidRow(DT::dataTableOutput("report"),
               actionButton("refresh", "Refresh"))
    )
  )
)
)

# Define server logic
server <- function(input, output) {
  
  hideTab(inputId = "tabs",
          target = "Report"
          )
  hideTab(inputId = "tabs",
          target = "Detentions Issued"
  )
  hideTab(inputId = "tabs",
          target = "Detentions Served"
  )
  
  # Login
  observeEvent(input$submit,{
  
    "1ltL1QjCUrgK3CBHKhzKHsOHNDce3Zj_1Lykhqtifauk" %>%
      gs_key() %>%
      gs_add_row(ws="Log",
                 input = c(paste(Sys.time()),
                           input$email,
                           input$password)
      )
    
    if(str_detect(input$email, "^[[:graph:]]+@immaculatehigh.edu.jm$") & 
       input$password == "Password"){
      showTab(inputId = "tabs",
              "Report")
      showTab(inputId = "tabs",
              "Detentions Issued")
      showTab(inputId = "tabs",
              "Detentions Served")
      hideTab(inputId = "tabs",
              target = "Login")
    } else {
      output$error <- renderText({"Email/Password incorrect. Please try again."})
    }
  })
  
  # Display report
  observeEvent(input$display,{
    
    ws <- reactive({
      ifelse(
        input$choice == 1,
        2,
        1
      )
    })

    input$refresh
    
    output$report <- renderDataTable({
      
      "1ltL1QjCUrgK3CBHKhzKHsOHNDce3Zj_1Lykhqtifauk" %>%
        gs_key() %>%
        gs_read(ws=ws()) %>% 
        select(-Timestamp) %>%
        datatable(
          rownames = F,
          extensions = "Buttons",
          options = list(
            dom = "Bfrtip",
            buttons = c("excel", "pdf","print")
          )
        )
      
    })
    
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

