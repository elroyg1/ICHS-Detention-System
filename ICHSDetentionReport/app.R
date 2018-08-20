library(shiny)
library(shinyjs)
library(shinythemes)
library(googlesheets)
library(dplyr)
library(stringr)
library(DT)

# Credentials
login <-"1ltL1QjCUrgK3CBHKhzKHsOHNDce3Zj_1Lykhqtifauk" %>%
  gs_key() %>%
  gs_read(ws = "Credentials")

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
      passwordInput("password", "Password"),
      actionButton("submit","Submit"),
      textOutput("error")
    )
  ),
  
  #Profile Panel
  tabPanel(
    "Profile",
    
    wellPanel(
      textOutput("username"),
      textOutput("login_email"),
      passwordInput("current_password","Current Password"),
      passwordInput("new_password","New Password"),
      actionButton("update", "Update"),
      textOutput("update_error")
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
  hideTab(inputId = "tabs",
          target = "Profile")
  
  # Login
  observeEvent(input$submit,{

    "1ltL1QjCUrgK3CBHKhzKHsOHNDce3Zj_1Lykhqtifauk" %>%
      gs_key() %>%
      gs_add_row(ws="Log",
                 input = c(paste(Sys.time()),
                           input$email,
                           input$password)
      )
    
    if(input$password == last(login$Password[input$email==login$Email])){
      showTab(inputId = "tabs",
              "Profile")
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
  
  #Update Profile

    output$username <- renderText({
      last(login$Name[input$email==login$Email])
    })
    
    output$login_email <- renderText({
      input$email
    })
  observeEvent(input$update,{
    if(input$current_password == last(login$Password[input$email==login$Email])){
      "1ltL1QjCUrgK3CBHKhzKHsOHNDce3Zj_1Lykhqtifauk" %>%
        gs_key() %>%
        gs_add_row(
          ws="Credentials",
          input = c(last(login$Name[input$email==login$Email]),
                    input$email,
                    input$new_password)
        )
      reset("new_password")
      reset("current_password")
    }else{
      output$update_error <- renderText({"Current password incorrect. Please try again."})
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

