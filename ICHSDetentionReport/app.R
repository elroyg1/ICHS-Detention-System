library(shiny)
library(shinyjs)
library(shinythemes)
library(googlesheets)
library(dplyr)
library(stringr)
library(DT)

# Credentials
login <-ss %>%
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
      extendShinyjs(text = "shinyjs.closeWindow = function() { window.close(); }",
                    functions = c("closeWindow")),
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
         src = "https://docs.google.com/forms/d/e/1FAIpQLSfhQtVah0CshARv_3VzJ4CeCGb8dkh1AT38hNqFZSYp_va-GA/viewform?embedded=true",
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
         src = "https://docs.google.com/forms/d/e/1FAIpQLSd31k-RnlkuqTb7ErGvD2DPTM-o0hjJps8wBIpqitBcw9QjkQ/viewform?embedded=true",
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
),

# Logout Tab

tabPanel(
  "Logout",
  wellPanel(
    actionButton("logout","Logout")
  )
)
)

# Define server logic
server <- function(input, output,session) {
  
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
  hideTab(inputId = "tabs",
          target = "Logout")
  
  # Login
  observeEvent(input$submit,{

    ss %>%
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
      showTab(inputId = "tabs",
              "Logout")
      hideTab(inputId = "tabs",
              target = "Login")
      if(length(login$Password[input$email==login$Email])==1){
        updateNavbarPage(session,
                         "tabs",
                         selected = "Profile")
      }else{
        updateNavbarPage(session,
                         "tabs",
                         selected = "Detentions Issued")
      }
      reset(input$email)
      reset(input$password)
    } else {
      output$error <- renderText({"Email/Password incorrect. Please try again."})
    }
  })
  
  #Update Profile

    output$username <- renderText({
     paste("Welcome ",last(login$Name[input$email==login$Email]), 
           ". If this is your first time, please enter your new password using the form below.") 
    })
   
  observeEvent(input$update,{
    if(input$current_password == last(login$Password[input$email==login$Email])){
      
      ss %>%
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
      
      ss %>%
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
  
  # Logout
  observeEvent(input$logout,{
    js$closeWindow()
    stopApp()
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

