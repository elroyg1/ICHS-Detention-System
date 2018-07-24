library(shiny)
library(googlesheets)
library(dplyr)
library(DT)

# Define UI for application
ui <- navbarPage(
   
   # Application title
   title = "ICHS Detention",
   
   # Tab for issuance
   tabPanel(
     "Detention Issuance",
     wellPanel(
       htmlOutput("googleForm1",
                  container = tags$iframe,
                  src = "https://docs.google.com/forms/d/e/1FAIpQLSfYQnGM67fs34TTBeU5XEOuxfZx0_iQ9cEWaIyrmiHTXexppA/viewform?embedded=true",
                  width = 700,
                  height = 520,
                  frameborder = 0,
                  marginheight = 0)
     )
   ),
   
   # Tab for Detention served
   tabPanel(
     "Detention Served",
     htmlOutput(
       "googleForm2",
       container = tags$iframe,
       src = "https://docs.google.com/forms/d/e/1FAIpQLSc_L7aNSBdJ4UE7DpsX2NNdIKkOYt9qTg1KiCk4lWzHkiWvWw/viewform?embedded=true",
       width = 700,
       height = 520,
       frameborder = 0,
       marginheight = 0
     )
   ),
   
   # Tab for output
   tabPanel(
     "Report",
     sidebarLayout(
       sidebarPanel(
         textInput(
           "lname",
           "Last Name"
         ),
         textInput(
           "fname",
           "First Name"
         ),
         textInput(
           "grade",
           "Grade"
         )
       ),
       mainPanel(
         dataTableOutput("table"),
         actionButton("refresh", "Refresh Sheet")
       )
     )
   )
)

# Define server logic
server <- function(input, output) {
   
   output$table <- renderDataTable({
     
     input$refresh

     report_table %>%
       filter(`First Name` == input$fname,
              `Last Name` == input$lname,
              `Grade` == input$grade) %>%
       datatable(
         extensions = "Buttons",
         options = list(
           dom = "Bfrtip",
           buttons = c("excel", "pdf","print")
         )
       )
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

