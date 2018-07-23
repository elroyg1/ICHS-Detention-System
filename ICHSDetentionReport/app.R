library(shiny)
library(googlesheets)
library(dplyr)
library(DT)

# Link to googlesheet



# Define UI for application
ui <- navbarPage(
   
   # Application title
   title = "ICHS Detention",
   
   # Tab for input
   tabPanel(
     "Detention Entry",
     wellPanel(
       htmlOutput("googleForm",
                  container = tags$iframe,
                  src = "https://docs.google.com/forms/d/e/1FAIpQLSfYQnGM67fs34TTBeU5XEOuxfZx0_iQ9cEWaIyrmiHTXexppA/viewform?embedded=true",
                  width = 700,
                  height = 520,
                  frameborder = 0,
                  marginheight = 0)
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

     gs_key("1ltL1QjCUrgK3CBHKhzKHsOHNDce3Zj_1Lykhqtifauk",
            lookup = F,
            visibility = "public")%>%
       gs_read()%>%
       filter(`First Name` == input$fname,
              `Last Name` == input$lname) %>%
       select(`Date given`:`Date served`) %>%
       datatable(
         extensions = "Buttons",
         options = list(
           dom = "Bfrtip",
           buttons = c("excel", "pdf","print")
         )
       )
   }
     
   )
}

# Run the application 
shinyApp(ui = ui, server = server)

