library(shiny)
library(shinythemes)
library(googlesheets)
library(dplyr)
library(DT)

### Get Data
detentionserved <- gs_key("1ltL1QjCUrgK3CBHKhzKHsOHNDce3Zj_1Lykhqtifauk") %>%
  gs_read(ws = 1)

detentionissued <- gs_key("1ltL1QjCUrgK3CBHKhzKHsOHNDce3Zj_1Lykhqtifauk") %>%
  gs_read(ws = 2)

report_table <- detentionissued %>%
  left_join(detentionserved, by = c("First Name",
                                    "Last Name",
                                    "Grade",
                                    c("Date given" = "Date Given"))) %>%
  select(2,4,5,8,9,12,13)
###

# Define UI for application
ui <- navbarPage(
  
  theme = shinytheme("cerulean"),
   
   # Application title
   title = "ICHS Detention System",
   
   # Tab for issuance
   tabPanel(
     "Detentions Issued",
     sidebarLayout(
       
       sidebarPanel(
         htmlOutput(
           "issue",
           container = tags$iframe,
           src = "https://docs.google.com/forms/d/e/1FAIpQLSfYQnGM67fs34TTBeU5XEOuxfZx0_iQ9cEWaIyrmiHTXexppA/viewform?embedded=true",
           width = 400,
           height = 625,
           frameborder = 0,
           marginheight = 0)
         ),
     
       mainPanel(
         DT::dataTableOutput("issued"),
         actionButton("refresh", "Refresh")
       )
     )
   ),

   # Tab for Detention served
   tabPanel(
     "Detentions Served",
     
     sidebarLayout(
       
       sidebarPanel(
         htmlOutput(
         "serve",
         container = tags$iframe,
         src = "https://docs.google.com/forms/d/e/1FAIpQLSc_L7aNSBdJ4UE7DpsX2NNdIKkOYt9qTg1KiCk4lWzHkiWvWw/viewform?embedded=true",
         width = 400,
         height = 625,
         frameborder = 0,
         marginheight = 0
       )
       ),
       mainPanel(
         DT::dataTableOutput("served"),
         actionButton("refresh", "Refresh")
         )
     )
   )
)

# Define server logic
server <- function(input, output) {
   
   output$served <- DT::renderDataTable({
     
     input$refresh

     report_table %>%
       DT::datatable(
         extensions = "Buttons",
         options = list(
           dom = "Bfrtip",
           buttons = c("excel", "pdf","print")
         )
       )

   })
   
   output$issued <- DT::renderDataTable({
     
     input$refresh

     detentionissued %>%
       select(-Timestamp) %>%
       DT::datatable(
         extensions = "Buttons",
         options = list(
           dom = "Bfrtip",
           buttons = c("excel","pdf","print")
         )
       )
   })

   }

# Run the application 
shinyApp(ui = ui, server = server)

