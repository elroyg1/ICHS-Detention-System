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
     "Detentions Issued",
     sidebarLayout(
       
       sidebarPanel(
         htmlOutput(
           "issued",
           container = tags$iframe,
           src = "https://docs.google.com/forms/d/e/1FAIpQLSfYQnGM67fs34TTBeU5XEOuxfZx0_iQ9cEWaIyrmiHTXexppA/viewform?embedded=true",
           width = 400,
           height = 625,
           frameborder = 0,
           marginheight = 0)
         ),
     
       mainPanel(
         DT::dataTableOutput("issued")
       )
     )
   ),

   # Tab for Detention served
   tabPanel(
     "Detentions Served",
     
     sidebarLayout(
       
       sidebarPanel(
         htmlOutput(
         "served",
         container = tags$iframe,
         src = "https://docs.google.com/forms/d/e/1FAIpQLSc_L7aNSBdJ4UE7DpsX2NNdIKkOYt9qTg1KiCk4lWzHkiWvWw/viewform?embedded=true",
         width = 400,
         height = 625,
         frameborder = 0,
         marginheight = 0
       )
       ),
       mainPanel(
         DT::dataTableOutput("served")
         )
     )
   )
)

# Define server logic
server <- function(input, output) {
   
   output$served <- DT::renderDataTable({

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

