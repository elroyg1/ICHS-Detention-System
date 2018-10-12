library(googlesheets)
library(tidyverse)

shinyToken <- gs_auth()

saveRDS(shinyToken, "shiny_app_token.rds")

gs_auth(token = "googlesheet_token.rds")

ss <- "14RVRF2rkEy4ARX-wOW9JL_1EPrVf_HiHXukfG14Njvc" %>%
  gs_key()

rtest<- "100"