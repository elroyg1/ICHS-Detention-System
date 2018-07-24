library(googlesheets)

# shiny_token <- gs_auth()

# saveRDS(shiny_token,"shiny_app_token.rds")

gs_auth(token = "shiny_app_token.rds")

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

