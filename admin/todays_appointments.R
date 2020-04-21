
#### lists today's appointments

library(rcanvas)
library(tidyverse)
library(httr)
library(jsonlite)

canvas_token <- "" # ENTER YOUR CANVAS ACCESS TOKEN
set_canvas_token(canvas_token)

course_id <- "course_9242" # Analysing Data
# Assuming you put your name in the title of the calendar thingy, give your first name
organiser <- "Milan"

appointment_groups <- rcanvas:::canvas_query(
  "https://canvas.sussex.ac.uk/api/v1/appointment_groups",
  list(scope="manageable",
       `context_codes[]` = course_id,
       per_page = 100), "GET") %>%
  rcanvas:::paginate() %>%
  purrr::map(httr::content, "text") %>% 
  purrr::map(jsonlite::fromJSON, flatten = TRUE) %>%
  dplyr::bind_rows() %>%
  filter(grepl(tolower(organiser), tolower(title))) %>% # Filters your appointments
  select(id)

appointments <- pmap_df(appointment_groups, function(id)  rcanvas:::canvas_query(
  paste0("https://canvas.sussex.ac.uk/api/v1/appointment_groups/", id),
  list(`include[]` = "child_events",
       per_page = 100), "GET") %>%
    rcanvas:::paginate() %>%
    purrr::map(httr::content, "text") %>% 
    purrr::map(jsonlite::fromJSON, flatten = TRUE) %>%
    pluck(1) %>%
    pluck("appointments") %>% filter(child_events_count != 0)
)


today_slots <- appointments %>%
  pull(child_events) %>%
  dplyr::bind_rows() %>%
  filter(start_at == Sys.Date()) %>% # this filters out today's appointments
  select(start_at, user.name, user.sortable_name) %>%
  rename(time = start_at,
         name = user.name,
         cand_no = user.sortable_name) %>%
  mutate(time = format(lubridate::ymd_hms(time), format = "%H:%M", tz=Sys.timezone()),
         cand_no = sub("Candidate No :\\s*", "", cand_no))

write.csv(today_slots, "todays_schedule.csv", row.names=F)
