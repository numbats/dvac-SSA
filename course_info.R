######## Course info ########
library(tidyverse)

# Start of unit (a Monday) and length
start_unit <- as.Date("2026-11-23")
n_weeks    <- 4

# No lecture on this day
no_lecture <- as.Date("2026-12-04")

# Topics: one row per lecture
topics <- tribble(
  ~Lecture, ~Topic, ~Reference, ~Reference_URL,
  1,  "Introduction to Data Visualisation", "", "",
  2,  "The Good, the Bad and the Ugly of Data Visualisation", "", "",
  3,  "Visualisations in Power BI", "", "",
  4,  "Visualisations in R: Part 1", "", "",
  5,  "Wrangling your Data for Visualisation", "", "",
  6,  "Visualisations in R: Part 2", "", "",
  7,  "Iterating and Polishing your Visualisation", "", "",
  8,  "Communication Theory", "", "",
  9,  "Building a Data Narrative", "", "",
  10, "Advanced Topics: Infographics, Uncertainty and Missingness", "", "",
  11, "Advanced Topics: Animation and Interactivity", "", "",
  12, "Revision", "", ""
)

# Lecture dates: Mon/Wed/Fri in the unit (minus the skipped day),
# plus the Revision session after semester (date TBD)
calendar <- tibble(Date = seq(start_unit, by = "day", length.out = 7 * n_weeks)) |>
  filter(format(Date, "%u") %in% c("1", "3", "5"), Date != no_lecture) |>
  mutate(Lecture = row_number()) |>
  add_row(Date = as.Date(NA), Lecture = 12L)

# Combine topics and dates, then add the no-lecture day
schedule <- topics |>
  left_join(calendar, by = "Lecture") |>
  select(Lecture, Date, everything()) |>
  bind_rows(tibble(Date = no_lecture, 
                   Topic = "------ No lecture -----", 
                   Reference = "",
                   Reference_URL = "")) |>
  arrange(Date)


# # Add assignment details
# lastmon <- function(x) {
#     7 * floor(as.numeric(x - 1 + 4) / 7) + as.Date(1 - 4, origin = "1970-01-01")
# }
# 
# assignments <- read_csv(here::here("assignments.csv")) |>
#     mutate(
#         Date = lastmon(Due),
#         Moodle = paste0("https://learning.monash.edu/mod/assign/view.php?id=", Moodle),
#         File = paste0("assignments/", File)
#     )
# 
# schedule <- schedule 
#     # full_join(assignments, by = "Date") |>
#     # mutate(Week = if_else(is.na(Week) & Date > "2024-05-20", 13, Week))
# 
# show_assignments <- function(week) {
#     ass <- schedule |>
#         filter(
#             Week >= week & (week > Week - 3 | week > 8),
#             !is.na(Assignment),
#         ) |>
#         select(Assignment:File)
#     if (NROW(ass) > 0) {
#         cat("\n\n## Assignments\n\n")
#         for (i in seq(NROW(ass))) {
#             cat("* [", ass$Assignment[i], "](../", ass$File[i], ") is due on ",
#                 format(ass$Due[i], "%A %d %B.\n"),
#                 sep = ""
#             )
#         }
#     }
# }
# 
# 
# submit <- function(schedule, assignment) {
#     ass <- schedule |>
#         filter(Assignment == assignment)
#     due <- format(ass$Due, "%e %B %Y") |> stringr::str_trim()
#     url <- ass$Moodle
#     button <- paste0(
#         "<br><br><hr><b>Due: ", due, "</b><br>",
#         "<a href=", url, " class = 'badge badge-large badge-blue'>",
#         "<font size='+2'>&nbsp;&nbsp;<b>Submit</b>&nbsp;&nbsp;</font><br></a>"
#     )
#     cat(button)
# }
