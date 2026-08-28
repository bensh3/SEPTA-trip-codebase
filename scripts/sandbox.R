source("scripts/fxn_swiftly_to_tides.R")
library(tidyverse)

stopVisits <- get_stop_visits(
  selAgency = "septa",
  selRoute = "45,43",
  selDirection = "0",
  selStartDate = lubridate::ymd("2026-07-11"),
  selEndDate = lubridate::ymd("2026-07-12")
)
