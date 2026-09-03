source("scripts/fxn_swiftly_to_tides.R")
library(tidyverse)

stopVisits <- get_stop_visits(
  selAgency = "septa-rail",
  selRoute = "NOR,WAR",
  selDirection = "0,1",
  selStartDate = ymd("2026-07-11"),
  selEndDate = ymd("2026-07-12")
)
