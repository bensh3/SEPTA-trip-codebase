library(tidyverse)
source("fxn_swiftly.R")

selAgency <- "septa"
selRoute <- "58"
selDirection <- "0,1"
selStartDate <- ymd("2026-07-12")

arr_dep <- get_arr_dep(selAgency, selRoute, selDirection, selStartDate)
path_obvs <- get_path_obvs(selAgency, selRoute, selDirection, selStartDate)

stop_visits <- left_join(arr_dep, select(path_obvs, service_date, trip_id, stop_id = to_stop_id, stop_path_length)) |>
  mutate(dwell = observed_departure_time - observed_arrival_time) |>
  select(
    service_date,
    trip_id_performed = trip_id,
    trip_stop_sequence = stop_order,
    scheduled_stop_sequence = gtfs_stop_sequence,
    vehicle_id = departure_vehicle_id,
    dwell,
    stop_id,
    timepoint = is_schedule_adherence_stop,
    schedule_arrival_time = scheduled_arrival_time,
    schedule_departure_time = scheduled_departure_time,
    actual_arrival_time = observed_arrival_time,
    actual_deprature_time = observed_departure_time,
    distance = stop_path_length
  )
