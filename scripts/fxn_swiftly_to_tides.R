source("scripts/fxn_swiftly.R")

get_trips_performed <- function(selAgency, selRoute, selDirection, selStartDate) {
  dplyr::left_join(
    dplyr::mutate(get_trip_obvs(selAgency, selRoute, selDirection, selStartDate), routeId = as.character(routeId)),
    dplyr::select(get_routes(selAgency, selRoute)$routes, routeId = id, route_type_agency = type)
  ) |>
    dplyr::select(
      service_date = serviceDate,
      trip_id_performed = tripId,
      vehicle_id = vehicleIds,
      trip_id_scheduled = tripId,
      route_id = routeId,
      route_type_agency,
      pattern_id = tripPatternId,
      direction_id = directionId,
      operator_id = driverIds,
      block_id = blockId,
      schedule_trip_start = scheduledTripStartTime,
      actual_trip_start = observedTripStartTime,
      actual_trip_end = observedTripEndTime,
      schedule_relationship = scheduleRelationship
    )
}

get_stop_visits <- function(selAgency, selRoute, selDirection, selStartDate) {
  dplyr::left_join(
    get_arr_dep(selAgency, selRoute, selDirection, selStartDate),
    dplyr::select(
      get_path_obvs(selAgency, selRoute, selDirection, selStartDate),
      service_date,
      trip_id,
      stop_id = to_stop_id,
      stop_path_length
    )
  ) |>
    dplyr::arrange(service_date, route_id, direction_id, stop_id, observed_arrival_time) |>
    dplyr::mutate(
      dwell = observed_departure_time - observed_arrival_time,
      .deviance = observed_arrival_time - scheduled_arrival_time,
      .headway = observed_arrival_time - dplyr::lag(observed_arrival_time),
      .by = c(service_date, stop_id)
    ) |>
    dplyr::select(
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
      distance = stop_path_length,
      .deviance,
      .headway
    )
}
