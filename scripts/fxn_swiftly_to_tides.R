source("scripts/fxn_swiftly_api.R")

get_trips_performed <- function(selAgency, selRoute, selDirection, selStartDate, selEndDate = NULL) {
  raw_trips <- get_trip_obvs(
    agencyKey = selAgency,
    routes = selRoute,
    directionId = selDirection,
    startDate = selStartDate,
    endDate = selEndDate
  )

  # FIX 2026-09-04: trip_id_scheduled was previously sourced from the same
  # tripId column as trip_id_performed, so the two could never differ. TIDES
  # keeps them separate on purpose, to show a trip that was added, rerouted,
  # or otherwise did not match its originally scheduled counterpart.
  #
  # get_trip_obvs() does not appear to return a distinct scheduled-trip
  # identifier today. Rather than silently duplicating trip_id_performed
  # again, this looks for a scheduledTripId field and falls back to NA with
  # a warning if one is not present, so the gap stays visible instead of
  # quietly wrong. Confirm the correct source field with Swiftly support and
  # update this block once it is known.
  if ("scheduledTripId" %in% names(raw_trips)) {
    raw_trips <- dplyr::mutate(raw_trips, trip_id_scheduled = scheduledTripId)
  } else {
    raw_trips <- dplyr::mutate(raw_trips, trip_id_scheduled = NA_character_)
    warning(
      "get_trips_performed(): no scheduledTripId field found in get_trip_obvs() ",
      "output. trip_id_scheduled has been set to NA rather than duplicating ",
      "trip_id_performed. See the FIX note in this function.",
      call. = FALSE
    )
  }

  raw_trips <- dplyr::mutate(raw_trips, routeId = as.character(routeId))

  routes_ref <- dplyr::select(
    get_routes(agencyKey = selAgency, route = selRoute)$routes,
    routeId = id,
    route_type_agency = type
  )

  dplyr::left_join(raw_trips, routes_ref, by = "routeId") |>
    dplyr::select(
      service_date = serviceDate,
      trip_id_performed = tripId,
      vehicle_id = vehicleIds,
      trip_id_scheduled,
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

get_stop_visits <- function(selAgency, selRoute, selDirection, selStartDate, selEndDate = NULL) {
  get_arr_dep(
    agencyKey = selAgency,
    routes = selRoute,
    directionId = selDirection,
    startDate = selStartDate,
    endDate = selEndDate
  ) |>
    dplyr::arrange(service_date, route_id, direction_id, stop_id, observed_arrival_time) |>
    dplyr::mutate(
      dwell = observed_departure_time - observed_arrival_time,
      .deviance = observed_arrival_time - scheduled_arrival_time,
      .headway = observed_arrival_time - dplyr::lag(observed_arrival_time),
      .by = c(service_date, route_id, direction_id, stop_id)
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
      actual_departure_time = observed_departure_time,
      .deviance,
      .headway
    )
}
