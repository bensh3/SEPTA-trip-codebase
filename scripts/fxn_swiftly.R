# Swiftly functions ------------------------------------------------------

#' Swiftly OTP core request function
#'
#' @param agencyKey Swiftly agency key
#' @param resource URL suffix for specific OTP API
#' @param ... Additional parameters passed
#' @return table of route_ids and directions
get_swiftly <- function(agencyKey, category, resource, startDate, endDate, ..., resformat = "csv") {
  startDate <- format(startDate, "%m-%d-%Y")
  endDate <- if (!is.null(endDate)) {
    format(endDate, "%m-%d-%Y")
  }
  params <- list(startDate = startDate, endDate = endDate, ..., format = resformat)

  res <- httr2::request("https://api.goswift.ly/") |>
    httr2::req_url_path_append(paste(category, agencyKey, resource, sep = "/")) |>
    httr2::req_headers(`Authorization` = Sys.getenv("swiftly_api_hist")) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_perform()

  if (resformat == "csv") {
    readr::read_csv(I(rawToChar(res$body)), show_col_types = F)
  }
}

#' GET arrival/departure observations
#'
#' @param routeKey single route
#' @param directionId 0 or 1
#' @return stop-level arrivals departures data
get_arr_dep <- function(
  agencyKey,
  routes,
  directionId,
  startDate,
  daysOfWeek = NULL,
  endDate = NULL,
  excludeDates = NULL,
  onlyFirstStopOfTrip = NULL,
  onlyScheduleAdherenceStops = NULL,
  scheduledStopsOnly = NULL
) {
  get_swiftly(
    agencyKey = agencyKey,
    category = "otp",
    resource = "arrivals-departures",
    routes = routes,
    directionId = directionId,
    startDate = startDate,
    daysOfWeek = daysOfWeek,
    endDate = endDate,
    excludeDates = excludeDates,
    onlyFirstStopOfTrip = onlyFirstStopOfTrip,
    onlyScheduleAdherenceStops = onlyScheduleAdherenceStops,
    scheduledStopsOnly = scheduledStopsOnly
  )
}

#' GET path observations
#'
#' @param routeKey single route
#' @param directionId 0 or 1
#' @return stop segment-level path observations
get_path_obvs <- function(
  agencyKey,
  routes,
  directionId,
  startDate,
  beginTime = NULL,
  daysOfWeek = NULL,
  endDate = NULL,
  endTime = NULL,
  excludeDates = NULL,
  groupByTimepoint = NULL,
  tripId = NULL
) {
  get_swiftly(
    agencyKey = agencyKey,
    category = "run-times",
    resource = "path-observations",
    routes = routes,
    directionId = directionId,
    startDate = startDate,
    daysOfWeek = daysOfWeek,
    endDate = endDate,
    excludeDates = excludeDates,
    groupByTimepoint = groupByTimepoint,
    tripId = tripId
  )
}
