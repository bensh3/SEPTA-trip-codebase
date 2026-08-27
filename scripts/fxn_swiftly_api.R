# Swiftly functions ------------------------------------------------------

#' Swiftly OTP core request function
#'
#' @param agencyKey Swiftly agency key
#' @param resource URL suffix for specific OTP API
#' @param startDate Start date passed as Date
#' @param endDate End date passed as Date
#' @param ... Additional parameters passed
#' @param resformat format to GET Swiftly data
#' @return body of Swiftly API request as a tibble
get_swiftly <- function(agencyKey, category, resource, startDate, endDate, ..., resformat = "csv") {
  if (!is.null(agencyKey)) {
    agencyKey <- match.arg(agencyKey, c("septa", "septa-rail"))
  }
  if (!is.null(startDate)) {
    if (!inherits(startDate, "Date")) {
      stop("`startDate` must be a date")
    }
    startDate <- format(startDate, "%m-%d-%Y")
  }
  if (!is.null(endDate)) {
    if (!inherits(endDate, "Date")) {
      stop("`endDate` must be a date")
    }
    endDate <- format(endDate, "%m-%d-%Y")
  }
  params <- list(startDate = startDate, endDate = endDate, ..., format = resformat)

  res <- httr2::request("https://api.goswift.ly/") |>
    httr2::req_url_path_append(paste(category, agencyKey, resource, sep = "/")) |>
    httr2::req_headers(`Authorization` = Sys.getenv("swiftly_api_hist")) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_perform()

  if (resformat == "csv") {
    readr::read_csv(I(rawToChar(res$body)), show_col_types = F)
  } else if (resformat == "json") {
    jsonlite::fromJSON(rawToChar(res$body), flatten = T)$data
  }
}

#' GET agency routes
#'
#' @param route single route
#' @param verbose additional information
#' @return route information
get_routes <- function(agencyKey, route, verbose = NULL) {
  get_swiftly(
    agencyKey = agencyKey,
    category = "info",
    resource = "routes",
    startDate = NULL,
    endDate = NULL,
    route = route,
    verbose = verbose,
    resformat = "json"
  )
}

#' GET arrival/departure observations
#'
#' @param routes single or multiple routes
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

#' GET trip comparion to scheduled
#'
#' @param routes single route
#' @param directionId 0 or 1
#' @return trip comparison to scheduled
get_trip_comp_scheduled <- function(
  agencyKey,
  routes,
  directionId = NULL,
  startDate,
  beginTime = NULL,
  daysOfWeek = NULL,
  endDate = NULL,
  endTime = NULL,
  excludeDates = NULL,
  additionalGroupBy = NULL,
  allowableLongSecs = NULL,
  allowableShortSecs = NULL
) {
  get_swiftly(
    agencyKey = agencyKey,
    category = "run-times",
    resource = "trip-comparison-to-scheduled",
    routes = routes,
    directionId = directionId,
    startDate = startDate,
    daysOfWeek = daysOfWeek,
    endDate = endDate,
    excludeDates = excludeDates,
    additionalGroupBy = additionalGroupBy,
    allowableLongSecs = allowableLongSecs,
    allowableShortSecs = allowableShortSecs
  )
}

#' GET trip observations
#'
#' @param routes single route
#' @param directionId 0 or 1
#' @return trip observations
get_trip_obvs <- function(
  agencyKey,
  routes,
  directionId = NULL,
  startDate,
  beginTime = NULL,
  daysOfWeek = NULL,
  endDate = NULL,
  endTime = NULL,
  excludeDates = NULL
) {
  get_swiftly(
    agencyKey = agencyKey,
    category = "run-times",
    resource = "trip-observations",
    routes = routes,
    directionId = directionId,
    startDate = startDate,
    daysOfWeek = daysOfWeek,
    endDate = endDate,
    excludeDates = excludeDates
  )
}

#' GET path observations
#'
#' @param routes single route
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

#' GET missing service
#'
#' @param routes single route
#' @param directionId 0 or 1
#' @return stop segment-level path observations
get_missing_service <- function(
  agencyKey,
  routes,
  directionId,
  startDate,
  beginTime = NULL,
  daysOfWeek = NULL,
  endDate = NULL,
  endTime = NULL,
  excludeDates = NULL,
  includeNtdFields = NULL
) {
  get_swiftly(
    agencyKey = agencyKey,
    category = "service-metrics",
    resource = "missing-service",
    routes = routes,
    directionId = directionId,
    startDate = startDate,
    daysOfWeek = daysOfWeek,
    endDate = endDate,
    excludeDates = excludeDates,
    includeNtdFields = includeNtdFields
  )
}
