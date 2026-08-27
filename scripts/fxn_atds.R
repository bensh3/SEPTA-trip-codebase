# ATDS functions ---------------------------------------------------------

get_rroc <- function(resource, ...) {
  params <- list(...)

  httr2::request("https://controlcenter.septa.org/common/api/") |>
    httr2::req_url_path_append(resource) |>
    httr2::req_url_query(!!!params) |>
    httr2::req_perform() |>
    httr2::resp_body_json(simplifyVector = T)
}

get_atds_interlocking <- function(startDate = NULL, endDate = NULL, route = NULL, direction = NULL) {
  if (!is.null(startDate)) {
    if (!inherits(startDate, "Date")) {
      stop("`startDate` must be a date")
    }
    startDate <- format(startDate, "%m/%d/%Y")
  }
  if (!is.null(endDate)) {
    if (!inherits(endDate, "Date")) {
      stop("`endDate` must be a date")
    }
    endDate <- format(endDate, "%m/%d/%Y")
  }
  if (!is.null(route)) {
    route <- match.arg(route, c("R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8"))
  }
  if (!is.null(direction)) {
    direction <- match.arg(direction, c("N", "S"))
  }
  get_rroc(
    "atds/delay-by-interlocking/index-new.php",
    start = startDate,
    end = endDate,
    route = route,
    direction = direction
  ) |>
    purrr::pluck(1)
}

get_atds_daily_delays <- function(oneDate = NULL) {
  if (!is.null(oneDate)) {
    if (!inherits(oneDate, "Date")) {
      stop("`oneDate` must be a date")
    }
    oneDate <- format(oneDate, "%m/%d/%Y")
  }
  get_rroc("atds/get-daily-delays-data.php", date = oneDate)
}

get_atds_onedate <- function(train = NULL, oneDate = NULL) {
  if (!is.null(oneDate)) {
    if (!inherits(oneDate, "Date")) {
      stop("`oneDate` must be a date")
    }
    oneDate <- format(oneDate, "%m/%d/%Y")
  }
  get_rroc("atds/get-atds-train-sched-by-date-data.php", train = train, date = oneDate)
}

get_atds_remarks <- function(startDate = NULL, endDate = NULL, keywords = NULL) {
  if (!is.null(startDate)) {
    if (!inherits(startDate, "Date")) {
      stop("`startDate` must be a date")
    }
    startDate <- format(startDate, "%m/%d/%Y")
  }
  if (!is.null(endDate)) {
    if (!inherits(endDate, "Date")) {
      stop("`endDate` must be a date")
    }
    endDate <- format(endDate, "%m/%d/%Y")
  }
  get_rroc("atds/remarks/", start = startDate, end = endDate, keywords = keywords)
}

get_tardy_daily <- function(selection = NULL, oneDate = NULL) {
  if (!is.null(selection)) {
    selection <- match.arg(selection, c("delays", "anulled", "anulled-miles"))
  }
  if (!is.null(oneDate)) {
    if (!inherits(oneDate, "Date")) {
      stop("`oneDate` must be a date")
    }
    oneDate <- format(oneDate, "%m/%d/%Y")
  }
  get_rroc(paste0("tardy/get-daily-", selection, "-data.php", date = oneDate))
}
