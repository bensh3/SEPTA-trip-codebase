source("scripts/fxn_atds.R")

ns_ntc <- get_atds_remarks(ymd("2026-01-01"), ymd("2026-09-01"), keywords = "FORD") |>
  bind_rows() |>
  filter(Origin %in% c("FORD", "KALB") & Destination %in% c("FORD", "KALB")) |>
  mutate(Date = mdy(word(str_replace_all(Date, "\\s+", " "), 1, 3)))

ns_ntc_detail <- map2(ns_ntc$Train, ns_ntc$Date, \(x, y) get_atds_onedate(x, y)) |> bind_rows()
