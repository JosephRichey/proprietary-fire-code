box::use(
  DBI[...],
  tibble[...],
  dplyr[...],
  lubridate[...],
  hms[as_hms],
  logger[...],
  shinyalert[...],
)

#' @export
CON <- tryCatch({
  DBI::dbConnect(
    drv = RMariaDB::MariaDB(),
    user = Sys.getenv("DB_USER"),
    password = Sys.getenv("DB_USER_PASSWORD"),
    host = Sys.getenv("DB_HOST"),
    port = 3306,
    dbname = Sys.getenv("DB_NAME"),
    ssl.ca = 'global-bundle.pem'
  )
}, error = function(e) {
  log_error(glue::glue("Database connection failed: {e$message}"),
            namespace = "app_data")
  NULL  # fallback to NULL if connection fails
})

if(is.null(CON)) {
  log_error("Database connection is NULL. Exiting.",
            namespace = "app_data")
  stop("Database connection failed. Exiting.")
} else {
  log_success("Database connection established successfully.", namespace = "app_data")
}



##### BOOTSTRAPPED FUNCTIONs ######
tz <- dbGetQuery(
  CON,
  "SELECT * FROM setting
  WHERE domain = 'global' AND
    setting_key = 'ltz'") |>
  pull(setting_value)

ConvertToLocalPosix <- function(dt,
                                input = c("datetime", "date"),
                                output = c("datetime", "date")) {
  input <- match.arg(input)
  output <- match.arg(output)

  # Restrict incompatible input-output combinations
  invalid_combo <- (input == "date"   && output == "datetime")

  if (invalid_combo) {
    stop(glue::glue("Invalid conversion: cannot format input type '{input}' as output type '{output}'"))
  }

  tz_local <- tz

  # Dates can only be converted to dates, so return as is
  if (input == "date" && output == "date") {
    return(as.Date(dt))
  }

  # Normalize input into UTC
  dt_utc <- as.POSIXct(dt, tz = "UTC")

  # Convert to local timezone
  dt_local <- lubridate::with_tz(dt_utc, tzone = tz_local)

  if(input == 'datetime' && output == 'date') {
    return(as.Date(dt_local, tz = tz_local))
  }

  # Default is to return as datetime.
  return(dt_local)
}

##### INCIDENT ######
#' @export
Incident <- dbGetQuery(CON, 'SELECT * FROM incident')

#' @export
Incident_Unit <- dbGetQuery(CON, 'SELECT * FROM incident_unit')

#' @export
Response <- dbGetQuery(CON, 'SELECT * FROM response')

#' @export
Unit <- dbGetQuery(CON, "SELECT * FROM unit") |>
  mutate(
    unit_type = if_else(
      is_active == 1,
      unit_type,
      paste("*", unit_type)
    )
  )

#' @export
Unit <- Unit |>
  bind_rows(
    data.frame(
    id = c(nrow(Unit) + 1, 0),
    unit_type = c('Mixed', 'None'),
    is_active = c(NA, NA)
  )
)

#' @export
Area <- dbGetQuery(CON, "SELECT * FROM area")

#' @export
Setting <- dbGetQuery(CON, "SELECT * FROM setting")

#' @export
Dispatch_Code <- dbGetQuery(CON, "SELECT * FROM dispatch_code")

##### Training #####

#' @export
Training <- dbGetQuery(CON, "SELECT * FROM training") |>
  mutate(start_time = ConvertToLocalPosix(start_time,
                                          input = 'datetime',
                                          output = 'datetime'),
         end_time = ConvertToLocalPosix(end_time,
                                        input = 'datetime',
                                        output = 'datetime'))

#' @export
Attendance <- dbGetQuery(CON, "SELECT * FROM attendance") |>
  mutate(check_in = ConvertToLocalPosix(check_in,
                                        input = 'datetime',
                                        output = 'datetime'),
         check_out = ConvertToLocalPosix(check_out,
                                         input = 'datetime',
                                         output = 'datetime'))



#' @export
Training_Classifcaion <- dbGetQuery(CON, "SELECT * FROM training_classification") |>
  mutate(training_category = if_else(
    is_active == 1,
    training_category,
    paste("*", training_category)
  )
  )


#' @export
training_category_filter <- Training_Classifcaion |>
  filter(!is.na(training_category)) |>
  pull(training_category) |>
  unique()

#' @export
training_category_active <- Training_Classifcaion |>
  filter(is_active == 1) |>
  pull(training_category) |>
  unique()

#' @export
current_local_date <- Sys.time() |>
    with_tz(tz) |>
    as.Date(tz = tz)


