# mantua_payroll_data.R
# -----------------------------------------------------------------------------
# Mantua Fire Department — Payroll Data Wrangling
#
# Minimum hour rules (based on call START time):
#   Day   (08:00 – 21:59):  1 hour minimum
#   Night (22:00 – 07:59):  2 hour minimum
#
# Returns: detail
#   One row per firefighter × response, with columns:
#     incident_id, cad_identifier, incident_start,
#     dispatch_code, dispatch_type, area,
#     response_id, response_start, response_end,
#     firefighter_id, full_name, firefighter_role,
#     time_adjustment, actual_hours, paid_hours,
#     hourly_rate, actual_gross, paid_gross
#
# Usage (in Qmd or Shiny):
#   source("mantua_payroll_data.R")   # expects CON, start_date, end_date in scope
# -----------------------------------------------------------------------------

sql_detail <- "
  SELECT
    i.id                                              AS incident_id,
    i.cad_identifier,
    i.incident_start,
    COALESCE(dc.dispatch_code, '—')                   AS dispatch_code,
    COALESCE(dc.dispatch_type, '—')                   AS dispatch_type,
    COALESCE(ar.area, '—')                            AS area,
    r.id                                              AS response_id,
    r.response_start,
    r.response_end,
    f.id                                              AS firefighter_id,
    f.full_name,
    f.firefighter_role,
    fr.time_adjustment,
    ROUND(
        (TIMESTAMPDIFF(SECOND, r.response_start, r.response_end) / 3600.0)
        + fr.time_adjustment,
        4
    )                                                 AS actual_hours,
    COALESCE(fp.hourly_rate, 0)                       AS hourly_rate
  FROM   incident i
  LEFT JOIN response r
         ON r.incident_id = i.id
         AND r.is_deleted IS NULL
  LEFT JOIN firefighter_response fr
         ON fr.response_id = r.id
  LEFT JOIN firefighter f
         ON f.id = fr.firefighter_id
  LEFT JOIN dispatch_code dc
         ON dc.id = i.dispatch_id
  LEFT JOIN area ar
         ON ar.id = i.area_id
  LEFT JOIN firefighter_pay_rate fp
         ON fp.firefighter_id = f.id
  WHERE  i.is_deleted IS NULL
    AND  DATE(i.incident_start) BETWEEN ? AND ?
  ORDER  BY i.incident_start, r.id, f.full_name
"

detail <- dbGetQuery(CON, sql_detail, params = list(start_date, end_date))

# ── Type coercions ────────────────────────────────────────────────────────────
detail <- detail |>
  mutate(
    response_start = with_tz(as.POSIXct(response_start, tz = "UTC"), "America/Denver"),
    response_end   = with_tz(as.POSIXct(response_end,   tz = "UTC"), "America/Denver"),
    incident_start = with_tz(as.POSIXct(incident_start, tz = "UTC"), "America/Denver")
  )

# ── Minimum hour logic ────────────────────────────────────────────────────────
# Determine shift based on hour of response_start
# Day   = 08:00 to 21:59  →  1 hour minimum
# Night = 22:00 to 07:59  →  2 hour minimum

detail <- detail |>
  mutate(
    start_hour  = hour(response_start),
    is_night    = start_hour >= 22 | start_hour < 8,
    min_hours   = if_else(is_night, 2, 1),
    paid_hours  = pmax(actual_hours, min_hours, na.rm = TRUE)
  ) |>
  select(-start_hour, -is_night, -min_hours)

# ── Pay calculations ──────────────────────────────────────────────────────────
detail <- detail |>
  mutate(
    actual_gross = pmax(actual_hours, 0) * hourly_rate,
    paid_gross   = paid_hours * hourly_rate
  )
