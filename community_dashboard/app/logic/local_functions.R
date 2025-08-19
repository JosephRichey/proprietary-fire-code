box::use(
  logger[...],
  dplyr[...],
  glue[...],
  lubridate[...],
  bslib[...],
  shiny[...],

)

box::use(
  ./global_functions[...],
)

#' @export
#' This creates an accordion with quick date filters. Use in conjunction with
#' QuickDateServer
QuickDateUI <- function(ns) {
  accordion(
    open = TRUE,
    accordion_panel(
      title = "Quick Date Filters",
      open = FALSE,
      icon = bsicons::bs_icon("calendar-fill"),
      actionButton(
        ns('set_rolling_month'),
        'Rolling Month'
      ),
      br(),
      actionButton(
        ns('set_last_month'),
        'Last Month'
      ),
      br(),
      actionButton(
        ns('set_last_quarter'),
        'Last Quarter'),
      br(),
      actionButton(
        ns('set_rolling_year'),
        'Rolling Year'
      ),
      br(),
      actionButton(
        ns('set_this_year'),
        'This Year'
      ),
    )
  )
}

#' @export
QuickDateServer <- function(input, today, date_filter_range) {
  observeEvent(input$set_last_month, {
    date_filter_range(list(floor_date(today - months(1), "month"), ceiling_date(today - months(1), "month") - days(1)))
  })

  observeEvent(input$set_rolling_month, {
    date_filter_range(list(today - months(1), today))
  })

  observeEvent(input$set_last_quarter, {
    # Get the previous quarter
    prev_qtr_start <- floor_date(today, "quarter") - months(3)
    prev_qtr_end   <- ceiling_date(prev_qtr_start, "quarter") - days(1)

    date_filter_range(list(prev_qtr_start, prev_qtr_end))
  })

  observeEvent(input$set_rolling_year, {
    date_filter_range(list(today - years(1) + days(1), today))
  })

  observeEvent(input$set_this_year, {
    date_filter_range(list(floor_date(today, "year"), ceiling_date(today, "year") - days(1)))
  })

}
