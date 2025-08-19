box::use(
  gt[...],
  shiny[...],
  bslib[...],
  shinyWidgets[...],
  DT[...],
  dplyr[filter, ...],
  DBI[...],
  tibble[...],
  lubridate[...],
  hms[...],
  bsicons[...],
  plotly[...],
  ggplot2[...],
  tidyr[...],
  scales[...],
  grDevices[...],
  forcats[fct_reorder],
  stats[setNames],
  glue[...],
  purrr[...],
  reactable[...],
  shinyscreenshot[...],
)


box::use(
  ../logic/app_data,
  ../logic/global_functions[...],
  ../logic/local_functions[...],
  ./utils[...],
)

# This theme follows the other DT themes.This is the global setting.
options(reactable.theme = reactableTheme(
  color = 'white',
  backgroundColor = '#333',
  borderColor = 'black',
  borderWidth = '1px',
  stripedColor = '#555',
  highlightColor = '#81D7B6',
  pageButtonHoverStyle = list(
    color = 'white',
    backgroundColor = '#a05050'
  ),
  pageButtonActiveStyle = list(
    color = 'white',
    backgroundColor = '#87292b'
  ),
))




UI <- function(id) {
  ns <- NS(id)
  tagList(
    QuickDateUI(ns)
  )
}

Output <- function(id) {
  ns <- NS(id)

  tagList(
    h2('Incident Summary'),
    card(
      card_header(
        h5(textOutput(ns("headerText"))),
      ),
      layout_columns(
        value_box(
          title = "Total Incidents",
          value = uiOutput(ns("total_incidents")),
          showcase = bs_icon("bar-chart-fill", color = "white")
        ),
        value_box(
          title = 'Department Hours',
          value = uiOutput(ns('department_hours')),
          showcase = bs_icon('clock-fill', color = "white")
        ),
        card(
          height = "100%",
          card_body_fill(           # gives a fill + overflow container
            DTOutput(ns("call_type_table"))
          )
        ),
        col_widths  = c(4, 4, 4)
      )
    ),
    card(
      plotlyOutput(ns("call_plot"))
    )
  )
}

Server <- function(id, ag_level, rdfs) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      today <- app_data$current_local_date

      min_time <- GetSetting('global', group = 'time', key = 'minimum_incident_time')


      ##### REACTIVES #####
      date_filter_range <- reactiveVal(list(today - months(1), today))

      # All_Incidents has incident level data
      All_Incidents <- reactive({
        # browser()

        Incident_With_Units <- app_data$Incident |>
          # Removed deleted incidents
          filter(is.na(is_deleted)) |>
          # Filter to date range
          filter(incident_start >= date_filter_range()[[1]] &
                 incident_start <= date_filter_range()[[2]] + 1) |>
          select(-is_deleted, -deleted_by) |>
          left_join(
            app_data$Incident_Unit |> select(-id),
            by = c('id' = 'incident_id')
          ) |>
          left_join(app_data$Unit |> select(-is_active),
                    by = c('unit_type_id' = 'id')) |>
          left_join(app_data$Dispatch_Code |> select(-is_active),
                    by = c('dispatch_id' = 'id')) |>
          # Adjust units to include None
          # Set incident time to min time if using minimum time
          mutate(
            unit_type = coalesce(unit_type, "None"),
            unit_type_id = coalesce(unit_type_id, 0L),
            incident_length = as.numeric(
              difftime(incident_end, incident_start, units = "hours")
            ) |> round(2),
            incident_length = pmax(incident_length, min_time)
          ) |>
          select(-dispatch_id, -area_id, -unit_type_id)

        Unit_Type_Count <- Incident_With_Units |>
          select(id, unit_type) |>
          group_by(id) |>
          summarise(
            unit_type_count = length(unique(unit_type)),
            unit_type_expanded = paste(unique(unit_type), collapse = ", "),
            .groups = "drop"
          )

        # Set mixed unit type for mixed calls
        Incident_With_Units_Mixed <- Incident_With_Units |>
          left_join(
            Unit_Type_Count,
            by = 'id'
          ) |>
          mutate(
            unit_type = if_else(unit_type_count > 1, 'Mixed', unit_type),
            incident_date = as.Date(incident_start, tz = GetSetting('global', group = 'time', key = 'ltz'))
          )

        # Remove dead space from incident time
        Dead_Space <- app_data$Response |>
          filter(is.na(is_deleted)) |>
          arrange(incident_id, response_start) %>%
          group_by(incident_id) %>%
          mutate(
            # Use numeric for cummax, then back to POSIXct
            prev_end = lag(
              as.POSIXct(
                .POSIXct(
                  cummax(
                    as.numeric(
                      response_end
                      )
                    ),
                  tz = attr(response_end, "tzone")
                  )
                )
              )
          ) |>
          mutate(
            gap_min = pmax(as.numeric(difftime(response_start, prev_end, units = "mins")), 0)
          ) |>
          summarise(
            dead_space_hours = sum(gap_min, na.rm = TRUE) / 60,
            .groups = "drop"
          )

        Final <- Incident_With_Units_Mixed |>
          left_join(
            Dead_Space,
            by = c('id' = 'incident_id')
          ) |>
          mutate(
            has_response = if_else(is.na(dead_space_hours), FALSE, TRUE),
            incident_length = if_else(is.na(incident_length), 0, incident_length),
            dead_space_hours = if_else(is.na(dead_space_hours), 0, dead_space_hours),
            incident_length = incident_length - dead_space_hours
          ) |>
          mutate(
            across(c(canceled, dropped, is_reviewed, is_locked),
                   ~ recode(.x, `1` = TRUE, `0` = FALSE))) |>
          unique() |>
          relocate(unit_type_count, .before = unit_type) |>
          relocate(unit_type_expanded, .after = unit_type) |>
          relocate(incident_date, .before = incident_start)

        return(Final)

      })


      # Quick Date Server
      QuickDateServer(input, today, date_filter_range)

      ##### HEADER #####
      output$headerText <- renderText({
        glue::glue('{date_filter_range()[[1]] |> format("%m-%d-%Y")} to {date_filter_range()[[2]] |> format("%m-%d-%Y")}')
      })

      ##### MAIN PANE #####
      output$total_incidents <- renderText({
          # At the department level,
          # Include all incidents, regardless of responses
          All_Incidents() |>
            nrow() |>
            as.character()
      })

      output$department_hours <- renderText({
        All_Incidents() |>
          filter(has_response) |> # Only include hours with responses
          summarise(total_hours = sum(incident_length, na.rm = TRUE)) |>
          pull(total_hours) |>
          round(0) |>
          as.character()
      })

      output$call_type_table <- renderDataTable({
        All_Incidents() |>
          group_by(dispatch_code) |>
          summarise(
            total_incidents = n(),
            .groups = 'drop'
          ) |>
          arrange(desc(total_incidents)) |>
          rename(
            'Dispatch Code' = dispatch_code,
            'Total Incidents' = total_incidents
            ) |>
          datatable(
            rownames = FALSE,
            selection = 'none',
            height = "100%",
            options = list(
              dom = 't',
              paging = FALSE,
              searching = FALSE,
              columnDefs = list(
                list(className = 'dt-center', targets = "_all")
              ),
              scrollY = TRUE
            ),

          )

      })



      output$call_plot <- renderPlotly({
        df <- All_Incidents() |>
          select(-incident_length) |>
          group_by(incident_date, unit_type) |>
          summarize(
            count = n(),
            .groups = 'drop'
          )

        if(nrow(df) == 0) req(FALSE)

        range_days <- as.numeric(diff(range(df$incident_date)))
        bin_by <- if (range_days <= 30) "week" else "month"

        # Collapse into period
        df_grouped <- df |>
          mutate(period = floor_date(incident_date, bin_by)) |>
          group_by(period, unit_type) |>
          summarise(total = sum(count), .groups = "drop")

        # Complete missing combinations
        all_periods <- seq(min(df_grouped$period), max(df_grouped$period), by = bin_by)
        all_types <- unique(df$unit_type)

        complete_df <- df_grouped |>
          tidyr::complete(
            period   = all_periods,
            unit_type = all_types,
            fill = list(total = 0)
          ) |>
          arrange(period, unit_type) |>
          # Force categorical x with a stable order so all traces stack in the same bin
          mutate(period = factor(period, levels = all_periods))

        # Labels for ticks (month vs week range)
        labels_df <- tibble::tibble(period = all_periods) |>
          mutate(
            # period = factor(period, levels = all_periods),
            label = if (bin_by == "month") {
              # e.g., "Jul 2025"
              format(period, "%b %Y")
            } else {
              # e.g., "Jun 29–Jul 05, 2025" (year shown on the end date)
              paste0(
                format(period, "%b %d"), "–",
                format(period + lubridate::days(6), "%b %d, %Y")
              )
            }
          )

        tickvals <- levels(complete_df$period)
        ticktext <- labels_df$label

        # Sum for annotations
        totals <- complete_df |>
          group_by(period) |>
          summarise(total = sum(total), .groups = "drop")

        # Define color palette
        colors <- MakeColorsPlot(
          unique(complete_df$unit_type)
        )

        # Build the plot
        plot_ly(
          data = complete_df,
          x = ~period,
          y = ~total,
          color = ~unit_type,
          colors = colors,
          type = "bar",
          text = ~ifelse(total > 0, total, ""),
          textposition = "inside"
        ) |>
          add_text(
            data = totals,
            x = ~period,
            y = ~total + 0.5,
            text = ~total,
            textposition = "outside",
            showlegend = FALSE,
            inherit = FALSE
          ) |>
          layout(
            barmode = "stack",
            title = glue::glue("Incident Counts by {stringr::str_to_title(bin_by)}"),
            xaxis = list(
              title = stringr::str_to_title(bin_by),
              type = "category",
              categoryorder = "array",
              categoryarray = tickvals,  # ensure order
              tickmode = "array",
              tickvals = tickvals,
              ticktext = ticktext
            ),
            yaxis = list(title = "Number of Incidents"),
            plot_bgcolor = '#222222',
            paper_bgcolor = '#222222',
            font = list(color = '#FFFFFF')
          )


      })

    }
  )
}
