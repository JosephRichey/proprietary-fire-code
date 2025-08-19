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
  shinyscreenshot[...],
)


box::use(
  ../logic/app_data,
  ../logic/local_functions[...],
  ../logic/global_functions[...],
  ./utils[MakeColorsPlot, MakeColorsBoxes, known_pal, icon_map]
)



#' @export
UI <- function(id) {
  ns <- NS(id)

  tagList(
    QuickDateUI(ns),
  )
}

#' @export
Output <- function(id) {
  ns <- NS(id)

  ui_boxes <- uiOutput(ns("category_boxes"))

  tagList(
    h2('Training Summary'),
    card(
      card_header(
        h5(textOutput(ns("headerText"))),
      ),
      layout_columns(
        value_box(
          title = "Total Training Hours",
          value = textOutput(ns("total_hours")),
          showcase = bs_icon("clock-fill", color = "white")
        ),
        layout_column_wrap(
          width = 1,
          ui_boxes
        ),
        col_widths = c(4, 8),
        row_widths = c(1, 1)
      )
    ),
    card(
      plotlyOutput(ns("ff_hours_plot"))
    )
  )
}

#' @export
Server <- function(id, rdfs) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      today <- app_data$current_local_date

      ##### REACTIVES #####

      date_filter_range <- reactiveVal(list(today - months(1), today))

      Full_Attendance <- reactive({

        app_data$Training |>
          filter(is.na(is_deleted)) |>
          select(id, start_time, end_time, classification_id,
                 credit_hours, training_description) |>
          left_join(
            app_data$Training_Classifcaion |>
              select(-is_active),
            by = c('classification_id' = 'id')
          ) |>
          left_join(
            app_data$Attendance,
            by = c('id' = 'training_id')
          )  |>
          mutate(
            attended_length = as.numeric(difftime(check_out, check_in, units = "hours")) |> round(2),
            training_date = with_tz(start_time, tzone = GetSetting('global', group = 'time', key = 'ltz')) |> as.Date(),
          ) |>
          select(
            -c(id, classification_id)
          ) |>
          filter(training_date >= date_filter_range()[[1]] & training_date <= date_filter_range()[[2]] + 1)  |>
          filter(credit == 1 | is.na(credit)) |>
          select(
            training_date, firefighter_id, reported_hours = credit_hours,
            training_category
          )

      })

      ##### HEADER #####
      output$headerText <- renderText({
        glue::glue('{date_filter_range()[[1]] |> format("%m-%d-%Y")} to {date_filter_range()[[2]] |> format("%m-%d-%Y")}')
      })

      ##### UPDATE DATE RANGES #####
      QuickDateServer(input, today, date_filter_range)

      ##### UI OUTPUTS (BOXES) #####

      output$total_hours <- renderText({
        Full_Attendance() |>
          summarise(h = sum(reported_hours %||% 0, na.rm = TRUE)) |>
          pull(h) |>
          round(1)
      })

      output$category_boxes <- renderUI({
        df <- Full_Attendance() |>
          mutate(reported_hours = coalesce(reported_hours, 0)) |>
          group_by(training_category) |>
          summarise(reported_hours = sum(reported_hours), .groups = "drop") |>
          filter(reported_hours > 0) |>
          mutate(training_category = as.character(training_category)) |>
          arrange(startsWith(training_category, "*"), tolower(training_category))

        if (nrow(df) == 0) return(NULL)

        # colors & icons (as you defined earlier)
        col_map <- MakeColorsPlot(df$training_category)

        boxes <- lapply(seq_len(nrow(df)), function(i) {
          cat <- df$training_category[i]
          hrs <- round(df$reported_hours[i], 1)
          icn <- icon_map[[cat]] %||% bs_icon("file-fill")
          col_hex <- MakeColorsBoxes(cat)

          bslib::value_box(
            title    = glue::glue("{cat}"),
            value    = tags$span(hrs),
            showcase = if (cat %in% names(known_pal)) {
              tags$span(
                style = glue::glue("color:{col_hex}; display:inline-flex; line-height:0;"),
                icn
              )
            } else NULL,
            style = "height:auto;"
          )
        })

        n <- length(boxes)
        ncols <- if (n <= 4) 2 else 3
        col_widths <- rep(12 / ncols, ncols)

        # IMPORTANT: splice boxes into ... with do.call()
        do.call(
          bslib::layout_columns,
          c(boxes,
            list(
              col_widths = col_widths,
              row_heights = "auto",
              gap = "8px",
              align_items = "start"
            )
          )
        )
      })


      output$ff_hours_plot <- renderPlotly({

        # Step 1: Process raw data
        Training_Data <- Full_Attendance() |>
          mutate(
            training_date  = as.Date(training_date),
            reported_hours = coalesce(reported_hours, 0)
          )

        if (nrow(Training_Data) == 0) req(NULL)

        # Decide bin size based on date span (<= 30 days -> week, else month)
        range_days <- as.numeric(diff(range(Training_Data$training_date, na.rm = TRUE)))
        bin_by <- if (range_days <= 30) "week" else "month"

        # Collapse into period and aggregate
        df_grouped <- Training_Data |>
          mutate(period = lubridate::floor_date(training_date, bin_by)) |>
          group_by(period, training_category) |>
          summarise(Total_Length = sum(reported_hours), .groups = "drop")

        # Complete category x period grid
        all_periods <- seq(min(df_grouped$period, na.rm = TRUE),
                           max(df_grouped$period, na.rm = TRUE),
                           by = bin_by)
        categories <- sort(unique(Training_Data$training_category))

        Plot_Data <- df_grouped |>
          tidyr::complete(
            period = all_periods,
            training_category = categories,
            fill = list(Total_Length = 0)
          ) |>
          # keep your special ordering rule (*-prefixed last, rest alpha)
          mutate(
            training_category = forcats::fct_reorder(
              training_category,
              as.numeric(!startsWith(as.character(training_category), "*")) * 1000 +
                rank(tolower(as.character(training_category)))
            ),
            # force categorical x with stable order so stacks align
            period = factor(period, levels = all_periods)
          ) |>
          arrange(period, training_category)

        # Totals for annotations
        total_data <- Plot_Data |>
          group_by(period) |>
          summarise(Total_Length = sum(Total_Length), .groups = "drop")

        # Tick labels (month vs week range)
        labels_df <- tibble::tibble(period = all_periods) |>
          mutate(
            label = if (bin_by == "month") {
              format(period, "%b %Y")               # e.g., "Jul 2025"
            } else {
              paste0(                               # e.g., "Jun 29–Jul 05, 2025"
                format(period, "%b %d"), "–",
                format(period + lubridate::days(6), "%b %d, %Y")
              )
            }
          )

        tickvals <- levels(Plot_Data$period)
        ticktext <- labels_df$label

        # Colors & legend order (preserved)
        categories_chr <- levels(Plot_Data$training_category) |> as.character()
        fill_values <- MakeColorsPlot(categories_chr)

        legend_order <- Plot_Data |>
          distinct(training_category) |>
          mutate(training_category = as.character(training_category)) |>
          arrange(
            startsWith(training_category, "*"),       # puts * entries last
            tolower(training_category)                # alphabetize the rest
          ) |>
          pull(training_category)

        # Plot
        plot_ly(
          Plot_Data,
          x = ~period,
          y = ~Total_Length,
          color = ~training_category,
          colors = fill_values,
          type = 'bar',
          text = ~ifelse(Total_Length > 0, paste0(round(Total_Length, 1), " hrs"), ""),
          textposition = "inside",
          hoverinfo = "text"
        ) %>%
          add_text(
            data = total_data,
            x = ~period,
            y = ~Total_Length + 0.25,
            text = ~paste0(round(Total_Length, 1), " hrs"),
            textposition = "outside",
            showlegend = FALSE,
            inherit = FALSE
          ) %>%
          layout(
            xaxis = list(
              title = stringr::str_to_title(bin_by),
              titlefont = list(color = '#FFFFFF'),
              tickfont  = list(color = '#FFFFFF'),
              gridcolor = '#2d2d2d',
              type = "category",
              categoryorder = "array",
              categoryarray = tickvals,
              tickmode = "array",
              tickvals = tickvals,
              ticktext = ticktext
            ),
            yaxis = list(
              title = "Training Hours",
              titlefont = list(color = '#FFFFFF'),
              tickfont  = list(color = '#FFFFFF'),
              gridcolor = '#2d2d2d',
              zeroline = FALSE
            ),
            plot_bgcolor = '#222222',
            paper_bgcolor = '#222222',
            font = list(color = '#FFFFFF'),
            barmode = 'stack',
            legend = list(
              traceorder = "normal",
              itemsizing = "trace",
              tracegroupgap = 0,
              title = list(text = "Category"),
              yanchor = "top",
              orientation = "v"
            ),
            coloraxis = list(  # keep your category array hint for legend order
              categoryorder = "array",
              categoryarray = legend_order
            ),
            title = glue::glue("Training Hours by {stringr::str_to_title(bin_by)}")
          )
      })

    }
  )
}
