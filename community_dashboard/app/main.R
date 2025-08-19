box::use(
  shiny[...],
  bslib[...],
  DT[...],
  DBI[dbDisconnect, dbGetQuery],
  shiny.router[...],
  fontawesome[...],
  bsicons[...],
  logger[...],
)

box::use(
  logic/app_data,
  logic/global_functions[...],
  view/training_mod,
  view/incident_mod,
  # view/personnel,
)

menu <- tags$ul(
  class = "nav-icons",
  tags$li(a(href = route_link("/"), fa("book-open", title = "Training"), class = 'active')),
  tags$li(a(href = route_link("incident"), fa("calendar-check", title = "Incident"))),
  # tags$li(a(href = route_link("personnel"), fa("user", title = "Personnel"))),
)

#' @export
ui <- function(id) {
  ns <- NS(id)
  page_fluid(
    title = paste0(GetSetting('global', key = 'fire_department_name'), " - Community Dashboard"),

    # Theme
    theme = bs_theme(version = 5,
                     primary = "#87292b",
                     secondary = '#a05050',
                     success = "#2b8764",
                     info = "#377eb8",
                     warning = "#D76F33",
                     danger = "#892A6B",#"#9933CC",
                     light = "#565656",
                     "accordion-button-active-bg" = "#87292b",
                     "accordion-button-active-color" = "white",
                     "nav-underline-link-active-color" = "#2b8764",
                     "nav-link-color" = "white",
                     "nav-link-hover-color" = "#a05050",
                     "btn-hover-border-shade-amount" = '100%',
                     bootswatch = "darkly"),

    # Sidebar with icons
    div(class = "nav-icons", menu),

    # Header
    div(class = "app-header",
        img(src = 'static/logo.png',
            style = 'width: 40px; margin-right: 10px'),
        span(GetSetting('global', key = 'fire_department_name'), " - Community Dashboard")),

    # Main content
    div(class = "main-content",
        router_ui(

          route("/",
                layout_sidebar(
                  sidebar = sidebar(
                    open = 'desktop',
                    training_mod$UI(ns('training_page'))
                  ),
                  training_mod$Output(ns('training_page'))
                )
          ),
          route("incident",
                layout_sidebar(
                  sidebar = sidebar(
                    open = "desktop",
                    incident_mod$UI(ns('incident_page'))
                  ),
                  incident_mod$Output(ns('incident_page'))
                )
                ),
          # route(
          #   "personnel",
          #   # personnel$UI(ns("personnel_page"))
          #   'hello'
          #   )
          ),

    nav_spacer(),
    helpText("v1.0.0-beta © CC 2025 FirePulse LLC")
    )
  )
}

#' @export
server <- function(id) {
  moduleServer(id, function(input, output, session) {
    shinycssloaders::showPageSpinner(
      type = 6,
      color = "#87292b"
    )

    session$onFlushed(function() {
      shinycssloaders::hidePageSpinner()
    })

    ##### Global Stuff #####
    ns <- session$ns

    ##### Modules Servers #####

    training_mod$Server('training_page')

    incident_mod$Server('incident_page')

    # personnel$Server('personnel_page', rdfs)

    router_server()


    # Disconnect from the database on app close
    session$onSessionEnded(function() {
      DBI::dbDisconnect(app_data$CON)
      print('Disconnected from database.')
    })

    # Catch any unhandled errors
    options(shiny.error = function() { logger::log_error(geterrmessage(), "\n") })


  })
}

