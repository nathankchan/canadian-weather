# app.R
# Shiny R app to interactively view Canadian historical weather data by range.

library(tidyverse)
library(plotly)
library(htmlwidgets)
library(shiny)
library(shinycssloaders)
library(DT)
library(arrow)
library(leaflet)
library(processx)

source("pipeline.R", local = TRUE)

# ---------------------------------------------------------------------------
# Station metadata & UI text
# ---------------------------------------------------------------------------

expand_station_name <- function(name) {
  name |>
    str_to_title() |>
    str_replace_all("\\bInt'[Ll]\\b", "International") |>
    str_replace_all("\\bIntl\\b", "International") |>
    str_replace_all("\\(Aut\\)", "(Automatic)") |>
    str_replace_all("\\bAut\\b", "Automatic") |>
    str_replace_all("\\bAuto\\b", "Automatic") |>
    str_replace_all("\\bRcs\\b", "Reference Climate Station") |>
    str_replace_all("\\bCs\\b", "Climate Station") |>
    str_replace_all("\\bClimate$", "Climate Station") |>
    str_replace_all("\\bA$", "Airport") |>
    str_replace_all("\\bCda\\b", "Agriculture Canada") |>
    str_replace_all("\\bCcg\\b", "Coast Guard")
}

stationlist <- read_csv("stationlist.csv", show_col_types = FALSE) |>
  mutate(
    Province = str_to_title(Province),
    Name = expand_station_name(Name)
  ) |>
  arrange(Name)
station_choices <- split(
  setNames(
    paste0(stationlist$`Station ID`, ".parquet"),
    stationlist$Name
  ),
  stationlist$Province
)

stationlist <- stationlist |>
  mutate(parquet_file = paste0(`Station ID`, ".parquet"))

province_pal <- colorFactor(
  palette = c(
    "#E41A1C",
    "#377EB8",
    "#4DAF4A",
    "#984EA3",
    "#FF7F00",
    "#A65628",
    "#F781BF",
    "#999999",
    "#66C2A5",
    "#FC8D62",
    "#8DA0CB",
    "#E78AC3",
    "#A6D854"
  ),
  domain = stationlist$Province
)

exclude_cols <- c("Longitude (x)", "Latitude (y)", "Climate ID", "Year")

ui_intro_text <- paste0(
  "Generate interactive plots of historical hourly weather data",
  " for active Canadian weather stations."
)
ui_zaxis_help_text <- paste0(
  "Limits are with respect to the entire data range.",
  " Values below describe the maximum and minimum for the data displayed."
)

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Canadian Historical Weather Data"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      div(
        style = "display: flex; gap: 8px; margin-bottom: 8px;",
        actionButton(
          "toggle_help",
          label = "Help text: ON",
          class = "btn-sm btn-default"
        )
      ),
      tags$div(
        style = "display:none;",
        checkboxInput("showhelp", label = NULL, value = TRUE)
      ),

      conditionalPanel(
        condition = "input.showhelp == true",
        p(ui_intro_text, style = "color: grey;")
      ),

      selectInput(
        inputId = "province",
        label = "Choose province or territory",
        choices = c("All provinces / territories" = "", names(station_choices)),
        selected = ""
      ),

      selectInput(
        inputId = "station",
        label = "Choose station to display",
        choices = station_choices,
        selected = station_choices[[1]][[1]]
      ),

      selectInput(
        inputId = "selectedcolumn",
        label = "Choose data to display",
        choice = c("")
      ),

      tags$label("Specify date range", class = "control-label"),
      conditionalPanel(
        condition = "input.showhelp == true",
        htmlOutput(outputId = "datehelp")
      ),
      div(
        style = "display: flex; flex-wrap: wrap; gap: 4px; margin-bottom: 8px;",
        actionButton("preset_2w", "2 wks", class = "btn-sm", disabled = NA),
        actionButton("preset_2m", "2 mos", class = "btn-sm", disabled = NA),
        actionButton("preset_2y", "2 yrs", class = "btn-sm", disabled = NA),
        actionButton("preset_10y", "10 yrs", class = "btn-sm", disabled = NA),
        actionButton("preset_30y", "30 yrs", class = "btn-sm", disabled = NA),
        actionButton("preset_max", "Max", class = "btn-sm", disabled = NA)
      ),
      tags$details(
        class = "collapsible-section",
        tags$summary("Custom"),
        dateRangeInput(
          inputId = "dates",
          label = NULL
        ),
        sliderInput(
          inputId = "date_slider",
          label = NULL,
          min = as.Date("2000-01-01"),
          max = as.Date("2025-12-31"),
          value = c(as.Date("2023-01-01"), as.Date("2025-12-31")),
          timeFormat = "%Y-%m-%d",
          width = "100%"
        )
      ),

      tags$head(
        tags$style(HTML(
          "#datehelp { color: grey; }
          details.collapsible-section > summary {
            list-style: none; cursor: pointer; color: #428bca;
          }
          details.collapsible-section > summary::-webkit-details-marker { display: none; }
          details.collapsible-section > summary:hover { text-decoration: underline; }
          details.collapsible-section > summary::before {
            content: '\u25b6 '; font-size: 0.75em; margin-right: 0.25em; vertical-align: middle;
          }
          details.collapsible-section[open] > summary::before { content: '\u25bc '; }
          details.collapsible-section:not([open]) { margin-bottom: 8px; }"
        )),
        tags$script(HTML(
          "
          Shiny.addCustomMessageHandler('setButtonDisabled', function(msg) {
            var el = document.getElementById(msg.id);
            if (el) el.disabled = msg.disabled;
          });
        "
        ))
      ),

      radioButtons(
        inputId = "autozaxis",
        label = "Autoscale height",
        choices = list("Yes" = TRUE, "No" = FALSE),
        selected = TRUE
      ),

      conditionalPanel(
        condition = "input.showhelp == true",
        p(span(
          "Turn off autoscaling to facilitate comparisons across date ranges.",
          style = "color:grey"
        ))
      ),

      conditionalPanel(
        condition = "input.autozaxis == 'FALSE'",
        sliderInput(
          inputId = "zaxislimits",
          label = "Specify height limits",
          min = 0,
          max = 1,
          value = c(0, 1),
          step = 0.1
        ),
        conditionalPanel(
          condition = "input.showhelp == true",
          p(span(
            ui_zaxis_help_text,
            style = "color:grey"
          ))
        ),
        tableOutput(outputId = "zaxistable")
      ),

      conditionalPanel(
        condition = "input.maintabs !== 'Table' && input.maintabs !== 'Map'",
        hr(style = "border-top: 1px dotted #808080;"),
        radioButtons(
          inputId = "stats_grouping",
          label = "Sort statistics table by",
          choices = list(
            "Variable, then Dataset" = "variable_then_group",
            "Dataset, then Variable" = "group_then_variable"
          ),
          selected = "variable_then_group"
        )
      ),

      hr(style = "border-top: 1px dotted #808080;"),
      tags$details(
        class = "collapsible-section",
        tags$summary(strong("Export data")),
        p(span("Selected range", style = "color:grey")),
        fluidRow(
          column(
            6,
            downloadButton(
              "export_filtered_csv",
              "CSV",
              class = "btn-sm btn-block"
            )
          ),
          column(
            6,
            downloadButton(
              "export_filtered_parquet",
              "Parquet",
              class = "btn-sm btn-block"
            )
          )
        ),
        p(span("Full station dataset", style = "color:grey")),
        fluidRow(
          column(
            6,
            downloadButton("export_full_csv", "CSV", class = "btn-sm btn-block")
          ),
          column(
            6,
            downloadButton(
              "export_full_parquet",
              "Parquet",
              class = "btn-sm btn-block"
            )
          )
        )
      ),

      hr(style = "border-top: 1px dotted #808080;"),
      tags$details(
        class = "collapsible-section",
        tags$summary(strong("Update data")),
        div(
          style = "display: flex; gap: 8px; margin-bottom: 8px; margin-top: 8px;",
          actionButton(
            "update_station",
            label = "Update station",
            class = "btn-sm btn-default",
            icon = icon("refresh")
          ),
          actionButton(
            "update_all",
            label = "Update all data",
            class = "btn-sm btn-warning",
            icon = icon("download")
          )
        ),
        uiOutput("update_status")
      ),

      conditionalPanel(
        condition = "input.showhelp == true",
        hr(style = "border-top: 1px dotted #808080;"),
        p(a(
          "Source",
          href = "https://climate.weather.gc.ca/historical_data/search_historic_data_e.html"
        )),
        p(span(
          em("Data are published by Environment and Climate Change Canada."),
          style = "color:grey"
        )),
        p(span(
          em("Built by"),
          "Nathan K. Chan",
          em(
            " in ",
            strong("R"),
            "and",
            strong("Shiny", .noWS = "after"),
            ". Visit the project on Github ",
            a(
              "here",
              href = "https://nathankchan.github.io/canadian-weather/",
              .noWS = "after"
            ),
            "."
          ),
          style = "color:grey"
        ))
      )
    ),

    mainPanel(
      width = 9,
      tabsetPanel(
        id = "maintabs",
        tabPanel(
          title = "Map",
          div(
            style = "padding: 8px 0;",
            actionButton(
              "map_zoom_all",
              "All stations",
              class = "btn-sm btn-default"
            ),
            actionButton(
              "map_zoom_selected",
              "Selected station",
              class = "btn-sm btn-default"
            ),
            tags$details(
              class = "collapsible-section",
              style = "margin-top: 6px;",
              tags$summary(strong("Zoom to province / territory")),
              div(
                style = "display: flex; flex-wrap: wrap; gap: 4px; padding-top: 6px;",
                lapply(names(station_choices), function(prov) {
                  actionButton(
                    inputId = paste0(
                      "map_zoom_",
                      gsub("[^A-Za-z0-9]", "_", prov)
                    ),
                    label = prov,
                    class = "btn-sm btn-default"
                  )
                })
              )
            )
          ),
          leafletOutput(
            outputId = "displaymap",
            width = "100%",
            height = "800px"
          )
        ),
        tabPanel(
          title = "Surface Plot",
          withSpinner(plotlyOutput(
            outputId = "display3dplot",
            width = "auto",
            height = "600px"
          ))
        ),
        tabPanel(
          title = "Line Chart",
          withSpinner(plotlyOutput(
            outputId = "displayline",
            width = "auto",
            height = "600px"
          ))
        ),
        tabPanel(
          title = "Heat Map",
          withSpinner(plotlyOutput(
            outputId = "displayheat",
            width = "auto",
            height = "600px"
          ))
        ),
        tabPanel(
          title = "Table",
          DTOutput(
            outputId = "displaytable"
          )
        )
      ),
      conditionalPanel(
        condition = "input.maintabs !== 'Table' && input.maintabs !== 'Map'",
        DTOutput(outputId = "displaystats")
      )
    )
  )
)

# ---------------------------------------------------------------------------
# Server helpers
# ---------------------------------------------------------------------------

compute_stats <- function(vec) {
  n_total <- length(vec)
  n_miss <- sum(is.na(vec))
  n_obs <- n_total - n_miss
  if (n_obs == 0L) {
    return(data.frame(
      N = n_obs,
      Missing = paste0(n_miss, " (", round(100 * n_miss / n_total, 1), "%)"),
      Min = NA_real_,
      Max = NA_real_,
      Range = NA_real_,
      Mean = NA_real_,
      Median = NA_real_,
      SD = NA_real_,
      IQR = NA_real_,
      check.names = FALSE
    ))
  }
  data.frame(
    N = n_obs,
    Missing = paste0(n_miss, " (", round(100 * n_miss / n_total, 1), "%)"),
    Min = min(vec, na.rm = TRUE),
    Max = max(vec, na.rm = TRUE),
    Range = max(vec, na.rm = TRUE) - min(vec, na.rm = TRUE),
    Mean = mean(vec, na.rm = TRUE),
    Median = median(vec, na.rm = TRUE),
    SD = sd(vec, na.rm = TRUE),
    IQR = IQR(vec, na.rm = TRUE),
    check.names = FALSE
  )
}

build_stats_group <- function(data, numeric_cols, group_label) {
  rows <- lapply(numeric_cols, function(col) {
    row <- compute_stats(data[[col]])
    row$Variable <- col
    row$Dataset <- group_label
    row
  })
  do.call(rbind, rows)
}

to_camel_case <- function(x) {
  words <- strsplit(x, "[^A-Za-z0-9]+")[[1]]
  words <- words[nchar(words) > 0]
  paste0(toupper(substr(words, 1, 1)), tolower(substring(words, 2))) |>
    paste(collapse = "")
}

export_filename <- function(station_id, province, name, suffix, ext) {
  paste0(
    paste(
      station_id,
      to_camel_case(province),
      to_camel_case(name),
      suffix,
      sep = "_"
    ),
    ".",
    ext
  )
}

compute_xaxis <- function(plot_data) {
  n <- ncol(plot_data)
  if (n > 365) {
    xindex <- which(substr(colnames(plot_data), 9, 12) == "01")
    xindex <- xindex[seq(
      from = 1,
      to = length(xindex),
      by = floor(length(xindex) / 12)
    )]
  } else if (n > 12) {
    xindex <- seq(from = 1, to = n, by = floor(n / 12))
  } else {
    xindex <- seq_len(n)
  }

  list(
    title = "",
    tickmode = "array",
    ticktext = colnames(plot_data)[xindex] |> as.Date() |> format("%Y-%b-%d"),
    tickvals = xindex,
    range = c(1, n)
  )
}

format_date_range <- function(start_date, end_date) {
  paste0(
    as.Date(start_date) |> format("%Y-%b-%d"),
    " to ",
    as.Date(end_date) |> format("%Y-%b-%d")
  )
}

format_plot_title <- function(column, start_date, end_date) {
  paste0(column, " (", format_date_range(start_date, end_date), ")")
}

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  current_dates <- reactiveVal(NULL)
  current_column <- reactiveVal(NULL)
  data_version <- reactiveVal(0L)
  update_proc <- reactiveVal(NULL)
  update_target <- reactiveVal(NULL)
  update_progress_file <- reactiveVal(NULL)
  update_total <- reactiveVal(0L)
  update_result <- reactiveVal(NULL)

  observeEvent(
    input$toggle_help,
    {
      new_val <- !isTRUE(input$showhelp)
      updateCheckboxInput(session, "showhelp", value = new_val)
      updateActionButton(
        session,
        "toggle_help",
        label = if (new_val) "Help text: ON" else "Help text: OFF"
      )
    },
    ignoreInit = TRUE
  )

  # --- Update station button ---
  observeEvent(
    input$update_station,
    {
      req(is.null(update_proc()))
      sid <- station_id()
      req(sid)
      session$sendCustomMessage(
        "setButtonDisabled",
        list(id = "update_station", disabled = TRUE)
      )
      session$sendCustomMessage(
        "setButtonDisabled",
        list(id = "update_all", disabled = TRUE)
      )
      update_target("station")
      update_result(NULL)
      proc <- process$new(
        command = "Rscript",
        args = c(
          "-e",
          sprintf(
            paste0(
              "source('pipeline.R', local = TRUE); ",
              "update_selected_station('%s')"
            ),
            sid
          )
        ),
        stdout = "|",
        stderr = "|"
      )
      update_proc(proc)
    },
    ignoreInit = TRUE
  )

  # --- Update all data button ---
  observeEvent(
    input$update_all,
    {
      req(is.null(update_proc()))
      session$sendCustomMessage(
        "setButtonDisabled",
        list(id = "update_station", disabled = TRUE)
      )
      session$sendCustomMessage(
        "setButtonDisabled",
        list(id = "update_all", disabled = TRUE)
      )
      update_target("all")
      update_result(NULL)
      pf <- tempfile("update_progress_", fileext = ".txt")
      writeLines("0", pf)
      update_progress_file(pf)
      update_total(nrow(stationlist))
      proc <- process$new(
        command = "Rscript",
        args = c(
          "-e",
          sprintf(
            paste0(
              "source('pipeline.R', local = TRUE); ",
              "update_all_stations(progress_file = '%s')"
            ),
            pf
          )
        ),
        stdout = "|",
        stderr = "|"
      )
      update_proc(proc)
    },
    ignoreInit = TRUE
  )

  # --- Poll for update completion ---
  observe({
    proc <- update_proc()
    req(proc)
    invalidateLater(1000)
    if (!proc$is_alive()) {
      exit_code <- proc$get_exit_status()
      target <- update_target()
      update_proc(NULL)
      update_target(NULL)
      pf <- update_progress_file()
      if (!is.null(pf) && file.exists(pf)) {
        unlink(pf)
      }
      update_progress_file(NULL)
      update_total(0L)
      session$sendCustomMessage(
        "setButtonDisabled",
        list(id = "update_station", disabled = FALSE)
      )
      session$sendCustomMessage(
        "setButtonDisabled",
        list(id = "update_all", disabled = FALSE)
      )
      if (identical(exit_code, 0L)) {
        data_version(data_version() + 1L)
        update_result(list(
          status = "success",
          message = if (target == "station") {
            "Station updated successfully."
          } else {
            "All data updated successfully."
          }
        ))
      } else {
        stderr_out <- tryCatch(
          proc$read_error_lines(),
          error = function(e) ""
        )
        update_result(list(
          status = "error",
          message = paste("Update failed.", paste(stderr_out, collapse = " "))
        ))
      }
    }
  })

  # --- Update status: single renderUI driven by reactive state ---
  output$update_status <- renderUI({
    proc <- update_proc()
    result <- update_result()

    # Running: show spinner (with progress for "all")
    if (!is.null(proc)) {
      invalidateLater(1000)
      target <- update_target()
      if (identical(target, "all")) {
        pf <- update_progress_file()
        total <- update_total()
        completed <- 0L
        if (!is.null(pf) && file.exists(pf)) {
          content <- tryCatch(
            readLines(pf, warn = FALSE),
            error = function(e) ""
          )
          dots <- paste(content[-1], collapse = "")
          completed <- nchar(dots)
        }
        pct <- if (total > 0L) round(100 * completed / total, 1) else 0
        return(tags$span(
          style = "color: grey; font-size: 0.85em;",
          icon("spinner", class = "fa-spin"),
          sprintf(
            "Updating all data... %d/%d (%.1f%%) stations updated.",
            completed,
            total,
            pct
          )
        ))
      }
      return(tags$span(
        style = "color: grey; font-size: 0.85em;",
        icon("spinner", class = "fa-spin"),
        "Updating station..."
      ))
    }

    # Finished: show result
    if (!is.null(result)) {
      if (identical(result$status, "success")) {
        return(tags$span(
          style = "color: green; font-size: 0.85em;",
          icon("check"),
          result$message
        ))
      }
      return(tags$span(
        style = "color: red; font-size: 0.85em;",
        icon("exclamation-triangle"),
        result$message
      ))
    }

    NULL
  })

  selected_data <- reactive({
    data_version()
    req(input$station)
    open_dataset(paste0("./data/", input$station))
  })

  station_numeric_cols <- reactive({
    req(input$station)
    ds <- selected_data()
    type_strs <- sapply(ds$schema$fields, function(f) f$type$ToString())
    ds$schema$names[
      grepl("^(int|uint|float|double)", type_strs, ignore.case = TRUE) &
        !ds$schema$names %in% exclude_cols
    ]
  })

  filtered_data <- reactive({
    dates <- current_dates()
    req(dates)
    start_posix <- as.POSIXct(dates[1])
    end_posix <- as.POSIXct(dates[2] + 1L)
    selected_data() |>
      filter(
        `Date/Time (LST)` >= start_posix,
        `Date/Time (LST)` < end_posix
      ) |>
      collect()
  })

  full_stats_data <- reactive({
    selected_data() |>
      select(all_of(station_numeric_cols())) |>
      collect()
  })

  stats_table <- reactive({
    cols <- station_numeric_cols()
    combined <- rbind(
      build_stats_group(filtered_data(), cols, "Selected Range"),
      build_stats_group(full_stats_data(), cols, "Full Dataset")
    )
    combined$dataset_priority <- ifelse(
      combined$Dataset == "Selected Range",
      0L,
      1L
    )
    combined
  })

  plot_columns <- reactive({
    col <- current_column()
    req(col)
    filtered_data()[, c("Date/Time (LST)", "Time (LST)", col)]
  })

  plot_3dinput <- reactive({
    out <- plot_columns()
    colnames(out) <- c("Date", "Hour", "Value")
    out[["Date"]] <- format(out[["Date"]], "%Y-%m-%d")

    out |>
      pivot_wider(names_from = Date, values_from = Value) |>
      select(-Hour) |>
      as.matrix()
  })

  plot_lineinput <- reactive({
    out <- plot_columns()
    cbind.data.frame(
      DateTime = as.POSIXct(out[["Date/Time (LST)"]]),
      Value = out[[current_column()]]
    )
  })

  plot_heatinput <- reactive({
    out <- plot_columns()
    colnames(out) <- c("Date", "Hour", "Value")
    out[["Hour"]] <- format(as.POSIXct(out[["Hour"]]), format = "%H") |>
      as.numeric()
    out
  })

  output$zaxistable <- renderTable(
    {
      col <- current_column()
      req(col)

      minmax <- filtered_data()[[col]]
      cbind.data.frame(
        Minimum = min(minmax, na.rm = TRUE),
        Maximum = max(minmax, na.rm = TRUE)
      )
    },
    width = "100%",
    align = "c"
  )

  output$display3dplot <- renderPlotly({
    plot_data <- plot_3dinput()

    plot_xaxis <- compute_xaxis(plot_data)
    plot_yaxis <- list(
      title = "Hour",
      tickmode = "array",
      ticktext = c("4:00", "8:00", "12:00", "16:00", "20:00", "24:00"),
      tickvals = c(4, 8, 12, 16, 20, 24)
    )
    plot_zaxis <- list(title = current_column())

    if (!isTRUE(as.logical(input$autozaxis))) {
      plot_zaxis$range <- c(input$zaxislimits[1], input$zaxislimits[2])
    }

    dates_fmt <- format(as.Date(colnames(plot_data)), "%Y-%b-%d")
    n_hours <- nrow(plot_data)
    hours_fmt <- paste0(seq_len(n_hours), ":00")
    hover_text <- matrix(
      sprintf(
        "Date: %s<br>Hour: %s<br>Value: %s",
        rep(dates_fmt, each = n_hours),
        rep(hours_fmt, times = ncol(plot_data)),
        as.character(plot_data)
      ),
      nrow = n_hours
    )

    plot_ly(z = ~plot_data, lighting = list(ambient = 0.9)) |>
      add_surface(
        text = hover_text,
        hoverinfo = "text",
        showscale = TRUE,
        colorbar = list(title = list(text = current_column())),
        contours = list(
          z = list(
            show = FALSE,
            usecolormap = TRUE,
            highlightcolor = "#ff0000",
            project = list(z = FALSE)
          )
        )
      ) |>
      layout(
        title = list(
          text = paste0(
            "<br>",
            current_column(),
            " for ",
            station_info()$name,
            "<br>",
            "<sup>",
            format(current_dates()[1], "%Y-%b-%d"),
            " to ",
            format(current_dates()[2], "%Y-%b-%d"),
            "</sup>"
          )
        ),
        scene = list(
          xaxis = plot_xaxis,
          yaxis = plot_yaxis,
          zaxis = plot_zaxis,
          scale = list(title = list(text = current_column())),
          camera = list(eye = list(x = 1.5, y = -1.5, z = 0.75)),
          aspectmode = "manual",
          aspectratio = list(x = 2, y = 1, z = 1)
        )
      )
  })

  output$displayline <- renderPlotly({
    plot_data <- plot_lineinput()

    plot_out <- ggplot(
      data = plot_data,
      aes(x = DateTime, y = Value)
    ) +
      geom_line(linewidth = 0.1, color = "blue") +
      scale_x_datetime(date_labels = "%Y-%b-%d") +
      labs(
        x = "Date",
        y = current_column(),
        title = paste0(
          current_column(),
          " for ",
          station_info()$name
        ),
        subtitle = paste0(
          format(current_dates()[1], "%Y-%b-%d"),
          " to ",
          format(current_dates()[2], "%Y-%b-%d")
        )
      ) +
      theme_light()

    if (!isTRUE(as.logical(input$autozaxis))) {
      plot_out <- plot_out + ylim(input$zaxislimits)
    }

    ggplotly(plot_out)
  })

  output$displayheat <- renderPlotly({
    plot_data <- plot_heatinput()

    plot_out <- ggplot(
      data = plot_data,
      aes(
        x = as.Date(Date),
        y = Hour,
        text = paste0(
          "Date: ",
          format(as.Date(Date), "%Y-%b-%d"),
          "<br>Hour: ",
          as.integer(Hour),
          ":00",
          "<br>Value: ",
          Value
        )
      )
    ) +
      geom_tile(aes(fill = Value)) +
      scale_x_date(date_labels = "%Y-%b-%d") +
      scale_y_continuous(
        n.breaks = 13,
        labels = function(x) paste0(as.integer(x), ":00"),
        limits = c(0, 23)
      ) +
      labs(
        x = "Date",
        y = "Hour",
        fill = current_column(),
        title = paste0(
          current_column(),
          " for ",
          station_info()$name
        ),
        subtitle = paste0(
          format(current_dates()[1], "%Y-%b-%d"),
          " to ",
          format(current_dates()[2], "%Y-%b-%d")
        )
      )

    if (!isTRUE(as.logical(input$autozaxis))) {
      plot_out <- plot_out +
        scale_fill_viridis_c(limits = input$zaxislimits) +
        theme_light()
    } else {
      plot_out <- plot_out +
        scale_fill_viridis_c() +
        theme_light()
    }

    ggplotly(plot_out, tooltip = "text")
  })

  output$displaytable <- renderDT({
    req(input$station)
    datatable(
      filtered_data(),
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        scrollY = "600px",
        scrollCollapse = TRUE,
        pageLength = 50,
        dom = "tip"
      )
    )
  })

  output$displaystats <- renderDT({
    col <- current_column()
    req(col)
    tbl <- stats_table()
    selected_col <- col

    tbl$var_priority <- ifelse(tbl$Variable == selected_col, 0L, 1L)
    if (input$stats_grouping == "group_then_variable") {
      tbl <- tbl[
        order(tbl$dataset_priority, tbl$var_priority, tbl$Variable),
      ]
      tbl <- tbl[, c(
        "Dataset",
        "Variable",
        "N",
        "Missing",
        "Min",
        "Max",
        "Range",
        "Mean",
        "Median",
        "SD",
        "IQR"
      )]
    } else {
      tbl <- tbl[
        order(tbl$var_priority, tbl$Variable, tbl$dataset_priority),
      ]
      tbl <- tbl[, c(
        "Variable",
        "Dataset",
        "N",
        "Missing",
        "Min",
        "Max",
        "Range",
        "Mean",
        "Median",
        "SD",
        "IQR"
      )]
    }

    datatable(
      tbl,
      rownames = FALSE,
      options = list(
        dom = "t",
        pageLength = -1,
        scrollX = TRUE,
        scrollY = "300px",
        scrollCollapse = TRUE,
        columnDefs = list(list(defaultContent = "NA", targets = "_all"))
      )
    ) |>
      formatRound(
        columns = c("Min", "Max", "Range", "Mean", "Median", "SD", "IQR"),
        digits = 2
      ) |>
      formatStyle(
        "Variable",
        target = "row",
        fontWeight = styleEqual(selected_col, "bold")
      )
  })

  output$datehelp <- renderText({
    date_range <- selected_data() |>
      summarise(
        min_date = min(`Date/Time (LST)`, na.rm = TRUE),
        max_date = max(`Date/Time (LST)`, na.rm = TRUE)
      ) |>
      collect()
    firstdate <- format(as.POSIXct(date_range$min_date), "%Y-%m-%d")
    lastdate <- format(as.POSIXct(date_range$max_date), "%Y-%m-%d")
    paste0(
      "<p>Select the most recent data or specify a custom date range. ",
      "Data are available from <i>",
      firstdate,
      "</i> to <i>",
      lastdate,
      "</i>.</p>"
    )
  })

  output$displaymap <- renderLeaflet({
    leaflet(stationlist) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~`Longitude (Decimal Degrees)`,
        lat = ~`Latitude (Decimal Degrees)`,
        fillColor = ~ province_pal(Province),
        radius = 6,
        stroke = FALSE,
        fillOpacity = 0.8,
        label = ~ lapply(paste0("<b>", Name, "</b><br>", Province), HTML),
        labelOptions = labelOptions(
          style = list("font-size" = "13px"),
          direction = "auto"
        ),
        popup = ~ paste0("<b>", Name, "</b><br>", Province),
        layerId = ~parquet_file
      ) |>
      addLegend(
        position = "bottomright",
        pal = province_pal,
        values = ~Province,
        title = "Province / Territory",
        opacity = 0.8
      ) |>
      fitBounds(
        min(stationlist$`Longitude (Decimal Degrees)`),
        min(stationlist$`Latitude (Decimal Degrees)`),
        max(stationlist$`Longitude (Decimal Degrees)`),
        max(stationlist$`Latitude (Decimal Degrees)`)
      )
  })

  observeEvent(input$map_zoom_all, {
    leafletProxy("displaymap") |>
      fitBounds(
        min(stationlist$`Longitude (Decimal Degrees)`),
        min(stationlist$`Latitude (Decimal Degrees)`),
        max(stationlist$`Longitude (Decimal Degrees)`),
        max(stationlist$`Latitude (Decimal Degrees)`)
      )
  })

  observeEvent(input$map_zoom_selected, {
    req(input$station)
    selected_row <- stationlist[
      paste0(stationlist$`Station ID`, ".parquet") == input$station,
    ]
    if (nrow(selected_row) > 0L) {
      leafletProxy("displaymap") |>
        setView(
          lng = selected_row$`Longitude (Decimal Degrees)`[1],
          lat = selected_row$`Latitude (Decimal Degrees)`[1],
          zoom = 10
        )
    }
  })

  observeEvent(
    input$station,
    {
      selected_row <- stationlist[
        paste0(stationlist$`Station ID`, ".parquet") == input$station,
      ]
      if (nrow(selected_row) > 0L) {
        leafletProxy("displaymap") |>
          setView(
            lng = selected_row$`Longitude (Decimal Degrees)`[1],
            lat = selected_row$`Latitude (Decimal Degrees)`[1],
            zoom = 10
          )
      }
    },
    ignoreInit = TRUE
  )

  lapply(names(station_choices), function(prov) {
    observeEvent(
      input[[paste0("map_zoom_", gsub("[^A-Za-z0-9]", "_", prov))]],
      {
        prov_stations <- stationlist[stationlist$Province == prov, ]
        leafletProxy("displaymap") |>
          fitBounds(
            min(prov_stations$`Longitude (Decimal Degrees)`),
            min(prov_stations$`Latitude (Decimal Degrees)`),
            max(prov_stations$`Longitude (Decimal Degrees)`),
            max(prov_stations$`Latitude (Decimal Degrees)`)
          )
      },
      ignoreInit = TRUE
    )
  })

  pending_station <- reactiveVal(NULL)

  observeEvent(input$displaymap_marker_click, {
    click <- input$displaymap_marker_click
    req(click)
    clicked_row <- stationlist[stationlist$parquet_file == click$id, ]
    req(nrow(clicked_row) == 1L)
    if (input$province == clicked_row$Province) {
      # Province is already correct — station choices are already populated,
      # so update the station directly without touching the province dropdown.
      updateSelectInput(session, "station", selected = clicked_row$parquet_file)
    } else {
      # Province needs to change. Stash the intended station so the province
      # observer can select it once it has repopulated the station choices.
      pending_station(clicked_row$parquet_file)
      updateSelectInput(session, "province", selected = clicked_row$Province)
    }
  })

  observeEvent(
    input$province,
    {
      ps <- pending_station()
      if (!is.null(ps)) {
        pending_station(NULL)
        choices <- if (input$province == "") {
          station_choices
        } else {
          station_choices[[input$province]]
        }
        updateSelectInput(session, "station", choices = choices, selected = ps)
      } else if (input$province == "") {
        updateSelectInput(
          session,
          "station",
          choices = station_choices,
          selected = station_choices[[1]][[1]]
        )
      } else {
        updateSelectInput(
          session,
          "station",
          choices = station_choices[[input$province]],
          selected = station_choices[[input$province]][[1]]
        )
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$station,
    {
      numeric_cols <- station_numeric_cols()
      dr <- full_date_range()

      min_date <- as.POSIXct(dr$min_date)
      max_date <- as.POSIXct(dr$max_date)
      station_min <- as.Date(min_date)
      station_max <- as.Date(max_date)

      # Retain current column if it exists in the new station; else default
      prev_col <- current_column()
      new_col <- if (!is.null(prev_col) && prev_col %in% numeric_cols) {
        prev_col
      } else {
        "Temp (°C)"
      }

      # Retain current date range if it overlaps the new station; else default
      prev_dates <- current_dates()
      default_start <- as.Date(max_date - 365 * 24 * 3600 * 2)
      default_end <- station_max
      if (
        !is.null(prev_dates) &&
          prev_dates[1] <= station_max &&
          prev_dates[2] >= station_min
      ) {
        new_start <- max(prev_dates[1], station_min)
        new_end <- min(prev_dates[2], station_max)
      } else {
        new_start <- default_start
        new_end <- default_end
      }

      new_dates <- c(new_start, new_end)
      if (!identical(current_column(), new_col)) {
        current_column(new_col)
      }
      if (!identical(current_dates(), new_dates)) {
        current_dates(new_dates)
      }

      freezeReactiveValue(input, "selectedcolumn")
      updateSelectInput(
        session = session,
        inputId = "selectedcolumn",
        choices = numeric_cols,
        selected = new_col
      )
      freezeReactiveValue(input, "dates")
      updateDateRangeInput(
        inputId = "dates",
        min = min_date,
        start = new_start,
        max = max_date,
        end = new_end
      )
      freezeReactiveValue(input, "date_slider")
      updateSliderInput(
        session,
        "date_slider",
        min = station_min,
        max = station_max,
        value = c(new_start, new_end)
      )
    },
    priority = 1
  )

  observeEvent(
    input$date_slider,
    {
      req(input$date_slider)
      if (!identical(as.Date(input$dates), as.Date(input$date_slider))) {
        freezeReactiveValue(input, "dates")
        updateDateRangeInput(
          session,
          "dates",
          start = input$date_slider[1],
          end = input$date_slider[2]
        )
      }
    },
    ignoreNULL = TRUE
  )

  observeEvent(
    input$dates,
    {
      req(input$dates, input$station)
      new_dates <- as.Date(input$dates)
      if (!identical(current_dates(), new_dates)) {
        current_dates(new_dates)
      }
      if (!identical(as.Date(input$date_slider), new_dates)) {
        freezeReactiveValue(input, "date_slider")
        updateSliderInput(
          session,
          "date_slider",
          value = new_dates
        )
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  observeEvent(
    input$selectedcolumn,
    {
      req(input$selectedcolumn)
      if (!identical(current_column(), input$selectedcolumn)) {
        current_column(input$selectedcolumn)
      }
    },
    ignoreNULL = TRUE,
    ignoreInit = TRUE
  )

  # `by = NULL` means "jump to the earliest available date" (preset_max).
  preset_configs <- list(
    list(id = "preset_2w", by = "-2 weeks"),
    list(id = "preset_2m", by = "-2 months"),
    list(id = "preset_2y", by = "-2 years"),
    list(id = "preset_10y", by = "-10 years"),
    list(id = "preset_30y", by = "-30 years"),
    list(id = "preset_max", by = NULL)
  )

  lapply(preset_configs, function(cfg) {
    local({
      id <- cfg$id
      by <- cfg$by
      observeEvent(
        input[[id]],
        {
          req(input$station)
          dr <- full_date_range()
          station_max <- as.Date(as.POSIXct(dr$max_date))
          station_min <- as.Date(as.POSIXct(dr$min_date))
          start <- if (is.null(by)) {
            station_min
          } else {
            max(station_min, seq(station_max, by = by, length.out = 2)[2])
          }
          updateDateRangeInput(
            session,
            "dates",
            start = start,
            end = station_max
          )
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE
      )
    })
  })

  observe({
    dr <- full_date_range()
    station_max <- as.Date(as.POSIXct(dr$max_date))
    station_min <- as.Date(as.POSIXct(dr$min_date))

    for (cfg in preset_configs) {
      disabled <- if (is.null(cfg$by)) {
        FALSE
      } else {
        seq(station_max, by = cfg$by, length.out = 2)[2] < station_min
      }
      session$sendCustomMessage(
        "setButtonDisabled",
        list(id = cfg$id, disabled = disabled)
      )
    }
  })

  observeEvent(
    input$selectedcolumn,
    {
      req(input$selectedcolumn)

      col_data <- isolate(selected_data()) |>
        select(all_of(input$selectedcolumn)) |>
        collect()

      out <- range(col_data[[1]], na.rm = TRUE)
      buffer <- (out[2] - out[1]) / 10

      updateSliderInput(
        inputId = "zaxislimits",
        min = out[1] - buffer,
        max = out[2] + buffer,
        value = c(out[1], out[2])
      )
    },
    ignoreNULL = TRUE
  )

  station_id <- reactive({
    req(input$station)
    sub("\\.parquet$", "", input$station)
  })

  station_info <- reactive({
    req(input$station)
    row <- stationlist[
      paste0(stationlist$`Station ID`, ".parquet") == input$station,
    ]
    list(province = row$Province, name = row$Name)
  })

  full_date_range <- reactive({
    selected_data() |>
      summarise(
        min_date = min(`Date/Time (LST)`, na.rm = TRUE),
        max_date = max(`Date/Time (LST)`, na.rm = TRUE)
      ) |>
      collect()
  })

  output$export_filtered_csv <- downloadHandler(
    filename = function() {
      info <- station_info()
      dr <- input$dates
      suffix <- paste0(
        "SelectedRange_",
        format(as.Date(dr[1]), "%Y%m%d"),
        "_to_",
        format(as.Date(dr[2]), "%Y%m%d")
      )
      export_filename(station_id(), info$province, info$name, suffix, "csv")
    },
    content = function(file) write_csv(filtered_data(), file)
  )

  output$export_filtered_parquet <- downloadHandler(
    filename = function() {
      info <- station_info()
      dr <- input$dates
      suffix <- paste0(
        "SelectedRange_",
        format(as.Date(dr[1]), "%Y%m%d"),
        "_to_",
        format(as.Date(dr[2]), "%Y%m%d")
      )
      export_filename(station_id(), info$province, info$name, suffix, "parquet")
    },
    content = function(file) write_parquet(filtered_data(), file)
  )

  output$export_full_csv <- downloadHandler(
    filename = function() {
      info <- station_info()
      dr <- full_date_range()
      suffix <- paste0(
        "FullDataset_",
        format(as.Date(dr$min_date), "%Y%m%d"),
        "_to_",
        format(as.Date(dr$max_date), "%Y%m%d")
      )
      export_filename(station_id(), info$province, info$name, suffix, "csv")
    },
    content = function(file) write_csv(selected_data() |> collect(), file)
  )

  output$export_full_parquet <- downloadHandler(
    filename = function() {
      info <- station_info()
      dr <- full_date_range()
      suffix <- paste0(
        "FullDataset_",
        format(as.Date(dr$min_date), "%Y%m%d"),
        "_to_",
        format(as.Date(dr$max_date), "%Y%m%d")
      )
      export_filename(station_id(), info$province, info$name, suffix, "parquet")
    },
    content = function(file) write_parquet(selected_data() |> collect(), file)
  )
}

# ---------------------------------------------------------------------------
# App
# ---------------------------------------------------------------------------

shinyApp(ui, server)
