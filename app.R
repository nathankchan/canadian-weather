# app.R

library(tidyverse)
library(plotly)
library(htmlwidgets)
library(shiny)
library(shinycssloaders)
library(DT)
library(arrow)

# Runs a Shiny R app to interactively view Canadian historical weather data by range

stationlist <- read_csv("stationlist.csv", show_col_types = FALSE) %>%
  arrange(Name)
station_choices <- split(
  setNames(
    paste0(stationlist$`Station ID`, ".parquet"),
    stationlist$Name
  ),
  stationlist$Province
)

ui <- fluidPage(
  titlePanel("Canadian Historical Weather Data"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      p(span(
        "Generate interactive plots of historical hourly weather data for active Canadian weather stations.",
        style = "color:blue"
      )),

      radioButtons(
        inputId = "showhelp",
        label = span("Display help text", style = "color:blue"),
        choices = list("Yes" = TRUE, "No" = FALSE),
        selected = TRUE
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
        choices = c("Select a station..." = ""),
        selected = ""
      ),

      selectInput(
        inputId = "selectedcolumn",
        label = "Choose data to display",
        choice = c("")
      ),

      dateRangeInput(
        inputId = "dates",
        label = "Specify date range"
      ),

      tags$head(tags$style("#datehelp{color:grey;}")),

      conditionalPanel(
        condition = "input.showhelp == 'TRUE'",
        htmlOutput(outputId = "datehelp")
      ),

      radioButtons(
        inputId = "autozaxis",
        label = "Autoscale height",
        choices = list("Yes" = TRUE, "No" = FALSE),
        selected = TRUE
      ),

      conditionalPanel(
        condition = "input.showhelp == 'TRUE'",
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
          condition = "input.showhelp == 'TRUE'",
          p(span(
            "Limits are with respect to the entire data range. Values below describe the maximum and minimum for the data displayed.",
            style = "color:grey"
          ))
        ),
        tableOutput(outputId = "zaxistable")
      ),

      conditionalPanel(
        condition = "input.showhelp == 'TRUE'",
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
        tabPanel(
          title = "Surface Plot",
          withSpinner(plotlyOutput(
            outputId = "display3dplot",
            width = "auto",
            height = "800px"
          ))
        ),
        tabPanel(
          title = "Line Chart",
          withSpinner(plotlyOutput(
            outputId = "displayline",
            width = "auto",
            height = "800px"
          ))
        ),
        tabPanel(
          title = "Heat Map",
          withSpinner(plotlyOutput(
            outputId = "displayheat",
            width = "auto",
            height = "800px"
          ))
        ),
        tabPanel(
          title = "Table",
          DTOutput(
            outputId = "displaytable"
          )
        )
      )
    )
  )
)


server <- function(input, output, session) {
  selected_data <- reactive({
    req(input$station)
    open_dataset(paste0("./data/", input$station))
  })

  filtered_data <- reactive({
    req(input$dates)
    start_posix <- as.POSIXct(input$dates[1])
    end_posix <- as.POSIXct(input$dates[2] + 1L)
    selected_data() %>%
      filter(
        `Date/Time (LST)` >= start_posix,
        `Date/Time (LST)` < end_posix
      ) %>%
      collect()
  })

  plot_3dinput <- reactive({
    req(input$selectedcolumn)

    out <- filtered_data()[, c(
      "Date/Time (LST)",
      "Time (LST)",
      input$selectedcolumn
    )]
    colnames(out) <- c("Date", "Hour", "Value")
    out[["Date"]] <- format(out[["Date"]], "%Y-%m-%d")

    out %>%
      pivot_wider(names_from = Date, values_from = Value) %>%
      select(-Hour) %>%
      as.matrix()
  })

  plot_lineinput <- reactive({
    req(input$selectedcolumn)

    out <- filtered_data()[, c(
      "Date/Time (LST)",
      "Time (LST)",
      input$selectedcolumn
    )]
    cbind.data.frame(
      DateTime = as.POSIXct(out[["Date/Time (LST)"]]),
      Value = out[[input$selectedcolumn]]
    )
  })

  plot_heatinput <- reactive({
    req(input$selectedcolumn)

    out <- filtered_data()[, c(
      "Date/Time (LST)",
      "Time (LST)",
      input$selectedcolumn
    )]
    colnames(out) <- c("Date", "Hour", "Value")

    out[["Hour"]] <- format(as.POSIXct(out[["Hour"]]), format = "%H") %>%
      as.numeric()

    return(out)
  })

  output$zaxistable <- renderTable(
    {
      req(input$selectedcolumn)

      minmax <- filtered_data()[[input$selectedcolumn]]
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

    if (ncol(plot_data) > 365) {
      xindex <- which(substr(colnames(plot_data), 9, 12) == "01")
      xindex <- xindex[seq(
        from = 1,
        to = length(xindex),
        by = floor(length(xindex) / 12)
      )]
    } else if (ncol(plot_data) > 12) {
      xindex <- seq(
        from = 1,
        to = ncol(plot_data),
        by = floor(ncol(plot_data) / 12)
      )
    } else {
      xindex <- seq_len(ncol(plot_data))
    }

    xlabels_df <- cbind.data.frame(
      xindex = xindex,
      xlabels = colnames(plot_data)[xindex] %>%
        as.Date() %>%
        format(., "%y-%b-%d")
    )

    plot_xaxis <- list(
      title = "",
      tickmode = "array",
      ticktext = xlabels_df$xlabels,
      tickvals = xlabels_df$xindex,
      range = c(1, ncol(plot_data))
    )

    plot_yaxis <- list(
      title = "",
      tickmode = "array",
      ticktext = c("0400h", "0800h", "1200h", "1600h", "2000h", "2400h"),
      tickvals = c(4, 8, 12, 16, 20, 24)
    )

    plot_zaxis <- list(
      title = input$selectedcolumn
    )

    if (input$autozaxis == FALSE) {
      plot_zaxis$range <- c(input$zaxislimits[1], input$zaxislimits[2])
    }

    plot_out <- plot_ly(z = ~plot_data, lighting = list(ambient = 0.9)) %>%
      add_surface(
        showscale = TRUE,
        colorbar = list(title = list(text = input$selectedcolumn)),
        contours = list(
          z = list(
            show = FALSE,
            usecolormap = TRUE,
            highlightcolor = "#ff0000",
            project = list(z = FALSE)
          )
        )
      ) %>%
      layout(
        title = list(
          text = paste0(
            "<br>",
            input$selectedcolumn,
            "<br>(",
            input$dates[1] %>% as.Date() %>% format(., "%Y-%b-%d"),
            " to ",
            input$dates[2] %>% as.Date() %>% format(., "%Y-%b-%d"),
            ")"
          )
        ),
        scene = list(
          xaxis = plot_xaxis,
          yaxis = plot_yaxis,
          zaxis = plot_zaxis,
          scale = list(title = list(text = input$selectedcolumn)),
          camera = list(
            eye = list(x = 1.5, y = -1.5, z = 0.75)
          ),
          aspectmode = "manual",
          aspectratio = list(
            x = 2,
            y = 1,
            z = 1
          )
        )
      )

    return(plot_out)
  })

  output$displayline <- renderPlotly({
    plot_data <- plot_lineinput()

    plot_out <- ggplot(
      data = plot_data,
      aes(x = DateTime, y = Value)
    ) +
      geom_line(linewidth = 0.1, color = "blue") +
      labs(
        x = "Date",
        y = input$selectedcolumn,
        title = paste0(
          input$selectedcolumn,
          " (",
          input$dates[1] %>% as.Date() %>% format(., "%Y-%b-%d"),
          " to ",
          input$dates[2] %>% as.Date() %>% format(., "%Y-%b-%d"),
          ")"
        )
      ) +
      theme_light()

    if (input$autozaxis == FALSE) {
      plot_out <- plot_out + ylim(input$zaxislimits)
    }

    return(ggplotly(plot_out))
  })

  output$displayheat <- renderPlotly({
    plot_data <- plot_heatinput()

    plot_out <- ggplot(
      data = plot_data,
      aes(x = as.Date(Date), y = Hour)
    ) +
      geom_tile(aes(fill = Value)) +
      scale_y_continuous(n.breaks = 13, limits = c(0, 23)) +
      labs(
        x = "Date",
        y = "Hour",
        fill = input$selectedcolumn,
        title = paste0(
          input$selectedcolumn,
          " (",
          input$dates[1] %>% as.Date() %>% format(., "%Y-%b-%d"),
          " to ",
          input$dates[2] %>% as.Date() %>% format(., "%Y-%b-%d"),
          ")"
        )
      )

    if (input$autozaxis == FALSE) {
      plot_out <- plot_out +
        scale_fill_viridis_c(limits = input$zaxislimits) +
        theme_light()
    } else {
      plot_out <- plot_out +
        scale_fill_viridis_c() +
        theme_light()
    }

    return(ggplotly(plot_out))
  })

  output$displaytable <- renderDT(
    {
      req(input$station)
      filtered_data()
    },
    options = list(width = "75%")
  )

  output$datehelp <- renderText({
    date_range <- selected_data() %>%
      summarise(
        min_date = min(`Date/Time (LST)`, na.rm = TRUE),
        max_date = max(`Date/Time (LST)`, na.rm = TRUE)
      ) %>%
      collect()
    firstdate <- format(as.POSIXct(date_range$min_date), "%Y-%m-%d")
    lastdate <- format(as.POSIXct(date_range$max_date), "%Y-%m-%d")
    out <- paste0(
      "<p>Data are available from <i>",
      firstdate,
      "</i> to <i>",
      lastdate,
      "</i>.</p>"
    )
    return(out)
  })

  observeEvent(input$province, {
    if (input$province == "") {
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
  })

  observe({
    req(input$station)
    ds <- selected_data()

    exclude_cols <- c("Longitude (x)", "Latitude (y)", "Climate ID", "Year")
    type_strs <- sapply(ds$schema$fields, function(f) f$type$ToString())
    numeric_cols <- ds$schema$names[
      grepl("^(int|uint|float|double)", type_strs, ignore.case = TRUE) &
        !ds$schema$names %in% exclude_cols
    ]

    date_range <- ds %>%
      summarise(
        min_date = min(`Date/Time (LST)`, na.rm = TRUE),
        max_date = max(`Date/Time (LST)`, na.rm = TRUE)
      ) %>%
      collect()

    min_date <- as.POSIXct(date_range$min_date)
    max_date <- as.POSIXct(date_range$max_date)

    updateSelectInput(
      session = session,
      inputId = "selectedcolumn",
      choices = numeric_cols,
      selected = "Temp (°C)"
    )
    updateDateRangeInput(
      inputId = "dates",
      min = min_date,
      start = max_date - 365 * 24 * 3600 * 2,
      max = max_date,
      end = max_date
    )
  })

  observe({
    req(input$selectedcolumn)
    req(input$dates)

    col_data <- selected_data() %>%
      select(all_of(input$selectedcolumn)) %>%
      collect() %>%
      .[[1]]

    out <- range(col_data, na.rm = TRUE)
    buffer <- (out[2] - out[1]) / 10

    updateSliderInput(
      inputId = "zaxislimits",
      min = out[1] - buffer,
      max = out[2] + buffer,
      value = c(out[1], out[2])
    )
  })
}


shinyApp(ui, server)
