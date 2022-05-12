# app.R

library(tidyverse)
library(plotly)
library(htmlwidgets)
library(shiny)

# required_packages <-
#   c("tidyverse",
#     "plotly",
#     "htmlwidgets",
#     "shiny")
# 
# check_installed <- suppressWarnings(unlist(lapply(required_packages, require, character.only = TRUE)))
# needed_packages <- required_packages[check_installed == FALSE]
# install.packages(needed_packages)
# lapply(required_packages, require, character.only = TRUE)

# data_all <- read_csv(file = "./processeddata/ONTARIO.csv.gz")

# Code for testing...

# plot_data <- data_all[, c("Date", "Hour", "Market.Demand")] %>%
#   pivot_wider(names_from = Date, values_from = Market.Demand) %>%
#   select(-Hour) %>% as.matrix()

# Runs a Shiny R app to interactively view Ontario market data by range

ui <- fluidPage(
  
  titlePanel("Canadian Historical Weather Data"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,

      p(span("Generate interactive plots of weather data from the largest city of each Canadian province.", style = "color:blue")),
      
      radioButtons(inputId = "showhelp",
                   label = span("Display help text", style = "color:blue"),
                   choices = list("Yes" = TRUE, "No" = FALSE),
                   selected = TRUE
      ),
      
      selectInput(inputId = "station",
                  label = "Choose station to display",
                  choices = list(
                    "ALBERTA | CALGARY INTL A" = "ALBERTA.csv.gz",
                    "BRITISH COLUMBIA | VANCOUVER INTL A" = "BRITISH COLUMBIA.csv.gz",
                    "MANITOBA | WINNIPEG INTL A" = "MANITOBA.csv.gz",
                    "NEW BRUNSWICK | MONCTON/GREATER MONCTON ROMEO LEBLANC INTL A" = "NEW BRUNSWICK.csv.gz",
                    "NEWFOUNDLAND | ST. JOHN'S INTL A" = "NEWFOUNDLAND.csv.gz",
                    "NORTHWEST TERRITORIES | YELLOWKNIFE A" = "NORTHWEST TERRITORIES.csv.gz",
                    "NOVA SCOTIA | HALIFAX STANFIELD INT'L A" = "NOVA SCOTIA.csv.gz",
                    "NUNAVUT | IQALUIT A" = "NUNAVUT.csv.gz",
                    "ONTARIO | TORONTO INTL A" = "ONTARIO.csv.gz",
                    "PRINCE EDWARD ISLAND | CHARLOTTETOWN A" = "PRINCE EDWARD ISLAND.csv.gz",
                    "QUEBEC | MONTREAL INTL A" = "QUEBEC.csv.gz",
                    "SASKATCHEWAN | SASKATOON INTL A" = "SASKATCHEWAN.csv.gz",
                    "YUKON TERRITORY| WHITEHORSE A" = "YUKON TERRITORY.csv.gz"
                  ),
                  selected = "ONTARIO.csv.gz"
      ),
      
      selectInput(inputId = "selectedcolumn",
                  label = "Choose data to display",
                  choice = c(""))
      
      ,
      
      dateRangeInput(inputId = "dates",
                     label = "Specify date range"
                     # ,
                     # start = (Sys.Date() - 365 * 2) %>% as.character(),
                     # end = Sys.Date() %>% as.character(),
                     # min = (Sys.Date() - 365 - 2) %>% as.character(),
                     # max = Sys.Date() %>% as.character()
                     ) 
      
      ,
      
      tags$head(tags$style("#datehelp{color:grey;}")),
      
      conditionalPanel(
        condition = "input.showhelp == 'TRUE'",
        htmlOutput(outputId = "datehelp")
      )
      
      ,

      radioButtons(inputId = "autozaxis",
                   label = "Autoscale height",
                   choices = list("Yes" = TRUE, "No" = FALSE),
                   selected = TRUE
                   ),

      conditionalPanel(
        condition = "input.showhelp == 'TRUE'",
        p(span("Turn off autoscaling to facilitate comparisons across date ranges.", style = "color:grey"))
      )
      
      ,

      conditionalPanel(
        condition = "input.autozaxis == 'FALSE'",
        sliderInput(inputId = "zaxislimits",
                    label = "Specify height limits",
                    min = 0, max = 1,
                    value = c(0, 1),
                    step = 0.1),
        conditionalPanel(
          condition = "input.showhelp == 'TRUE'",
          p(span("Limits are with respect to the entire data range. Values below describe the maximum and minimum for the data displayed.", style = "color:grey"))
        ),
        tableOutput(outputId = "zaxistable")
      )
      ,

      conditionalPanel(
        condition = "input.showhelp == 'TRUE'",
        hr(style = "border-top: 1px dotted #808080;"),
        p(a("Source", href = "https://climate.weather.gc.ca/historical_data/search_historic_data_e.html")),
        p(span(em("Data are published by Environment and Climate Change Canada."), style = "color:grey")),
        p(span(em("Built by"), "Nathan K. Chan", em(" in ", strong("R"), "and", strong("Shiny", .noWS = "after"), ". Visit the project on Github ", a("here", href = "https://nathankchan.github.io/canadian-weather/", .noWS = "after"), "."), style = "color:grey"))
      )

      # ,
      
      
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel(
          title = "Surface Plot", 
          plotlyOutput(
            outputId = "display3dplot",
            width = "auto",
            height = "800px"))
        ,
        tabPanel(
          title = "Line Chart", 
          plotlyOutput(
            outputId = "displayline",
            width = "auto",
            height = "800px"))
        ,
        tabPanel(
          title = "Heat Map", 
          plotlyOutput(
            outputId = "displayheat",
            width = "auto",
            height = "800px"))
        ,
        tabPanel(
          title = "Table", 
          dataTableOutput(
            outputId = "displaytable"))
      )
    )
    
  )
)


server <- function(input, output, session) {
  
  selected_data <- reactive({
    req(input$station)
    out <- read_csv(paste0("./processeddata/", input$station), show_col_types = FALSE)
    return(out)
  })
  
  plot_3dinput <- reactive({
    req(input$selectedcolumn)
    
    data_all <- selected_data()
    
    start_date <- which(as.character(data_all[["Date/Time (LST)"]], format = "%Y-%m-%d") %in% as.character(input$dates[1]))[1]
    end_date <- which(as.character(data_all[["Date/Time (LST)"]], format = "%Y-%m-%d") %in% as.character(input$dates[2])) %>% .[length(.)]

    out <- data_all[start_date:end_date, c("Date/Time (LST)", "Time (LST)", input$selectedcolumn)]
    colnames(out) <- c("Date", "Hour", "Value")
    out[["Date"]] <- as.character(out[["Date"]], format = "%Y-%m-%d")
    
    out <- out %>%
      pivot_wider(names_from = Date, values_from = Value) %>%
      select(-Hour) %>% as.matrix()

  })

  plot_lineinput <- reactive({
    req(input$selectedcolumn)
    data_all <- selected_data()
    
    start_date <- which(as.character(data_all[["Date/Time (LST)"]], format = "%Y-%m-%d") %in% as.character(input$dates[1]))[1]
    end_date <- which(as.character(data_all[["Date/Time (LST)"]], format = "%Y-%m-%d") %in% as.character(input$dates[2])) %>% .[length(.)]

    out <- data_all[start_date:end_date, c("Date/Time (LST)", "Time (LST)", input$selectedcolumn)]

    out <- cbind.data.frame(
      DateTime = as.POSIXct(out[["Date/Time (LST)"]]),
      Value = out[[input$selectedcolumn]]
    )
    
    return(out)

  })

  plot_heatinput <- reactive({
    req(input$selectedcolumn)
    data_all <- selected_data()

    start_date <- which(as.character(data_all[["Date/Time (LST)"]], format = "%Y-%m-%d") %in% as.character(input$dates[1]))[1]
    end_date <- which(as.character(data_all[["Date/Time (LST)"]], format = "%Y-%m-%d") %in% as.character(input$dates[2])) %>% .[length(.)]

    out <- data_all[start_date:end_date, c("Date/Time (LST)", "Time (LST)", input$selectedcolumn)]
    colnames(out) <- c("Date", "Hour", "Value")
    
    out[["Hour"]] <- format(as.POSIXct(out[["Hour"]]), format = "%H") %>% as.numeric()

    return(out)

  })
  
  output$zaxistable <- renderTable({
    req(input$selectedcolumn)
    data_all <- selected_data()

    start_date <- which(as.character(data_all[["Date/Time (LST)"]], format = "%Y-%m-%d") %in% as.character(input$dates[1]))[1]
    end_date <- which(as.character(data_all[["Date/Time (LST)"]], format = "%Y-%m-%d") %in% as.character(input$dates[2])) %>% .[length(.)]

    minmax <- data_all[start_date:end_date, input$selectedcolumn]
    minmax <- cbind.data.frame(
      Minimum = min(minmax),
      Maximum = max(minmax))

    out <- minmax

  }, width = "100%", align = "c")
  

  output$display3dplot <- renderPlotly({

    plot_data <- plot_3dinput()

    if (ncol(plot_data) > 365) {
      xindex <- which(substr(colnames(plot_data), 9, 12) == "01")
      xindex <- xindex[seq(from = 1, to = length(xindex), by = floor(length(xindex)/12))]
    } else if (ncol(plot_data) > 12) {
      xindex <- seq(from = 1, to = ncol(plot_data), by = floor(ncol(plot_data)/12))
    } else {
      xindex <- seq_len(ncol(plot_data))
    }

    xlabels_df <- cbind.data.frame(
      xindex = xindex,
      xlabels = colnames(plot_data)[xindex] %>% as.Date() %>% format(., "%y-%b-%d")
    )


    plot_xaxis <- list(
      title = "",
      tickmode = "array",
      ticktext = xlabels_df$xlabels,
      tickvals = xlabels_df$xindex,
      range = c(1, ncol(plot_data)))

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

    plot_out <- plot_ly(z = ~ plot_data,
            lighting = list(ambient = 0.9)) %>%
      add_surface(
        showscale = TRUE,
        colorbar = list(title = list(text = input$selectedcolumn)),
        contours = list(
          z = list(
            show = FALSE,
            usecolormap = TRUE,
            highlightcolor = "#ff0000",
            project = list(z = F)
          )
        )
      ) %>%
      layout(
        title = list(text = paste0(
          "<br>",
          input$selectedcolumn,
          "<br>(",
          input$dates[1] %>% as.Date() %>% format(., "%Y-%b-%d"),
          " to ",
          input$dates[2] %>% as.Date() %>% format(., "%Y-%b-%d"),
          ")"
        )),
        # legend = list(text = "Demand (MW)"),
        scene = list(
               xaxis = plot_xaxis,
               yaxis = plot_yaxis,
               zaxis = plot_zaxis,
               scale = list(title = list(text = input$selectedcolumn)),
               camera = list(
                 eye = list(x = 1.5,
                            y = -1.5,
                            z = 0.75)
               ),
               aspectmode = "manual",
               aspectratio = list(
                 x = 2,
                 y = 1,
                 z = 1
               ))
      )

    return(plot_out)

  })

  output$displayline <- renderPlotly({

    plot_data <- plot_lineinput()

    plot_out <- ggplot(
      data = plot_data,
      aes(x = DateTime, y = Value)
      ) +
      geom_line(size = 0.1, color = "blue") +
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
        )) +
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
      scale_y_continuous(n.breaks = 13,
                         limits = c(0,23)) +
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
        ))

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
  
  output$displaytable <- renderDataTable({
    req(input$station)
    data_all <- selected_data()
    
    start_date <- which(as.character(data_all[["Date/Time (LST)"]], format = "%Y-%m-%d") %in% as.character(input$dates[1]))[1]
    end_date <- which(as.character(data_all[["Date/Time (LST)"]], format = "%Y-%m-%d") %in% as.character(input$dates[2])) %>% .[length(.)]
    
    out <- data_all[start_date:end_date,]
    
    return(out)
  }, options = list(width = "75%"))
  
  output$datehelp <- renderText({
    data_all <- selected_data()
    firstdate <- as.character(data_all[["Date/Time (LST)"]][1], format = "%Y-%m-%d")
    lastdate <- as.character(data_all[["Date/Time (LST)"]][nrow(data_all)], format = "%Y-%m-%d")
    out <- paste0("<p>Data are available from <i>", firstdate, "</i> to <i>", lastdate, "</i>.</p>")
    return(out)
  })
  

  observe({
    req(input$station)
    data_all <- selected_data()
    updateSelectInput(
      session = session,
      inputId = "selectedcolumn",
      choices = colnames(data_all[, sapply(data_all, is.numeric)]),
      selected = "Temp (°C)"
    )
    updateDateRangeInput(
      inputId = "dates",
      min = data_all[["Date/Time (LST)"]][1],
      start = data_all[["Date/Time (LST)"]][nrow(data_all) - 365 * 24 * 2],
      max = data_all[["Date/Time (LST)"]][nrow(data_all)],
      end = data_all[["Date/Time (LST)"]][nrow(data_all)]
    )
    
  })
  
  observe({
    req(input$selectedcolumn)
    req(input$dates)
    
    data_all <- selected_data()
    
    out <- data_all[[input$selectedcolumn]]
    out <- range(out)
    
    buffer <- (out[2] - out[1])/10
    
    updateSliderInput(
      inputId = "zaxislimits",
      min = out[1] - buffer, 
      max = out[2] + buffer,
      value = c(out[1], out[2])
    )
  })
  
  
}


shinyApp(ui, server)
