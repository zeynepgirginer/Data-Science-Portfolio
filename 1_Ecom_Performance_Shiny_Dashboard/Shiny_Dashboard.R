# app.R

library(shiny)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)
library(googlesheets4)
library(stringr)

SHEET_URL <- "https://docs.google.com/spreadsheets/d/1pKqJEepDAQIEW9eBlI4qJ3uauE7LP7-nTk07DEdT0KE/edit?gid=0#gid=0"

read_matrix_sheet <- function() {
  df0 <- read_sheet(
    SHEET_URL,
    col_names = FALSE,
    col_types = "c",
    guess_max = 50000
  )
  
  n <- ncol(df0)
  
  # Only use the data until the first empty cell on row 2 after coloumn C.
  row2_vals <- as.character(df0[2, 3:n])
  is_empty  <- is.na(row2_vals) | trimws(row2_vals) == ""
  
  if (any(is_empty)) {
    first_empty_rel <- which(is_empty)[1]
    last_rel        <- first_empty_rel - 1
    if (last_rel < 1) stop("Dates not found")
    last_col        <- 2 + last_rel
  } else {
    last_col <- n
  }
  
  date_cols_idx <- 3:last_col
  date_strings  <- as.character(df0[2, date_cols_idx])
  
  # date parse: m-d-y
  d1 <- suppressWarnings(mdy(date_strings))
  if (any(is.na(d1))) {
    warning("Some date headers could not be parsed.")
  }
  date_parsed <- d1
  
  colnames(df0)[1:2] <- c("category", "metric")
  colnames(df0)[date_cols_idx] <- format(date_parsed, "%Y-%m-%d")
  
  df0 <- df0[, c(1, 2, date_cols_idx), drop = FALSE]
  
  # 1-2nd row headers are titles, values start from row 3.
  df <- df0[-c(1, 2), , drop = FALSE]
  
  df$category <- df$category |> as.character() |> str_trim()
  df$metric   <- df$metric   |> as.character() |> str_trim()

  
  df <- df |>
    filter(
      category != "", !is.na(category),
      metric   != "", !is.na(metric),
    )
  
  tidy <- df |>
    pivot_longer(
      cols      = -(category:metric),
      names_to  = "date",
      values_to = "value"
    ) |>
    mutate(
      date  = as.Date(date),
      value = value |>
        as.character() |>
        stringr::str_replace_all("[^0-9\\.-]", "") |>
        as.numeric()
    ) |>
    filter(!is.na(date))
  
  tidy
}

# UI

ui <- fluidPage(
  titlePanel("Category Performance Dashboard"),
  sidebarLayout(
    sidebarPanel(
      actionButton("refresh", "Refresh data"),
      tags$hr(),
      radioButtons(
        "view_mode",
        "View mode",
        choices = c(
          "Compare metrics within category"   = "metrics",
          "Compare categories within metric"  = "categories"
        ),
        selected = "metrics"
      ),
      uiOutput("category_ui"),
      uiOutput("metric_ui"),
      dateRangeInput(
        "range1", "Date range 1",
        start = Sys.Date() - 7,
        end   = Sys.Date(),
        format = "dd-mm-yyyy"
      ),
      checkboxInput("enable_range2", "Compare with another date range", FALSE),
      conditionalPanel(
        condition = "input.enable_range2 == true",
        dateRangeInput(
          "range2", "Date range 2",
          start = Sys.Date() - 14,
          end   = Sys.Date() - 7,
          format = "dd-mm-yyyy"
        )
      )
    ),
    mainPanel(
      h4("Summary KPIs"),
      tableOutput("kpi_table"),
      tags$hr(),
      plotlyOutput("main_plot", height = 360),
      tags$hr(),
      h4("Table - Range 1"),
      downloadButton("download_r1", "Download Range 1 (.csv)"),
      br(), br(),
      dataTableOutput("table_r1"),
      conditionalPanel(
        condition = "input.enable_range2 == true",
        tags$hr(),
        h4("Table - Range 2"),
        downloadButton("download_r2", "Download Range 2 (.csv)"),
        br(), br(),
        dataTableOutput("table_r2")
      )
    )
  )
)

# Server

server <- function(input, output, session) {
  
  data_r <- reactiveVal(NULL)
  
  load_data <- function() {
    df <- read_matrix_sheet()
    data_r(df)
  }
  
  observe({ load_data() })
  
  observeEvent(input$refresh, {
    load_data()
    showNotification("Data refreshed", type = "message")
  })
  
  raw <- reactive({
    req(!is.null(data_r()))
    data_r()
  })
  
  current_mode <- reactive({
    if (is.null(input$view_mode)) "metrics" else input$view_mode
  })
  
  # Category UI (default MDA)
  output$category_ui <- renderUI({
    cats <- raw() |>
      distinct(category) |>
      arrange(category) |>
      pull()
    
    mode <- current_mode()
    multiple_flag <- if (mode == "metrics") FALSE else TRUE
    default_sel   <- if ("MDA" %in% cats) "MDA" else head(cats, 1)
    
    selectInput(
      "category",
      if (mode == "metrics") "Category" else "Category(ies)",
      choices  = cats,
      selected = default_sel,
      multiple = multiple_flag
    )
  })
  
  # Metric UI
  output$metric_ui <- renderUI({
    mets <- raw() |>
      distinct(metric) |>
      arrange(metric) |>
      pull()
    
    mode <- current_mode()
    
    if (mode == "metrics") {
      selectInput(
        "metric", "Metric(s)",
        choices  = mets,
        selected = head(mets, 2),
        multiple = TRUE
      )
    } else {
      selectInput(
        "metric", "Metric",
        choices  = mets,
        selected = head(mets, 1),
        multiple = FALSE
      )
    }
  })
  
  filtered_data <- function(date_from, date_to) {
    req(input$category, input$metric)
    cats <- input$category
    mets <- input$metric
    
    raw() |>
      filter(
        category %in% cats,
        metric   %in% mets,
        date >= date_from,
        date <= date_to
      ) |>
      arrange(date, category, metric)
  }
  
  data_r1 <- reactive({
    req(input$range1)
    range <- input$range1
    req(!is.null(range[1]), !is.null(range[2]))
    filtered_data(as.Date(range[1]), as.Date(range[2]))
  })
  
  data_r2 <- reactive({
    req(input$enable_range2)
    req(input$range2)
    range <- input$range2
    req(!is.null(range[1]), !is.null(range[2]))
    filtered_data(as.Date(range[1]), as.Date(range[2]))
  })
  
  # KPI Table
  output$kpi_table <- renderTable({
    df1 <- data_r1()
    
    if (nrow(df1) == 0) {
      return(data.frame(
        Metric  = "No data",
        Range1  = NA,
        Range2  = NA
      ))
    }
    
    total1 <- sum(df1$value, na.rm = TRUE)
    avg1   <- mean(df1$value, na.rm = TRUE)
    dmin1  <- min(df1$date, na.rm = TRUE)
    dmax1  <- max(df1$date, na.rm = TRUE)
    
    # default: empty range 2
    total2 <- NA
    avg2   <- NA
    dmin2  <- NA
    dmax2  <- NA
    change_total <- NA
    
    if (isTRUE(input$enable_range2)) {
      df2 <- data_r2()
      if (nrow(df2) > 0) {
        total2 <- sum(df2$value, na.rm = TRUE)
        avg2   <- mean(df2$value, na.rm = TRUE)
        dmin2  <- min(df2$date, na.rm = TRUE)
        dmax2  <- max(df2$date, na.rm = TRUE)
        if (total2 != 0) {
          change_total <- (total2 - total1) / total1 * 100
        }
      }
    }
    
    data.frame(
      Metric = c("Total", "Average", "Start date", "End date", "Total change (R1 vs R2, %)"),
      `Range 1` = c(
        format(round(total1, 1), big.mark = ","),
        format(round(avg1, 2), big.mark = ","),
        format(dmin1, "%d-%m-%Y"),
        format(dmax1, "%d-%m-%Y"),
        ""
      ),
      `Range 2` = c(
        ifelse(is.na(total2), "", format(round(total2, 1), big.mark = ",")),
        ifelse(is.na(avg2),   "", format(round(avg2, 2), big.mark = ",")),
        ifelse(is.na(dmin2),  "", format(dmin2, "%d-%m-%Y")),
        ifelse(is.na(dmax2),  "", format(dmax2, "%d-%m-%Y")),
        ifelse(is.na(change_total), "", format(round(change_total, 1), big.mark = ","))
      ),
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
  }, rownames = FALSE)
  
  # Plot
  format_val <- function(x) {
    format(round(x, 2), big.mark = ",")
  }
  build_plot <- reactive({
    tryCatch({
      req(input$category, input$metric)
      df1 <- data_r1()
      
      if (nrow(df1) == 0) {
        return(
          plot_ly(
            x = numeric(0),
            y = numeric(0),
            type = "scatter",
            mode = "markers"
          ) |>
            layout(title = "No data for selected range")
        )
      }
      
      
      mode <- current_mode()
      p_obj <- plot_ly()
      
      # Range 1 index
      dates1 <- sort(unique(df1$date))
      df1 <- df1 |>
        mutate(
          pos        = match(date, dates1),
          date_label = format(date, "%d-%m-%Y")
        )
      
      # Range 2 index
      has_r2 <- isTRUE(input$enable_range2)
      df2 <- NULL
      
      if (has_r2) {
        df2 <- data_r2()
        if (nrow(df2) == 0) {
          has_r2 <- FALSE
        } else {
          dates2 <- sort(unique(df2$date))
          df2 <- df2 |>
            mutate(
              pos        = match(date, dates2),
              date_label = format(date, "%d-%m-%Y")
            )
        }
      }
      
      good_series <- function(d) {
        !is.null(d) &&
          nrow(d) > 0 &&
          "pos" %in% names(d) &&
          any(!is.na(d$pos))
      }
      
      if (mode == "metrics") {
        # 1 cat, multiple metrics
        for (m in input$metric) {
          d_sub <- df1 |> filter(metric == m)
          if (good_series(d_sub)) {
            p_obj <- add_trace(
              p_obj,
              x    = d_sub$pos,
              y    = d_sub$value,
              type = "scatter",
              mode = "lines+markers",
              name = m,
              text = paste(
                "Date:", d_sub$date_label,
                "<br>Value:", format_val(d_sub$value)
              ),
              

              hoverinfo = "text+name"
            )
          }
        }
        if (has_r2) {
          for (m in input$metric) {
            d_sub2 <- df2 |> filter(metric == m)
            if (good_series(d_sub2)) {
              p_obj <- add_trace(
                p_obj,
                x    = d_sub2$pos,
                y    = d_sub2$value,
                type = "scatter",
                mode = "lines+markers",
                name = paste0(m, " (R2)"),
                line = list(dash = "dash"),
                text = paste(
                  "Date:", d_sub2$date_label,
                  "<br>Value:", format_val(d_sub2$value)
                ),
                

                hoverinfo = "text+name"
              )
            }
          }
        }
      } else {
        # 1 cat, multiple metrics
        for (c in input$category) {
          d_sub <- df1 |> filter(category == c)
          if (good_series(d_sub)) {
            p_obj <- add_trace(
              p_obj,
              x    = d_sub$pos,
              y    = d_sub$value,
              type = "scatter",
              mode = "lines+markers",
              name = c,
              text = paste(
                "Date:", d_sub$date_label,
                "<br>Value:", format_val(d_sub$value)
              ),
              
              hoverinfo = "text+name"
            )
          }
        }
        if (has_r2) {
          for (c in input$category) {
            d_sub2 <- df2 |> filter(category == c)
            if (good_series(d_sub2)) {
              p_obj <- add_trace(
                p_obj,
                x    = d_sub2$pos,
                y    = d_sub2$value,
                type = "scatter",
                mode = "lines+markers",
                name = paste0(c, " (R2)"),
                line = list(dash = "dash"),
                text = paste(
                  "Date:", d_sub2$date_label,
                  "<br>Value:", format_val(d_sub2$value)
                ),
                hoverinfo = "text+name"
              )
            }
          }
        }
      }
      
      title_txt <- if (mode == "metrics") {
        paste0(
          paste(input$category, collapse = ", "),
          " | ",
          paste(input$metric, collapse = ", ")
        )
      } else {
        paste0(
          paste(input$metric, collapse = ", "),
          " | ",
          paste(input$category, collapse = ", ")
        )
      }
      
      p_obj |>
        layout(
          title = title_txt,
          xaxis = list(title = "Index (day index in date range)"),
          yaxis = list(title = "Value"),
          hovermode = "closest"
        )
    }, error = function(e) {
      plot_ly(
        x = numeric(0),
        y = numeric(0),
        type = "scatter",
        mode = "markers"
      ) |>
        layout(
          title = paste("Plot error:", e$message)
        )
    })
    
  })
  
  output$main_plot <- renderPlotly({
    build_plot()
  })
  
  # Tables
  output$table_r1 <- renderDataTable({
    df <- data_r1()
    if ("value" %in% names(df)) {
      df$value <- format(df$value, big.mark = ",")
    }
    df
  })
  
  
  output$table_r2 <- renderDataTable({
    df <- data_r2()
    if ("value" %in% names(df)) {
      df$value <- format(df$value, big.mark = ",")
    }
    df
  })
  
  
  # CSV Export
  output$download_r1 <- downloadHandler(
    filename = function() "range1_data.csv",
    content = function(file) {
      write.csv(data_r1(), file, row.names = FALSE)
    }
  )
  
  output$download_r2 <- downloadHandler(
    filename = function() "range2_data.csv",
    content = function(file) {
      write.csv(data_r2(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)
