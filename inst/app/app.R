library(shiny)
library(bslib)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)

df0 <- myappkg::clean_dataset %>%
  mutate(
    R0    = as.numeric(R0),
    VE    = as.numeric(VE),
    value = as.numeric(value)
  ) 

r0_rng <- range(df0$R0, na.rm = TRUE)
ve_rng <- range(df0$VE, na.rm = TRUE)

ui <- page_fluid(
  theme = bs_theme(bootswatch = "flatly"),
  titlePanel("R0 × VE → value"),
  sidebarLayout(
    sidebarPanel(
      selectInput("mode","Graphic Mode",
                  choices = c("heatmap"= "heat",
                              "contour"= "contour",
                              "multilines(R0)"= "lines_R0",
                              "multilines(VE)"= "lines_VE"),
                  selected = "heat"),
      sliderInput("r0_range","R0 filter",
                  min = r0_rng[1], max = r0_rng[2], value = r0_rng, step = diff(r0_rng)/100),
      sliderInput("ve_range","VE filter",
                  min = ve_rng[1], max = ve_rng[2], value = ve_rng, step = diff(ve_rng)/100),
      checkboxInput("use_plotly","Plotly",value= TRUE),
      checkboxInput("show_points","Scatter Points in Line Chart",value = TRUE),
      downloadButton("download_csv","download the filtered data (csv)"),
      card(
        card_header("Parameter Explanations"),
        HTML(paste0(
          "<b>R0</b>: In a fully susceptible population, the average number of new infections caused by one infectious individual; larger R0 means higher transmissibility.<br/>",
          "<b>VE (Vaccine Effectiveness)</b>: Proportionate reduction in infection/transmission due to vaccination (range 0–1).<br/>",
          "<b>value</b>: The model outcome under given R0 and VE.<br/><br/>",
          "<b>How to read the charts:</b><br/>",
          "Heatmap: same colors indicate similar value.<br/>",
          "Contour (filled): equal-value bands/lines. ",
          "Note: Plotly may not fully support filled contours; uncheck Plotly for best static rendering.<br/>",  
          "Multilines (group by R0): each line = fixed R0; x-axis = VE; observe how value changes with VE.<br/>",
          "Multilines (group by VE): each line = fixed VE; x-axis = R0; observe how value changes with R0."
        )))
      ),
    mainPanel(
      card(
        card_header("Figures"),
        uiOutput("plot_ui")
      ),
      card(
        card_header("Data preview"),
        dataTableOutput("tbl")
      )
    )
  )
)

server <- function(input, output, session) {

  dat <- reactive({
    req(input$r0_range, input$ve_range)
    df0 %>%
      filter(R0 >= input$r0_range[1], R0 <= input$r0_range[2],
             VE >= input$ve_range[1], VE <= input$ve_range[2]) %>%
      arrange(R0, VE)
  })

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("filtered_R0_", input$r0_range[1], "-", input$r0_range[2],
             "_VE_", input$ve_range[1], "-", input$ve_range[2], ".csv")
    },
    content = function(file) {
      write.csv(dat(), file, row.names = FALSE)
    }
  )

  output$tbl <- renderDataTable({
    datatable(dat(), options = list(pageLength = 10, scrollX = TRUE))
  })

  output$plot_ui <- renderUI({
    if (isTRUE(input$use_plotly)) {
      plotlyOutput("plt", height = "520px")
    } else {
      plotOutput("plt_static", height = "520px")
    }
  })

  make_gg <- reactive({
    d <- dat()
    req(nrow(d) > 0)

    if (input$mode == "heat") {
      ggplot(d, aes(R0, VE, fill = value)) +
        geom_tile() +
        labs(x = "R0", y = "VE", fill = "value", title = "Heatmap") +
        theme_minimal()
    } else if (input$mode == "contour") {
      ggplot(d, aes(R0, VE, z = value)) +
        geom_contour_filled(alpha = 0.8) +
        labs(x = "R0", y = "VE", fill = "value", title = "Filled Contours") +
        theme_minimal()
    } else if (input$mode == "lines_R0") {
      d1 <- d %>% mutate(R0 = factor(R0))
      p <- ggplot(d1, aes(x = VE, y = value, color = R0, group = R0)) +
        geom_line()
      if (input$show_points) p <- p + geom_point(size = 1)
      p + labs(x = "VE", y = "value", color = "R0",
               title = "value vs VE by R0") +
        theme_minimal()
    } else { # lines_VE
      d2 <- d %>% mutate(VE = factor(VE))
      p <- ggplot(d2, aes(x = R0, y = value, color = VE, group = VE)) +
        geom_line()
      if (input$show_points) p <- p + geom_point(size = 1)
      p + labs(x = "R0", y = "value", color = "VE",
               title = "value vs R0 by VE") +
        theme_minimal()
    }
  })

  output$plt <- renderPlotly({
    req(isTRUE(input$use_plotly))
    ggplotly(make_gg(), tooltip = c("x","y","fill","colour","value"))
  })

  output$plt_static <- renderPlot({
    req(!isTRUE(input$use_plotly))
    make_gg()
  })
}

shinyApp(ui, server)
