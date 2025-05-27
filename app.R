library(dplyr)
library(glue)
library(htmltools)
library(leaflet)
library(plotly)
library(purrr)
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(sf)
library(sp)
library(stringr)
library(thematic)

# DEFINE HELPER FUNCTION --------------------

#' A function to pass into leaflet::labFormat()
#'
#' @param label A factor indicating a quintile ("1", "2", "3", "4", "5")
#'
#' @return A string. "1 (Least deprived)" for the lowest quintile.
#' "5 (Most deprived)" for the highest quintile. All others are returned as is.
#'
#' @examples rename_legend_label("1")
rename_legend_label <- function(label) {
  if (label == "1") {
    return("1 (Least deprived)")
  } else if (label == "5") {
    return("5 (Most deprived)")
  } else {
    return(label)
  }
}

# ENABLE BOOTSTRAP THEME FOR PLOTLY OUTPUT --------------------
thematic_shiny(font = "auto")

# LOAD DATA --------------------
inspq2_chsa <- readr::read_rds("inspq_chsa_q5.rds")

# DEFINE VIEW OF INSET MAPS --------------------
# BC: https://www.latlong.net/place/british-columbia-canada-18663.html
# Lower Mainland: center of Metro Vancouver Regional District
# Greater Victoria: center of Capital Regional District
# Central Okanagan: center of Central Okanagan Regional District
view_df <- data.frame(
  region = c("Whole Province", "Lower Mainland", "Greater Victoria", "Central Okanagan"),
  lng = c(-125.65, -122.95, -123.69, -119.46),
  lat = c(54.73, 49.26, 48.56, 49.95),
  zoom = c(4.75, 9, 9, 9)
)

# BUILD THE USER INTERFACE OBJECT --------------------

ui <- fluidPage(
  theme = shinytheme("cerulean"),
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"), # remove minor ticks
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      wellPanel(
        sliderInput("year", "Year", min = 2001, max = 2016, value = 2016, step = 1, sep = "")
      ),
      wellPanel(
        selectInput("indicator", "Indicator", c("Material Deprivation" = "material", 
                                                "Social Deprivation" = "social",
                                                "Total Deprivation" = "total"))
      ),
      wellPanel(
        selectInput("inset", "Quick Zoom", c("Whole Province", "Lower Mainland", "Greater Victoria", "Central Okanagan"))
      ),
      markdown(
        "**Source**: Material and Social Deprivation Index. [INSPQ Public Health Expertise and Reference Centre](https://www.inspq.qc.ca/en/deprivation/material-and-social-deprivation-index).
        Prepared by Population Health Surveillance and Epidemiology, 
        Office of the Provincial Health Officer, BC Ministry of Health, November 2021."
      ),
      markdown(
        "**Notes**: CHSA quintiles were produced by aggregating the population-weighted 
        Dissemination Area factor scores of the original INSPQ deprivation data 
        after imputation of missing data. The R package [Tongfen](https://mountainmath.github.io/tongfen/index.html)
        was used to create the historical linkage and aggregation across census cycles.
        Linear interpolation was used to estimate inter-census factor score values 
        from which percentiles and quintiles were created."
      ),
    ),
    mainPanel = mainPanel(
      fluidRow(
        h3(textOutput("text_map"))
      ),
      fluidRow(
        leafletOutput("map", height = 500) # make map taller
      ),
      fluidRow(
        h3(textOutput("text_lineplot"))
      ),
      fluidRow(
        plotlyOutput("lineplot")
      )
    )
  )
)

# DEFINE THE SERVER FUNCTION --------------------

server <- function(input, output) {

  # filter the input data
  map_data <- reactive({
    inspq2_chsa %>%
      dplyr::filter(year == input$year, indicator == input$indicator)
  })

  # map values to a color palette
  color_pal <- reactive({
    colorFactor("PuRd", domain = map_data()$value)
  })

  # render the map title
  output$text_map <- renderText({
    glue("{str_to_title(input$indicator)} Deprivation by Community Health Service Area,
          BC, {input$year}")
  })

  # render the static elements of the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles()
  })

  # create reactive observers according to https://rstudio.github.io/leaflet/shiny.html 
  # modify the coloring and tooltip when a new year or indicator is chosen
  observe({
    map_data <- map_data()
    color_pal <- color_pal()

    labels <- sprintf(
      "<strong>%s</strong>%s<br/><strong>%s</strong>%s<br/><strong>%s</strong>%g",
      "LHA ", map_data$lha_name, "CHSA ", map_data$chsa_name, "Quintile ", map_data$value
    ) %>%
      lapply(htmltools::HTML)

    leafletProxy("map", data = map_data) %>%
      clearShapes() %>%
      addPolygons(
        layerId = ~cmnty_hlth_serv_area_code, # link polygons to plotly graph according to https://stackoverflow.com/a/51375468
        fillColor = ~ color_pal(value),
        weight = 1.5,
        opacity = 1,
        color = "white",
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 1.5,
          color = "grey",
          bringToFront = TRUE
        ),
        label = ~labels,
        labelOptions = labelOptions(
          textsize = "12px",
          direction = "auto"
        )
      )
  })

  # modify the view when a new region is selected
  observe({
    view <- view_df %>%
      dplyr::filter(region == input$inset)

    leafletProxy("map") %>%
      setView(lng = view$lng, lat = view$lat, zoom = view$zoom)
  })

  # modify the legend
  observe({
    color_pal <- color_pal()
    map_data <- map_data()

    leafletProxy("map", data = map_data) %>%
      clearControls() %>% # remove old legend
      addLegend(
        position = "bottomright",
        pal = color_pal,
        values = ~value,
        labFormat = labelFormat(transform = function(x) map_chr(x, rename_legend_label)),
        title = "Quintile"
      )
  })

  # responds when user clicks on a new polygon or new indicator is selected
  observeEvent(input$map_shape_click, {
    # capture information of selected polygon according to https://community.rstudio.com/t/shiny-leaflet-link-map-polygons-to-reactive-plotly-graphs/40527/2
    click <- input$map_shape_click

    # filter the input data based on *LHA* of selected CHSA
    lha_df <- reactive({
      inspq2_chsa %>%
        dplyr::filter(
          indicator == input$indicator,
          local_hlth_area_code == stringr::str_sub(click$id, 1, 3)
        )
    })

    # render the line plot title
    output$text_lineplot <- renderText({
      lha <- lha_df() %>%
        head(1)

      glue("{str_to_title(input$indicator)} Deprivation by Community Health Service Area,
                 {lha$lha_name}, 2001-2016")
    })

    # evaluate number of lines to plot
    n_chsa <- lha_df() %>%
      dplyr::pull(cmnty_hlth_serv_area_code) %>%
      unique() %>%
      length()

    # render line plot using ggplot2 and plotly
    output$lineplot <- renderPlotly({
      plt <- ggplot(
        lha_df(),
        aes(x = year, y = value, group = cmnty_hlth_serv_area_code, color = chsa_name)
      ) +
        geom_line(data = lha_df() %>% filter(cmnty_hlth_serv_area_code == click$id)) +
        {
          if (n_chsa > 1) {
            geom_line(
              data = lha_df() %>% filter(cmnty_hlth_serv_area_code != click$id),
              alpha = 0.5
            )
          }
        } + # create layer conditionally according to https://stackoverflow.com/a/37397907
        lims(y = c("1", "2", "3", "4", "5")) +
        labs(x = "Year", y = "Quintile", color = "") +
        theme_grey(base_size = 12)

      ggplotly(plt) %>%
        style(hoverinfo = "none") %>%
        config(displayModeBar = FALSE) # remove the toolbar according to https://plotly-r.com/control-modebar.html#remove-the-entire-modebar
    })
  })
}

shinyApp(ui, server)
