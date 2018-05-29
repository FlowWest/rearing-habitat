fp_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12,
             tags$h2('Floodplain Habitat Calculator'))
    ),
    fluidRow(
      column(width = 12, class = 'col-md-3',
             h4('Inputs'),
             selectInput(ns('watershed'), 'Stream', 
                         choices = watersheds$watershed, width = '210px'),
             radioButtons(ns('species'), 'Species', 
                          choiceNames = c('Fall Run', 'Spring Run', 'Steelhead'), 
                          choiceValues = c('fr', 'sr', 'st')),
             numericInput(ns('flow'), 'Flow (cfs)', 1000, width = '210px'),
             numericInput(ns('suit'), 'Suitability Factor', .27, 0, 1, .01, width = '210px'),
             htmlTemplate('templates/fp_modeling_details.html', 
                          mod_details = textOutput(ns('mod_details')),
                          mod_link = uiOutput(ns('ref_link')))),
      column(width = 12, class = 'col-md-5',
             h4('Suitable Floodplain Area (acres)'),
             div(id = 'calc_out', h5(textOutput(ns('fp_acres')))),
             hr(),
             h5('Details'),
             tabsetPanel(
               tabPanel('Hydrology', 
                        h6('CALSIM modeled flow 1980-1999'),
                        plotlyOutput(ns('flow_summary'))),
               tabPanel('Floodplain Activation', 
                        h6('CALSIM modeled flow 1980-1999'),
                        plotlyOutput(ns('habitat_time'))),
               tabPanel('Flow to Floodplain Area', 
                        plotlyOutput(ns('flow2area_plt')))
             )),
      column(width = 12, class = 'col-md-4',
             h4('Habitat Extents'),
             leafletOutput(ns('extents'), height = '500px'))
    )
  )
}

fp <- function(input, output, session) {
  ns <- session$ns
  
  output$fp_acres <- renderText(round(floodplain_acres() * input$suit, 2))

  output$mod_details <- renderText(print_model_details(input$watershed, input$species))
  
  output$ref_link <- renderUI(
    tags$a('Model Reference', href = .metadata[.metadata$watershed == input$watershed, 'model_reference'], target = '_blank')
  )
  
  no_spring_run <- reactive(is.na(pull(filter(.metadata, watershed == input$watershed), SR_rearing_length_mi)))
  
  watershed_flows <- reactive({
    if (input$watershed == 'Lower-mid Sacramento River') {
      flows %>% 
        filter(watershed == 'Lower-mid Sacramento River2') %>%
        mutate(month = factor(month(date), labels = month.abb))
    } else {
      flows %>% 
        filter(watershed == input$watershed) %>% 
        mutate(month = factor(month(date), labels = month.abb))
    }
    
  })
  
  flow2area <- reactive({
   
    ws_fp <- paste0(gsub(' |-', '_', tolower(input$watershed)), '_floodplain')
    df <- do.call(`::`, list(pkg = 'cvpiaHabitat', name = ws_fp))
    
    if (input$species == 'sr' & no_spring_run()) {
      data.frame()
    } else {
      switch(input$species,
             'fr' = select(df, flow_cfs, floodplain_acres = FR_floodplain_acres),
             'sr' = select(df, flow_cfs, floodplain_acres = SR_floodplain_acres),
             'st' = select(df, flow_cfs, floodplain_acres = ST_floodplain_acres))
    }
    
  })
  
  floodplain_acres <- reactive({
    validate(need(nrow(flow2area()) > 0, 'No spring run'))
    f <- approxfun(flow2area()$flow_cfs, flow2area()$floodplain_acres, rule = 2)
    f(input$flow)
  })
  
  output$area_curve <- renderTable({
    validate(need(nrow(flow2area()) > 0, 'No spring run'))
    
    flow2area()
  })

  flow2areaYtitle <- reactive({
    if (input$watershed %in% c('Upper Sacramento River', 'Upper-mid Sacramento River',
                               'Lower-mid Sacramento River', 'Lower Sacramento River')) {
      'suitable floodplain area (acres)'
    } else {
      'total floodplain area (acres)'
    }
  })
  
  output$flow2area_plt <- renderPlotly({
    validate(need(nrow(flow2area()) > 0, 'No spring run'))
    
    flow2area() %>%
      plot_ly(x = ~flow_cfs, y = ~floodplain_acres, type = 'scatter', mode = 'lines',
              text = ~paste(format(round(floodplain_acres, 1), big.mark = ',', trim = FALSE), 'acres'), 
              hoverinfo = 'text') %>% 
      layout(yaxis = list(title = flow2areaYtitle()), 
             xaxis = list(title = 'flow (cfs)')) %>% 
      config(displayModeBar = FALSE)
  })
  
  output$habitat_time <- renderPlotly({
    
    watershed_flows() %>%
      mutate(fp_hab = cvpiaHabitat::set_floodplain_habitat(input$watershed, input$species, flow_cfs) * 0.000247105 * input$suit) %>% 
      plot_ly(x = ~date, y = ~fp_hab, hoverinfo = 'text',
              text = ~paste(format(date, '%b %Y'), ':', format(round(fp_hab, 1), big.mark = ',', trim = FALSE), 'acres')) %>% 
      add_bars() %>% 
      layout(yaxis = list(title = 'suitable floodplain area (acres)')) %>% 
      config(displayModeBar = FALSE)
    
  })
  
  output$flow_summary <- renderPlotly({
    watershed_flows() %>% 
      plot_ly(x = ~month, y = ~flow_cfs, type = 'box') %>% 
      layout(boxmode = 'group') %>% 
      layout(yaxis = list(title = 'flow (cfs)')) %>% 
      config(displayModeBar = FALSE)
  })
  
  observeEvent(input$watershed, {
    shinyjs::reset('flow')
  })
  
  observe({
    if (input$watershed %in% c('Upper Sacramento River', 'Upper-mid Sacramento River',
                               'Lower-mid Sacramento River', 'Lower Sacramento River')) {
      shinyjs::hide('suit')
      shiny::updateNumericInput(session = session, inputId = 'suit', value = 1)
    } else {
      shinyjs::show('suit')
      shiny::updateNumericInput(session = session, inputId = 'suit', value = .27)
    }
  })
  
  output$extents <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Map") %>% 
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addPolylines(data = str, group = 'Steelhead Rearing', label = ~River,
                   color = '#5e3c99', opacity = .8, weight = 3) %>%
      addPolylines(data = sts, group = 'Steelhead Spawning', label = ~River,
                   color = '#e66101', opacity = 1, weight = 3) %>%
      addPolylines(data = spr, group = 'Spring Run Rearing', label = ~River,
                   color = '#5e3c99', opacity = .8, weight = 3) %>% 
      addPolylines(data = sps, group = 'Spring Run Spawning', label = ~River,
                   color = '#e66101', opacity = 1, weight = 3) %>% 
      addPolylines(data = fr, group = 'Fall Run Rearing', label = ~River,
                   color = '#5e3c99', opacity = .8, weight = 3) %>% 
      addPolylines(data = fs, group = 'Fall Run Spawning', label = ~River,
                   color = '#e66101', opacity = 1, weight = 3) %>% 
      addLayersControl(
        baseGroups = c("Map", "Satellite"),
        overlayGroups = c('Fall Run Rearing', 'Fall Run Spawning', 
                          'Spring Run Rearing','Spring Run Spawning', 
                          'Steelhead Rearing', 'Steelhead Spawning')) %>% 
      hideGroup('Spring Run Spawning') %>%
      hideGroup('Spring Run Rearing') %>%
      hideGroup('Steelhead Spawning') %>%
      hideGroup('Steelhead Rearing') %>%
      addLegend(colors = c('#5e3c99', '#e66101'), labels = c('rearing', 'spawning'),
                position = 'topleft', title = 'Habitat Type') %>% 
      setView(lat = 40.61158, lng = -122.4461, zoom = 10)
  })
  
  center <- reactive({
    subset(fr, River == input$watershed) %>% 
      fortify()
  })
  
  proxy <- leafletProxy('extents')
  
  observe({
    proxy %>% 
      setView(lng = center()$long[1], lat = center()$lat[1], zoom = 10)
  })
  
  observeEvent(input$species, {
    if (input$species == 'fr') {
            proxy %>%
              showGroup('Fall Run Rearing') %>%
              showGroup('Fall Run Spawning') %>%
              hideGroup('Spring Run Spawning') %>%
              hideGroup('Spring Run Rearing') %>%
              hideGroup('Steelhead Spawning') %>%
              hideGroup('Steelhead Rearing')
          } else if (input$species == 'sr') {
            proxy %>%
              showGroup('Spring Run Rearing') %>%
              showGroup('Spring Run Spawning') %>%
              hideGroup('Fall Run Spawning') %>%
              hideGroup('Fall Run Rearing') %>%
              hideGroup('Steelhead Spawning') %>%
              hideGroup('Steelhead Rearing')
          } else if (input$species == 'st'){
            proxy %>%
              showGroup('Steelhead Rearing') %>%
              showGroup('Steelhead Spawning') %>%
              hideGroup('Spring Run Spawning') %>%
              hideGroup('Spring Run Rearing') %>%
              hideGroup('Fall Spawning') %>%
              hideGroup('Fall Rearing')
          }
  })

}