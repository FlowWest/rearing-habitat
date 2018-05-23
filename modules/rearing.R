rearingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(style = 'padding-top: 30px',
      column(width = 12, class = 'col-md-3',
             selectInput(ns('stream_reach'), 'Reach', 
                         choices = spawning_locations,
                         width = '220px'),
             tags$h5('Adult Escapement*'),
             uiOutput(ns('num_adults')),
             tags$h4('Habitat Available'),
             tags$h5('Spawning Habitat'),
             tags$div(style = 'display:inline-block', uiOutput(ns('spawn_hab_input'))),
             tags$div(style = 'display:inline-block', tags$p('acres')),
             uiOutput(ns('spawn_hab_flow')),
             tags$h5('Fry Rearing Habitat'),
             tags$div(style = 'display:inline-block', uiOutput(ns('fry_hab_input'))),
             tags$div(style = 'display:inline-block', tags$p('acres')),
             uiOutput(ns('fry_hab_flow')),
             checkboxInput(ns('use_flow'), 'Adjust Habitat Values by Flow (cfs)'),
             tags$h6('*default value is the median escapement of 2001-2015')
      ),
      column(width = 12, class = 'col-md-9',
             fluidRow(
               column(width = 12, class = 'col-md-6',
                      tags$div(class = 'fish_box',
                               tags$img(src = 'spawn.png'),
                               tags$h4('Spawners'),
                               tags$div(class = 'table-div',
                                        tags$table(
                                          tags$tr(
                                            tags$td(tags$h5('Total')),
                                            tags$td(tags$h5(textOutput(ns('num_spawners'))), align = "right")
                                          ),
                                          tags$tr(
                                            tags$td(tags$h5('Available Habitat')),
                                            tags$td(tags$h5(textOutput(ns('spawn_hab_exist'))), align = "right")
                                          ),
                                          tags$tr(
                                            tags$td(tags$h5('Needed Habitat')),
                                            tags$td(tags$h5(textOutput(ns('spawn_hab_need'))), align = "right")
                                          ),
                                          tags$tr(
                                            tags$td(tags$h5('Habitat Limited')),
                                            tags$td(tags$h5(textOutput(ns('spawn_hab_limited'))), align = "right")
                                          )
                                        )))),
               column(width = 12, class = 'col-md-6',
                      tags$div(class = 'fish_box',
                               tags$img(src = 'fry.png', style = 'padding-top:17px;'),
                               tags$h4('Fry'),
                               tags$div(class = 'table-div',
                                        tags$table(
                                          tags$tr(
                                            tags$td(tags$h5('Total')),
                                            tags$td(tags$h5(textOutput(ns('num_fry'))), align = "right")
                                          ),
                                          tags$tr(
                                            tags$td(tags$h5('Available Habitat')),
                                            tags$td(tags$h5(textOutput(ns('fry_hab_exist'))), align = "right")
                                          ),
                                          tags$tr(
                                            tags$td(tags$h5('Needed Habitat')),
                                            tags$td(tags$h5(textOutput(ns('fry_hab_need'))), align = "right")
                                          ),
                                          tags$tr(
                                            tags$td(tags$h5('Habitat Limited')),
                                            tags$td(tags$h5(textOutput(ns('fry_hab_limited'))), align = "right")
                                          )
                                        ))))
             ),
             fluidRow(
               column(width = 12, class = 'col-md-10', 
                      tabsetPanel(
                        tabPanel('Escapement', 
                                 tags$h5('Grand Tab Escapement - Fall', style = 'width: 400px;'),
                                 plotlyOutput(ns('grand_plot'))),
                        tabPanel('Instream Habitat', plotlyOutput(ns('wua')))
                      ))
             ))
    )
    
  )
}

rearingServer <- function(input, output, session) {
  
  ns <- session$ns
  
  med_fry_habitat <- reactive({
    median_flow_hab %>% 
      filter(watershed == input$stream_reach) %>% 
      pull(fry_hab)
  })
  
  med_spawn_habitat <- reactive({
    median_flow_hab %>% 
      filter(watershed == input$stream_reach) %>% 
      pull(spawn_hab)
  })
  
  default_spawners <- reactive({
    grandtab %>% 
      filter(watershed == input$stream_reach, type == 'Natural', year > 2000) %>% 
      pull(count) %>% 
      median()
  })
  
  spawners <- reactive({
    req(input$adults)
    as.numeric(input$adults)
  })
  
  
  output$num_spawners <- renderText({pretty_num(spawners())})
  
  
  fry <- reactive({
    req(input$spawn)
    if (spawn_need() > as.numeric(input$spawn)) {
      as.numeric(input$spawn) * 4046.86 / 12.4 * 5500 * .485
    } else {
      spawners() * .5 * 5500 * .485
    }
  })
  
  output$num_fry <- renderText(pretty_num(fry()))
  
  output$spawn_hab_input <- renderUI({
    if (input$use_flow) {
      hab <- square_meters_to_acres(set_spawning_habitat(watershed = input$stream_reach, 
                                                         species = 'fr', 
                                                         flow = input$spawn, month = 1))
      textInput(ns('spawn'), label = NULL, value = round(hab, 2), width = '80px') 
    } else {
      textInput(ns('spawn'), label = NULL, value = round(med_spawn_habitat(), 2), width = '80px')  
    }
  })
  
  
  output$fry_hab_input <- renderUI({
    if (input$use_flow) {
      hab <- square_meters_to_acres(set_instream_habitat(watershed = input$stream_reach, 
                                                         species = 'fr', life_stage = 'fry', 
                                                         flow = input$fry_flow, month = 1))
      textInput(ns('fry'), label = NULL, value = round(hab, 2), width = '80px')
    } else {
      textInput(ns('fry'), label = NULL, value = round(med_fry_habitat(), 2), width = '80px') 
    }
  })
  
  
  output$fry_hab_flow <- renderUI({
    numericInput(inputId = ns('fry_flow'), label = 'Flow (cfs)', value = 0, width = '80px')
  })
  
  output$spawn_hab_flow <- renderUI({
    numericInput(inputId = ns('spawn_flow'), label = 'Flow (cfs)', value = 0, width = '80px')
  })
  
  output$num_adults <- renderUI({
    textInput(inputId = ns('adults'), label = NULL, 
              value = ceiling(default_spawners()), width = '160px')
  })
  
  fry_need <- reactive(fry() * fry_territory / 4046.86)
  
  spawn_need <- reactive(spawners() * .5 * 12.4 / 4046.86)
  
  output$fry_hab_need <- renderText(paste(as.character(pretty_num(fry_need(), 2)), 'acres'))
  output$spawn_hab_need <- renderText(paste(as.character(pretty_num(spawn_need(), 2)), 'acres'))
  
  output$spawn_hab_exist <- renderText(paste(as.character(input$spawn), 'acres'))
  output$fry_hab_exist <- renderText(paste(as.character(input$fry), 'acres'))
  
  output$spawn_hab_limited <- renderText(ifelse(as.numeric(input$spawn) - spawn_need() >= 0, 'No', 'Yes'))
  output$fry_hab_limited <- renderText(ifelse(as.numeric(input$fry) - fry_need() >= 0, 'No', 'Yes'))
  
  gt <- reactive(filter(grandtab, watershed == input$stream_reach))
  dbd <- reactive(filter(doubling, watershed == input$stream_reach))
  
  output$grand_plot <- renderPlotly({
    gt() %>%
      plot_ly(x = ~year, y = ~count, type = 'bar', color = ~type,
              colors = c('#636363', '#252525'), hoverinfo = 'text', 
              text = ~paste(type, '<br>', 'Year', year, '<br>Count', format(count, big.mark = ',', trim = FALSE))) %>% 
      add_trace(data = dbd(), x = c(1952,2015), y = ~doubling_goal, type = 'scatter', mode = 'lines+markers',
                line = list(color = 'rgb(0, 0, 0)', dash = 'dash'), inherit = FALSE,
                hoverinfo = 'text', text = ~paste('Doubling Goal', format(doubling_goal, big.mark = ',', trim = FALSE))) %>%
      layout(yaxis = list(title = 'count'), showlegend = FALSE, barmode = 'stack') %>% 
      config(displayModeBar = FALSE)
  })
  
  
  output$wua <- renderPlotly({
    flow_to_acres %>% 
      filter(watershed == input$stream_reach) %>% 
      plot_ly(x = ~flow, y = ~spawn, type = 'scatter', mode = 'lines',
              name = 'spawning', hoverinfo = 'text',
              text = ~paste(pretty_num(flow), 'cfs','<br>', pretty_num(spawn), 'acres')) %>% 
      add_lines(y = ~fry, name = 'fry', 
                text = ~paste(pretty_num(flow), 'cfs','<br>', pretty_num(fry), 'acres')) %>% 
      add_lines(y = ~juv, name = 'juvenile', 
                text = ~paste(pretty_num(flow), 'cfs','<br>', pretty_num(juv), 'acres')) %>% 
      layout(yaxis = list(title = 'habitat (acres)', rangemode = 'tozero'),
             xaxis = list(title = 'flow (cfs)')) %>% 
      config(displayModeBar = FALSE)
  })
  
  observe({
    if (input$use_flow) {
      shinyjs::enable("fry_hab_flow")
    } else {
      shinyjs::disable("fry_hab_flow")
    }
  })
}


