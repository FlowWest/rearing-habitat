rearingUI <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(width = 12, class = 'col-md-3',
             selectInput(ns('stream_reach'), 'Reach', 
                         choices = spawning_locations,
                         width = '220px'),
             tags$h4('Habitat Available (Acres)'),
             radioButtons(inputId = ns('ic_fp'), label = NULL, choices = c('in channel', 'floodplain', 'both')),
             tags$div(style = 'display:inline-block', uiOutput(ns('spawn_hab_input'))),
             tags$div(style = 'display:inline-block', uiOutput(ns('fry_hab_input'))),
             uiOutput(ns('num_adults'))
      ),
      column(width = 12, class = 'col-md-9',
             fluidRow(
               column(width = 12, class = 'col-md-6',
                      tags$img(src = 'spawn.png'),
                      tags$h4('Spawners'),
                      tags$table(
                        tags$tr(
                          tags$td(tags$h5('Total')),
                          tags$td('1000', align = "right")
                        ),
                        tags$tr(
                          tags$td(tags$h5('Available Habitat')),
                          tags$td('68 acres', align = "right")
                        ),
                        tags$tr(
                          tags$td(tags$h5('Needed Habitat')),
                          tags$td('2 acres', align = "right")
                        ),
                        tags$tr(
                          tags$td(tags$h5('Habitat Limited')),
                          tags$td('No', align = "right")
                        )
                      )),
               column(width = 12, class = 'col-md-6',
                      tags$img(src = 'fry.png', style = 'padding-top:17px;'),
                      tags$h4('Fry'),
                      tags$table(
                        tags$tr(
                          tags$td(tags$h5('Total')),
                          tags$td('1000', align = "right")
                        ),
                        tags$tr(
                          tags$td(tags$h5('Available Habitat')),
                          tags$td('68 acres', align = "right")
                        ),
                        tags$tr(
                          tags$td(tags$h5('Needed Habitat')),
                          tags$td('2 acres', align = "right")
                        ),
                        tags$tr(
                          tags$td(tags$h5('Habitat Limited')),
                          tags$td('No', align = "right")
                        )
                      ))
             ),
             fluidRow(
               column(width = 12)
             ))
    )
    
  )
}

rearingServer <- function(input, output, session) {
  
  ns <- session$ns
  
  fry_habitat <- reactive({
    2
  })
  
  spawn_habitat <- reactive({
    2.444444
  })
  
  spawners <- reactive({
    40
  })
  
  output$spawn_hab_input <- renderUI({
    textInput(ns('spawn'), 'Spawning', value = round(spawn_habitat(), 2), width = '80px')
  })

    
  output$fry_hab_input <- renderUI({
    textInput(ns('fry'), 'Fry', value = round(fry_habitat(), 2), width = '80px')
  })
  
  output$num_adults <- renderUI({
    textInput(inputId = ns('adults'), label = 'Adult Escapement', 
              value = ceiling(spawners()), width = '160px')
  })

  
}


