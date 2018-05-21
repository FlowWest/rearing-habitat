tagList(
  shinyjs::useShinyjs(),
  fluidPage(
    title = 'CVPIA Carrying Capacity',
    theme = shinythemes::shinytheme('paper'),
    tags$header(includeCSS('styles.css')),
    rearingUI('app')
  )
)