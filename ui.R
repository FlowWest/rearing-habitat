tagList(
  shinyjs::useShinyjs(),
  navbarPage(
    title = 'Central Valley Habitat Calculator',
    collapsible = TRUE,
    theme = shinytheme('flatly'),
    header = includeCSS('styles.css'),
    tabPanel('Instream',
             instreamUI('app')),
    tabPanel('Floodplain',
             fp_UI('one')),
    tabPanel('About',
             fluidRow(id='about_pg',
                      column(width = 12, style = 'padding: 0;',
                             div(id = 'about_txt_box',
                                 div(id = 'about_txt',
                                     tags$p('This application was developed using the 
                             best available data and modeling gathered through the
                             CVPIA Structured Decision Making process. If you have
                             any more up-to-date habitat modeling results or data to contribute
                             please contact ', 
                                            tags$a('Mark Tompkins ', href="mailto:mtompkins@flowwest.com"),
                                            'or ',
                                            tags$a('Sadie Gill.', href="mailto:sgill@flowwest.com")),
                                     tags$p('Additionally, the modeling data that powers this application is available
                             for use in the R package, ', 
                                            tags$a('cvpiaHabitat', href='https://flowwest.github.io/cvpiaHabitat/', target='_blank'),
                                            ', developed by FlowWest.'),
                                     tags$hr(),
                                     div(id = 'logo',
                                         tags$a(tags$img(src = 'TransLogoTreb.png', width = 150, style = "display: block; margin: 0 auto;"),
                                                href = 'http://www.flowwest.com/', target = '_blank'),
                                         tags$h6('App created and maintained by',
                                                 tags$a('Sadie Gill', href = 'https://github.com/thesadie', target = '_blank'),
                                                 style = "text-align: center; color: #ffffff;"))))))
    )
  )
)