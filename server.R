shinyServer(function(input, output) {
  callModule(instreamServer, 'app')
  callModule(fp, 'one')
})