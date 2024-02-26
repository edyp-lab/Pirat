
ui <- fluidPage(
  tagList(
    mod_Pirat_ui('pirat')
  ))

server <- function(input, output, session) {

  
  data(Exp1_R25_pept, package = 'DAPARdata')
  data(subbouyssie, package = 'Pirat')
  
  obj <- subbouyssie
  res <- mod_Pirat_server('pirat',
                         obj = reactive({obj}),
                         reset = reactive({NULL}),
                         verbose = FALSE)

  
   observeEvent(res()$trigger, {
  #   #print(rv$res$dataOut()$value)
  #   #print(rv$res$dataOut()$widgets)
    # browser()
     print(res()$value$data.imputed)
     print(res()$value$params)
   })
}

shinyApp(ui=ui, server=server)