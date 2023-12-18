data(bouyssie)

ui <- fluidPage(
  tagList(
    mod_Pirat_ui('pirat')
  ))

server <- function(input, output, session) {


  res <- mod_Pirat_server('pirat',
                         obj = reactive({bouyssie}),
                         reset = reactive({NULL}))

  
   observeEvent(res()$trigger, {
  #   #print(rv$res$dataOut()$value)
  #   #print(rv$res$dataOut()$widgets)
    # browser()
     print(res()$value$data.imputed)
     print(res()$value$params)
   })
}

shinyApp(ui=ui, server=server)