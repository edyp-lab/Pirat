
#' @title ## To be customized ##
#' 
#' @description 
#' ## To be customized ##
#' 
#' @name extra_module
#'
#' @examples 
#' if (interactive()){
#' data(ft_na)
#' ui <- foo_ui('query')
#' 
#' server <- function(input, output, session) {
#'   
#'   rv <- reactiveValues(
#'     res = NULL
#'   )
#'   ll.tags <- c('None' = 'None', 
#'   qMetadata.def(typeDataset(ft_na[[1]]))$node)
#'   
#'   rv$res <- foo_server('query', 
#'   reset = reactive({NULL}),
#'   is.enabled = reactive({NULL})
#'   )
#'   
#'   observeEvent(rv$res$dataOut()$value, ignoreNULL = TRUE, ignoreInit = TRUE, {
#'     print(rv$res$dataOut()$value)
#'     print(rv$res$dataOut()$widgets)
#'   })
#' }
#' 
#' shinyApp(ui=ui, server=server)
#' }
NULL


long_run_op <- function(num_iter) {
  for (x in 1:num_iter) {
    message("working... [peptide group=",x,"]")
    Sys.sleep(0.5)
  }
  message("calculation finished.")
  return(rnorm(num_iter))
}


#' @param id xxx
#' 
#' @rdname extra_module
#' 
mod_Pirat_ui <- function(id){
  
  ns <- NS(id)
  
  # This is an example of what it is expected
  # You must 
  # All the widgets must be defined in the server part. Here, 
  # one have only calls with uiOutput() function.
  
  # Each line correspond to one widget and the id is composed of
  # the name of the widget followed by '_ui'
  tagList(
    uiOutput(ns('extension_ui')),
    #shiny::numericInput(ns('num_iter'), 'Iterations', 10, min=2, max=20),
    shiny::actionButton(ns("run"), "Run"),
    #uiOutput(ns('widget2_ui')),
    #uiOutput(ns('widget3_ui')),
    uiOutput(ns('valid_btn_ui'))
  )
}


mod_Pirat_server <- function(id,
                             obj = reactive({NULL}),
                             reset = reactive({NULL})
                             ) {
  
  
  
  # Define default selected values for widgets
  # This is only for simple workflows
  # This list must contain one item per widget (defined in the ui() function
  # The name of each item is the same as in the ui minus the suffix '_ui')
  widgets.default.values <- list(
    extension = 'base'
   # widget2 = NULL,
   # widget3 = NULL
  )
  
  moduleServer(id,function(input, output, session) {
    ns <- session$ns
    
   #rv <- reactiveValues(dataOut = NULL)
    dataOut <- reactiveValues(
      trigger = NULL,
      value = NULL,
      widgets = NULL
    )
    
    output$extension_ui <- renderUI({
      widget <- selectInput(ns('extension'), 'Algorithm', 
                            choices = c('base' = 'base', 
                                        '2 pg' = '2',
                                        'S' = 'S',
                                        'T' = 'T'),
                            selected = widgets.default.values$extension)
    })
    
    shiny::observeEvent(input$run, {
      shiny::withProgress(
        withCallingHandlers(
          #out <- long_run_op(num_iter=input$num_iter),
           out <- pipeline_llkimpute(bouyssie),
           
          message=function(m) if(grepl("\\[peptide group=[0-9]+\\]", m$message)) {
            val <- as.numeric(gsub(".*\\[peptide group=([0-9]+)\\].*$", "\\1", m$message))
            cat ('val = ', val)
            cat("Stacksize: ", session$progressStack$size(),"\n")
            shiny::setProgress(value=val, message = paste0(val, '/'))
          }
        ),
        #message=paste0("working...", realtimevalue()),
        max=5,
        value=0
      )
    })
     
    # observeEvent(input$valid_btn, {
    #   
    #   dataOut$value <- pipeline_llkimpute(bouyssie,  extension = input$extension)
    #   
    #   print('test')
    #   dataOut$trigger <- as.numeric(Sys.time())
    #   dataOut$widgets <- list(extension = input$extension)
    # })
    
    
    reactive({dataOut})
  })
  
}


