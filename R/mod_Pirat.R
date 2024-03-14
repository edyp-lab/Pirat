
#' @title Pirat shiny app
#' 
#' @description 
#' ## To be customized ##
#' 
#' @param id The id of the module
#' @param obj An instance of the class `SummarizedExperiment`.
#' @param reset A boolean which indicates whether to reset the widgets or not.
#' @param verbose A boolean (FALSE as default) which indicates whether to 
#' display more details on the process
#' 
#' @name mod_Pirat
#' 
#' @examples
#' if(interactive()){
#' data(subbouyssie)
#' 
#' # Builds the instance of `SummarizedExperiment`
#' obj.se <- pirat2SE(subbouyssie$peptides_ab, subbouyssie$adj, 
#' subbouyssie$mask_prot_diff, subbouyssie$mask_pep_diff )
#' 
#' #Launch the app
#' app <- mod_Pirat(obj.se)
#' shiny::runApp(app)
#' 
#' }
#' 
#' 
#' @return A shiny app
#'
NULL

#' @rdname mod_Pirat
#' @return A shiny app
#' @importFrom shiny NS tagList uiOutput actionButton uiOutput plotOutput
#' @export
#' 
mod_Pirat_ui <- function(id){
  
  ns <- NS(id)
  
  tagList(
    uiOutput(ns('extension_ui')),
    shiny::actionButton(ns("run"), "Run Pirat"),
    uiOutput(ns('valid_btn_ui')),
    plotOutput(ns('correlation_plot_UI'))
  )
}


#' @rdname mod_Pirat
#' 
#' @importFrom shiny moduleServer reactiveVal reactiveValues renderUI 
#' selectInput observeEvent withProgress setProgress req reactive renderPlot
#' @importFrom SummarizedExperiment assay
#' @importFrom S4Vectors metadata
#' @export
#' @return A shiny app
#' 
mod_Pirat_server <- function(id,
                             obj = reactive({NULL}),
                             reset = reactive({NULL}),
                             verbose = FALSE
                             ) {

  # Define default selected values for widgets
  # This is only for simple workflows
  # This list must contain one item per widget (defined in the ui() function
  # The name of each item is the same as in the ui without the suffix '_ui')
  widgets.default.values <- list(
    extension = 'base'
   # widget2 = NULL,
   # widget3 = NULL
  )
  
  moduleServer(id,function(input, output, session) {
    ns <- session$ns
    
   maxval <- reactiveVal(0)
   
   data <- reactiveVal(NULL)
   
    dataOut <- reactiveValues(
      trigger = NULL,
      value = NULL,
      widgets = NULL
    )
    
    observe({
      req(obj())
      
      if(!inherits(obj(), 'SummarizedExperiment')){
        return(NULL)
      }
      
      data(list(
        peptides_ab = t(assay(obj())),
        adj = metadata(obj())$adj,
        mask_prot_diff = metadata(obj())$mask_prot_diff,
        mask_pep_diff = metadata(obj())$mask_pep_diff
      )
      )
      
    })
    
    output$extension_ui <- renderUI({
      selectInput(ns('extension'), 'Algorithm',
                  choices = c('base' = 'base', 
                              '2 pg' = '2',
                              'S' = 'S',
                              'T' = 'T'),
                  selected = widgets.default.values$extension,
                  width = '150px')
    })
    
    
    GetNbPg <- reactive({5})
    
    shiny::observeEvent(input$run, {
      
      maxval(GetNbPg())
      shiny::withProgress(
        withCallingHandlers(
         # out <- long_run_op(num_iter=10),
           dataOut$value <- my_pipeline_llkimpute(data(),
                                               extension = input$extension,
                                               verbose = verbose),
           dataOut$trigger <- as.numeric(Sys.time()),
           dataOut$widgets <- list(extension = input$extension),
           
           
          message = function(m){
            msg <- unlist(strsplit(m$message, split=' '))
            if(msg[1] == 'Peptide_group') {
              val <- as.numeric(msg[2])
              shiny::setProgress(value = val, message = m$message)
            }
          }
        ),
        max = maxval()
      )
    })
    
    
    output$correlation_plot_UI <- renderPlot({
      req(data())
      plot_pep_correlations(pep.data = data())
    })
     
    # observeEvent(input$valid_btn, {
    #   
    #   dataOut$value <- my_pipeline_llkimpute(bouyssie,  extension = input$extension)
    #   
    #   print('test')
    #   dataOut$trigger <- as.numeric(Sys.time())
    #   dataOut$widgets <- list(extension = input$extension)
    # })
    
    
    reactive({dataOut})
  })
  
}



#' @rdname mod_Pirat
#' @export
#' @importFrom shiny reactive shinyApp fluidPage
#' @return A shiny app
#' 
mod_Pirat <- function(obj){
  ui <- fluidPage(
    mod_Pirat_ui('pirat')
  )
  
  server <- function(input, output, session) {
    mod_Pirat_server('pirat', obj = reactive({obj}))
  }
  
  app <- shiny::shinyApp(ui, server)
}
