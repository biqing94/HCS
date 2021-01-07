source("HomecageAnalysisFunctions.R")
using("shiny", "shinyFiles", "shinybusy", "readxl")

# Define UI 
ui <- fluidPage(

    # Application title
    titlePanel("HomeCageScan"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            shinyFilesButton("masterfile", label="Master file", 
                             title="Choose master file describing data files", multiple=FALSE),
            verbatimTextOutput("mfname", placeholder = TRUE), 
            hr(),
            shinyDirButton("dir", label="Data directory", 
                           title="Choose the directory containing all your HCS data"),
            verbatimTextOutput("dirname", placeholder = TRUE) ,
            hr(),
            actionButton("loaddata", "Load data"),
            hr(),
            radioButtons("format", label = "Output format", choices=c("Rectangular", "for Prism"),
                                                               inline = TRUE),
            shinySaveButton("savedata", label = "Export data",
                            title = "Save exported data as...", filetype="xlsx"),
           ),

        # Show data table of master file
        mainPanel(
          tabsetPanel(id="tabset",
            tabPanel("Master file", dataTableOutput("masterTable")),
            tabPanel("Summary data", 
                     radioButtons("sumfrac", label = "Value type", choices=c("Sum", "Fraction"),
                                  inline = TRUE),
                     dataTableOutput("summaryTable"))
          )
        )
    )
)

# Define server logic 
server <- function(input, output, session) {
    volumes= getVolumes()()
    values <- reactiveValues(
        datapath = "",
        mastername = "",
        sum_data = NULL
    )
    
    
    # load master file
    shinyFileChoose(input, 'masterfile', roots=c(volumes, datadir=values$datapath), filetypes=c('', 'xlsx'))
    observeEvent(
        ignoreNULL = TRUE,
        eventExpr = {input$masterfile},
        handlerExpr = {
             values$mastername <- parseFilePaths(roots=volumes, input$masterfile)$datapath
     })
      
    output$mfname <- renderText({
        values$mastername
    })    
    
    
   master <- reactive({
       req(values$mastername)
       read_excel(values$mastername)

   })

    # select data directory
    shinyDirChoose(input, 'dir', roots=volumes)

    output$dirname <- renderText({
        values$datapath
    })    
    
    observeEvent(
        ignoreNULL = TRUE,
        eventExpr = {input$dir},
        handlerExpr = {
           values$datapath <- parseDirPath(volumes, input$dir)
    })

    # display master table
    output$masterTable<- renderDataTable(
        master(),
        options = list(pageLength = 10)
    )
    
    # load and summarize data
    summary_data <- reactive({
      req(values$sum_data)
      switch(input$sumfrac,
             "Sum" = values$sum_data$summary_all,
             "Fraction" = values$sum_data$fraction_all)
    })
    
    
     observeEvent(input$loaddata, {
       updateTabsetPanel(session, "tabset",
                        selected = "Summary data")
       show_modal_spinner()
       values$sum_data <- 
        process_metafile(metaData = master(),
                       datapath = values$datapath,
                       aggregate_by = NULL, 
                       args_list = list(start.value=59.99, 
                                        rename_variable = NULL, 
                                        breaks=NULL, 
                                        labels=NULL, 
                                        combine=NULL))
       remove_modal_spinner()
    })
     
     # display summarized data
    output$summaryTable<- renderDataTable(
      summary_data(), 
      options = list(pageLength=10)
    )
    
    # export data
    observeEvent(
      ignoreNULL = TRUE,
      eventExpr = {input$savedata},              
      handlerExpr = {
        shinyFileSave(input, "savedata", roots=volumes, session=session)
        outfile <- parseSavePath(volumes, input$savedata)$datapath
        req(outfile)
        
        if (input$format == "Rectangular"){
          outlist <- list(summary_data()) 
          names(outlist) <- input$sumfrac
          write.xlsx(outlist, outfile, col.names = TRUE, row.names = FALSE, append = FALSE, showNA = FALSE) 
        } else if (input$format == "for Prism"){
          export_for_Prism(summary_data(), filter = NULL, outfilename=outfile)
        }
        showNotification(paste("Data saved to", outfile), type="message")
      })
      }

# Run the application 
shinyApp(ui = ui, server = server)
