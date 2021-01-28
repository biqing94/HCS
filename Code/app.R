source("HomecageAnalysisFunctions.R")
using("shiny", "shinyFiles", "shinybusy", "readxl")

# Define UI 
ui <- fluidPage(
    # Unbold text input labels bin breaks 1, bin breaks 2
    tags$head(tags$style(HTML("label {font-weight: 500}"))),

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
            shinyFilesButton("behaviorsfile", label="Combine behaviors", 
                             title="Choose file to combine behaviors", multiple=FALSE),
            splitLayout(cellWidths = c("80%", "20%"), 
                        verbatimTextOutput("bname", placeholder = TRUE), 
                        actionButton("reset_behaviorsfile", label = "Reset")),
            splitLayout(cellWidths = c("30%", "30%"),
                        textInput("break1", label="Bin break 1",value="1",width="60px"), 
                        textInput("break2", label="Bin break 2",value="630",width="60px")),
            hr(),
            actionButton("loaddata", "Load data"),
            hr(),
            radioButtons("format", label = "Output format", choices=c("Rectangular", "for Prism"),
                                                               inline = TRUE),
            shinySaveButton("savedata", label = "Export data",
                            title = "Save exported data as...", filetype="xlsx")
           ),

        # Show data table of master file
        mainPanel(
          tabsetPanel(id="tabset",
            tabPanel("Master file", dataTableOutput("masterTable")),
            tabPanel("Summary data", 
                     radioButtons("sumfrac", label = "Value type", choices=c("Sum", "Fraction"),
                                  inline = TRUE),
                     dataTableOutput("summaryTable",width="150%"))
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
        behaviorsname = NULL,
        binbreaks1 = 0,
        binbreaks2 = 0,
        sum_data = NULL
    )
    
    # load file to combine behaviors
    shinyFileChoose(input, 'behaviorsfile', roots=c(volumes), filetypes=c('', 'xlsx'))
  
    observeEvent(
      ignoreNULL = TRUE,
      eventExpr = {input$behaviorsfile},
      handlerExpr = {
        values$behaviorsname <- parseFilePaths(roots=volumes, input$behaviorsfile)$datapath
    })
      
    output$bname <- renderText({
      values$behaviorsname
    })  

    observeEvent(input$reset_behaviorsfile, {
      values$behaviorsname = NULL
    })
    
  
    # Bin breaks    
    observeEvent({
      input$break1 
      input$break2},
      handlerExpr = {
        if(input$break1!=0){
          values$binbreaks1 <- as.numeric(input$break1)
        }
        if(input$break2!=0){
          values$binbreaks2 <- as.numeric(input$break2)
          }
        },
      ignoreNULL = TRUE)
    
    
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
    
    # Subset data when filter is applied
    summary_data_subset <- reactive({
      req(values$sum_data)
      
      # Indices of filtered rows
      s = input$summaryTable_rows_all
      switch(input$sumfrac,
             "Sum" = values$sum_data$summary_all[s,],
             "Fraction" = values$sum_data$fraction_all[s,])
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
                                        breaks=unique(c(1,values$binbreaks1,values$binbreaks2,630)), 
                                        combine=values$behaviorsname))
       remove_modal_spinner()
    })
     
    # display summarized data
    output$summaryTable<- renderDataTable(
      summary_data(),
      filter="top",
      options = list(pageLength=10,
                     columnDefs=list(list(width = '200px',targets = "_all", class="dt-head-center dt-center")))
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
          outlist <- list(summary_data_subset()) 
          names(outlist) <- input$sumfrac
          write.xlsx(outlist, outfile, col.names = TRUE, row.names = FALSE, append = FALSE, showNA = FALSE) 
        } else if (input$format == "for Prism"){
          export_for_Prism(summary_data_subset(), filter = NULL, outfilename=outfile)
        }
        showNotification(paste("Data saved to", outfile), type="message")
      })
      }

# Run the application 
shinyApp(ui = ui, server = server)

