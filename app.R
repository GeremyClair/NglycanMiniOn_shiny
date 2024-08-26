# NGlycanMiniOn by HB and GC

library(shiny)
library(DT)
library(NGlycanMiniOn)
library(ggplot2)
library(shinyjs)

# UI

ui <- navbarPage(
  title = div("NGlycanMiniOn", style = "font-weight: bold;"),
  id = "navbar",
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .navbar {
        background-color: #f8f9fa;
        position: fixed;
        width: 100%;
        z-index: 1000;
        top: 0;
      }
      body {
        padding-top: 70px;
      }
      .navbar-nav > li > a {
        font-weight: bold;
        padding-left: 20px;
        padding-right: 20px;
      }
      .navbar-brand {
        padding-left: 20px;
      }
      .dataTables_wrapper .dataTables_scrollBody {
        max-height: calc(100vh - 180px) !important;
      }
      #visualization_plot {
        height: calc(100vh - 150px) !important;
      }
      .download-btn {
        position: absolute;
        right: 20px;
        z-index: 1000;
      }
      .enrichment-spacing, .visualization-spacing {
        margin-top: 10px;
      }
      .refresh-btn {
        position: absolute;
        right: 20px;
        top: 10px;
        z-index: 1001;
        background-color: #007bff;
        color: white;
        border-radius: 5px;
        padding: 5px 10px;
        cursor: pointer;
      }
    "))
  ),

  div(
    class = "refresh-btn",
    "Refresh",
    onclick = "location.reload();"
  ),
  
  # Tab 1:- Data Ingestion
  
  tabPanel(
    "Data Ingestion",
    tabsetPanel(
      tabPanel(strong("List Mode"),
               h3("Data Ingestion - List Mode"),
               fluidRow(
                 column(
                   width = 6,
                   fileInput("file_upload_query", "Upload Query Data",
                             accept = c(
                               "text/csv",
                               "text/comma-separated-values,text/plain",
                               ".csv")),
                   actionButton("load_query", "Load Query")
                 ),
                 column(
                   width = 6,
                   radioButtons("universe_option", "Select Universe Data Source:",
                                choices = c("Upload Universe Data" = "upload_universe",
                                            "Use Nglycan_db from NGlycanMiniOn" = "nglycan_db"),
                                selected = "nglycan_db"),
                   conditionalPanel(
                     condition = "input.universe_option == 'upload_universe'",
                     fileInput("file_upload_universe", "Upload Universe Data", 
                               accept = c(
                                 "text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
                   ),
                   actionButton("load_universe", "Load Universe")
                 )
               ),
               hr(),
               h4("NGlycan Miner Results"),
               div(style = "position: relative; height: 40px;",
                   downloadButton("download_nglycan", "Download CSV", class = "download-btn", disabled = TRUE)),
               DT::dataTableOutput("nglycan_table")
      ),
      tabPanel(strong("Ranked Mode"),
               h3("Data Ingestion - Ranked Mode"),
               fileInput("file_upload_rank", "Upload Ranked Table Data",
                         accept = c(
                           "text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")),
               actionButton("load_rank", "Load Rank Table"),
               div(style = "position: relative; height: 40px;",
                   downloadButton("download_rank", "Download CSV", class = "download-btn", disabled = TRUE)),
               DT::dataTableOutput("rank_table")
      )
    )
  ),
  
  # Tab 2:- Enrichment
  
  tabPanel(
    "Enrichment",
    h3("Enrichment"),
    textOutput("data_source_info"),
    div(class = "enrichment-spacing"),
    uiOutput("test_options"),
    actionButton("run_enrichment", "Run Enrichment Test"),
    div(style = "position: relative; height: 40px;",
        downloadButton("download_enrichment", "Download CSV", class = "download-btn", disabled = TRUE)),
    DT::dataTableOutput("enrichment_results")
  ),
  
  # Tab 3:- Visualization
  
  tabPanel(
    "Visualization",
    h3("Visualization"),
    textOutput("visualization_source_info"),
    div(style = "position: relative; height: 40px;",
        actionButton("run_visualization", "Run Visualization", style = "margin: 8px 0;"),
        downloadButton("download_pdf", "Download PDF", class = "download-btn", style = "position: absolute; right: 0px;", disabled = TRUE)
    ),
    plotOutput("visualization_plot", height = "100%")
  )
)

# Server

server <- function(input, output, session) {
  # Reactive values to store the data and state
  query_data <- reactiveVal(NULL)
  universe_data <- reactiveVal(NULL)
  rank_table_data <- reactiveVal(NULL)
  nglycan_miner_result <- reactiveVal(NULL)
  last_used_mode <- reactiveVal(NULL)
  enrichment_result <- reactiveVal(NULL)
  visualization_result <- reactiveVal(NULL)
  
  # Clear Enrichment and Visualization results
  clearResults <- function() {
    enrichment_result(NULL)
    visualization_result(NULL)
    shinyjs::disable("download_enrichment")
    shinyjs::disable("download_pdf")
  }
  
  # Load query data when button is clicked
  observeEvent(input$load_query, {
    req(input$file_upload_query)
    file <- input$file_upload_query$datapath
    data <- read.csv(file)
    query_data(data)
    showNotification("Query data loaded successfully.", type = "message")
    
    clearResults()
  })
  
  # Load Universe data based on selection and run NGlycanMiner
  observeEvent(input$load_universe, {
    req(query_data())
    
    if (input$universe_option == "nglycan_db") {
      universe <- data.frame(ID = NGlycanMiniOn::Nglycan_db)
      universe_data(universe)
      showNotification("Nglycan_db loaded successfully.", type = "message")
    } else if (input$universe_option == "upload_universe") {
      req(input$file_upload_universe)
      file <- input$file_upload_universe$datapath
      data <- read.csv(file)
      universe_data(data)
      showNotification("Custom universe data loaded successfully.", type = "message")
    }
    
    universe_vector <- as.character(universe_data()$ID)
    table <- NGlycan_miner(universe_vector)
    
    query_vector <- as.character(query_data()$ID)
    table$present_in_query <- table$ID %in% query_vector
    
    nglycan_miner_result(table)
    last_used_mode("list")
    showNotification("NGlycan Miner and query check completed successfully.", type = "message")
    
    shinyjs::enable("download_nglycan")
    
    clearResults()
  })
  
  # Load Rank data based on selection and run NGlycanMiner
  observeEvent(input$load_rank, {
    req(input$file_upload_rank)
    file <- input$file_upload_rank$datapath
    data <- read.csv(file)
    
    # Extract the ID and pvalue columns
    id_vector <- as.character(data$ID)
    pvalues <- data$pvalue
    
    # Run NGlycan Miner on the ID column
    mined_table <- NGlycan_miner(id_vector)
    
    # Add the present_in_query column
    query_vector <- as.character(query_data()$ID)
    mined_table$present_in_query <- mined_table$ID %in% query_vector
    
    # Add back the pvalue column
    final_table <- merge(mined_table, data, by = "ID")
    
    rank_table_data(final_table)
    last_used_mode("rank")
    showNotification("Rank table processed and displayed successfully.", type = "message")
    
    shinyjs::enable("download_rank")
    
    clearResults()
  })
  
  # Output the NGlycanMiner results
  output$nglycan_table <- DT::renderDataTable({
    req(nglycan_miner_result())
    datatable(nglycan_miner_result(), options = list(
      pageLength = 25,  
      scrollX = TRUE, 
      scrollY = "70vh" 
    ))
  })
  
  # Output the rank table data
  output$rank_table <- DT::renderDataTable({
    req(rank_table_data())
    datatable(rank_table_data(), options = list(
      pageLength = 25,
      scrollX = TRUE, 
      scrollY = "70vh" 
    ))
  })
  
  # Provide download for the NGlycanMiner results
  output$download_nglycan <- downloadHandler(
    filename = function() {
      paste("nglycan_results-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(nglycan_miner_result(), file, row.names = FALSE)
    }
  )
  
  # Provide download for the Rank table results
  output$download_rank <- downloadHandler(
    filename = function() {
      paste("rank_results-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(rank_table_data(), file, row.names = FALSE)
    }
  )
  
  # Update UI for the Enrichment tab based on the last used mode
  output$data_source_info <- renderText({
    mode <- last_used_mode()
    if (is.null(mode)) {
      return("No data has been uploaded yet.")
    } else if (mode == "list") {
      return("Using results from the List Mode tab.")
    } else {
      return("Using results from the Ranked Mode tab.")
    }
  })
  
  # Dynamically generate test options if List Mode data is used
  output$test_options <- renderUI({
    if (last_used_mode() == "list") {
      selectInput("test_type", "Select Test Type:",
                  choices = c("Fisher" = "fisher",
                              "EASE" = "ease",
                              "Binomial" = "binomial",
                              "Hypergeometric" = "hypergeometric"))
    } else {
      NULL
    }
  })
  
  # Run the selected enrichment test when the button is clicked
  observeEvent(input$run_enrichment, {
    mode <- last_used_mode()
    
    if (mode == "list") {
      # Get the universe and query data
      universe <- universe_data()
      query <- query_data()
      
      # Determine which test to run based on selection
      test_type <- input$test_type
      result <- switch(test_type,
                       fisher = Nglycan_Fisher(query$ID, universe$ID),
                       ease = Nglycan_EASE(query$ID, universe$ID),
                       binomial = Nglycan_binomial(query$ID, universe$ID),
                       hypergeometric = Nglycan_hypergeometric(query$ID, universe$ID))
      
      # Rearrange the Nglycan_in_query column to be the last column if it exists
      if ("Nglycan_in_query" %in% names(result)) {
        result <- result[, c(setdiff(names(result), "Nglycan_in_query"), "Nglycan_in_query")]
      }
      
    } else if (mode == "rank") {
      # Use Nglycan_KS for enrichment if Ranked Mode data is used
      result <- Nglycan_KS(rank_table_data(), order = "ascending")
      
    } else {
      showNotification("No valid data available for enrichment.", type = "error")
      return()
    }
    
    # Store and display the enrichment result
    enrichment_result(result)
    showNotification("Enrichment test completed successfully.", type = "message")
    
    # Enable the download button when data is available
    shinyjs::enable("download_enrichment")
  })
  
  # Output the enrichment results
  output$enrichment_results <- DT::renderDataTable({
    req(enrichment_result())
    datatable(enrichment_result(), options = list(
      pageLength = 25,
      scrollX = TRUE,
      scrollY = "70vh"
    ))
  })
  
  # Provide download for the Enrichment results
  output$download_enrichment <- downloadHandler(
    filename = function() {
      paste("enrichment_results-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(enrichment_result(), file, row.names = FALSE)
    }
  )
  
  # Update UI for the Visualization tab based on the last used mode
  output$visualization_source_info <- renderText({
    mode <- last_used_mode()
    if (is.null(mode)) {
      return("No data has been uploaded yet.")
    } else if (mode == "list") {
      return("Using results from the List Mode tab.")
    } else {
      return("Using results from the Ranked Mode tab.")
    }
  })
  
  # Run the visualization when button is clicked
  observeEvent(input$run_visualization, {
    mode <- last_used_mode()
    if (mode == "list") {
      query <- as.character(query_data()$ID)
      universe <- as.character(universe_data()$ID)
      
      list_fig <- list(query = query, universe = universe)
      visualization_output <- freq_ontologies(list_fig)
    } else if (mode == "rank") {
      query <- as.character(rank_table_data()$ID)
      visualization_output <- freq_ontologies(query)
    } else {
      showNotification("No valid data available for visualization.", type = "error")
      return()
    }
    
    visualization_result(visualization_output)
    showNotification("Visualization completed successfully.", type = "message")
    
    shinyjs::enable("download_pdf")
  })
  
  # Output the visualization results as a plot
  output$visualization_plot <- renderPlot({
    req(visualization_result())
    visualization_result() +
      theme(
        axis.title = element_text(size = 18),
        axis.text = element_text(size = 16),
        legend.text = element_text(size = 16)
      )
  })
  
  # Provide a download handler for the PDF export
  output$download_pdf <- downloadHandler(
    filename = function() {
      paste("visualization-", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      g <- visualization_result()
      if (!is.null(g)) {
        ggsave(file, plot = g, device = "pdf", width = 11, height = 8.5)
      }
    }
  )
}

# Run application 
shinyApp(ui = ui, server = server)
