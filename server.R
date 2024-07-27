# shinyServer.R

# Packages Needed
library(shiny)
library(data.table)

# Increase upload size
options(shiny.maxRequestSize=30*1024^2)

server <- function(input, output, session) {
  # Reactive value to store uploaded file names and metadata
  uploaded_files <- reactiveVal(NULL)
  metadata <- reactiveVal(data.frame())
  
  observeEvent(input$file1, {
    req(input$file1)
    files <- input$file1
    num_files <- nrow(files)
    
    output$metadata_inputs <- renderUI({
      tagList(
        tags$table(
          tags$tr(
            tags$th("File Name"),
            tags$th("Genotype"),
            tags$th("Treatment"),
            tags$th("Temperature (C)"),
            tags$th("Sex")
          ),
          lapply(1:num_files, function(i) {
            tags$tr(
              tags$td(files$name[i]),
              tags$td(textInput(paste0("genotype_", i), NULL)),
              tags$td(textInput(paste0("treatment_", i), NULL)),
              tags$td(numericInput(paste0("temperature_", i), NULL, value = 25.0, step = 0.1)),
              tags$td(selectInput(paste0("sex_", i), NULL, choices = c("M", "F")))
            )
          })
        )
      )
    })
  })
  
  observeEvent(input$submit, {
    req(input$file1)
    files <- input$file1
    num_files <- nrow(files)
    
    metadata_list <- lapply(1:num_files, function(i) {
      list(
        File = files$name[i],
        Genotype = input[[paste0("genotype_", i)]],
        Treatment = input[[paste0("treatment_", i)]],
        Temperature = input[[paste0("temperature_", i)]],
        Sex = input[[paste0("sex_", i)]]
      )
    })
    
    metadata_df <- do.call(rbind, lapply(metadata_list, as.data.frame))
    
    # Create a directory to save files if it does not exist
    if (!dir.exists("uploaded_files")) {
      dir.create("uploaded_files")
    }
    
    # Save the uploaded files to the server
    for (i in 1:num_files) {
      file_path <- file.path("uploaded_files", files$name[i])
      file.copy(files$datapath[i], file_path)
    }
    
    uploaded_files(metadata_df)
    
    # Display a confirmation message
    showModal(modalDialog(
      title = "Upload Complete",
      "Files and metadata have been successfully uploaded and saved.",
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  # Display the metadata table
  output$saved_files <- renderTable({
    req(uploaded_files())
    uploaded_files()
  })
}