library(shiny)
library(cronR)
library(miniUI)

# UI for the app
ui <- fluidPage(
  titlePanel("CronR Task Scheduler"),
  sidebarLayout(
    sidebarPanel(
      h4("Job Details"),
      textInput("script_path", "Path to R Script:", 
                placeholder = "Enter the full path to the R script on the server"),
      textInput("job_id", "Job ID:", placeholder = "Enter a unique job ID"),
      textInput("description", "Job Description:", placeholder = "Describe the task"),
      selectInput(
        "frequency",
        "Frequency:",
        choices = c("minutely", "hourly", "daily", "weekly", "monthly"),
        selected = "daily"
      ),
      textInput("time", "Start Time:", value = "8AM", placeholder = "e.g., 8AM")
    ),
    mainPanel(
      h4("Scheduled Cron Jobs"),
      verbatimTextOutput("cron_jobs_list"),  # Display the list of cron jobs in readable format
      hr(),
      actionButton("list_jobs", "List Cron Jobs"),
      actionButton("add_job", "Add Cron Job"),
      hr(),
      textInput("remove_id", "Job ID to Remove:", placeholder = "Enter Job ID to remove"),
      actionButton("remove_job", "Remove Cron Job")
    )
  )
)

# Server Logic
server <- function(input, output, session) {
  observeEvent(input$list_jobs, {
    jobs_output <- tryCatch(cron_ls(), error = function(e) NULL)
    
    if (is.null(jobs_output) || !nzchar(jobs_output)) {
      job_list <- "No cron jobs found."
    } else {
      # Split the output by line and process it
      jobs_lines <- unlist(strsplit(jobs_output, "\n"))
      
      # Extract job details
      job_entries <- list()
      job_entry <- list()
      for (line in jobs_lines) {
        if (grepl("^## id:", line)) {
          if (length(job_entry) > 0) job_entries <- append(job_entries, list(job_entry))
          job_entry <- list(id = sub("^## id:\\s*", "", line))
        } else if (grepl("^## desc:", line)) {
          job_entry$desc <- sub("^## desc:\\s*", "", line)
        } else if (grepl("^[0-9]+ [0-9]+ \\* \\* \\*", line)) {  # Matches a cron schedule format
          job_entry$command <- line
        }
      }
      if (length(job_entry) > 0) job_entries <- append(job_entries, list(job_entry))
      
      # Format the output
      job_list <- sapply(job_entries, function(entry) {
        paste(
          "Job ID:", entry$id, "\n",
          "Description:", ifelse(!is.null(entry$desc), entry$desc, "N/A"), "\n",
          "Command:", ifelse(!is.null(entry$command), entry$command, "N/A"), "\n",
          "------"
        )
      })
      job_list <- paste(job_list, collapse = "\n")
    }
    
    # Render the job list
    output$cron_jobs_list <- renderText({
      if (!is.null(job_list)) {
        return(job_list)
      } else {
        return("Unable to fetch cron jobs.")
      }
    })
  })
  
  observeEvent(input$add_job, {
    req(input$script_path, input$job_id, input$frequency, input$time)
    
    # Validate that the script path exists
    script_path <- input$script_path
    if (!file.exists(script_path)) {
      showNotification("The specified script path does not exist.", type = "error")
      return()
    }
    Sys.chmod(script_path, mode = "0755")
    
    tryCatch({
      cmd <- cron_rscript(script_path)
      cron_add(
        command = cmd,
        frequency = input$frequency,
        at = input$time,
        id = input$job_id,
        description = input$description,
        ask = FALSE
      )
      showNotification("Cron job added successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error adding cron job:", e$message), type = "error")
    })
  })
  
  # Remove a cron job
  observeEvent(input$remove_job, {
    job_id <- input$remove_id
    req(job_id)
    
    tryCatch({
      cron_rm(id = job_id, ask = FALSE)
      showNotification("Cron job removed successfully!", type = "message")
    }, error = function(e) {
      showNotification(paste("Error removing cron job:", e$message), type = "error")
    })
  })
}

# Run the app
shinyApp(ui = ui, server = server)
