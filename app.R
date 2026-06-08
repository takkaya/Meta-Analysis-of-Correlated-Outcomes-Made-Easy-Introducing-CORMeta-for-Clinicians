
# Flexible R Shiny App for Multivariate Meta-Analysis
library(shiny)
library(DT)
library(openxlsx)
library(ggplot2)
library(shinyBS)
# install.packages("rsconnect")
library(rsconnect)
library(rsconnect)

library(rsconnect)
library(shinythemes)

# 
# rsconnect::setAccountInfo(
#   name = "your_account_name",
#   token = "your_token",
#   secret = "your_secret"
# )
# 
# # rsconnect::deployApp("C:/Users/user/Downloads/flexible_meta_app (1) (1).R")
# 
# shiny::runApp("C:/Users/user/Downloads/flexible_meta_app (1) (1).R")


# Custom metafor functions must be sourced
source("functions.R")

ui <- fluidPage(
  theme = shinytheme("cosmo"),  # or "cerulean", "cosmo", "superhero", etc.
  titlePanel("CORMeta: Meta-analysis of Correlated Outcomes"),
  sidebarLayout(
    sidebarPanel(
      helpText("To get started, you can either upload your own summary dataset as a .csv file, or download and use the example dataset by clicking the 'Download Sample Data' button above. Then upload it below to run the analysis."),
      downloadButton("download_example", "Download Sample Data"),
      fileInput("file_data", "Upload Summary Data (.csv)", accept = ".csv"),
      bsTooltip("file_data", "Your file must include effect sizes, standard errors, study ID, and outcome columns.", "right"),
      
      
      selectizeInput("y_col", "Select Effect Size Column:", choices = NULL, options = list(create = TRUE)),
      selectizeInput("se_col", "Select Standard Error Column:", choices = NULL, options = list(create = TRUE)),
      selectizeInput("study_col", "Select Study ID Column:", choices = NULL, options = list(create = TRUE)),
      
      selectInput("rho_profile", "Select ρ Profile:",
                  choices = c("Custom" = "custom", "Surrogate (ρ=0.1)" = "surrogate", 
                              "Psychological (ρ=0.5)" = "psych", "Longitudinal (ρ=0.8)" = "longitudinal", 
                              "Independent (ρ=0)" = "independent")),
      conditionalPanel(
        condition = "input.rho_profile === 'independent'",
        helpText("Note: If each study contributes only one outcome and you select an independent correlation structure (ρ = 0), the model is equivalent to a traditional univariate meta-analysis. In this case, the residual correlation matrix is the identity matrix, and the analysis reduces to estimating a single pooled effect using standard methods.")
      ),
      
      bsTooltip("rho_profile", 
                "Select a pre-defined residual correlation profile. You must click 'Generate Dummy Correlation Matrix' after choosing.", 
                "right"),
      
      conditionalPanel(
        condition = "input.rho_profile == 'custom'",
        tags$div(style = "color:red; font-style:italic; margin-bottom:10px;",
                 "You selected 'Custom'. Please upload your own correlation matrix below.")
      ),
      
      numericInput("rho", "Set ρ (Residual Correlation):", value = 0, min = 0, max = 0.99, step = 0.05),
      
      fileInput("file_corr", "Upload Correlation Matrix (.csv)", accept = ".csv"),
      bsTooltip("file_corr", "Optional. Upload a square correlation matrix for outcomes.", "right"),
      
      actionButton("generate_R", "Generate Dummy Correlation Matrix"),
      tags$div(style = "font-size: 12px; margin-top: 5px; color: #555;",
               "Note: After selecting a ρ profile (other than 'Custom'), click this button to generate the matrix."),
      downloadButton("download_generated_R", "Download Generated R"),
      tags$div(style = "font-size: 12px; margin-top: 5px; color: #555;",
               "Optional:click this button to download and review the generated correlation matrix."),
      selectInput("filter_var", "Subgroup Analysis By:", choices = NULL),
      tags$div(style = "font-size: 12px; margin-top: 5px; color: #555;",
               "Note: You can run a subgroup analysis by selecting a variable and either a numeric range or category."),
      uiOutput("filter_value_ui"),
      
      actionButton("run", "Run Meta-Analysis"),
      downloadButton("download", "Download Results"),
      downloadButton("download_plot", "Download Forest Plot (PNG)")
      
    ),
    mainPanel(
      DTOutput("table"),
      plotOutput("forestplot", height = "600px"),
      verbatimTextOutput("summary")
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  R <- reactiveVal(NULL)
  has_generated_R <- reactiveVal(FALSE)
  result <- reactiveVal(NULL)
  generated_R <- reactiveVal(NULL)
  observe({
    sample_file <- "example_data.csv"
    if (file.exists(sample_file)) {
      df <- read.csv(sample_file, check.names = FALSE)
      colnames(df) <- trimws(colnames(df))
      
      names(df)[names(df) == "Estimated effect size(beta)"] <- "effect_size"
      names(df)[names(df) == "SE of the beta"] <- "std_error"
      names(df)[names(df) == "Study name"] <- "study_id"
      
      rownames(df) <- 1:nrow(df)
      data(df)
      
      updateSelectInput(session, "filter_var", choices = names(df))
      updateSelectizeInput(session, "y_col", choices = names(df), selected = "effect_size", server = TRUE)
      updateSelectizeInput(session, "se_col", choices = names(df), selected = "std_error", server = TRUE)
      updateSelectizeInput(session, "study_col", choices = names(df), selected = "study_id", server = TRUE)
    }
  })
  
  
  observeEvent(input$rho_profile, {
    has_generated_R(FALSE)
    rho_val <- switch(input$rho_profile,
                      "surrogate" = 0.1,
                      "psych" = 0.5,
                      "longitudinal" = 0.8,
                      "independent" = 0,
                      "custom" = isolate(input$rho))
    updateNumericInput(session, "rho", value = rho_val)
  })
  
  observeEvent(input$file_data, {
    df <- read.csv(input$file_data$datapath, check.names = FALSE)
    colnames(df) <- trimws(colnames(df))
    
    # Rename key columns to internal consistent names
    names(df)[names(df) == "Estimated effect size(beta)"] <- "effect_size"
    names(df)[names(df) == "SE of the beta"] <- "std_error"
    names(df)[names(df) == "Study name"] <- "study_id"
    
    rownames(df) <- 1:nrow(df)
    data(df)
    
    updateSelectInput(session, "filter_var", choices = names(df))
    updateSelectizeInput(session, "y_col", choices = names(df), selected = "effect_size", server = TRUE)
    updateSelectizeInput(session, "se_col", choices = names(df), selected = "std_error", server = TRUE)
    updateSelectizeInput(session, "study_col", choices = names(df), selected = "study_id", server = TRUE)
  })
  
  
  
  observeEvent(input$file_corr, {
    r <- read.csv(input$file_corr$datapath)
    R(as.matrix(r[, -1]))
  })
  
  observeEvent(input$generate_R, {
    has_generated_R(TRUE)
    req(data())
    df <- data()
    n <- nrow(df)
    rho <- input$rho
    cohort_vec <- df[[input$study_col]]
    
    R_matrix <- matrix(0, n, n)
    for (i in 1:n) {
      for (j in 1:n) {
        if (i == j) {
          R_matrix[i, j] <- 1
        } else if (cohort_vec[i] == cohort_vec[j]) {
          R_matrix[i, j] <- rho
        }
      }
    }
    generated_R(R_matrix)
    showNotification(paste("Generated correlation matrix with ρ =", rho), type = "message")
    if (is.null(R())) R(R_matrix)
    has_generated_R(TRUE)  # ✅ set flag here
  })
  
  
  output$download_generated_R <- downloadHandler(
    filename = function() {"generated_R_matrix.csv"},
    content = function(file) {
      R_out <- generated_R()
      R_df <- cbind(ID = 1:nrow(R_out), as.data.frame(R_out))
      write.csv(R_df, file, row.names = FALSE)
    }
  )
  
  output$filter_value_ui <- renderUI({
    req(data(), input$filter_var)
    df <- data()
    var <- input$filter_var
    if (is.numeric(df[[var]])) {
      sliderInput("filter_value", "Value Range:",
                  min = min(df[[var]], na.rm = TRUE),
                  max = max(df[[var]], na.rm = TRUE),
                  value = range(df[[var]], na.rm = TRUE))
    } else {
      selectInput("filter_value", "Categories:", choices = unique(df[[var]]), multiple = TRUE)
    }
  })
  
  observeEvent(input$run, {
    # Check if the user forgot to generate the dummy correlation matrix
    if (input$rho_profile != "custom" && is.null(input$file_corr) && !has_generated_R()) {
      showNotification("❗ Please click 'Generate Dummy Correlation Matrix' before running the analysis.", type = "error")
      return()  # stop execution
    }
    
    req(data(), R())
    df <- data()
    r_mat <- R()
    r_working <- diag(1, nrow(df))
    
    # Apply subgroup filtering if selected
    if (!is.null(input$filter_var) && !is.null(input$filter_value)) {
      var <- input$filter_var
      val <- input$filter_value
      if (is.numeric(df[[var]])) {
        df <- df[df[[var]] >= val[1] & df[[var]] <= val[2], ]
      } else {
        df <- df[df[[var]] %in% val, ]
      }
    }
    
    # Subset matrices based on filtered data
    idx <- as.numeric(rownames(df))
    r_mat <- r_mat[idx, idx]
    r_working <- r_working[idx, idx]
    
    # Run the meta-analysis
    y <- df[[input$y_col]]
    v <- df[[input$se_col]]^2
    
    res <- metafor.mod(y, v, R = r_mat, RWorking = r_working, data = df)
    result(list(data = df, res = res))
  })
  
  
  output$table <- renderDT({ req(result()); datatable(result()$data) })
  output$forestplot <- renderPlot({ req(result()); result()$res$plot })
  output$summary <- renderPrint({
    req(result())
    list(
      mu_hat = result()$res$value_1,
      se = result()$res$value_2,
      tau2 = result()$res$value_3,
      pvalue = result()$res$value_4
    )
  })
  
  output$download <- downloadHandler(
    filename = function() { paste0("meta_results_", Sys.Date(), ".xlsx") },
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "Data")
      writeData(wb, "Data", result()$data)
      addWorksheet(wb, "Summary")
      writeData(wb, "Summary", as.data.frame(t(unlist(list(
        mu_hat = result()$res$value_1,
        se = result()$res$value_2,
        tau2 = result()$res$value_3,
        pvalue = result()$res$value_4
      )))))
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  output$download_example <- downloadHandler(
    filename = function() { "example_data.csv" },
    content = function(file) {
      file.copy("example_data.csv", file)
    }
  )
  output$download_plot <- downloadHandler(
    filename = function() {
      paste0("forest_plot_", Sys.Date(), ".png")
    },
    content = function(file) {
      req(result())
      png(file, width = 1000, height = 800, res = 150)
      print(result()$res$plot)
      dev.off()
    }
  )
  
  
}

shinyApp(ui, server)
