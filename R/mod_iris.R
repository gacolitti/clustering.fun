#' iris UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' 
#' @noRd 
#'
#' @import shiny
#' @importFrom DT DTOutput 
#' 
mod_iris_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(
        numericInput(ns("n_clust"), "Number of Clusters", value = 3, min = 2),
        selectizeInput(
          ns("clust_vars"),
          "Clustering Variables",
          choices = names(iris2),
          selected = names(iris2),
          multiple = TRUE
        ), 
        selectizeInput(ns("x_var"), "Variable on x-axis", choices = names(iris2)),
        selectizeInput(ns("y_var"), "Variable on y-axis", choices = names(iris2), selected = "Sepal.Width"),
        checkboxInput(ns("use_normalized"), "Normalized Data", value = TRUE),
        checkboxInput(ns("add_convex_hulls"), "Add Convex Hulls", value = TRUE)
      ),
      mainPanel(
        plotOutput(ns("plot"), height = "40vh"),
        DTOutput(ns("table"))
      )
    )
  )
}
    
#' iris Server Functions
#'
#' @noRd 
#' 
#' @import shiny
#' @import data.table
#' @importFrom ggplot2 ggplot geom_point aes_string theme_minimal geom_polygon 
#' @importFrom DT renderDT datatable
mod_iris_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    clust <- reactive({
      
      req(input$clust_vars)
      
      # Subset columns based on input$clust_vars
      d <- iris
      d <- iris[, input$clust_vars]
      
      # Normalize data
      if (input$use_normalized) {
        d <- d %>% scale()  
      }
      
      # Perform kmeans clustering
      kmeans(x = d, centers = input$n_clust, nstart = 25)
      
    })
    
    output$plot <- renderPlot({
      
      # Ensure clustering variables are not missing
      validate(need(length(input$clust_vars) > 0, "Clustering variables cannot be missing."))
      
      # Ensure x and y variables are different
      validate(need(input$x_var != input$y_var, "Variables on the x and y axes must be different."))
      
      d <- iris 
      
      d$cluster <- as.factor(clust()$cluster)
      
      # Add scatterplot of points 
      p <- ggplot() +
        geom_point(
          data = d,
          aes_string(
            x = input$x_var,
            y = input$y_var,
            color = "cluster"
          ),
          size = 3
        ) +
        theme_minimal()
      
      # Calculate convex hulls and plot
      if (input$add_convex_hulls) {
        setDT(d)
        hulls <- d[, .SD[chull(.SD)], by = .(cluster), .SDcols = c(input$x_var, input$y_var)]
        
        p <- p +
          geom_polygon(
            data = hulls,
            aes_string(
              x = input$x_var,
              y = input$y_var,
              fill = "cluster"
            ),
            alpha = 0.3
          ) 
      }
      
      # Highlight selected rows in the table by making corresponding points in the plot larger
      if (!is.null(input$table_rows_selected)) {
        
        d_table <- failed_pred_dt()[input$table_rows_selected, ]
        
        # Make cluster a factor and ensure all levels are added so colors of the points are accurate
        d_table$cluster <- factor(d_table$cluster, levels = seq_len(input$n_clust))
        
        # Plot selected points
        p <- p +
          geom_point(
            data = d_table,
            aes_string(
              x = input$x_var,
              y = input$y_var,
              color = "cluster" 
            ),
            show.legend = FALSE,
            size = 10
          ) 
      }
      
      p
      
    })
    
    # Add a table of rows for which clustering failed to predict the true cluster based on 
    # the original Species column. 
    failed_pred_dt <- reactive({
      req(clust())
      
      modal <- function(v) {
        uniqv <- unique(v)
        uniqv[which.max(tabulate(match(v, uniqv)))]
      }
      
      d <- iris
      d$cluster <- clust()$cluster
      
      dt <-setDT(d)[, predicted_species := modal(Species), by = .(cluster)][
        which(Species != predicted_species)
      ]
      
    })
    
    
    # Render table of incorrect predictions. 
    output$table <- renderDT({
      
      datatable(
        failed_pred_dt(), 
        caption = paste0(
          "Table of incorrect predictions. ",
          "Select rows to highlight them in the plot."
        ),
        options = list(scrollY = "40vh", paging = FALSE)
      )
      
    })
 
  })
}
    