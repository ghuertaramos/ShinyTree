#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

if (!"treeio" %in% installed.packages()) {
  if (!"devtools" %in% installed.packages()) install.packages("devtools")
  devtools::install_github("GuangchuangYu/treeio")
} else if (packageVersion("treeio") < "1.5.1.2") {
  if (!"devtools" %in% installed.packages()) install.packages("devtools")
  devtools::install_github("GuangchuangYu/treeio")
}

if (!"purrr" %in% installed.packages()) install.packages("purrr")


purrr::walk(c("shiny", "shinyjs", "tidyverse", 
              "ggtree", "tidytree", "shinyalert"), ~{
                if (!.x %in% installed.packages()) install.packages(.x)
              })


suppressWarnings({
  suppressPackageStartupMessages({
    library(shiny)
    library(shinyjs)
    library(tidyverse)
    library(ggtree)
    library(tidytree)
    library(treeio)
    library(shinyalert)
    library(ggplot2)
    library(ape)
  })
})

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

session$onSessionEnded(stopApp)

  rv <- reactiveValues(
    data = NULL,
    clear = FALSE
  )
  
  
  observeEvent(input$upload_tree, {
    rv$clear <- FALSE
  }, priority = 1000)
  
  # This reactive value reads in the tree object using one of the
  # treeio import functions. If the function called, based on input$file_type
  # fails, NULL is returned
  tree <- reactive({
    req(input$upload_tree, input$file_type,
        input$upload_tree, !rv$clear)
    
    file <- input$upload_tree$datapath
    
    output <- switch(
      input$file_type,
      tree = possibly(read.tree, otherwise = NULL)(file),
      phylip = possibly(read.phylip, otherwise = NULL)(file),
      newick = possibly(read.newick, otherwise = NULL)(file),
      nexus = possibly(read.nexus, otherwise = NULL)(file)
    )

    
    return(output)
  })
  
  
  # This tree_df function 
  tree_df <- reactive({
    req(tree())
    output <- tree() %>% 
      as_data_frame()
  })
  
  observe({
    req(input$upload_tree)
    
    if (is.null(tree())) {
      shinyalert("Tree import error", paste("There was an error when trying to read your tree!",
                                            "Did you select the correct file format?"),
                 type = "error")
    }
  })
  
  # p <- sub_tree %>% 
  #   ggtree(aes(color = group))  %<+% labels_df +
  #   geom_tiplab(aes(label = paste(genus, species)), 
  #               size = input$subtree_text_size) +
  #   theme_tree2() +
  #   scale_color_manual(values = c(`1` = "red", `0` = "black"))

output$uploadedtree <- renderPlot({
    p  <- ggtree(tree()) +
      geom_tiplab(aes(), size=6, vjust=0.5, hjust = 0.5, offset = 0.9) +
      labs(title="Tree", caption="")
    
    plot (p) })  

output$treePlot <- renderPlot({
  p  <- ggtree(tree(), layout=input$tree_shape) +
                 geom_tiplab(aes(), size=6, vjust=0.5, hjust = 0.5, offset = 0.9) +
                 labs(title="Tree", caption="")

plot (p) })

})
