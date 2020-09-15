#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# 
shinyUI(fluidPage(
    titlePanel("Phylogenetic trees"),
    tabsetPanel(
        id='mainpanel',
        # tabPanel('Introduction',
        #          tags$iframe(style="height:600px; width:100%", src="Applied_Phylogenetics_InClass.pdf")),
        # 
        tabPanel(
            title = "Upload Tree",
            fluidRow(
                class = "inputs",
                column(
                    6, 
                    selectInput(
                        inputId = "file_type",
                        label = "Select Tree File Type:",
                        choices = c(
                            "Tree" = "tree",  
                            "Phylip" = "phylip", 
                            "Nexus" = "nexus",
                            "Newick" = "newick"
                        ),
                        selected = "tree"
                    )
                ),
                column(
                    6,
                    fileInput(
                        inputId = "upload_tree",
                        label = "Select Tree File:"
                    )
                ),
                plotOutput("uploadedtree"),
            )),
        # tabPanel('Trait Table',div(dataTableOutput("traitTable")),style='font-size:80%'),
        tabPanel('Tree shape',
                 radioButtons("tree_shape", "Shape", choices=c("rectangular", "slanted", "daylight", "circular"), inline = T),
                 plotOutput("treePlot")
        ),
    p("A")
    
)))