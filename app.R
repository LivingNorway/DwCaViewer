#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(xml2)
library(leaflet)
library(visdat)
library(DT)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Darwin Core Archive Viewer"),

    # Sidebar with for selection of data set (sleeping at the moment)
    sidebarLayout(
        sidebarPanel(selectInput("dataset", "Choose a dataset:",
                choices = c("FeFo", "Statskog", "Fjellstyrene"),

        ),
        actionButton("run", "View data")),
        

        # Show a plot of the generated distribution
        mainPanel(width=5, 
                 tabsetPanel(type="pills", 
        
        tabPanel("Select data set",             
           h3("Resource metadata"),
           br(),
           br(),
           h4("This data resource containts the following data tables"),
           tableOutput("list_files"),
           br(),
           br(),
           h4("DOI"),
           textOutput("DOI1"), 
           br(), 
           h4("Citation"),
           textOutput("gbif_citation")
        ),
        
        tabPanel("View Event data", 
            h3("Event Data"),
            DT::dataTableOutput("table1")
        ),
        
        tabPanel("View Occurence data", 
                 h3("Occurence Data"),
                 DT::dataTableOutput("table2")
        ),
        
        tabPanel("Explore Geographic patterns",
                 h3("Map"),
                 leafletOutput("mymap")
                 ),
        
        tabPanel("Visualize data",
                 h3("various data viz"),
                 plotOutput("dataviz1")
        )
        
    )
)))

# Define server logic 
server <- function(input, output) {
    
    observeEvent(input$run,{ #Run Button
        
        datasetID <- "c47f13c1-7427-45a0-9f12-237aad351040"
        dataset <- RJSONIO::fromJSON(paste0("http://api.gbif.org/v1/dataset/",datasetID,"/endpoint"))
        endpoint_url <- dataset[[1]]$url 
        
        download.file(endpoint_url, destfile="data/temp.zip", mode="wb")
        unzip ("data/temp.zip", exdir = "data")
        
        my_dat1 <- as_tibble(read_delim("data/event.txt", delim="\t", quote = ""))
        my_dat2 <- as_tibble(read_delim("data/occurrence.txt", delim="\t", quote = ""))
        dat2 <- left_join(my_dat2, my_dat1, by="eventID")
        
        meta <- read_xml("data/eml.xml") %>% as_list() 
        
        
        output$table1 <- DT::renderDataTable(my_dat1)
        output$table2 <- DT::renderDataTable(my_dat2)
        
        
        output$DOI1 <- renderText(attr(meta$eml$additionalMetadata$metadata$gbif$citation,"identifier"))
        output$gbif_citation <- renderText(meta$eml$additionalMetadata$metadata$gbif$citation[[1]])        
        output$list_files <- renderTable(as.matrix(dir("data", pattern="\\.txt$"), ncol=1))
        
        output$mymap <- renderLeaflet({
            leaflet() %>%
                addProviderTiles(providers$Esri.NatGeoWorldMap,
                                 options = providerTileOptions(noWrap = TRUE, 
                                                               preferCanvas = TRUE)) %>%
                addMarkers(lat=~decimalLatitude, lng=~decimalLongitude, data=(dat2))                     
            
                                    })
        
        output$dataviz1 <- renderPlot({
            vis_guess((my_dat1))
            
        })
        
         })
    
    ##
   
    
    ##

    
    
}

# Run the application 
shinyApp(ui = ui, server = server)
