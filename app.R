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
<<<<<<< HEAD
library(DT)
=======
library(DT) # known error with Shiny Datatable fixed by calling DT explictly 
#see https://stackoverflow.com/questions/58995381/shiny-datatable-error-datatables-warning-table-id-datatables-table-0-request

>>>>>>> b4610663f2ec70ce7992ca801335975d5d6d7ae9

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Darwin Core Archive Viewer"),

    # Sidebar with for selection of data set (sleeping at the moment)
    sidebarLayout(
        sidebarPanel(selectInput("dataset", "Choose a dataset:",
                choices = c("FeFo", "Statskog", "Fjellstyrene","Other"),

        ),
        actionButton("run", "View data"),
        textInput("ID", label = h3("If you select 'Other' above please enter a gbif event ID"), 
                  value = "e306fa70-381e-4330-8e68-1f447b46a850") #trying to see if any event dataset can be used 
        ),
        

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
<<<<<<< HEAD
        
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

    
=======
        if(input$dataset == "FeFo"){
            datasetID <- "c47f13c1-7427-45a0-9f12-237aad351040"
            dataset <- RJSONIO::fromJSON(paste0("http://api.gbif.org/v1/dataset/",datasetID,"/endpoint"))
            endpoint_url <- dataset[[1]]$url 
            
            download.file(endpoint_url, destfile="data/temp.zip", mode="wb")
            unzip ("data/temp.zip", exdir = "data")  
            my_dat1 <- read.csv("data/event.txt", sep="\t", encoding = "UTF-8") %>% select(-id)
            my_dat2 <- read.csv("data/occurrence.txt", sep="\t", encoding = "UTF-8") %>% select(-id) 
            meta <- read_xml("data/eml.xml") %>% as_list() 
            output$table1 <- DT::renderDataTable(my_dat1)
            output$table2 <- DT::renderDataTable(my_dat2)
            output$DOI1 <- renderText(attr(meta$eml$additionalMetadata$metadata$gbif$citation,"identifier"))
            output$gbif_citation <- renderText(meta$eml$additionalMetadata$metadata$gbif$citation[[1]])        
            output$list_files <- renderTable(as.matrix(dir("data", pattern="\\.txt$"), ncol=1))
        
        }
        if(input$dataset=="Statskog"){
            datasetID <- "6a948a1c-7e23-4d99-b1c1-ec578d0d3159"
            dataset <- RJSONIO::fromJSON(paste0("http://api.gbif.org/v1/dataset/",datasetID,"/endpoint"))
            endpoint_url <- dataset[[1]]$url 
            
            
            download.file(endpoint_url, destfile="data/temp.zip", mode="wb")
            unzip ("data/temp.zip", exdir = "data") 
            my_dat1 <- read.csv("data/event.txt", sep="\t", encoding = "UTF-8") %>% select(-id)
            my_dat2 <- read.csv("data/occurrence.txt", sep="\t", encoding = "UTF-8") %>% select(-id) 
            meta <- read_xml("data/eml.xml") %>% as_list() 
            output$table1 <- DT::renderDataTable(my_dat1)
            output$table2 <- DT::renderDataTable(my_dat2)
            output$DOI1 <- renderText(attr(meta$eml$additionalMetadata$metadata$gbif$citation,"identifier"))
            output$gbif_citation <- renderText(meta$eml$additionalMetadata$metadata$gbif$citation[[1]])        
            output$list_files <- renderTable(as.matrix(dir("data", pattern="\\.txt$"), ncol=1))
           
        }
        if(input$dataset=="Fjellstyrene"){
            datasetID <- "b49a2978-0e30-4748-a99f-9301d17ae119"
            dataset <- RJSONIO::fromJSON(paste0("http://api.gbif.org/v1/dataset/",datasetID,"/endpoint"))
            endpoint_url <- dataset[[1]]$url 
            
            download.file(endpoint_url, destfile="data/temp.zip", mode="wb")
            unzip ("data/temp.zip", exdir = "data")
            my_dat1 <- read.csv("data/event.txt", sep="\t", encoding = "UTF-8") %>% select(-id)
            my_dat2 <- read.csv("data/occurrence.txt", sep="\t", encoding = "UTF-8") %>% select(-id) 
            meta <- read_xml("data/eml.xml") %>% as_list() 
            output$table1 <- DT::renderDataTable(my_dat1)
            output$table2 <- DT::renderDataTable(my_dat2)
            output$DOI1 <- renderText(attr(meta$eml$additionalMetadata$metadata$gbif$citation,"identifier"))
            output$gbif_citation <- renderText(meta$eml$additionalMetadata$metadata$gbif$citation[[1]])        
            output$list_files <- renderTable(as.matrix(dir("data", pattern="\\.txt$"), ncol=1))
            
        }
        if(input$dataset=="Other"){
            dataset <- RJSONIO::fromJSON(paste0("http://api.gbif.org/v1/dataset/",input$ID,"/endpoint"))
            endpoint_url <- dataset[[1]]$url 
            
            download.file(endpoint_url, destfile="data/temp.zip", mode="wb")
            unzip ("data/temp.zip", exdir = "data")
            my_dat1 <- read.csv("data/event.txt", sep="\t", encoding = "UTF-8") %>% select(-id)
            my_dat2 <- read.csv("data/occurrence.txt", sep="\t", encoding = "UTF-8") %>% select(-id) 
            meta <- read_xml("data/eml.xml") %>% as_list() 
            output$table1 <- DT::renderDataTable(my_dat1)
            output$table2 <- DT::renderDataTable(my_dat2)
            output$DOI1 <- renderText(attr(meta$eml$additionalMetadata$metadata$gbif$citation,"identifier"))
            output$gbif_citation <- renderText(meta$eml$additionalMetadata$metadata$gbif$citation[[1]])        
            output$list_files <- renderTable(as.matrix(dir("data", pattern="\\.txt$"), ncol=1))
            
            
        }
                })
    
    plotdata<-eventReactive(input$run,{
        plotdata<-read.csv("data/event.txt", sep="\t", encoding = "UTF-8")
        plotdata%>%select(-id)->plotdata})
    
    
   #                         })})
   #  
    
    ##

    
    output$mymap <- renderLeaflet({ #this is very slow - need to improve the plotdata part
        plotdata() %>%
            dplyr::select(decimalLatitude, decimalLongitude) %>%
            drop_na() -> plot.dat
        leaflet() %>%
            addProviderTiles(providers$Esri.NatGeoWorldMap,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>%
            addMarkers(data=plot.dat,lng = ~decimalLongitude, lat=~decimalLatitude)
    })

    ##
    output$dataviz1 <- renderPlot({
        dat <- data.frame(plotdata())
        vis_guess(dat)

    })

>>>>>>> b4610663f2ec70ce7992ca801335975d5d6d7ae9
    
}

# Run the application 
shinyApp(ui = ui, server = server)

