#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(leaflet)
library(geojsonio)
library(DT)
library(sp)
library(markdown)

# Define UI for application that draws a histogram
ui <- navbarPage("Welcome :)",
             tabPanel("Geojson > CSV",
              shinythemes::themeSelector() ,  # <--- Add this somewhere in the UI
   # Show a plot of the generated distribution
   sidebarLayout(
     sidebarPanel(
     radioButtons("radio", h3("Choix de ma source de données")
                  , choices = list(  "Depuis un fichier GeoJSON" = 2
                  , "Autre (a venir)" = 1)
                  , selected = 2),
     
     tags$hr(),#Ligne
     conditionalPanel(
       condition = "input.radio == 2",
       fileInput(   "file1"
                  , label ='monGeojson'
                  , buttonLabel =  "Rechercher..."
                  , placeholder = "Pas de fichier pour le moment"
                  , accept = c(
                    "text/csv",
                    "text/comma-separated-values,text/plain",
                    ".csv",
                    ".geojson"
                    )
         )),
     
     conditionalPanel(
       condition = "input.radio == 1"
       , textInput("url1"
                 , "URL"
                 , value = "Depuis une url"
                 , width = NULL
                 , placeholder = NULL)
     )
    

     , tags$hr()#Ligne
     , uiOutput(outputId = "checkbox"
                , outputArgs = character()
                , quoted=TRUE
                , inline=TRUE)
     
     , conditionalPanel("output.fileUploaded === true"
                        , tags$hr()
                        , h3("Telechargement")
                        , uiOutput("downloadData"))
     
     ),
  
  mainPanel(
      leafletOutput('mymap')
    , conditionalPanel(  "output.fileUploaded === true"
                       , dataTableOutput('table')
                       )
      )
))
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    #version fichier  a telecharger
  
    output$download_item <- downloadHandler(
      filename = function() {
        paste("data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        data=data()
        data= data[, input$checkbox]
        temp=states()
        data=as.data.frame(cbind(data, coordinates(temp)))
        
        write.csv(data, file)
      }
      
    )
    
    #donload button
    output$downloadData <-renderUI({
      if(!is.null(input$checkbox)) {
        downloadButton('download_item', 'Download Output File')
      }
    })
    outputOptions(output, 'downloadData', suspendWhenHidden=FALSE)
    
    #partie donnees
    data = reactive({
      req(states())
      df = states()
      df = df@data
      data = as.data.frame(df)
      data
    })
    
    #Lecture du fichier geo
    states <- reactive({
      req(input$file1)
      
      states=geojsonio::geojson_read(input$file1$datapath, what='sp')
      states
      
      })
    

    output$table <- DT::renderDataTable({

      data_complete =       DT::datatable(  data()
                    , options = list(searching=TRUE)
                    , rownames= FALSE
                    )
    })
    
    
    #Choix des variables
    output$checkbox <- renderUI({
      choice <-  colnames(data())
      selectInput( inputId = "checkbox"
                     ,"Suppression des variables inutiles"
                     , choices = choice[1:length(choice)]
                     , multiple = TRUE, selectize = TRUE
                     , selected =  choice[1:length(choice)]
                     )
    })
    
    outputOptions(output, 'checkbox', suspendWhenHidden=FALSE)

    
    #Test pour voir si le fichier est uploaded
    output$fileUploaded <- reactive({
      return(nrow(data())>0)
    })
    
    outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
    
    #function map
    map = reactive({
      
      req(states())
      
      map=states() %>%
        leaflet() %>%
        # Base groups
        addTiles(group = "OSM (default)") %>%
        addProviderTiles(providers$Stamen.Toner, group = "Toner") %>%
        addProviderTiles(providers$Stamen.TonerLite, group = "Toner Lite") %>%
        # Layers control
        addLayersControl(
          baseGroups = c("OSM (default)", "Toner", "Toner Lite"),
          options = layersControlOptions(collapsed = TRUE)
        )
      
      if(class(states())[1]=="SpatialPointsDataFrame"){
        map = map %>% 
          addCircleMarkers()
        }
      if(class(states())[1]=="SpatialPolygonsDataFrame"){
        map = map %>% 
          addPolygons()
      }
      map
      
      })
    
    output$mymap <- withProgress(message = 'Making plot', value = 0, {renderLeaflet({map()})})

}

# Run the application 
shinyApp(ui = ui, server = server)