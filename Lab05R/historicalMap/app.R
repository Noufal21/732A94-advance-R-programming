#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Lab05R)
# Define UI for application that draws a histogram
ui <- fluidPage(
    
    # Application title
    titlePanel("World map from 1946-2014"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Range:",
                        min = 1946,
                        max = 2014,
                        value = 100)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("distPlot","100%",800)
        )
    ),
    titlePanel("World map from 1900-2014"),
    sidebarLayout(
        sidebarPanel(
            selectInput("dataSource", "Countries:",
                        c("Finland" = "fi-8",
                          "Switzerland" = "ch-8",
                          "Norway" = "no-4",
                          "Denmark" = "dk-7",
                          "Sweden" = "se-4",
                          "United State"="us-4",
                          "Greenland"="gl-7"
                        )),
            uiOutput("myslider")
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            leafletOutput("map2","100%",800)
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    object = Lab05R::thenmapsApi$new()
    output$distPlot <- renderLeaflet({
        geoJson_world = object$worldmap(input$bins) 
        
        leaflet(options = leafletOptions(minZoom = 2, maxZoom = 18))  %>%
            addProviderTiles(providers$BasemapAT) %>% addProviderTiles(providers$CartoDB)  %>%
            addProviderTiles(providers$Hydda.Base)%>%
            addGeoJSON(geoJson_world, weight = 1, color = "#444444",fill = FALSE)
        
    })
    output$myslider = renderUI({
        min = 1900
        max = 2014
        if(input$dataSource == "dk-7")
        {
            min = 1971
        }
        if(input$dataSource == "gl-7")
        {
            min = 1975
        }
        sliderInput("swed","Range:",min = min,max = 2014,value = 100)
    })
    
    output$map2 <- renderLeaflet({
        
        topoData_swed = object$multiData(input$swed,input$dataSource) 
        
        leaflet(options = leafletOptions(minZoom = 2, maxZoom = 18)) %>% addTiles() %>% setView(lat=object$center_cor[[input$dataSource]][1],lng=object$center_cor[[input$dataSource]][2], zoom = object$center_cor[[input$dataSource]][3]) %>%
            addProviderTiles(providers$BasemapAT) %>% addProviderTiles(providers$CartoDB) %>% addProviderTiles(providers$Hydda.Base)%>% 
            addGeoJSON(topoData_swed, weight = 1, color = "#444444",fill = FALSE)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
