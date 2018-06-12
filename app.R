# Run Shiny App by clicking "Run App"

source("realestateyvr.R")

library(sp)
library(maptools)
library(RColorBrewer)
library(rgdal)
library(classInt)
library(raster)
library(shinydashboard)
library(shiny)
library(shinyjs)
library(magrittr)
library(rmarkdown)
library(mapview)
library(htmltools)
library(htmlwidgets)
library(rsconnect)
library(webshot)

#rsconnect::deployApp('~/Desktop/realestateyvr/', account = 'rayas') <- Shiny Web App Launch

getwd


ui <-tabsetPanel(
    tabPanel("2016",fluidPage(theme = "bootstrap.css",
  
 h1("2016 Housing Landscape",  align = "center"),
 
leafletOutput("map1", height = "700px", width = "100%"),
p("Made with love by Soraiya Salemohamed", align = "center", style="color:#e95420BF; padding:15px"),
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 60, left = 20, right = 20, bottom = "auto",
                width = 330, height = "auto", style='padding:15px',
                #Drop down of Variables in Shapefile
                selectInput("variable", "Pick a Theme",
                            names(CTVan16@data)[11:16]),
                #Colour options from colorbrewer
                selectInput("colourbrewerpalette1", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                #Interval options
                selectInput("classIntStyle1", "Interval Style",
                            c("Jenks Natural Breaks" = "jenks",
                              "Quantile" = "quantile",
                              "Equal Interval" = "equal",
                              "Pretty" = "pretty")),
                #Plot histogram
                plotOutput(outputId = "distPlot")
                
    
                

          
      )
    )
  ),

  tabPanel("2011",fluidPage(theme = "bootstrap.css",
                            h1("2011 Housing Landscape", align = "center"),
                            leafletOutput("map2", height = "700px", width = "100%"),
                            p("Made with love by Soraiya Salemohamed", align = "center", style="color:#e95420BF; padding:15px"),
                            absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                          draggable = TRUE, top = 60, left = 20, right = 20, bottom = "auto",
                                          width = 330, height = "auto", style='padding:15px',
                                          #Drop down of Variables in Shapefile
                                          selectInput("variable2", "Pick a Theme",
                                                      names(CTVan11@data)[11:16]),
                                          #Colour options from colorbrewer
                                          selectInput("colourbrewerpalette2", "Color Scheme",
                                                      rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                          ),
                                          #Interval options
                                          selectInput("classIntStyle2", "Interval Style",
                                                      c("Jenks Natural Breaks" = "jenks",
                                                        "Quantile" = "quantile",
                                                        "Equal Interval" = "equal",
                                                        "Pretty" = "pretty")),
                                          #Plot histogram
                                          plotOutput(outputId = "distPlot2")
                                          
                            ))),

tabPanel("2006",fluidPage(theme = "bootstrap.css",
                          h1("2006 Housing Landscape", align = "center"),
                          leafletOutput("map3", height = "700px", width = "100%"),
                          p("Made with love by Soraiya Salemohamed", align = "center", style="color: #e95420BF; padding:15px"),
                          absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                                        draggable = TRUE, top = 60, left = 20, right = 20, bottom = "auto",
                                        width = 330, height = "auto", style='padding:15px',
                                        #Drop down of Variables in Shapefile
                                        selectInput("variable3", "Pick a Theme",
                                                    names(CTVan06@data)[6:11]),
                                        #Colour options from colorbrewer
                                        selectInput("colourbrewerpalette3", "Color Scheme",
                                                    rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                                        ),
                                        #Interval options
                                        selectInput("classIntStyle3", "Interval Style",
                                                    c("Jenks Natural Breaks" = "jenks",
                                                      "Quantile" = "quantile",
                                                      "Equal Interval" = "equal",
                                                      "Pretty" = "pretty")),
                                        #Plot histogram
                                        plotOutput(outputId = "distPlot3")
                                        
                          ))),
#About Page
tabPanel("About",fluidPage(theme = "bootstrap.css",
                          h4("Background", align = "left"),
                          p("Vancouver is grappling with a housing crisis. According to a recent study made by Point2Homes, a real estate firm, Vancouver ranks the most unaffordable city in North America – challenging cities like Manhattan and San Francisco (Tierney, 2017). The average detached house in Vancouver is $1.831 million (Globe and Mail, 2017)."),
                          p("Exorbitant prices are forcing families to move to the outskirts, resulting in an urban sprawl as more citizens are moving to the periphery. Housing prices are impacting everyone, particularly the younger generation. According to the 2016 Canadian Census, one in three citizens aged 20-34 years old live at home with their parents."),
                          p("The Vancouver Housing Affordability tool aims to illustrate the city’s residential landscape over the past 10 years using Canadian Census data. It provides two functions: displays housing trends from 2006 to 2016 and calculates how long it would take the average Vancouverite to save in order to pay for the minimum down payment in each neighbourhood (Census Tract), also referred as the housing affordability index. Depending on the user’s variable choice, a choropleth map will be generated."),  
p("The formula for Years Needed to Save to Pay for Down Payment = (Mean Dwelling Cost x Down Payment) / (Mean Income x 0.15 )"),  
                          p("0.15 = The average savings a person is willing to save per year"),
                          p("Down payment varies on residential value"),
                          p("Have any questions or suggestions? You can reach me at soraiyas@gmail.com")
                        
                            
                            
                          )))




server <- function(input, output, session){
  
  #2016
  
  #Plot Output
  output$distPlot <- renderPlot({
    
    x <- CTVan16@data[[input$variable]]

    hist(x, col = "#e95420BF", border = "white", main = input$variable, xlab=NA, cex.main=0.8)
    
  })
  
  
  #Map

  output$map1 <- renderLeaflet({
   leaflet(CTVan16) %>% addProviderTiles("CartoDB.Positron") %>%
      setView(-123.116226, 49.246292, zoom = 10)
  })
  

  
  #observer for the map elements to redraw
  observe({
    breaks <- classIntervals(CTVan16[[input$variable]], n=5, style=input$classIntStyle1)
    breaks <- unique(breaks$brks)
    
    pal <- colorBin(palette = input$colourbrewerpalette1, 
                    domain = CTVan16@data[[input$variable]]
                    #create bins using the breaks object from earlier
                    
    )
    leafletProxy("map1", data = CTVan16) %>%
      clearShapes() %>% 
      addPolygons(stroke = F, 
                  fillOpacity = 0.5,
                  smoothFactor = 0.5,
                  opacity = 1,
                  color = ~pal(CTVan16@data[[input$variable]]),
                  fillColor = ~pal(CTVan16@data[[input$variable]]),
                  popup = paste("CTUID",CTVan16@data$CTUID, "<br>", "Theme Value:",
                                CTVan16@data[[input$variable]]),
                  popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)
                  
                  
      )
  })
  
  
  #observer for the legend to redraw
  observe({
    breaks <- classIntervals(CTVan16@data[[input$variable]], n=5, style=input$classIntStyle1)
    breaks <- unique(breaks$brks)
    
    pal <- colorBin(palette = input$colourbrewerpalette1, 
                    domain = CTVan16@data[[input$variable]],
                    #create bins using the breaks object from earlier
                    bins = breaks
    )
    
    proxy <- leafletProxy("map1", data = CTVan16)
    proxy %>% clearControls() %>%
      addLegend("topright", 
                pal= pal, 
                values = ~CTVan16@data[[input$variable]], 
                title = input$variable, 
                labFormat = labelFormat(prefix = ""),
                opacity = 1
      )
  })
  
  #2011
  
  #Plot Output
  output$distPlot2 <- renderPlot({
    
    x <- CTVan11@data[[input$variable2]]
    
    hist(x, col = "#e95420BF", border = "white", main = input$variable2, xlab=NA, cex.main=0.8)
    
  })
  
  #Map
  
  
  output$map2 <- renderLeaflet({
    leaflet(CTVan11) %>% addProviderTiles("CartoDB.Positron") %>%
      setView(-123.116226, 49.246292, zoom = 10)
  })
  
  #observer for the map elements to redraw
  observe({
    breaks <- classIntervals(CTVan11[[input$variable2]], n=5, style=input$classIntStyle2)
    breaks <- unique(breaks$brks)
    
    pal <- colorBin(palette = input$colourbrewerpalette2, 
                    domain = CTVan11@data[[input$variable2]]
                    #create bins using the breaks object from earlier
                    
    )
    leafletProxy("map2", data = CTVan11) %>%
      clearShapes() %>% 
      addPolygons(stroke = F, 
                  fillOpacity = 0.5, 
                  smoothFactor = 0.5,
                  opacity = 1,
                  color = ~pal(CTVan11@data[[input$variable2]]),
                  fillColor = ~pal(CTVan11@data[[input$variable2]]),
                  popup = paste("CTUID",CTVan11@data$CTUID, "<br>", "Theme Value:",
                                CTVan11@data[[input$variable2]]),
                  popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)
                  
                  
      )
  })
  
  
  #observer for the legend to redraw
  observe({
    breaks <- classIntervals(CTVan11@data[[input$variable2]], n=5, style=input$classIntStyle2)
    breaks <- unique(breaks$brks)
    
    pal <- colorBin(palette = input$colourbrewerpalette2, 
                    domain = CTVan11@data[[input$variable2]],
                    #create bins using the breaks object from earlier
                    bins = breaks
    )
    
    proxy <- leafletProxy("map2", data = CTVan11)
    proxy %>% clearControls() %>%
      addLegend("topright", 
                pal= pal, 
                values = ~CTVan11@data[[input$variable2]], 
                title = input$variable2, 
                labFormat = labelFormat(prefix = ""),
                opacity = 1
      )
  })
  
  
  #2006
  
  #Plot Output
  output$distPlot3 <- renderPlot({
    
    x <- CTVan06@data[[input$variable3]]
    
    hist(x, col = "#e95420BF", border = "white", main = input$variable3, xlab=NA, cex.main=0.8)
    
  })
  
  #Map
  
  output$map3 <- renderLeaflet({
    leaflet(CTVan06) %>% addProviderTiles("CartoDB.Positron") %>%
      setView(-123.116226, 49.246292, zoom = 10)
  })
  
  #observer for the map elements to redraw
  observe({
    breaks <- classIntervals(CTVan06[[input$variable3]], n=5, style=input$classIntStyle3)
    breaks <- unique(breaks$brks)
    
    pal <- colorBin(palette = input$colourbrewerpalette3, 
                    domain = CTVan06@data[[input$variable3]]
                    #create bins using the breaks object from earlier
                    
    )
    leafletProxy("map3", data = CTVan06) %>%
      clearShapes() %>% 
      addPolygons(stroke = F, 
                  fillOpacity = 0.5, 
                  smoothFactor = 0.5,
                  opacity = 1,
                  color = ~pal(CTVan06@data[[input$variable3]]),
                  fillColor = ~pal(CTVan06@data[[input$variable3]]),
                  popup = paste("CTUID",CTVan06@data$CTUID, "<br>", "Theme Value:",
                                CTVan06@data[[input$variable3]]),
                  popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE)
                  
                  
      )
  })
  
  
  #observer for the legend to redraw
  observe({
    breaks <- classIntervals(CTVan06@data[[input$variable3]], n=5, style=input$classIntStyle3)
    breaks <- unique(breaks$brks)
    
    pal <- colorBin(palette = input$colourbrewerpalette3,
                    domain = CTVan06@data[[input$variable3]],
                    #create bins using the breaks object from earlier
                    bins = breaks
    )
    
    proxy <- leafletProxy("map3", data = CTVan06)
    proxy %>% clearControls() %>%
      addLegend("topright", 
                pal= pal, 
                values = ~CTVan06@data[[input$variable3]], 
                title = input$variable3, 
                labFormat = labelFormat(prefix = ""),
                opacity = 1
      )
  })
  

}




# Run the application 
shinyApp(ui = ui, server = server)


