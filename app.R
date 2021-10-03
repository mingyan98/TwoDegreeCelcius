## NASA Space Apps 2021 
## Warning: Things Are Heating Up !
## Team: TwoDegreeCelcius 
## Team Members: Yap Ming Yan
## Project: Extreme Heat Risk Information System (EHRIS) 
library(shiny)
library(tmap)
library(tmaptools)
library(leaflet)
library(RColorBrewer)
library(rgdal)
library(rgeos)
library(sf)
library(readr)

# datasets
Droughtmap = readOGR(dsn = "dataset/USDM_20210928_M", layer = "USDM_20210928")
intensity_vars = c("D0 (Abnormally Dry)", 
                   "D1 (Moderate Drought)", 
                   "D2 (Severe Drought)", 
                   "D3 (Extreme Drought)", 
                   "D4 (Exceptional Drought)")
Droughtmap@data$INTENSITY = intensity_vars

city_map = readOGR(dsn = "dataset/USA_Major_Cities", layer = "USA_Major_Cities")
city_subset_map = subset(city_map, city_map@data$CAPITAL == "National" | city_map@data$CAPITAL == "State")
City = city_subset_map[1:5,]
City@data$RISK = c("High Risk", "Moderate Risk", "Low Risk", "Moderate Risk", "High Risk")
City@data$RISK_TYPE = c("forest fire", "air pollution", "forest fire", "air pollution", "crop damage")


graph <- read_csv("dataset/graph.csv")
graph_year = graph$Year

level = c("Individual", "City", "City")
mitigation = c("Drink more water",
               "Building orientation,height and spacing", 
               "Solar, wind, and wave energy")
outcomes = c("Reduce likelyhood of fatigue",
             "Reduced need for conventional airconditioning",
             "Reduction of risks of widespread power loss or peak power loads under storm events and temperature extremes")
mitigation_city_level = data.frame("Level" =level,
                                   "Practical measures by sector" = mitigation,
                                   "Expected outcome" = outcomes)

# ui
ui = fluidPage(
   navbarPage(
       "Extreme Heat Risk Information System (EHRIS)",
       tabPanel("Map",
                div (class = "outer",
                     tags$style(type = "text/css", ".outer {position: fixed; top: 41px; left: 0; right: 0; bottom: 0; overflow: hidden; padding: 0}"),
                     tmapOutput("map", width = "100%", height = "100%"),
                     absolutePanel(id = "controls", 
                                   fixed = TRUE,
                                   draggable = TRUE,
                                   top = 75, left = 55, 
                                   width = 250,
                                   height = "auto",
                                  
                                   h4("Location: US" ),
                                   Sys.time(),
                                   h5("Instructions:"),
                                   
                                   helpText("Click on the layer icon to select map options")
                                   )
                     )
                ),
       tabPanel("Data",
                helpText("City data"),
                DT::dataTableOutput("cityDataTable")),
       tabPanel("Graph",
                plotOutput("graph"),
                helpText("Source: NASA/GISS/GISTEMP v4"),
                p("The Global temperature across land and ocean are growing at a increasing trends!")
                ),
       tabPanel("Measures",
                h1("Mitigation Measures"),
                helpText("The mitigation measures is categorize between individual level and city level"),
                DT::dataTableOutput("cityMitigationTable"),
                helpText("Some of the measures are adopted from source: Urban Climate Change Research Network")
                )
       
   )
)

# server
server = function(input,output){
    output$map = renderTmap({
        tm_shape(Droughtmap) +
            tm_fill("INTENSITY",
                    title = "Heat intensity",
                    label = intensity_vars,
                    palette = rev(heat.colors(5)),
                    id = "INTENSITY",
                    popup.vars = FALSE) +
            tm_borders(lwd = .5) +
            tm_view(set.view = 5) +
            tmap_options(check.and.fix = TRUE) +
            tm_shape(City) +
            tm_dots(
                col = "RISK",
                palette = c("High Risk" = "red", "Moderate Risk" = "yellow", "Low Risk" = "green"),
                id = "NAME",
                size = 0.1,
                title = "Risk Indicator",
                popup.vars = c("NAME", "CLASS", "ST", "POPULATION", "RISK", "RISK_TYPE"),
                ) +
            tm_text("NAME")
    })
    
    output$cityDataTable = DT::renderDataTable({
        DT::datatable(City@data[,c(1, 2, 3, 4, 7, 9, 50, 51)])
    })
    
    output$graph = renderPlot({
            plot(graph_year, graph$Land_Annual, type = "l",
                 col = "orange", lwd = 1,
                 xlim = c(1880,2021), ylim = c(-1,2),
                 xlab = "Year", ylab = "Temperature Anomaly w.r.t. 1951-80 (°C)",  
                 main = "Temperature Anomalies over Land and over Ocean")
            legend("topleft", 
                   c("Land Surface Air Temperature",
                     "Land Lowess Smoothing",
                     "Sea Surface Water Temperature",
                     "Sea Lowess Smoothing"),
                   lty = c(1,1,1,1),
                   pch = c(22, NA, 22, NA),
                   col = c("orange", "red", "lightblue","blue"),
                   lwd = c(1,3,1,3)
            )
            points(graph_year, graph$Land_Annual, pch = 22, bg = "orange", col = "black")
            lines(graph_year, graph$Ocean_Annual, type = "l", col = "lightblue", lwd = 1,)
            points(graph_year, graph$Ocean_Annual, pch = 22, bg = "lightblue", col = "black")
            lines(graph_year, graph$`Lowess(5)...3`, pch = 0, col = "red", lwd = 3,)
            lines(graph_year, graph$`Lowess(5)...5`, pch = 0, col = "blue", lwd = 3,)
    })
    
    output$cityMitigationTable = DT::renderDataTable({
        DT::datatable(mitigation_city_level)
    })
}

shinyApp(ui = ui, server = server)
# end of program