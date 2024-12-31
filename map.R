library(sf)
library(ggplot2)
library(leaflet)
library(htmltools)
setwd("D:/dataset/mendeley/inveschar/pm6ks8k59x-1/pm6ks8k59x-1")

geo_data <- st_read("TAMIL NADU_STATE.geojson")

ggplot(data = geo_data) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Map from GeoJSON Data")

str(geo_data)

head(geo_data)

m <- leaflet(geo_data) |> 
  setView(lng = 78, lat = 10, zoom = 5) |> 
  addProviderTiles("CartoDB.Positron") |> 
  addPolygons(
    fillColor = "lightgreen",  # Use a single color for all states
    weight = 1,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlightOptions = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE),
    label = ~STNAME,  # Show state names on hover
    labelOptions = labelOptions(
      style = list("font-weight" = "normal", padding = "3px 8px"),
      textsize = "15px",
      direction = "auto")
  )

m

install.packages("rmarkdown")
