# ------------------------------------------------------------------------------------------------------------------------------------------
# Script Description: 
# This script generates an interactive leaflet map to facilitate the visualization of sampling stations for specific ocean areas. 
# Colored markers denote each station and include pop-up windows displaying basic station-specific (e.g., lat, lon, date, etc.) information. 
# The leaflet integrates multiple layers (i.e., environmental parameters), such as bathymetry and chlorophyll satellite data; 
# however, other layers can be added according to the user's needs. 
# The map features include a scale bar, mini-map, measuring tools, a reset map button, and GPS control.
# Script licensed under the GNU General Public License (GPL) v3.0 
#
# -------------------------------------------------------------------------------------------------------------------------------------------

# Restart R session if needed
#.rs.restartR()

# Function to clear all graphical devices
close_graphics_devices <- function() {
  while (dev.cur() > 1) dev.off()
}

# Function to remove all workspace objects and invoke garbage collection
remove_workspace_objects <- function() {
  rm(list = ls(), envir = globalenv())
  gc()  # Invoke garbage collection
}

# Execute environment reset functions
close_graphics_devices()
remove_workspace_objects()

# -------------------------------------------------------------------------------------------------
# Load Required Libraries

# Use 'librarian' for managing package loading and installation.
if (!require(librarian)) install.packages("librarian")
librarian::shelf(dplyr, sf, terra, readxl, leaflet, leafem, leaflet.extras, htmltools, cmocean, glue)

# -------------------------------------------------------------------------------------------------
# Define File Paths

# Set file paths for input data excluding the output file
file_paths <- list(
  path_aux_file = "/path/to/station_lat_lon.xlsx",    # Files with info regarding stations (e.g., station number, lat, lon, sampling dates, etc.) 
  path_to_clim = "/path/to/chl_clim.nc",              # Chlorophyll-a NetCDF file (climatology or NRT image) 
  path_to_depth = "/path/to/depth.nc"                 # Bathymetry NetCDF file 
)

# Ensure all input files exist; stop execution if any are missing
stopifnot(all(file.exists(unlist(file_paths))))

# Define the output file path separately
path_output <- "/path/to/output_map.html"             # Set the path for saving the leaflet HTML file

# -------------------------------------------------------------------------------------------------
# Data Processing

# Read and preprocess info on stations, create the "position" column
station_lat_lon <- read_excel(file_paths$path_aux_file, col_types = c("numeric", "numeric", "numeric", "text", "text", "text")) %>%
  mutate(position = sprintf("[LAT:%.4f; LON:%.4f]", Lat, Lon))

# General map information, structured for readability (fill with the info you need) 
title_map <- tags$div(HTML('
  <div>
    <strong>Station Information</strong><br>
    <span>● GREEN icons: TARA stations</span><br>
    <span>● ORANGE icons: TREC stations</span><br>
    ... [Additional Information] ...
  </div>
'))

# Prepare station content for popups combining different columns
content <- paste(sep = "<br/>", station_lat_lon$date, station_lat_lon$popup, station_lat_lon$position)

# Load and process climatology and bathymetry data, ensuring compatible CRS
chl_clim <- terra::rast(file_paths$path_to_clim, subds  = "CHL")
chl_clim <- log10(chl_clim)   # Chl values in log to help pattern visualization on the map

r_bathy <- terra::rast(file_paths$path_to_depth, subds = "elevation")
terra::crs(r_bathy) <- terra::crs(chl_clim)             # Ensure matching CRS for bathymetry data
r_bathy <- terra::crop(r_bathy, terra::ext(chl_clim))   # Crop bathymetry data to match the extent of climatology data
# Reclassify bathymetry data to differentiate depths below 200m
r_bathy <- terra::classify(r_bathy, rcl = matrix(c(0, Inf, 0, -Inf, -200, NA), ncol=3, byrow=TRUE))

pal1 <- colorNumeric(cmocean("algae")(256), domain = range(values(chl_clim, na.rm = TRUE)), na.color = "transparent")
pal2 <- colorBin(cmocean("deep", direction = -1)(256), domain = range(values(r_bathy, na.rm = TRUE)), bins = 10, na.color = "transparent")

# Create the colour mapping for station types
station_lat_lon <- station_lat_lon %>%
  mutate(
    color = case_when(
      station_type == "TARA" ~ "green",
      station_type == "TREC" ~ "orange",
      TRUE ~ "red"  # Default to red for other types
    ),
    content = glue::glue("<b>{popup}</b> <br/>Location: [LAT:{round(Lat, 3)}; LON:{round(Lon, 3)}]")
  )

# --------------------------------------------------------------------------------------------------
# Create a Leaflet map

# Set a custom CRS for the Leaflet map
customCRS <- leafletCRS(proj4def = "+proj=longlat +datum=WGS84 +no_defs")

# Calculate the centroid of the stations to set the View on the area of interest
mean_lat <- mean(station_lat_lon$Lat, na.rm = TRUE)
mean_lon <- mean(station_lat_lon$Lon, na.rm = TRUE)

# Create the leaflet map
m <- leaflet(data = station_lat_lon, options = leafletOptions(crs = customCRS)) %>%
  addProviderTiles(providers$OpenStreetMap.France, options = providerTileOptions(minZoom = 3, maxZoom = 18, detectRetina = TRUE)) %>%
  setView(lng = mean_lon, lat = mean_lat, zoom = 8) %>%
  addMouseCoordinates() %>%
  addControl(title_map, position = "bottomright") %>%
  addSimpleGraticule(interval = 1) %>%
  
  # Layer 1: Chlorophyll data
  addRasterImage(chl_clim, colors = pal1, project = FALSE, opacity = 1, group = "Chla") %>%
  addLegend("bottomright", pal = pal1, opacity = 1, group = "Chla", values = terra::values(chl_clim),
            labFormat = labelFormat(transform = function(x) round(10^x, 3)), title = "Chla (mg/m³)") %>%
  
  # Layer 2: Bathymetry data
  addRasterImage(r_bathy, colors = pal2, project = FALSE, opacity = 1, group = "Bathy") %>%
  addLegend("bottomright", pal = pal2, opacity = 1, group = "Bathy", values = terra::values(r_bathy),
            labFormat = labelFormat(transform = function(x) round(x, 1)), title = "Depth (m)") %>%
  addLayersControl(position = "topleft", overlayGroups = c("Chla", "Bathy"), options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Chla", "Bathy")) %>%
  
  # Add awesome markers with colour assignment based on station type
  addAwesomeMarkers(
    data = station_lat_lon,
    popup = ~content,
    icon = awesomeIcons(
      icon = 'flag', iconColor = 'black', library = 'ion', 
      markerColor = ~color  # Dynamically assign marker color
    )
  ) %>%
  
  # Add map controls
  addScaleBar(position = "bottomleft") %>%
  addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE, minimized = FALSE) %>%
  addMeasure(position = "bottomleft", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", 
             activeColor = "darkgreen", completedColor = "red") %>%
  addResetMapButton() %>%
  addControlGPS(options = gpsOptions(position = "bottomleft", activate = TRUE, autoCenter = TRUE, 
                                     maxZoom = 18, setView = TRUE))

# Display the map in RStudio's Viewer
print(m)

# -------------------------------------------------------------------------------------------------
# Save the map

# Save the map to an HTML file with error handling for file overwrite
save_leaflet <- function(map, file, overwrite = TRUE) {
  if (!file.exists(file) || overwrite) {
    htmlwidgets::saveWidget(map, file, selfcontained = FALSE)
  } else {
    message("File already exists and 'overwrite' == FALSE. Nothing saved to file.")
  }
}

# Save the map to the defined output path
save_leaflet(m, path_output)

# Open the saved map in a browser if needed
browseURL(path_output)

# -------------------------------------------------------------------------------------------------
# End of the R script
