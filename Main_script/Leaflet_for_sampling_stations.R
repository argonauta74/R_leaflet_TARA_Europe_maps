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

# Load and process climatology and bathymetry data ensuring compatible CRS
chl_clim <- raster(file_paths$path_to_clim, varname = "CHL")
values(chl_clim) <- log10(values(chl_clim))                               # Chl values in log to help pattern visualization on map

r_bathy <- raster(file_paths$path_to_depth, varname = "elevation")
crs(r_bathy) <- CRS(projection(chl_clim))                                 # Ensure matching CRS for bathymetry data
r_bathy <- crop(r_bathy, extent(chl_clim))                                # Crop bathymetry data to match the extent of climatology data
# Reclassify bathymetry data to differentiate depths below 200m
rcl_matrix <- matrix(c(0, Inf, 0, -Inf, -200, NA), ncol=3, byrow=TRUE)
r_bathy <- reclassify(r_bathy, rcl_matrix, right=TRUE)

# Function to create color palettes for data visualization
createColorPalette <- function(scheme, bias, paletteFunction, domain, bins = NULL, reverse = FALSE) {
  plt <- GetColors(256, start = 0, end = 1, bias = bias, scheme = scheme)
  if (!is.null(bins)) {
    # For discrete color bins
    return(paletteFunction(palette = plt, domain = domain, bins = bins, na.color = "transparent", reverse = reverse))
  } else {
    # For continuous color scales
    return(paletteFunction(palette = plt, domain = domain, na.color = "transparent"))
  }
}

# Set color palettes for chlorophyll and bathymetry data visualization
domain_chl_clim <- range(values(chl_clim), na.rm = TRUE)
pal1 <- createColorPalette(scheme = "jet", bias = 0.75, paletteFunction = colorNumeric, domain = domain_chl_clim)

domain_r_bathy <- range(values(r_bathy), na.rm = TRUE)
pal2 <- createColorPalette(scheme = "drywet", bias = 1, paletteFunction = colorBin, domain = domain_r_bathy, bins = 10, reverse = TRUE)

# Define a function to determine marker color based on station type
getColor <- function(station_type) {
  if(station_type == "TARA") {
    "green"
  } else if(station_type == "TREC") {
    "orange"
  } else {
    "red"
  }
}

# Set a custom CRS for the Leaflet map
customCRS <- leafletCRS(proj4def = "+proj=longlat +datum=WGS84 +ellps=WGS84 +no_defs")

# --------------------------------------------------------------------------------------------------
# Create Leaflet map

# Initialize the Leaflet map with custom CRS, add base tiles, and configure various layers and controls
m <- leaflet(data = station_lat_lon, options = leafletOptions(crs = customCRS)) %>%
  addProviderTiles(providers$OpenStreetMap.France, options = providerTileOptions(minZoom = 3, maxZoom = 18, detectRetina = TRUE)) %>%
  fitBounds(extent(chl_clim)[1], extent(chl_clim)[4], extent(chl_clim)[2], extent(chl_clim)[3]) %>%
  addMouseCoordinates() %>% 
  addControl(title_map, position = "bottomright") %>%
  addSimpleGraticule(interval = 1) %>%
  # Layer 1
  addRasterImage(chl_clim, colors = pal1, project = TRUE, opacity = 1, group = "Chla") %>%
  addLegend("bottomright", pal = pal1, opacity = 1, group = "Chla", values = values(chl_clim),
            labFormat = labelFormat(transform = function(x) round(10^x, 2)), title = "Chla (mg/m³)") %>%
  # Layer 2                                  
  addRasterImage(r_bathy, colors = pal2, project = TRUE, opacity = 1, group = "Bathy") %>%
  addLegend("bottomright", pal = pal2, opacity = 1, group = "Bathy", values = values(r_bathy),
            labFormat = labelFormat(transform = function(x) round(x, 1)), title = "Depth (m)") %>%
  addLayersControl(position = "topleft", overlayGroups = c("Chla", "Bathy"), options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Chla", "Bathy"))

# Loop through each station row and add markers with appropriate icons and popups
for(i in 1:nrow(station_lat_lon)) {
  m <- m %>%
    addAwesomeMarkers(lng = station_lat_lon$Lon[i], lat = station_lat_lon$Lat[i], popup = content[i],
                      icon = awesomeIcons(icon = 'flag', iconColor = 'black', library = 'ion', 
                                          markerColor = getColor(station_lat_lon$station_type[i])))
}

# Add additional map features such as scale bar, mini map, measuring tools, reset map button, and GPS control
m <- m %>%
  addScaleBar(position = "bottomleft") %>%
  addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE, minimized = FALSE) %>%
  addMeasure(position = "bottomleft", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", 
             activeColor = "darkgreen", completedColor = "red") %>%
  addResetMapButton() %>%
  addControlGPS(options = gpsOptions(position = "bottomleft", activate = TRUE, autoCenter = TRUE, 
                                     maxZoom = 18, setView = TRUE))

# -------------------------------------------------------------------------------------------------
# Display and Save Map

# Display the map in RStudio's Viewer
print(m)

# Save the map to an HTML file with error handling for file overwrite
save_leaflet <- function(map, file, overwrite = TRUE) {
  if (!file.exists(file) || overwrite) {
    htmlwidgets::saveWidget(map, file, selfcontained = FALSE)
  } else {
    message("File already exists and 'overwrite' == FALSE. Nothing saved to file.")
  }
}

save_leaflet(m, path_output)

# -------------------------------------------------------------------------------------------------
# End of the R script
