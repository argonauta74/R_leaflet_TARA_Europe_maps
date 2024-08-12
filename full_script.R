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

# Uses 'pacman' for managing package loading and installation.
if (!require(librarian)) install.packages("librarian")
librarian::shelf(
  cmocean, dplyr, glue, here, htmltools, htmlwidgets, leafem, leaflet, leaflet.extras, readr, sf, terra)

# -------------------------------------------------------------------------------------------------
# Define File Paths

# Download from NOAA the specific NetCDF file
url_sst <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/202405/oisst-avhrr-v02r01.20240515.nc"
sst_nc     <- file.path("data", basename(url_sst))
if (!file.exists(sst_nc)) download.file(url_sst, sst_nc)

# Download Chlorophyll-a NetCDF file (climatology or NRT image) , if needed
chl_url <- "https://somesite.nasa.gov/chl.nc"                # TODO: update w/ valid URL
chl_nc  <- here(file.path("data", basename(chl_url)))
chl_nc  <- sst_nc                                            # TODO: remove this fake path setting
if (!file.exists(chl_nc)) download.file(chl_url, chl_nc)

# Download Bathymetry NetCDF file, if needed
depth_url <- "https://somesite.noaa.gov/depth.nc"            # TODO: update w/ valid URL
depth_nc  <- here(file.path("data", basename(depth_url)))
depth_nc <- sst_nc                                           # TODO: remove this fake path setting
if (!file.exists(chl_nc)) download.file(depth_url, depth_nc)

# Stations CSV file with info regarding stations (e.g., station number, lat, lon, sampling dates, etc.) 
stations_csv = here("data/random_stations.csv")

# Ensure all input files exist; stop execution if any are missing
stopifnot(all(file.exists(c(chl_nc, depth_nc))))

# Define the output file path separately
out_html <- here("index.html")             # Set the path for saving the leaflet HTML file

# -------------------------------------------------------------------------------------------------
# Data Processing

# General map information, structured for readability (fill with the info you need) 
title_map <- tags$div(HTML('
  <div>
    <strong>Station Information</strong><br>
    <span>● GREEN icons: TARA stations</span><br>
    <span>● ORANGE icons: TREC stations</span><br>
    ... [Additional Information] ...
  </div>
'))

# Set extent (for raster cropping) and bounding box (for map fitting)
e <- ext(295, 322, 50, 68)                           # extent of Labrador Sea area for raster clipping [0,360]
b <- as.numeric(as.vector(e)) + c(-360, -360, 0, 0)  # shift back to expected [-180,180]

# Load and process climatology and bathymetry data ensuring compatible CRS
r_sst <- rast(sst_nc, subds = "sst") |>
  crop(e) |> 
  projectRasterForLeaflet("bilinear")

# Load and process climatology and bathymetry data ensuring compatible CRS
#r_chl <- rast(chl_nc, subds = "CHL") |> 
r_chl <- rast(chl_nc, subds = "sst") |>      # TODO: update varname to not fake_nc
  crop(e) |> 
  projectRasterForLeaflet("bilinear")
values(r_chl) <- log10(values(r_chl) + 0.00001)          # Chl values in log to help pattern visualization on map

# r_depth <- rast(depth_nc, subds = "elevation") |> 
r_depth <- rast(depth_nc, subds = "sst") |>   # TODO: update varname to not fake_nc
  crop(e) |> 
  projectRasterForLeaflet("bilinear")

# TODO: remove for not fake_nc
values(r_depth) <- scales::rescale(values(r_depth), to = c(-500, 500))

# Reclassify bathymetry data to differentiate depths below 200m
rcl_matrix <- matrix(c(0, Inf, 0, -Inf, -200, NA), ncol=3, byrow=TRUE)
r_depth    <- classify(r_depth, rcl_matrix, right=TRUE)

# Set color palettes for chlorophyll and bathymetry data visualization
pal_sst <- colorNumeric(
  cmocean("balance")(256),
  domain   = range(values(r_sst, na.rm = TRUE)),
  na.color = "transparent")

pal_chl <- colorNumeric(
  cmocean("algae")(256),
  domain   = range(values(r_chl, na.rm = TRUE)),
  na.color = "transparent")

pal_depth <- colorBin(
  cmocean("deep")(256),
  domain   = range(values(r_depth, na.rm = TRUE)),
  na.color = "transparent")

# Stations ----
if (!file.exists(stations_csv)){

  # Create a sample of stations for testing  
  n_pts <- 10
  pts <- mask(!is.na(r_sst), r_sst) |> 
    as.polygons() |> 
    st_as_sf() |> 
    st_sample(size = n_pts) |> 
    st_transform(4326) |>
    st_as_sf() |> 
    rename(geom = 1) |> 
    mutate(
      id   = 1:n_pts,
      type = sample(c("TARA", "TREC", "Other"), n_pts, replace = T),
      lon  = st_coordinates(geom)[, 1],
      lat  = st_coordinates(geom)[, 2])
  
  pts |> 
    st_drop_geometry() |> 
    write_csv(stations_csv)
}

pts <- read_csv(stations_csv, show_col_types = F) |> 
  st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) |> 
  mutate(
    color = case_match(
      type,
      "TARA"  ~ "green",
      "TREC"  ~ "orange",
      "Other" ~ "red"),
    content = glue("<b>Station {id}</b> ({type}) <br/>Location: {round(lon, 3)}, {round(lat, 3)}"))

# --------------------------------------------------------------------------------------------------
# Create Leaflet map

# Initialize the Leaflet map with custom CRS, add base tiles, and configure various layers and controls
m <- leaflet() |>
  addProviderTiles(providers$OpenStreetMap.France, options = providerTileOptions(minZoom = 3, maxZoom = 18, detectRetina = TRUE)) |>
  fitBounds(b[1], b[3], b[2], b[4]) |>
  addMouseCoordinates() |> 
  addControl(title_map, position = "bottomright") |>
  addSimpleGraticule(interval = 5) |>
  # sst
  addRasterImage(r_sst, colors = pal_sst, project = F, opacity = 1, group = "SST") |>
  addLegend(
    "bottomright", pal = pal_sst, opacity = 1, group = "SST", values = values(r_sst),
    title = "SST (°C)") |>
  # chl
  addRasterImage(r_chl, colors = pal_chl, project = F, opacity = 1, group = "Chla") |>
  addLegend("bottomright", pal = pal_chl, opacity = 1, group = "Chla", values = values(r_chl),
            labFormat = labelFormat(transform = function(x) round(10^x, 2)), title = "Chla (mg/m³)") |>
  # depth
  addRasterImage(r_depth, colors = pal_depth, project = F, opacity = 1, group = "Depth") |>
  addLegend("bottomright", pal = pal_depth, opacity = 1, group = "Depth", values = values(r_depth),
            labFormat = labelFormat(transform = function(x) round(x, 1)), title = "Depth (m)") |>
  addLayersControl(position = "topleft", overlayGroups = c("SST", "Chla", "Depth"), options = layersControlOptions(collapsed = FALSE)) |>
  hideGroup(c("Chla", "Depth")) |> 
  # stations
  addAwesomeMarkers(
    data  = pts,
    popup = ~content,
    icon  = awesomeIcons(
      icon = 'flag', iconColor = 'black', library = 'ion', 
      markerColor = ~color)) |> 
  # Add additional map features such as scale bar, mini map, measuring tools, reset map button, and GPS control
  addScaleBar(position = "bottomleft") |>
  addMiniMap(tiles = providers$Esri.WorldStreetMap, toggleDisplay = TRUE, minimized = FALSE) |>
  addMeasure(position = "bottomleft", primaryLengthUnit = "meters", primaryAreaUnit = "sqmeters", 
             activeColor = "darkgreen", completedColor = "red") |>
  addResetMapButton() |>
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

save_leaflet(m, out_html)

# -------------------------------------------------------------------------------------------------
# End of the R script
