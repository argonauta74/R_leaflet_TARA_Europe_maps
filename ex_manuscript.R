# Load the libraries
library(leaflet)
library(terra)

# Download from NOAA the specific NetCDF file
url_sst <- "https://www.ncei.noaa.gov/data/sea-surface-temperature-optimum-interpolation/v2.1/access/avhrr/202405/oisst-avhrr-v02r01.20240515.nc"
sst_nc     <- file.path("data", basename(url_sst))
if (!file.exists(sst_nc)) download.file(url_sst, sst_nc)

# Open the downloaded NetCDF file with the rast function
r <- rast(sst_nc)

# Crop the raster on the Labrador Sea area
r <- crop(r, ext(295, 322, 50, 68))

# Create a simple color palette
pal <- colorNumeric(
  c("#0C2C84", "#41B6C4", "#FFFFCC"), 
  values(r), na.color = "transparent")

m <- leaflet() |>  # Return a Leaflet map widget
  addTiles() |>    # Add default map tiles
  # Add the layer (i.e., SST)
  addRasterImage(r, colors = pal, project = FALSE, opacity = 1) |>
  # Add the color bar
  addLegend(pal = pal, values = values(r), title = "SST")

m # Print the map
