# R Code to convert harmonized landsat/sentinel images into EVI
library(tidyverse)
#https://github.com/nasa/HLS-Data-Resources/blob/main/r/HLS_Tutorial.Rmd
earthdatalogin::edl_netrc()
s = rstac::stac("https://cmr.earthdata.nasa.gov/stac/LPCLOUD/")
HLS_col <- list("HLSS30_2.0", "HLSL30_2.0")
roi <- terra::vect("../Raw_data/map.geojson")
roi_extent <- terra::ext(roi)
bbox <- c(roi_extent$xmin, roi_extent$ymin, roi_extent$xmax, roi_extent$ymax)
roi_datetime <- '2025-03-01T00:00:00Z/2026-03-01T23:59:59Z'
items <- s %>%
  rstac::stac_search(collections = HLS_col,
                     bbox = bbox,
                     datetime = roi_datetime,
                     limit = 100) %>%
  rstac::post_request()
assets <- rstac::items_assets(items)
print(assets)
browse_image_url <- items$features[[1]]$assets$browse$href
browse <-imager::load.image(browse_image_url)
plot(browse)
sf_items <- rstac::items_as_sf(items)
# Retrieve Granule ID for each feature
granule_id <- sapply(items$features, function(feature) feature$id)
# Add as first column in sf_items
sf_items <- cbind(granule = granule_id, sf_items)
# Define a function to extract asset urls for selected bands
# # This also includes a check to ensure the correct bands are extracted
# # # depending on the collection (HLSL30 or HLSS30)
extract_asset_urls <- function(feature) {
  collection_id <- feature$collection
  if (collection_id == "HLSS30_2.0") {
    bands = c('B02', 'B8A','B04','Fmask')
  } else if (collection_id == "HLSL30_2.0") {
    bands = c('B02', 'B05','B04','Fmask')}
  sapply(bands, function(band) feature$assets[[band]]$href)
}
# Retrieve Asset URLs for each feature using our extract_asset_urls function and transpose them to columns
asset_urls <- t(sapply(items$features, extract_asset_urls))

colnames(asset_urls) <- c('blue', 'nir', 'red', 'fmask')
sf_items <- cbind(sf_items, asset_urls)

# Filter based on cloud cover
sf_items <- sf_items[sf_items$eo.cloud_cover < 30,]
# Reset Row Indices
row.names(sf_items) <- NULL

terra::setGDALconfig("GDAL_HTTP_UNSAFESSL", value = "YES")
terra::setGDALconfig("GDAL_HTTP_COOKIEFILE", value = ".rcookies")
terra::setGDALconfig("GDAL_HTTP_COOKIEJAR", value = ".rcookies")
terra::setGDALconfig("GDAL_DISABLE_READDIR_ON_OPEN", value = "EMPTY_DIR")
terra::setGDALconfig("CPL_VSIL_CURL_ALLOWED_EXTENSIONS", value = "TIF")

# This function reads an HLS scene from a URL, applies the scale factor if necessary, and optionally crops and
# masks the scene based on a polygon. It requries the above GDAL configurations and a .netrc file. A .netrc
# can be created by running `earthdatalogin::edl_netrc()`.
open_hls <- function(url, roi = NULL) {
  # Add VSICURL prefix
  url <- paste0('/vsicurl/', url)
  # Retrieve metadata
  meta <- terra::describe(url)
  # Check if dataset is Quality Layer (Fmask) - no scaling this asset (int8 datatype)
  is_fmask <- any(grep("Fmask", meta))
  # Check if Scale is present in band metadata
  will_autoscale <- any(grep("Scale:", meta))
  # Read the raster
  r <- terra::rast(url)
  # Apply Scale Factor if necessary
  if (!will_autoscale && !is_fmask){
    print(paste("No scale factor found in band metadata. Applying scale factor of 0.0001 to", basename(url)))
    r <- r * 0.0001
  }
  # Crop if roi specified
  if (!is.null(roi)){
    # Reproject roi to match crs of r
    roi_reproj <- terra::project(roi, terra::crs(r))
    r <- terra::mask(terra::crop(r, roi_reproj), roi_reproj)
  }
  return(r)
}

# Test opening and crop
red <- open_hls(sf_items$red[1], roi)
terra::plot(red)

red_stack <- lapply(sf_items$red, open_hls, roi = roi)
nir_stack <- lapply(sf_items$nir, open_hls, roi = roi)
blue_stack <- lapply(sf_items$blue, open_hls, roi = roi)
fmask_stack <- lapply(sf_items$fmask, open_hls, roi = roi)

calculate_EVI <- function(nir, red, blue){
  evi <- 2.5 * (nir - red) / (nir + 6 * red - 7.5 * blue + 1.0)
  return(evi)
}

# Calculate EVI For All of our Scenes
evi_stack <- mapply(calculate_EVI, nir_stack, red_stack, blue_stack, SIMPLIFY = FALSE)

# Rename the Scenes in our List
names(evi_stack) <- sf_items$datetime
# Create a single Rast Object from our list
evi_stacks <- terra::rast(evi_stack)

# Test fmask
# fmask <- fmask_stack[[23]]
# plot(fmask)

selected_bit_nums <- c(1,2,3,4,5)

# Filter based on quality
build_mask <- function(fmask, selected_bit_nums){
  # Create a mask of all zeros
  mask <- terra::rast(fmask, vals=0)
  for (b in selected_bit_nums){
    # Apply Bitwise AND to fmask values and selected bit numbers
    mask_temp <- terra::app(fmask, function(x) bitwAnd(x, bitwShiftL(1,b)) >0)
    # Update Mask to maintain only 1 layer with bitwise OR
    mask <- mask | mask_temp
  }
  return(mask)
}

qmask <- build_mask(fmask[[1]], selected_bit_nums)
plot(qmask)

# Create List of Masks
qmask_stack <- lapply(fmask_stack, build_mask, selected_bit_nums=selected_bit_nums)

# Apply Mask to EVI using NA Values
evi_masked <- mapply(function(x, y) {
  terra::mask(x, y, maskvalue = TRUE, updatevalue = NA)
}, evi_stack, qmask_stack, SIMPLIFY = FALSE)

evi_masked <- terra::rast(evi_masked)

# Add Date Only Column
sf_items$date <- sapply(sf_items$datetime, function(x) strsplit(x, "T")[[1]][1])

evi_mean <- terra::global(evi_masked, 'mean', na.rm=TRUE)

stats <- data.frame(
  mean = evi_mean
)
stats$Date <- ymd_hms(sf_items$datetime) # convert string to date format (ISO 8601)

### MODEL

source("../R/calc_curve.R")
df <- stats %>%
  select(Date, mean) %>%
  rename(evi = mean) %>%
  mutate(img_doy = yday(Date),
         id = "gcrew") #separate by year if you want

fits_beck <- unique(df$id) %>%
  map(calc_curve, df = df, method = "Beck") %>%
  bind_rows()

pred_df_beck <- data.frame(id = rep(unique(fits_beck$id), each = 365),
                           doy = rep(1:365, length(unique(fits_beck$id)))) %>%
  left_join(fits_beck %>%
              pivot_wider(names_from = param_name,
                          values_from = c(param_value, stdError))) %>%
  mutate(pred = param_value_mn + (param_value_mx - param_value_mn) *
           (1/(1 + exp(-param_value_rsp * (doy - param_value_sos))) +
              1/(1 + exp(param_value_rau * (doy - param_value_eos))))) %>%
  left_join(df %>% rename(doy = img_doy)) %>%
  mutate(method = "Beck")


pred_df_beck %>%
  ggplot(aes(x = doy, y = evi)) +
  geom_point() +
  geom_line(aes(y = pred), color = "blue") +
  facet_wrap(~id) +
  theme_minimal() +
  labs(title = "EVI and Double Log Fit (Beck Method)",
       x = "Day of Year",
       y = "EVI")

formatted <- pred_df_beck %>%
  select(id, doy, evi, pred, method, Date) %>%
  rename(evi_observed = evi,
         evi_predicted = pred) %>%
  mutate(Date_mod = as.Date(doy - 1, origin = "2025-01-01"))

out <- bind_rows(formatted, formatted %>%
                   mutate(Date_mod = Date_mod + years(1))) %>%
  mutate(evi_observed = ifelse(year(Date) == year(Date_mod), evi_observed, NA)) %>%
  select(-Date) %>%
  rename(Date = Date_mod)

write_csv(out, "../processed_data/EVI.csv")

