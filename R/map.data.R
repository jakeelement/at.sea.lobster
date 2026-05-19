#' @title map.data
#' @import dplyr sf maps magick
#' @description Plot coordinate data on a map from any data frame/table.
#' @param data A data frame or table containing coordinate columns.
#' @param lat Name of the latitude column.
#' @param lon Name of the longitude column.
#' @param coord.format Coordinate format in `lat` and `lon`. Supports `"decimal_degrees"` (default) or `"ddmm"`.
#' @param zoom Numeric zoom level from 0 to 100.
#' @param main Optional plot title.
#' @export
map.data <- function(data,
                     lat,
                     lon,
                     coord.format = "decimal_degrees",
                     zoom = 50,
                     main = NULL) {

  if(is.null(data) || !is.data.frame(data) || nrow(data) == 0) {
    warning("No rows to map.")
    return(invisible(NULL))
  }

  if(!all(c(lat, lon) %in% names(data))) {
    stop("`lat` and `lon` must be column names present in `data`.")
  }

  ddmm_to_dd <- function(x, west = FALSE){
    x <- suppressWarnings(as.numeric(x))
    deg <- floor(x / 100)
    mins <- x - (deg * 100)
    dd <- deg + mins / 60
    if(west){
      dd <- -dd
    }
    dd
  }

  coord.format <- tolower(coord.format)
  if(coord.format %in% c("dd", "decimal", "decimal_degrees", "decimal degrees")) {
    lat_dd <- suppressWarnings(as.numeric(data[[lat]]))
    lon_dd <- suppressWarnings(as.numeric(data[[lon]]))
  } else if(coord.format %in% c("ddmm", "degmin", "degrees_minutes")) {
    lat_dd <- ddmm_to_dd(data[[lat]], west = FALSE)
    lon_dd <- ddmm_to_dd(data[[lon]], west = TRUE)
  } else {
    stop("Unsupported `coord.format`. Use 'decimal_degrees' or 'ddmm'.")
  }

  data$lat_dd <- lat_dd
  data$lon_dd <- lon_dd

  good <- !is.na(data$lat_dd) & !is.na(data$lon_dd)
  if(!any(good)) {
    warning("No valid coordinates found in provided columns.")
    return(invisible(NULL))
  }

  data_sf <- sf::st_as_sf(
    data[good, ],
    coords = c("lon_dd", "lat_dd"),
    crs = 4326,
    remove = FALSE
  )

  zoom <- suppressWarnings(as.numeric(zoom))
  zoom <- min(max(zoom, 0), 100)

  bbox <- sf::st_bbox(data_sf)
  z <- zoom / 100
  zoom_scale <- (1 - z)^2
  lon_mult <- 0.01 + 4 * zoom_scale
  lat_mult <- 0.01 + 4 * zoom_scale
  lon_min_pad <- 0.001
  lat_min_pad <- 0.001
  lon_pad <- max((bbox["xmax"] - bbox["xmin"]) * lon_mult, lon_min_pad)
  lat_pad <- max((bbox["ymax"] - bbox["ymin"]) * lat_mult, lat_min_pad)

  xlim <- as.numeric(c(bbox["xmin"] - lon_pad,
                       bbox["xmax"] + lon_pad))
  ylim <- as.numeric(c(bbox["ymin"] - lat_pad,
                       bbox["ymax"] + lat_pad))
  xlim <- c(max(-180, xlim[1]), min(180, xlim[2]))
  ylim <- c(max(-85, ylim[1]), min(85, ylim[2]))

  if(anyNA(c(xlim, ylim)) || xlim[1] >= xlim[2] || ylim[1] >= ylim[2]) {
    warning("Invalid map extent after coordinate conversion; using maps basemap fallback extent.")
    xlim <- c(-70, -50)
    ylim <- c(40, 50)
  }

  mapbox_token <- if(exists("mapbox.token", envir = .GlobalEnv)) get("mapbox.token", envir = .GlobalEnv) else NULL

  if(!is.null(mapbox_token) && nzchar(mapbox_token)) {
    map_width <- 900
    map_height <- 700

    mapbox_url <- paste0(
      "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/static/[",
      xlim[1], ",", ylim[1], ",", xlim[2], ",", ylim[2],
      "]/", map_width, "x", map_height,
      "?access_token=", mapbox_token
    )

    map_img_file <- tempfile(fileext = ".img")
    downloaded <- FALSE
    try(download.file(mapbox_url, map_img_file, mode = "wb", quiet = TRUE), silent = TRUE)
    if(file.exists(map_img_file) && file.info(map_img_file)$size > 0) {
      downloaded <- TRUE
    }

    mapbox_used <- FALSE
    if(downloaded) {
      mapbox_used <- tryCatch({
        map_img <- magick::image_read(map_img_file)
        map_img <- as.raster(map_img)

        plot(NA, xlim = xlim, ylim = ylim,
             xlab = "Longitude", ylab = "Latitude", axes = TRUE, asp = 1)
        rasterImage(map_img, xlim[1], ylim[1], xlim[2], ylim[2])
        TRUE
      }, error = function(e) FALSE)
    }

    if(!mapbox_used) {
      warning("MapBox tile download failed; falling back to maps basemap.")
      world_map <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
      plot(sf::st_geometry(world_map), col = "antiquewhite", border = "grey55", xlim = xlim, ylim = ylim, axes = TRUE)
    }
  } else {
    world_map <- sf::st_as_sf(maps::map("world", plot = FALSE, fill = TRUE))
    plot(sf::st_geometry(world_map), col = "antiquewhite", border = "grey55", xlim = xlim, ylim = ylim, axes = TRUE)
  }

  lfapolys <- readRDS(paste0(system.file("data", package = "at.sea.lobster"), "/LFAPolysSF.rds"))
  plot(lfapolys, add = TRUE, col = NA, border = "red")

  plot(sf::st_geometry(data_sf), add = TRUE, pch = 19, col = "blue")
  if(!is.null(main)) {
    title(main = main, xlab = "Longitude", ylab = "Latitude")
  } else {
    title(xlab = "Longitude", ylab = "Latitude")
  }
  box()

  invisible(data_sf)
}
