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

    center_lon <- as.numeric(mean(xlim))
    center_lat <- as.numeric(mean(ylim))
    center_lat <- max(min(center_lat, 85), -85)

    lon_range <- max(diff(xlim), 1e-06)

    merc_y <- function(lat_deg) {
      lat_rad <- lat_deg * pi / 180
      log(tan(pi / 4 + lat_rad / 2))
    }
    inv_merc_y <- function(y) {
      (2 * atan(exp(y)) - pi / 2) * 180 / pi
    }

    y_min <- merc_y(ylim[1])
    y_max <- merc_y(ylim[2])
    y_range <- max(y_max - y_min, 1e-09)

    zoom_lon <- log2((map_width * 360) / (lon_range * 512))
    zoom_lat <- log2((map_height * (2 * pi)) / (y_range * 512))
    zoom_level <- max(0, min(22, floor(min(zoom_lon, zoom_lat))))

    mapbox_url <- paste0(
      "https://api.mapbox.com/styles/v1/mapbox/satellite-v9/static/",
      center_lon, ",", center_lat, ",", zoom_level,
      "/", map_width, "x", map_height,
      "?logo=false&attribution=false&access_token=", mapbox_token
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

        world_size <- 512 * (2 ^ zoom_level)
        cx <- (center_lon + 180) / 360 * world_size
        cy <- (1 - (log(tan(pi / 4 + (center_lat * pi / 180) / 2)) / pi)) / 2 * world_size

        half_w <- map_width / 2
        half_h <- map_height / 2

        x_min_px <- cx - half_w
        x_max_px <- cx + half_w
        y_min_px <- cy - half_h
        y_max_px <- cy + half_h

        lon_min_img <- (x_min_px / world_size) * 360 - 180
        lon_max_img <- (x_max_px / world_size) * 360 - 180

        merc_y_top <- pi * (1 - 2 * (y_min_px / world_size))
        merc_y_bot <- pi * (1 - 2 * (y_max_px / world_size))
        lat_max_img <- inv_merc_y(merc_y_top)
        lat_min_img <- inv_merc_y(merc_y_bot)

        plot(NA,
             xlim = c(lon_min_img, lon_max_img),
             ylim = c(lat_min_img, lat_max_img),
             xlab = "Longitude", ylab = "Latitude", axes = TRUE, asp = 1)
        rasterImage(map_img, lon_min_img, lat_min_img, lon_max_img, lat_max_img)
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
