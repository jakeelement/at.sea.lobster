#' @title map.sets
#' @import dplyr RSQLite sf maps magick
#' @description Opens SET_INFO from a trip .db file and plots set coordinates as points.
#' @export
map.sets <- function(choose.trip = FALSE,
                     zoom = 50,
                     dat.dir = if(exists("dat.dir.global")) dat.dir.global else NULL,
                     trip.file = if(exists("last.trip.file")) last.trip.file else NULL){

  if(is.null(trip.file) && !choose.trip){
    return(print("No Trip File Chosen!"))
  }

  if(choose.trip){
    dlg_message("In the following window, select the .db trip file you want to map.")
    trip.file <- dlg_open()$res
    last.trip.file <<- trip.file
  }

  suppressWarnings({
    db <- dbConnect(RSQLite::SQLite(), trip.file)
    query <- paste0("SELECT * FROM SET_INFO")
    set <- dbSendQuery(db, query)
    set <- fetch(set)
    dbDisconnect(db)

    if(nrow(set) == 0){
      warning("SET_INFO has no rows to map.")
      return(invisible(NULL))
    }

    trip_id <- if("TRIP_ID" %in% names(set) && any(!is.na(set$TRIP_ID))) as.character(set$TRIP_ID[which(!is.na(set$TRIP_ID))[1]]) else "Unknown"

    map.data(
      data = set,
      lat = "LATDDMM",
      lon = "LONGDDMM",
      coord.format = "ddmm",
      zoom = zoom,
      main = paste0("SET_INFO set locations - TRIP_ID: ", trip_id)
    )
  })
}
