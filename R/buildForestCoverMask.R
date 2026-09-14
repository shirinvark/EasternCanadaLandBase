buildForestCoverMask <- function(sim) {
  
  message("Building forest cover mask...")
  
  stopifnot(
    inherits(sim$PlanningGrid, "SpatRaster"),
    inherits(sim$LandCover, "SpatRaster")
  )
  
  if (!terra::compareGeom(
    sim$PlanningGrid,
    sim$LandCover,
    stopOnError = FALSE
  )) {
    stop("LandCover geometry does not match PlanningGrid.")
  }
  
  sim$forestCoverMask <- terra::ifel(
    !is.na(sim$LandCover) &
      (
        sim$LandCover == 210 |
          sim$LandCover == 220 |
          sim$LandCover == 230
      ),
    1,
    0
  )
  
  names(sim$forestCoverMask) <- "forestCoverMask"
  
  sim
}