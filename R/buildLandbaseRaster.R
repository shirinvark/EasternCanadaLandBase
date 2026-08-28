#------------------------------------------------------------------------------
# Build Landbase Raster Stack
#
# Combines prepared spatial attributes into a common multi-layer SpatRaster
# aligned to the PlanningGrid.
#------------------------------------------------------------------------------

buildLandbaseRaster <- function(sim) {
  
  message("Building Landbase raster stack...")
  
  layers <- list(
    jurisdiction = sim$jurisdiction,
    bcr = sim$bcr,
    yieldCurveFamily = sim$yieldCurveFamily,
    ownership = sim$Ownership,
    protectedArea = sim$protectedArea,
    riparianFraction = sim$Riparian$riparianFraction
  )
  
  # Check geometry against PlanningGrid
  geomOK <- vapply(
    layers,
    function(x) {
      terra::compareGeom(
        sim$PlanningGrid,
        x,
        stopOnError = FALSE
      )
    },
    logical(1)
  )
  
  if (!all(geomOK)) {
    stop(
      "Landbase raster geometry mismatch: ",
      paste(names(geomOK)[!geomOK], collapse = ", ")
    )
  }
  
  sim$landbaseRaster <- terra::rast(layers)
  
  names(sim$landbaseRaster) <- names(layers)
  
  message(
    "✔ Landbase raster created with ",
    terra::nlyr(sim$landbaseRaster),
    " layers."
  )
  
  sim
}