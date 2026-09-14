#------------------------------------------------------------------------------
# Build Landbase Raster Stack
#
# Combines prepared and derived landbase attributes into a common
# multi-layer SpatRaster aligned to the PlanningGrid.
#------------------------------------------------------------------------------

buildLandbaseRaster <- function(sim) {
  
  message("Building Landbase raster stack...")
  
  layers <- list(
    SYU = sim$SYU,
    jurisdictionMap = sim$jurisdictionMap,
    bcr = sim$bcr,
    yieldCurveFamily = sim$yieldCurveFamily,
    ownership = sim$Ownership,
    protectedArea = sim$protectedArea,
    DMFL = sim$DMFL,
    landbaseClass = sim$landbaseClass,
    forestCoverMask = sim$forestCoverMask,
    isHarvestEligible = sim$isHarvestEligible,
    riparianFraction = sim$Riparian$riparianFraction,
    harvestableFraction = sim$harvestableFraction
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
    "Landbase raster created with ",
    terra::nlyr(sim$landbaseRaster),
    " layers."
  )
  
  sim
}