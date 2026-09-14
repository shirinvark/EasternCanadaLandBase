#------------------------------------------------------------------------------
# Build Harvest Eligibility
#
# Identifies forested cells that are classified as harvestable by the
# landbase rules.
#
# A cell is harvest-eligible when:
#   1. landbaseClass == 2 (harvestable)
#   2. forestCoverMask == 1 (forest)
#------------------------------------------------------------------------------

buildHarvestEligibility <- function(sim) {
  
  message("Building harvest eligibility...")
  
  stopifnot(
    inherits(sim$PlanningGrid, "SpatRaster"),
    inherits(sim$landbaseClass, "SpatRaster"),
    inherits(sim$forestCoverMask, "SpatRaster")
  )
  
  # Check geometry
  geomOK <- c(
    landbaseClass = terra::compareGeom(
      sim$PlanningGrid,
      sim$landbaseClass,
      stopOnError = FALSE
    ),
    forestCoverMask = terra::compareGeom(
      sim$PlanningGrid,
      sim$forestCoverMask,
      stopOnError = FALSE
    )
  )
  
  if (!all(geomOK)) {
    stop(
      "Geometry mismatch in harvest eligibility inputs: ",
      paste(names(geomOK)[!geomOK], collapse = ", ")
    )
  }
  
  # Harvest eligibility
  sim$isHarvestEligible <- terra::ifel(
    sim$landbaseClass == 2 &
      sim$forestCoverMask == 1,
    1,
    0
  )
  
  names(sim$isHarvestEligible) <- "isHarvestEligible"
  
  sim
}