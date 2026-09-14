#------------------------------------------------------------------------------
# Apply Riparian Reduction
#
# Reduces the harvestable fraction of eligible cells according to the
# proportion of each PlanningGrid cell occupied by riparian buffers.
#
# When riparian buffers are disabled, harvestableFraction is identical
# to isHarvestEligible.
#------------------------------------------------------------------------------

applyRiparianReduction <- function(sim) {
  
  message("Applying riparian reduction...")
  
  stopifnot(
    inherits(sim$PlanningGrid, "SpatRaster"),
    inherits(sim$isHarvestEligible, "SpatRaster"),
    is.list(sim$Riparian),
    inherits(sim$Riparian$riparianFraction, "SpatRaster")
  )
  
  riparianFraction <- sim$Riparian$riparianFraction
  
  # Check geometry
  if (!terra::compareGeom(
    sim$PlanningGrid,
    riparianFraction,
    stopOnError = FALSE
  )) {
    stop(
      "Riparian$riparianFraction geometry does not match PlanningGrid."
    )
  }
  
  # Apply riparian reduction
  if (isTRUE(P(sim)$.includeRiparianBuffers)) {
    
    message("✔ Riparian buffers enabled.")
    
    sim$harvestableFraction <-
      sim$isHarvestEligible * (1 - riparianFraction)
    
  } else {
    
    message("ℹ Riparian buffers disabled.")
    
    sim$harvestableFraction <- sim$isHarvestEligible
  }
  
  names(sim$harvestableFraction) <- "harvestableFraction"
  
  sim
}