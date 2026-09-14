#------------------------------------------------------------------------------
# Validate Landbase
#
# Performs final consistency checks on landbase outputs.
# Does not modify any landbase values.
#------------------------------------------------------------------------------

validateLandbase <- function(sim) {
  
  message("Validating landbase...")
  
  stopifnot(
    inherits(sim$PlanningGrid, "SpatRaster"),
    inherits(sim$landbaseClass, "SpatRaster"),
    inherits(sim$forestCoverMask, "SpatRaster"),
    inherits(sim$isHarvestEligible, "SpatRaster"),
    inherits(sim$harvestableFraction, "SpatRaster")
  )
  
  # ---------------------------------------------------------------------------
  # Geometry checks
  # ---------------------------------------------------------------------------
  
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
    ),
    isHarvestEligible = terra::compareGeom(
      sim$PlanningGrid,
      sim$isHarvestEligible,
      stopOnError = FALSE
    ),
    harvestableFraction = terra::compareGeom(
      sim$PlanningGrid,
      sim$harvestableFraction,
      stopOnError = FALSE
    )
  )
  
  if (!all(geomOK)) {
    stop(
      "Geometry mismatch in landbase outputs: ",
      paste(names(geomOK)[!geomOK], collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # landbaseClass must contain only 0, 1, or 2
  # ---------------------------------------------------------------------------
  
  landbaseValues <- unique(
    terra::values(sim$landbaseClass, na.rm = TRUE)
  )
  
  if (!all(landbaseValues %in% c(0, 1, 2))) {
    stop("landbaseClass contains values outside 0, 1, and 2.")
  }
  
  # ---------------------------------------------------------------------------
  # forestCoverMask must be binary
  # ---------------------------------------------------------------------------
  
  forestValues <- unique(
    terra::values(sim$forestCoverMask, na.rm = TRUE)
  )
  
  if (!all(forestValues %in% c(0, 1))) {
    stop("forestCoverMask contains values other than 0 and 1.")
  }
  
  # ---------------------------------------------------------------------------
  # isHarvestEligible must be binary
  # ---------------------------------------------------------------------------
  
  eligibilityValues <- unique(
    terra::values(sim$isHarvestEligible, na.rm = TRUE)
  )
  
  if (!all(eligibilityValues %in% c(0, 1))) {
    stop("isHarvestEligible contains values other than 0 and 1.")
  }
  
  # ---------------------------------------------------------------------------
  # Harvest-eligible cells must be forest and landbaseClass == 2
  # ---------------------------------------------------------------------------
  
  invalidEligibility <- terra::global(
    terra::ifel(
      sim$isHarvestEligible == 1 &
        (
          sim$landbaseClass != 2 |
            sim$forestCoverMask != 1
        ),
      1,
      0
    ),
    "sum",
    na.rm = TRUE
  )[1, 1]
  
  if (!is.na(invalidEligibility) && invalidEligibility > 0) {
    stop(
      "Some harvest-eligible cells are not forested or do not have ",
      "landbaseClass == 2."
    )
  }
  
  # ---------------------------------------------------------------------------
  # harvestableFraction must be between 0 and 1
  # ---------------------------------------------------------------------------
  
  fractionMin <- terra::global(
    sim$harvestableFraction,
    "min",
    na.rm = TRUE
  )[1, 1]
  
  fractionMax <- terra::global(
    sim$harvestableFraction,
    "max",
    na.rm = TRUE
  )[1, 1]
  
  if (
    (!is.na(fractionMin) && fractionMin < 0) ||
    (!is.na(fractionMax) && fractionMax > 1)
  ) {
    stop("harvestableFraction contains values outside [0, 1].")
  }
  
  message("Landbase validation passed.")
  
  sim
}