#------------------------------------------------------------------------------
# Build Landbase Class
#
# Classifies PlanningGrid cells using DMFL, protected-area, and ownership
# rules, with optional jurisdiction- and SYU-specific overrides.
#
# Classes:
#   0 = excluded from landbase
#   1 = included but non-harvestable
#   2 = harvestable
#
# Final priority:
#   1. Outside DMFL       -> 0
#   2. Protected area    -> protected-area rule
#   3. Otherwise         -> ownership rule
#
# SYU-specific overrides have priority over jurisdiction-specific overrides.
#------------------------------------------------------------------------------

buildLandbaseClass <- function(sim) {
  
  message("Building landbase class...")
  
  stopifnot(
    inherits(sim$PlanningGrid, "SpatRaster"),
    inherits(sim$DMFL, "SpatRaster"),
    inherits(sim$protectedArea, "SpatRaster"),
    inherits(sim$Ownership, "SpatRaster"),
    inherits(sim$jurisdiction, "SpatRaster"),
    inherits(sim$SYU, "SpatRaster")
  )
  
  # ---------------------------------------------------------------------------
  # Geometry checks
  # ---------------------------------------------------------------------------
  
  geomOK <- c(
    DMFL = terra::compareGeom(
      sim$PlanningGrid,
      sim$DMFL,
      stopOnError = FALSE
    ),
    protectedArea = terra::compareGeom(
      sim$PlanningGrid,
      sim$protectedArea,
      stopOnError = FALSE
    ),
    Ownership = terra::compareGeom(
      sim$PlanningGrid,
      sim$Ownership,
      stopOnError = FALSE
    ),
    jurisdiction = terra::compareGeom(
      sim$PlanningGrid,
      sim$jurisdiction,
      stopOnError = FALSE
    ),
    SYU = terra::compareGeom(
      sim$PlanningGrid,
      sim$SYU,
      stopOnError = FALSE
    )
  )
  
  if (!all(geomOK)) {
    stop(
      "Geometry mismatch in landbase inputs: ",
      paste(names(geomOK)[!geomOK], collapse = ", ")
    )
  }
  
  # ---------------------------------------------------------------------------
  # Load landbase rules
  # ---------------------------------------------------------------------------
  
  rules <- landbaseRuleExceptions()
  
  # ---------------------------------------------------------------------------
  # Default protected-area classification
  # ---------------------------------------------------------------------------
  
  paClass <- terra::subst(
    sim$protectedArea,
    from = as.numeric(names(rules$protectedArea$default)),
    to = as.numeric(unlist(rules$protectedArea$default))
  )
  
  # ---------------------------------------------------------------------------
  # Default ownership classification
  # ---------------------------------------------------------------------------
  
  ownershipClass <- terra::subst(
    sim$Ownership,
    from = as.numeric(names(rules$ownership$default)),
    to = as.numeric(unlist(rules$ownership$default))
  )
  
  # ---------------------------------------------------------------------------
  # Jurisdiction-specific protected-area overrides
  # ---------------------------------------------------------------------------
  
  if (length(rules$protectedArea$jurisdiction) > 0) {
    
    jurLookup <- levels(sim$jurisdiction)[[1]]
    
    for (jurName in names(rules$protectedArea$jurisdiction)) {
      
      jurID <- jurLookup$ID[
        jurLookup$jurisdiction == jurName
      ]
      
      if (length(jurID) == 0) {
        next
      }
      
      overrides <- rules$protectedArea$jurisdiction[[jurName]]
      
      for (code in names(overrides)) {
        
        paClass <- terra::ifel(
          sim$jurisdiction == jurID &
            sim$protectedArea == as.numeric(code),
          as.numeric(overrides[[code]]),
          paClass
        )
      }
    }
  }
  
  # ---------------------------------------------------------------------------
  # Jurisdiction-specific ownership overrides
  # ---------------------------------------------------------------------------
  
  if (length(rules$ownership$jurisdiction) > 0) {
    
    jurLookup <- levels(sim$jurisdiction)[[1]]
    
    for (jurName in names(rules$ownership$jurisdiction)) {
      
      jurID <- jurLookup$ID[
        jurLookup$jurisdiction == jurName
      ]
      
      if (length(jurID) == 0) {
        next
      }
      
      overrides <- rules$ownership$jurisdiction[[jurName]]
      
      for (code in names(overrides)) {
        
        ownershipClass <- terra::ifel(
          sim$jurisdiction == jurID &
            sim$Ownership == as.numeric(code),
          as.numeric(overrides[[code]]),
          ownershipClass
        )
      }
    }
  }
  
  # ---------------------------------------------------------------------------
  # SYU-specific protected-area overrides
  # ---------------------------------------------------------------------------
  
  if (
    length(rules$protectedArea$SYU) > 0 &&
    !is.null(sim$SYULookup)
  ) {
    
    for (syuName in names(rules$protectedArea$SYU)) {
      
      syuID <- sim$SYULookup$SYU_ID[
        sim$SYULookup$SYU_NAME == syuName
      ]
      
      if (length(syuID) == 0) {
        next
      }
      
      overrides <- rules$protectedArea$SYU[[syuName]]
      
      for (code in names(overrides)) {
        
        paClass <- terra::ifel(
          sim$SYU == syuID &
            sim$protectedArea == as.numeric(code),
          as.numeric(overrides[[code]]),
          paClass
        )
      }
    }
  }
  
  # ---------------------------------------------------------------------------
  # SYU-specific ownership overrides
  # ---------------------------------------------------------------------------
  
  if (
    length(rules$ownership$SYU) > 0 &&
    !is.null(sim$SYULookup)
  ) {
    
    for (syuName in names(rules$ownership$SYU)) {
      
      syuID <- sim$SYULookup$SYU_ID[
        sim$SYULookup$SYU_NAME == syuName
      ]
      
      if (length(syuID) == 0) {
        next
      }
      
      overrides <- rules$ownership$SYU[[syuName]]
      
      for (code in names(overrides)) {
        
        ownershipClass <- terra::ifel(
          sim$SYU == syuID &
            sim$Ownership == as.numeric(code),
          as.numeric(overrides[[code]]),
          ownershipClass
        )
      }
    }
  }
  
  # ---------------------------------------------------------------------------
  # Final landbase classification
  # ---------------------------------------------------------------------------
  
  sim$landbaseClass <- terra::ifel(
    sim$DMFL == 0,
    0,
    terra::ifel(
      sim$protectedArea > 0,
      paClass,
      ownershipClass
    )
  )
  
  names(sim$landbaseClass) <- "landbaseClass"
  
  sim
}