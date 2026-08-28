## Everything in this file and any files in the R directory are sourced during simInit()
## All functions and objects are put into the simList.
## Functions are called directly by name (new SpaDES convention)

defineModule(sim, list(
  name = "EasternCanadaLandbase",
  description = "Builds a planning landbase for Eastern Canada from prepared spatial inputs.",
  keywords = c("landbase", "Eastern Canada", "SpaDES", "planning"),
  authors = structure(list(
    list(
      given = "Shirin",
      family = "Varkouhi",
      role = c("aut", "cre"),
      email = "shirin.varkuhi@gmail.com"
    )
  ), class = "person"),
  
  childModules = character(0),
  version = list(EasternCanadaLandbase = "0.0.0.9000"),
  spatialExtent = NA,
  loadOrder = list("EasternCanadaDataPrep", "RiparianBuffers"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "EasternCanadaLandbase.Rmd"),
  reqdPkgs = list("terra", "sf", "LandR"),
 
    parameters = list(
      
      defineParameter(
        ".useLandbaseRules",
        "logical",
        TRUE,
        NA,
        NA,
        "Apply Protected Area and Ownership rules when defining the landbase."
      ),
      
      defineParameter(
        ".includeRiparianBuffers",
        "logical",
        TRUE,
        NA,
        NA,
        "Apply riparian buffers when calculating harvestable fraction."
      )
  ),
  inputObjects = data.table::rbindlist(list(
    
    expectsInput("PlanningGrid", "SpatRaster",
                 "Planning grid from EasternCanadaDataPrep"),
    expectsInput(
      "SYU",
      "SpatRaster",
      "Sustained Yield Unit raster aligned to PlanningGrid"
    ),
    expectsInput(
      "studyArea",
      objectClass = c("sf", "SpatVector"),
      desc = "Study area polygon"
    ),
    
    expectsInput("LandCover", "SpatRaster",
                 "Land cover raster aligned to PlanningGrid"),
 
    
    expectsInput("Riparian", "list",
             "List containing riparianFraction (SpatRaster)"),
    expectsInput(
      "jurisdiction",
      "SpatRaster",
      "Jurisdiction raster aligned to PlanningGrid"
    ),
    
    expectsInput(
      "bcr",
      "SpatRaster",
      "Bird Conservation Region raster aligned to PlanningGrid"
    ),
    
    expectsInput(
      "yieldCurveFamily",
      "SpatRaster",
      "Yield Curve Family raster aligned to PlanningGrid"
    ),
    expectsInput(
      "DMFL",
      "SpatRaster",
      "Designated Managed Forest Lands raster aligned to PlanningGrid; 1 = inside, 0 = outside"
    ),
    
    expectsInput(
      "Ownership",
      "SpatRaster",
      "National ownership raster"
    ),
    expectsInput(
      "protectedArea",
      "SpatRaster",
      "Protected area raster aligned to PlanningGrid"
    ),
    expectsInput(
      "SYULookup",
      objectClass = c("data.frame", "data.table"),
      desc = "Lookup table linking SYU raster IDs to SYU names"
    )
    
  ), fill = TRUE)
  ,
  
  outputObjects = data.table::rbindlist(list(
    
    createsOutput("isHarvestEligible", "SpatRaster",
                  "Binary harvest eligibility mask"),
    
    createsOutput("forestCoverMask", "SpatRaster",
                  "Binary forest/non-forest mask derived from LandCover"),
    
    createsOutput("harvestableFraction", "SpatRaster",
                  "Fraction of each PlanningGrid cell available for harvest after landbase and riparian constraints"),
    createsOutput(
      "landbaseRaster",
      "SpatRaster",
      "Multi-layer raster containing spatial attributes used by the landbase."
    ),
    createsOutput(
      "landbaseClass",
      "SpatRaster",
      "Landbase inclusion class: 0 = excluded, 1 = included non-harvestable, 2 = harvestable"
    ),
    
    createsOutput("Landbase", "list",
                  "Derived landbase container")
    
  ), fill = TRUE)
  
))

# =========================================================
# Event dispatcher
# =========================================================
doEvent.EasternCanadaLandbase <- function(sim, eventTime, eventType) {
  
  if (eventType == "init") {
    
    sim <- Init(sim)
    
    return(invisible(sim))
  }
  
  noEventWarning(sim)
}

# =========================================================
.inputObjects <- function(sim) {
  if (!SpaDES.core::suppliedElsewhere("studyArea", sim)) {
    
    message("🔵 Creating default studyArea (Eastern Canada)...")
    
    can <- rnaturalearth::ne_states(
      country = "Canada",
      returnclass = "sf"
    )
    
    east <- can[can$name_en %in% c(
      "Ontario","Quebec","New Brunswick",
      "Nova Scotia","Prince Edward Island",
      "Newfoundland and Labrador"
    ), ]
    
    east_union <- sf::st_union(east)
    
    sim$studyArea <- sf::st_sf(
      data.frame(id = 1),
      geometry = sf::st_transform(
        east_union,
        "ESRI:102001"
      )
    )
  }
  
  studyArea_sf <- sim$studyArea
  
  if (inherits(studyArea_sf, "SpatVector")) {
    
    studyArea_v <- studyArea_sf
    
  } else {
    
    studyArea_v <- terra::vect(studyArea_sf)
    
  }
  
  # =========================================================
  # 1) PlanningGrid
  # =========================================================
  
  
  if (
    !is.null(sim$PlanningGrid) &&
    inherits(sim$PlanningGrid, "SpatRaster")
  ) {
    
    message("✔ Using PlanningGrid supplied from EasternCanadaDataPrep.")
    
  } else {
    
    message("Standalone mode: creating synthetic PlanningGrid")
    
    study_v <- if (inherits(sim$studyArea, "SpatVector")) {
      sim$studyArea
    } else {
      terra::vect(sim$studyArea)
    }
    
    sim$PlanningGrid <- terra::rast(
      ext = terra::ext(study_v),
      resolution = 240,
      crs = terra::crs(study_v)
    )
    
    sim$PlanningGrid[] <- 1
    names(sim$PlanningGrid) <- "PlanningGrid"
  }
  # =========================================================
  # 2) Sustained Yield Unit (SYU)
  # =========================================================
  
  # =========================================================
  # SYU
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("SYU", sim)) {
    
    message("✔ Using SYU supplied from upstream.")
    
  } else {
    
    message("Standalone mode: creating synthetic SYU")
    
    sim$SYU <- terra::rast(sim$PlanningGrid)
    sim$SYU[] <- 1
    
    names(sim$SYU) <- "SYU"
  }
  
  # =========================================================
  # SYU Lookup
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("SYULookup", sim)) {
    
    sim$SYULookup <- data.frame(
      SYU_ID = 1,
      SYU_NAME = "StudyArea",
      stringsAsFactors = FALSE
    )
  }
  # =========================================================
  # 2) LandCover
  # =========================================================
 
  
  if (SpaDES.core::suppliedElsewhere("LandCover", sim)) {
    
    message("✔ Using LandCover supplied from upstream.")
    
  } else {
    
    message("Standalone mode: creating synthetic LandCover")
    
    sim$LandCover <- terra::rast(
      sim$PlanningGrid
    )
    
    sim$LandCover[] <- 210
    names(sim$LandCover) <- "LandCover"
  }

  # =========================================================
  # 4) Riparian
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("Riparian", sim)) {
    
    ripTmp <- terra::rast(sim$PlanningGrid)
    ripTmp[] <- 0
    
    sim$Riparian <- list(
      riparianFraction = ripTmp
    )
  }
  
  # =========================================================
  # 5) protectedArea
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("protectedArea", sim)) {
    
    message("✔ Using protectedArea supplied from EasternCanadaDataPrep.")
    
  } else {
    
    message("Standalone mode: creating synthetic protectedArea")
    
    sim$protectedArea <- terra::rast(sim$PlanningGrid)
    sim$protectedArea[] <- 0
    
    names(sim$protectedArea) <- "protectedArea"
  }
  
  
  # =========================================================
  # 6) jurisdiction
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("jurisdiction", sim)) {
    
    message("✔ Using jurisdiction supplied from EasternCanadaDataPrep.")
    
  } else {
    
    message("Standalone mode: creating synthetic jurisdiction")
    
    sim$jurisdiction <- terra::rast(sim$PlanningGrid)
    sim$jurisdiction[] <- 1
    
    names(sim$jurisdiction) <- "jurisdiction"
  }
  
  
  # =========================================================
  # 7) BCR
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("bcr", sim)) {
    
    message("✔ Using bcr supplied from EasternCanadaDataPrep.")
    
  } else {
    
    message("Standalone mode: creating synthetic bcr")
    
    sim$bcr <- terra::rast(sim$PlanningGrid)
    sim$bcr[] <- 1
    
    names(sim$bcr) <- "bcr"
  }
  
  
  # =========================================================
  # 8) Yield Curve Family
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("yieldCurveFamily", sim)) {
    
    message("✔ Using yieldCurveFamily supplied from EasternCanadaDataPrep.")
    
  } else {
    
    message("Standalone mode: creating synthetic yieldCurveFamily")
    
    sim$yieldCurveFamily <- terra::rast(sim$PlanningGrid)
    sim$yieldCurveFamily[] <- 1
    
    names(sim$yieldCurveFamily) <- "yieldCurveFamily"
  }
  
  
  # =========================================================
  # 9) Ownership
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("Ownership", sim)) {
    
    message("✔ Using Ownership supplied from EasternCanadaDataPrep.")
    
  } else {
    
    message("Standalone mode: creating synthetic Ownership")
    
    sim$Ownership <- terra::rast(sim$PlanningGrid)
    sim$Ownership[] <- 13
    
    names(sim$Ownership) <- "Ownership"
  }
  # =========================================================
  # 10) Designated Managed Forest Lands (DMFL)
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("DMFL", sim)) {
    
    message("✔ Using DMFL supplied from EasternCanadaDataPrep.")
    
  } else {
    
    message("DMFL not supplied: assuming all PlanningGrid cells are within DMFL.")
    
    sim$DMFL <- terra::rast(sim$PlanningGrid)
    sim$DMFL[] <- 1
    
    names(sim$DMFL) <- "DMFL"
  }
  return(invisible(sim))
}

## Summary:
## EasternCanadaLandbase defines the planning landbase
## from prepared spatial inputs.
##
## Protected Area and Ownership rules are interpreted
## here to determine landbase inclusion classes (0, 1, 2).

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

