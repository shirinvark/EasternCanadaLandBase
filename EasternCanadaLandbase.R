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
  parameters = list(),
  inputObjects = data.table::rbindlist(list(
    
    expectsInput("PlanningGrid", "SpatRaster",
                 "Planning grid from EasternCanadaDataPrep"),
    expectsInput(
      "studyArea",
      objectClass = c("sf", "SpatVector"),
      desc = "Study area polygon"
    ),
    
    expectsInput("LandCover", "SpatRaster",
                 "Land cover raster aligned to PlanningGrid"),
    
    expectsInput("standAge", "SpatRaster",
                 "Stand age raster aligned to PlanningGrid"),
    
    expectsInput("Riparian", "list",
             "List containing riparianFraction (SpatRaster)"),

expectsInput("LegalConstraints", "list",
             "List containing CPCAD_Raster (SpatRaster)")
    
  ), fill = TRUE)
  ,
  
  outputObjects = data.table::rbindlist(list(
    
    createsOutput("protectedAreaMask", "SpatRaster",
                  "Binary protected areas mask"),
    createsOutput("isHarvestEligible", "SpatRaster",
                  "Binary harvest eligibility mask"),
    
    createsOutput("forestCoverMask", "SpatRaster",
                  "Binary forestCoverMask excluding wetlands"),
    
    createsOutput("harvestableFraction", "SpatRaster",
                  "Effective forest area after protected and riparian reduction"),
    
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
  
  # =========================================================
  # 1) PlanningGrid
  # =========================================================
  
  if (!inherits(sim$rasterToMatch, "SpatRaster")) {
    
    study_v <- if (inherits(sim$studyArea, "SpatVector")) {
      sim$studyArea
    } else {
      terra::vect(sim$studyArea)
    }
    
    sim$rasterToMatch <- terra::rast(
      ext = terra::ext(study_v),
      resolution = 240,
      crs = terra::crs(study_v)
    )
  }
  
  sim$PlanningGrid <- sim$rasterToMatch
  terra::values(sim$PlanningGrid) <- 1
  
  # =========================================================
  # 2) LandCover
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("LandCover", sim)) {
    
    message("✔ Using LandCover_ supplied from upstream or user.")
    
  } else {
    
    message("Standalone mode: creating synthetic LandCover")
    
    sim$LandCover <- terra::rast(
      sim$PlanningGrid
    )
    
    sim$LandCover[] <- 210
    
  }
  
  # =========================================================
  # 3) StandAgeMap (SCANFI 2020 only)
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("standAge", sim)) {
    
    message("✔ Using standAge supplied from upstream or user.")
    
  } else {
    
    message("Standalone mode: creating synthetic standAge")
    
    sim$standAge <- terra::rast(
      sim$PlanningGrid
    )
    
    sim$standAge[] <- 80
    
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
  # 5) CPCAD
  # =========================================================
  
  # =========================================================
  # 5) LegalConstraints
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("LegalConstraints", sim)) {    
    message("Standalone mode: creating synthetic LegalConstraints")
    
    protTmp <- terra::rast(sim$PlanningGrid)
    protTmp[] <- 0
    
    sim$LegalConstraints <- list(
      CPCAD_Raster = protTmp
    )
  }
  
  return(invisible(sim))
}

## Summary:
## EasternCanadaLandbase builds the effective harvestable
## planning landbase from prepared spatial inputs.

## Policy interpretation, ecological modeling, and harvest
## decisions are intentionally excluded from this module.

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

