## Everything in this file and any files in the R directory are sourced during simInit()
## All functions and objects are put into the simList.
## Functions are called directly by name (new SpaDES convention).

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
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "EasternCanadaLandbase.Rmd"),
  reqdPkgs = list("terra", "sf", "LandR"),
  
  inputObjects = data.table::rbindlist(list(
    expectsInput("PlanningGrid_250m", "SpatRaster",
                 "Planning grid from EasternCanadaDataPrep"),
    expectsInput("LandCover", "SpatRaster",
                 "land cover raster"),
    expectsInput("studyArea", "sf",
                 "Study area polygon from upstream module"),
    
    expectsInput("CPCAD", "sf",
                 "Protected areas")
  ), fill = TRUE),
  
  outputObjects = data.table::rbindlist(list(
    
    createsOutput("standAgeMap", "SpatRaster",
                  "Stand age raster created for AAC calculations"),
    
    createsOutput("analysisUnitMap", "SpatRaster",
                  "Temporary analysis unit raster derived from land cover"),
    
    createsOutput("Landbase", "list",
                  "Derived landbase container")
    
  ), fill = TRUE)
))

# =========================================================
# Event dispatcher
# =========================================================
doEvent.EasternCanadaLandbase <- function(sim, eventTime, eventType) {
  
  if (eventType == "init") {
    
    sim <- .inputObjects(sim)
    sim <- Init(sim)
    
    return(invisible(sim))
  }
  
  noEventWarning(sim)
}



# =========================================================
# Init event (NEW NAMING CONVENTION – IMPORTANT)
# =========================================================
# =========================================================
# 
# =========================================================
#.inputObjects <- function(sim) {
  
  # ---- Stand Age ----
 # if (!SpaDES.core::suppliedElsewhere("standAgeMap")) {
    
  #  message("Creating standAgeMap")
    
   # dPath <- file.path(sim@paths$inputPath, "StandAge")
    #if (!dir.exists(dPath)) dir.create(dPath, recursive = TRUE)
    
    #sim$standAgeMap <- LandR::prepInputsStandAgeMap(
     # rasterToMatch   = sim$PlanningGrid_250m,
      #studyArea       = sim$studyArea,
      #destinationPath = dPath,
      #dataYear        = 2011
    #)
  #}
  
  # ---- Analysis Unit ----
  #if (!SpaDES.core::suppliedElsewhere("analysisUnitMap")) {
    
   # message("Creating temporary analysisUnitMap")
    
    #analysisUnitMap <- sim$LandCover
    #analysisUnitMap[] <- 0
    
    #analysisUnitMap[sim$LandCover == 210] <- 1
    #analysisUnitMap[sim$LandCover == 220] <- 2
    #analysisUnitMap[sim$LandCover == 230] <- 3
    #analysisUnitMap[sim$LandCover == 240] <- 4
    
    #sim$analysisUnitMap <- analysisUnitMap
  #}
  
  
#  return(invisible(sim))
#.inputObjects <- function(sim) {

# =========================================================
# Planning Grid
# =========================================================
.inputObjects <- function(sim) {
  
  # =========================================================
  # 1) LandCover FIRST (real spatial reference)
  # =========================================================
  if (!SpaDES.core::suppliedElsewhere("LandCover")) {
    
    message("Downloading SCANFI LandCover")
    
    dPath <- file.path(sim@paths$inputPath, "LandCover")
    if (!dir.exists(dPath)) dir.create(dPath, recursive = TRUE)
    
    # Use small test raster first
    testRas <- terra::rast(
      nrows = 200,
      ncols = 200,
      xmin = -2600000,
      xmax = -2550000,
      ymin = 7000000,
      ymax = 7050000,
      crs = "EPSG:3978"
    )
    
    terra::values(testRas) <- 1
    
    sim$LandCover <- LandR::prepInputs_SCANFI_LCC_FAO(
      rasterToMatch = testRas,
      studyArea     = terra::as.polygons(terra::ext(testRas),
                                         crs = terra::crs(testRas)),
      destinationPath = dPath
    )
  }
  
  
  # =========================================================
  # 2) PlanningGrid FROM LandCover
  # =========================================================
  sim$PlanningGrid_250m <- terra::rast(sim$LandCover)
  
  
  # =========================================================
  # 3) StudyArea
  # =========================================================
  sim$studyArea <- terra::as.polygons(
    terra::ext(sim$PlanningGrid_250m),
    crs = terra::crs(sim$PlanningGrid_250m)
  )
  
  
  # =========================================================
  # 4) StandAge aligned to grid
  # =========================================================
  if (!SpaDES.core::suppliedElsewhere("standAgeMap")) {
    
    message("Loading standAgeMap")
    
    dPath <- file.path(sim@paths$inputPath, "StandAge")
    if (!dir.exists(dPath)) dir.create(dPath, recursive = TRUE)
    
    sim$standAgeMap <- LandR::prepInputsStandAgeMap(
      rasterToMatch   = sim$PlanningGrid_250m,
      studyArea       = sim$studyArea,
      destinationPath = dPath,
      dataYear        = 2011
    )
  }
  
  
  # =========================================================
  # 5) AnalysisUnit
  # =========================================================
  analysisUnitMap <- sim$LandCover
  analysisUnitMap[] <- 0
  
  analysisUnitMap[sim$LandCover == 210] <- 1
  analysisUnitMap[sim$LandCover == 220] <- 2
  analysisUnitMap[sim$LandCover == 230] <- 3
  analysisUnitMap[sim$LandCover == 240] <- 4
  
  sim$analysisUnitMap <- analysisUnitMap
  
  
  # =========================================================
  # 6) Empty CPCAD
  # =========================================================
  sim$CPCAD <- sf::st_sf(
    geometry = sf::st_sfc(crs = terra::crs(sim$PlanningGrid_250m))
  )
  
  return(invisible(sim))
}

## Summary:
## EasternCanadaDataPrep standardizes spatial inputs and
## exposes clean, reusable objects for downstream analysis.
##
## Policy interpretation, ecological modeling, and harvest
## decisions are intentionally excluded from this module.

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

