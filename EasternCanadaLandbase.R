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
    expectsInput("standAgeMap", "SpatRaster",
                 "Stand age raster from upstream module"),
    expectsInput("riparianFraction", "SpatRaster",
                 "Fractional riparian influence raster"),
    expectsInput("CPCAD", "sf",
                 "Protected areas")
  ), fill = TRUE)
  ,
  
  outputObjects = data.table::rbindlist(list(
    
    createsOutput("protectedMask", "SpatRaster",
                  "Binary protected areas mask"),
    
    createsOutput("forestBase", "SpatRaster",
                  "Binary forest mask excluding wetlands"),
    
    createsOutput("merchantableForest", "SpatRaster",
                  "Effective forest area after protected and riparian reduction"),
    
    createsOutput("analysisUnitMap", "SpatRaster",
                  "Masked analysis unit raster"),
    
    createsOutput("Landbase", "list",
                  "Derived landbase container")
    
  ), fill = TRUE)
  
))

# =========================================================
# Event dispatcher
# =========================================================
doEvent.EasternCanadaLandbase <- function(sim, eventTime, eventType) {
  
  if (eventType == "init") {
    
    sim <- .inputObjects(sim)   # حتما اول این
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
# =========================================================
# .inputObjects
# =========================================================
.inputObjects <- function(sim) {
  
  # =========================================================
  # 1) PlanningGrid
  # =========================================================
  
  # =========================================================
  # 1) PlanningGrid
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("PlanningGrid_250m")) {
    
    message("Standalone mode: creating PlanningGrid inside NTEMS extent")
    
    # Read NTEMS once to get real extent + CRS
    nt_path <- file.path(sim@paths$inputPath, "CA_forest_VLCE2_2001.tif")
    nt <- terra::rast(nt_path)
    
    e <- terra::ext(nt)
    
    # Create small grid safely inside NTEMS
    sim$PlanningGrid_250m <- terra::rast(
      nrows = 50,
      ncols = 50,
      xmin  = e$xmin + 20000,
      xmax  = e$xmin + 20000 + (50 * 250),
      ymin  = e$ymin + 20000,
      ymax  = e$ymin + 20000 + (50 * 250),
      crs   = terra::crs(nt)
    )
    
    terra::values(sim$PlanningGrid_250m) <- 1
  }
  
  # =========================================================
  # 2) StudyArea
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("studyArea")) {
    
    message("Standalone mode: creating studyArea")
    
    sim$studyArea <- terra::as.polygons(
      terra::ext(sim$PlanningGrid_250m),
      crs = terra::crs(sim$PlanningGrid_250m)
    )
  }
  
  # =========================================================
  # 3) LandCover (NTEMS)
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("LandCover")) {
    
    message("Standalone mode: building NTEMS LandCover")
    
    lccOut <- LandR::prepInputs_NTEMS_LCC_FAO(
      year = 2001,
      cropTo = sim$PlanningGrid_250m,
      projectTo = sim$PlanningGrid_250m,
      disturbedCode = 240,
      faoYear = NULL
    )
    
    
    
    sim$LandCover <- lccOut$rstLCC
  }
  
  # =========================================================
  # 4) CPCAD
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("CPCAD")) {
    
    message("Standalone mode: creating empty CPCAD")
    
    sim$CPCAD <- sf::st_sf(
      geometry = sf::st_sfc(crs = terra::crs(sim$PlanningGrid_250m))
    )
  }
  
  # =========================================================
  # 5) Riparian
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("riparianFraction")) {
    
    message("Standalone mode: creating zero riparian raster")
    
    sim$riparianFraction <- terra::rast(sim$PlanningGrid_250m)
    sim$riparianFraction[] <- 0
  }
  
  # =========================================================
  # 6) StandAge
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("standAgeMap")) {
    
    message("Standalone mode: generating standAgeMap")
    
    sim$standAgeMap <- terra::rast(sim$PlanningGrid_250m)
    sim$standAgeMap[] <- sample(
      20:120,
      size = terra::ncell(sim$standAgeMap),
      replace = TRUE
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

