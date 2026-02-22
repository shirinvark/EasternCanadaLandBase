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
    
    createsOutput("protectedAreaMask", "SpatRaster",
                  "Binary protected areas mask"),
    
    createsOutput("forestMask", "SpatRaster",
                  "Binary forest mask excluding wetlands"),
    
    createsOutput("harvestableFraction", "SpatRaster",
                  "Effective forest area after protected and riparian reduction"),
    
    createsOutput("analysisUnitMap", "SpatRaster",
                  "Temporary development analysis unit raster (to be modularized later"),
    
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
  # 1) LandCover (اول باید مشخص شود)
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("LandCover")) {
    
    message("Standalone mode: creating synthetic NTEMS-like LandCover")
    
    # اگر PlanningGrid هنوز نیست، موقتاً یکی بسازیم
    if (!SpaDES.core::suppliedElsewhere("PlanningGrid_250m")) {
      
      sim$PlanningGrid_250m <- terra::rast(
        nrows = 50, ncols = 50,
        xmin = 0, xmax = 12500,
        ymin = 0, ymax = 12500,
        res  = 250,
        crs  = "EPSG:5070"
      )
      sim$PlanningGrid_250m[] <- 1
    }
    
    sim$LandCover <- terra::rast(sim$PlanningGrid_250m)
    
    sim$LandCover[] <- as.integer(sample(
      c(81, 210, 220, 230),
      terra::ncell(sim$LandCover),
      replace = TRUE
    ))
  }
  
  
  # =========================================================
  # 2) PlanningGrid (اگر هنوز وجود ندارد)
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("PlanningGrid_250m")) {
    
    message("Creating PlanningGrid centered on valid LandCover data")
    
    # پیدا کردن یک نقطه معتبر
    xy <- terra::spatSample(
      sim$LandCover,
      size = 1,
      method = "random",
      na.rm = TRUE,
      as.points = TRUE
    )
    
    coords <- terra::geom(xy)[1, c("x", "y")]
    
    x0 <- coords[1]
    y0 <- coords[2]
    
    sim$PlanningGrid_250m <- terra::rast(
      nrows = 50,
      ncols = 50,
      xmin  = x0 - (25 * 250),
      xmax  = x0 + (25 * 250),
      ymin  = y0 - (25 * 250),
      ymax  = y0 + (25 * 250),
      crs   = terra::crs(sim$LandCover)
    )
    
    sim$PlanningGrid_250m[] <- 1
  }
  
  
  # =========================================================
  # 3) StandAge
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("standAgeMap")) {
    
    message("Standalone mode: creating synthetic standAgeMap")
    
    sim$standAgeMap <- terra::rast(sim$PlanningGrid_250m)
    
    sim$standAgeMap[] <- sample(
      20:120,
      terra::ncell(sim$standAgeMap),
      replace = TRUE
    )
  }
  
  
  # =========================================================
  # 4) Riparian
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("riparianFraction")) {
    
    message("Standalone mode: creating zero riparianFraction")
    
    sim$riparianFraction <- terra::rast(sim$PlanningGrid_250m)
    sim$riparianFraction[] <- 0
  }
  
  
  # =========================================================
  # 5) CPCAD
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("CPCAD")) {
    
    message("Standalone mode: creating empty CPCAD")
    
    sim$CPCAD <- sf::st_sf(
      geometry = sf::st_sfc(crs = terra::crs(sim$PlanningGrid_250m))
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

