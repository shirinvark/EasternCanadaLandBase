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
    expectsInput(
      "studyArea",
      "sf",
      "Study area polygon"
    ),
    
    expectsInput("LandCover_250m", "SpatRaster",
                 "Land cover raster aligned to PlanningGrid_250m"),
    
    expectsInput("standAge_250m", "SpatRaster",
                 "Stand age raster aligned to PlanningGrid_250m"),
    
    expectsInput("Riparian", "list",
             "List containing riparianFraction (SpatRaster)"),

expectsInput("LegalConstraints", "list",
             "List containing CPCAD_Raster_250m (SpatRaster)")
    
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
  # 1) PlanningGrid (اول باید ساخته شود)
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("PlanningGrid_250m", sim)) {    
    message("Standalone mode: creating synthetic PlanningGrid")
    
    sim$PlanningGrid_250m <- terra::rast(
      nrows = 50, ncols = 50,
      xmin = 0, xmax = 12500,
      ymin = 0, ymax = 12500,
      res  = 250,
      crs  = "EPSG:5070"
    )
    
    sim$PlanningGrid_250m[] <- 1
  }
  
  
  # =========================================================
  # 2) LandCover
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("LandCover_250m", sim)) {
    
    message("✔ Using LandCover_250m supplied from upstream or user.")
    
  } else {
    
    dPath <- SpaDES.core::dataPath(sim)
    
    lc_dir <- file.path(dPath, "LandCover")
    dir.create(lc_dir, showWarnings = FALSE, recursive = TRUE)
    
    lc_file <- file.path(lc_dir, "LandCover_250m.tif")
    
    if (file.exists(lc_file)) {
      
      message("✔ LandCover_250m found locally. Loading...")
      
      sim$LandCover_250m <- terra::rast(lc_file)
      
    } else {
      
      message("⬇ LandCover_250m not found locally. Downloading from Drive...")
      
      sim$LandCover_250m <- Cache(
        prepInputs,
        url = "https://drive.google.com/uc?export=download&id=1Gzhd5VnIZ7MqRSRJmNFiGfVUHrKkP9Ag",
        destinationPath = lc_dir,
        targetFile = "LandCover_250m.tif",
        fun = terra::rast,
        overwrite = FALSE
      )
    }
  }
  
  # =========================================================
  # 3) StandAgeMap (SCANFI 2020 only)
  # =========================================================
  
  if (SpaDES.core::suppliedElsewhere("standAge_250m", sim)) {
    
    message("✔ Using standAge_250m supplied from upstream or user.")
    
  } else {
    
    dPath <- SpaDES.core::dataPath(sim)
    
    sa_dir <- file.path(dPath, "StandAge")
    dir.create(sa_dir, showWarnings = FALSE, recursive = TRUE)
    
    sa_file <- file.path(
      sa_dir,
      "standAge_250m.tif"
    )
    
    if (file.exists(sa_file)) {
      
      message("✔ standAge_250m found locally. Loading...")
      
      sim$standAge_250m <- terra::rast(sa_file)
      
    } else {
      
      message("⬇ standAge_250m not found locally. Downloading from Drive...")
      
      sim$standAge_250m <- Cache(
        prepInputs,
        url = "https://drive.google.com/uc?export=download&id=1OdZ7Tznk53KceEyt9dFOBOkxDHEX5X0U",
        destinationPath = sa_dir,
        targetFile = "standAge_250m.tif",
        fun = terra::rast,
        overwrite = FALSE
      )
    }
  }
  # =========================================================
  # 4) Riparian
  # =========================================================
  
  if (!SpaDES.core::suppliedElsewhere("Riparian", sim)) {
    
    ripTmp <- terra::rast(sim$PlanningGrid_250m)
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
    
    protTmp <- terra::rast(sim$PlanningGrid_250m)
    protTmp[] <- 0
    
    sim$LegalConstraints <- list(
      CPCAD_Raster_250m = protTmp
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

