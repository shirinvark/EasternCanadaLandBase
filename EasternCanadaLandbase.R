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
  reqdPkgs = list("terra", "sf"),
  
  inputObjects = data.table::rbindlist(list(
    expectsInput("PlanningRaster", "SpatRaster",
                 "Planning grid from EasternCanadaDataPrep"),
    expectsInput("LandCoverAligned", "SpatRaster",
                 "Aligned land cover raster"),
    expectsInput("CPCAD", "sf",
                 "Protected areas")
  ), fill = TRUE),
  
  outputObjects = data.table::rbindlist(list(
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
# Init event (NEW NAMING CONVENTION – IMPORTANT)
# =========================================================
Init <- function(sim) {
  
  checkObject(sim, "PlanningRaster", "SpatRaster")
  checkObject(sim, "LandCoverAligned", "SpatRaster")
  checkObject(sim, "CPCAD")
  
  # EasternCanadaLandbase assembles and maintains the spatial landbase
  # on which subsequent modules will classify managed forested stands
  # into analysis units. No ecological or management classification
  # is performed in this module.
  
  sim$Landbase <- list(
    planningRaster = sim$PlanningRaster,
    landcover      = sim$LandCoverAligned
  )
  
  invisible(sim)
}
