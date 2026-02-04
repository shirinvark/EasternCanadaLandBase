## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
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
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    
  ),
  inputObjects = bindrows(
    expectsInput("PlanningRaster", "SpatRaster",
                 "Planning grid from EasternCanadaDataPrep"),
    expectsInput("LandCoverAligned", "SpatRaster",
                 "Aligned land cover raster"),
    expectsInput("Provinces", "sf",
                 "Province polygons"),
    expectsInput("Riparian", "list",
                 "Riparian outputs including fractional influence"),
    expectsInput("CPCAD", "sf",
                 "Protected areas")
  )
  ,
  outputObjects = bindrows(
    createsOutput("Landbase", "list",
                  "Derived landbase masks and accounting tables")
  )
))

doEvent.EasternCanadaLandbase <- function(sim, eventTime, eventType) {
  
  if (eventType == "init") {
    sim <- EasternCanadaLandbaseInit(sim)
    return(invisible(sim))
  }
  
  noEventWarning(sim)
}


### template initialization
EasternCanadaLandbaseInit <- function(sim) {
  
  # sanity checks
  checkObject(sim, "PlanningRaster", "SpatRaster")
  checkObject(sim, "LandCoverAligned", "SpatRaster")
  checkObject(sim, "CPCAD")
  
  # placeholder landbase container
  sim$Landbase <- list(
    planningRaster = sim$PlanningRaster,
    landcover      = sim$LandCoverAligned,
  )
  
  invisible(sim)
}



.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

