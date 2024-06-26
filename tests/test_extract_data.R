#### Testing extract data function
require(apsimx)
apsimx_options(warn.versions = FALSE)

run.extract.tests <- get(".run.local.tests", envir = apsimx.options)

if(run.extract.tests){
  
  tmp.dir <- tempdir()
  dir(tmp.dir)
  ex.dir <- auto_detect_apsimx_examples()
  
  ### Test with all examples
  ex.dir.list <- dir(ex.dir, recursive = FALSE, pattern = "apsimx$")
  
  ex.dir.list2 <- ex.dir.list[c(2,4,5,7:11,13,16:20,22:24,28:30,32:35)]
  ## Excluded: AgPasture, BiomassRemovalFromPlant, Chickpea, Factorial, Grapevine
  ##           Graph, Pinus, Rotation, SCRUM, SimpleGrazing, Stock  
  ## Trying node = "Clock" - default
  for(i in ex.dir.list2){
    if(!file.exists(file.path(tmp.dir, i))) file.copy(from = file.path(ex.dir, i), to = tmp.dir)  
    cat("Simulation:", i, "\n")
    (edf <- extract_data_apsimx(i, src.dir = tmp.dir))  
    cat("\n")
  }
  
  ex.dir.list3 <- ex.dir.list[c(1, 3, 6, 12, 14, 15, 21, 25, 26, 27, 31)]
  
  for(i in ex.dir.list3){
    if(!file.exists(file.path(tmp.dir, i))) file.copy(from = file.path(ex.dir, i), to = tmp.dir)  
    cat("Simulation:", i, "\n")
    ## inspect_apsimx(i, src.dir = tmp.dir, node = "Other")
    if(i == "AgPasture.apsimx")  (edf.agp <- extract_data_apsimx(i, src.dir = tmp.dir, root = "AgPastureExample"))  
    if(i == "BiomassRemovalFromPlant.apsimx") (edf.brfp <- extract_data_apsimx(i, src.dir = tmp.dir, root = "SendingDatesFromOpperations"))  
    if(i == "Chickpea.apsimx") (edf.chp <- extract_data_apsimx(i, src.dir = tmp.dir, root = list("Continuous_TOS", "Cont_TOS")))  
    if(i == "Factorial.apsimx") (edf.fct <- extract_data_apsimx(i, src.dir = tmp.dir, root = list("RangeExperiment", "Base1")))  
    if(i == "Grapevine.apsimx") next ## (edf.grp <- extract_data_apsimx(i, src.dir = tmp.dir, root = "Vineyard"))  ## Grapevine does not have a 'Core.Zone'
    if(i == "Graph.apsimx") next
    if(i == "Pinus.apsimx") (edf.pns <- extract_data_apsimx(i, src.dir = tmp.dir, root = list("Plantation_IxF_Experiment", "Treatment", "Base")))  
    if(i == "Rotation.apsimx") (edf.rot <- extract_data_apsimx(i, src.dir = tmp.dir, root = list("Mar", "Mar")))
    if(i == "Rotation.apsimx") (edf.rot2 <- extract_data_apsimx(i, src.dir = tmp.dir, root = "Mar.Mar"))
    if(i == "SCRUM.apsimx") (edf.scrm <- extract_data_apsimx(i, src.dir = tmp.dir, root = "Crop Comparisions.CropCompBase"))
    if(i == "SimpleGrazing.apsimx"){
      cat("Start SimpleGrazing \n")
      pps <- inspect_apsimx(i, src.dir = tmp.dir, node = "Other", parm = list(1, 3:7), print.path = TRUE)
      for(j in pps){
        cat("parameter path:", j, "\n")
        rut <- strsplit(j, ".", fixed = TRUE)[[1]][3]
        (edf.sgrz <- extract_data_apsimx(i, src.dir = tmp.dir, root = rut))  
      }
    }
    if(i == "Stock.apsimx") break
    cat("\n")
  }
}

### Testing whether operations can be extracted
if(FALSE){
  tmpp.dir <- "~/Dropbox/apsimx-other-issues/matteolongo/fodderbeet_quadr"
  setwd(tmpp.dir)
  dir()
  inspect_apsimx("fodderbeet_quadr.apsimx", src.dir = tmpp.dir, node = "Other", parm = 3)
  inspect_apsimx("fodderbeet_quadr.apsimx", src.dir = tmpp.dir, node = "Operations", 
                 root = "quadr_0_l_fodderbeet")
  edfo <- extract_data_apsimx("fodderbeet_quadr.apsimx", src.dir = tmpp.dir, node = "Operations", 
                              root = "quadr_0_l_fodderbeet")
}