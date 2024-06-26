## Testing downloading soil data from ISRIC
## In repeated testing it seems that speed slows down when multiple queries are requested

require(apsimx)

run.test.isric.workflow <- get(".run.local.tests", envir = apsimx.options)

if(run.test.isric.workflow){
  
  trrry <- try(sp1 <- get_isric_soil_profile(lonlat = c(-93, 42)), silent = TRUE)
  
  if(!inherits(trrry, "try-error")){
    # This takes a few seconds
    system.time(sp1 <- get_isric_soil_profile(lonlat = c(-93, 42), fix = TRUE)) ## 1.508 seconds
    system.time(sp2 <- get_ssurgo_soil_profile(lonlat = c(-93, 42), fix = TRUE))  ## 1.858 seconds
    
    plot(sp1)
    
    plot(sp2[[1]])
    
    ## The code below now should add LL15, KL and XF
    sp3 <- get_isric_soil_profile(lonlat = c(-93, 42), fix = TRUE, xargs = list(crops = "Oilpalm"))
    ## Only get SSURGO tables
    ## stbls <- get_ssurgo_tables(lonlat = c(-93, 42))    
  }
}
