#' Data source is USDA-NRCS Soil Data Access. See package soilDB for more details
#' 
#' @title Retrieve soil data and convert it to a grid object of class \sQuote{soil_profile_grid}
#' @description Generate a synthetic soil profile grid based on the Web Coverage Service from soilDB package
#' @name get_wcs_soil_profile_grid
#' @param aoi area of interest compatible with \sQuote{mukey.wcs}
#' @param db soil database to use (default is gSSURGO)
#' @param soil.bottom see \code{\link{ssurgo2sp}}
#' @param method interpolation method see \code{\link{ssurgo2sp}}
#' @param nlayers number for layer for the new soil profile
#' @param check whether to check for reasonable values using \code{\link{check_apsimx_soil_profile}}.
#' TRUE by default. If \sQuote{fix} is TRUE, it will be applied only after the fix attempt.
#' @param fix whether to fix compatibility between saturation and bulk density (default is FALSE).
#' @param verbose default FALSE. Whether to print messages.
#' @param xargs additional arguments passed to \code{\link{apsimx_soil_profile}} function.
#' @return this function will always return a list. Each element of the list will
#' be an object of class \sQuote{soil_profile_grid}
# @export
#' @noRd
#' @examples 
#' \dontrun{
#' require(soilDB)
#' ## Example in progress
#' }
#' 
#' 

get_wcs_soil_profile_grid <- function(aoi, 
                                      db = "gSSURGO"){
                                      # soil.bottom = 200,
                                      # method = c("constant", "linear"),
                                      # nlayers = 10,
                                      # check = TRUE,
                                      # fix = FALSE,
                                      # verbose = FALSE,
                                      # xargs = NULL){
  
  if(!requireNamespace("soilDB", quietly = TRUE)){
    stop("The soilDB package is required for this function")
    return(NULL)
  }
  
  if(!requireNamespace("sp", quietly = TRUE)){
    stop("The sp package is required for this function")
    return(NULL)
  }
  
  if(!requireNamespace("sf", quietly = TRUE)){
    stop("The sf package is required for this function")
    return(NULL)
  }
  
  if(!requireNamespace("spData", quietly = TRUE)){
    stop("The spData package is required for this function")
    return(NULL)
  }
  
  ## Determine if the location is in the US
  # if(requireNamespace("maps", quietly = TRUE)){
  #   country <- maps::map.where(x = lon, y = lat)
  #   if(country != "USA" || is.na(country))
  #     stop("These coordinates do not correspond to a location in the USA. \n Did you specify the coordinates correctly?")
  # }
  
  #### Getting the mukey
  mukeys <- soilDB::mukey.wcs(aoi = aoi, 
                              db = db,
                              quiet = TRUE)
  
  ### Variables I need for ssurgo2sp
  ### Texture: sandtotal_r, silttotal_r, claytotal_r
  ### Hydrology: ksat_r, wtenthbar_r, wthirdbar_r, wfifteenbar_r, wsatiated_r
  ### Bulkdensity: 
  ### Organic matter: om_r
  # prpty <- c(sandtotal_r, silttotal_r, claytotal_r)
  # sda.properties <- soilDB::get_SDA_property(property = )
  # 
  # ### Mapunit ### -- this might contain the iacornsr
  # if(verbose == FALSE){
  #   mapunit <- suppressWarnings(suppressMessages(soilDB::get_mapunit_from_SDA(sql)))
  # }else{
  #   mapunit <- soilDB::get_mapunit_from_SDA(sql) 
  # }
  # 
  # ### Component ###
  # cmpnt <- fSDA@site
  # names(cmpnt) <- gsub("_", ".", names(cmpnt), fixed = TRUE)
  # cmpnt$geomdesc <- cmpnt$geompos
  # 
  # ## Retrieve the state from the areasymbol
  # if(shift <= 0 || length(unique(mapunit$areasymbol)) == 1){
  #   cmpnt$state <- unique(strtrim(mapunit$areasymbol, 2))
  # }else{
  #   cmpnt$state <- NA
  #   warning("This area includes more than one state. 
  #           I have not though about how to get the state in this case. Please submit an issue 
  #           with a reproducible example to https://github.com/femiguez/apsimx/issues")
  # }
  # 
  # ### Chorizon ###
  # chrzns <- fSDA@horizons
  # names(chrzns) <- gsub("_", ".", names(chrzns), fixed = TRUE)
  # ### Things missing from horizons: hzthk.r, partdensity, wsatiated.r, wfifteenbar.r, wtenthbar.r, wthirdbar.r,
  # if(sum(grepl("partdensity", names(chrzns))) == 0) chrzns$partdensity <- NA
  # if(sum(grepl("hzthk", names(chrzns))) == 0) chrzns$hzthk.r <- NA
  # if(sum(grepl("wsatiated", names(chrzns))) == 0) chrzns$wsatiated.r <- NA
  # if(sum(grepl("wfifteenbar", names(chrzns))) == 0) chrzns$wfifteenbar.r <- NA
  # if(sum(grepl("wthirdbar", names(chrzns))) == 0) chrzns$wthirdbar.r <- NA
  # 
  # if(shift <= 0){
  #   spg.sf <- sf::st_as_sf(spg)
  #   spg.sf[["MUKEY"]] <- res$mukey
  #   spg.sf[["AREASYMBOL"]] <- mapunit$areasymbol
  #   mapunit.shp <- spg.sf
  # }else{
  #   mapunit.shp <- sf::st_as_sf(res)
  # }
  # 
  # sp0 <- ssurgo2sp(mapunit = mapunit, component = cmpnt,
  #                  chorizon = chrzns, mapunit.shp = mapunit.shp,
  #                  nmapunit = nmapunit, nsoil = nsoil, xout = xout,
  #                  soil.bottom = soil.bottom, method = method, nlayers = nlayers,
  #                  verbose = verbose)
  # 
  # ans <- vector("list", length(sp0))
  # 
  # for(i in seq_along(sp0)){
  #   metadata <- attributes(sp0[[i]])
  #   metadata$DataSource <- paste("SSURGO (https://sdmdataaccess.nrcs.usda.gov/) through R package soilDB, R package apsimx function ssurgo2sp. Timestamp",Sys.time())
  #   metadata$names <- NULL; metadata$class <- NULL; metadata$row.names <- NULL;
  #   
  #   if(fix){
  #     icheck <- check
  #     check <- FALSE
  #   } 
  #   
  #   if(!is.null(xargs)){
  #     if(!is.null(xargs$crops)){
  #       crops <- xargs$crops
  #     }
  #   }else{
  #     crops <- c("Maize", "Soybean", "Wheat")
  #   }
  #   
  #   asp <- apsimx_soil_profile(nlayers = nlayers,
  #                              Thickness = sp0[[i]]$Thickness * 10,
  #                              BD = sp0[[i]]$BD,
  #                              AirDry = sp0[[i]]$AirDry,
  #                              LL15 = sp0[[i]]$LL15,
  #                              DUL = sp0[[i]]$DUL,
  #                              SAT = sp0[[i]]$SAT,
  #                              KS = sp0[[i]]$KS,
  #                              Carbon = sp0[[i]]$Carbon,
  #                              crop.LL = sp0[[i]]$LL15,
  #                              ParticleSizeClay = sp0[[i]]$ParticleSizeClay,
  #                              ParticleSizeSilt = sp0[[i]]$ParticleSizeSilt,
  #                              ParticleSizeSand = sp0[[i]]$ParticleSizeSand,
  #                              soil.bottom = soil.bottom,
  #                              metadata = metadata,
  #                              check = check, 
  #                              crops = crops)
  #   
  #   if(fix){
  #     asp <- fix_apsimx_soil_profile(asp, verbose = verbose)
  #     if(icheck)
  #       check_apsimx_soil_profile(asp)
  #   } 
  #   
  #   ans[[i]] <- asp
  # }
  # 
  # return(ans)
}