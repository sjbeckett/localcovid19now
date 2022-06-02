#' LoadCanada
#'
#' @description Reads in subnational data for Canada to calculate most recent estimate of per capita active COVID-19 cases.
#'
#' @note
#' Data aggregated by the COVID-19 Canada Open Data Working Group from local health resources at \url{https://github.com/ccodwg/Covid19Canada}.
#'
#' @references
#' Berry I, Soucy J-PR, Tuite A, Fisman D. Open access epidemiologic data and an interactive dashboard to monitor the COVID-19 outbreak in Canada. CMAJ. 2020 Apr 14;192(15):E420. doi: \url{https://doi.org/10.1503/cmaj.75262}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' \dontrun{
#' Canada <- LoadCanada()
#' }
#' @seealso [LoadCountries()]
#' @export
LoadCanada <- function() {
    #Load geometry and population data
    pop_canada <-NULL
    utils::data("geomCanada", "pop_canada", envir = environment())
    geomCanada <- sf::st_as_sf(geomCanada)
    
    geomCan <- sf::st_read("https://raw.githubusercontent.com/ccodwg/CovidTimelineCanada/main/geo/health_regions.geojson")
    sf::st_crs(geomCan) = 3347 # CRS for Canada:  Statistics Canada Lambert
    geomCanada = sf::st_transform(geomCan, crs = 4326)
    geomCanada$HR_UID = as.numeric(geomCanada$hruid)
    
    # Data aggregated by the COVID-19 Canada Open Data Working Group https://github.com/ccodwg/Covid19Canada
    # Berry I, Soucy J-PR, Tuite A, Fisman D. Open access epidemiologic data and an interactive dashboard to monitor the COVID-19 outbreak in Canada. CMAJ. 2020 Apr 14;192(15):E420. doi:  https://doi.org/10.1503/cmaj.75262.
    
    # Berry, I., O’Neill, M., Sturrock, S. L., Wright, J. E., Acharya, K., Brankston, G., Harish, V., Kornas, K., Maani, N., Naganathan, T., Obress, L., Rossi, T., Simmons, A. E., Van Camp, M., Xie, X., Tuite, A. R., Greer, A. L., Fisman, D. N., & Soucy, J.-P. R. (2021). A sub-national real-time epidemiological and vaccination database for the COVID-19 pandemic in Canada. Scientific Data, 8(1). doi: https://doi.org/10.1038/s41597-021-00955-2
    
    #CANADADATA <- vroom::vroom("https://raw.githubusercontent.com/ccodwg/Covid19Canada/master/timeseries_hr/cases_timeseries_hr.csv")
    CANADADATA <- vroom::vroom("https://raw.githubusercontent.com/ccodwg/CovidTimelineCanada/main/data/hr/cases_hr.csv")   
    pop_canada<- vroom::vroom("https://github.com/ccodwg/CovidTimelineCanada/raw/b9cfabfeaf256ca687564ec30d121b438e2f5d2b/geo/health_regions.csv")#contains pop and health region ID codes
    
    # shrink this data file
    DATES <- as.Date(CANADADATA$date, format = "%d-%m-%Y")
    DatIND <- which(DATES < max(DATES)-100)
    CANDATASMALLER <- CANADADATA[-DatIND, ]
    
    # Assemble the data required to calculate risk scores and match health region ID's
    CANADARISK <- c()
    C_hr <- unique(CANDATASMALLER$sub_region_1) # list of unique health regions
    C_hr <- C_hr[-which(C_hr == 9999)] # remove entries not assigned a health region
    
    
    for (aa in 1:length(C_hr)) { # loop over health regions
        subsetdata<- CANDATASMALLER[which(CANDATASMALLER$sub_region_1==C_hr[aa]),]
        MD <- max(subsetdata$date)
        LD = MD - 14
        MC = subsetdata$value[which(subsetdata$date==MD)]
        LC = subsetdata$value[which(subsetdata$date==LD)]
        
        CANADARISK$DateReport[aa] = as.character(MD)
        CANADARISK$HR_UID[aa] = C_hr[aa]
        CANADARISK$CaseDiff[aa] = (MC - LC) / 14 * 10   
    }
    
    CANDT = data.frame(CANADARISK)  
    
    # integrate datasets
    CanJoin = dplyr::inner_join(CANDT,pop_canada, by = c("HR_UID" = "hruid"))
    CanMap <- dplyr::inner_join(geomCanada, CanJoin, by = c("HR_UID" = "HR_UID"))
   
    Regions = c()
    Regions$abbrev = c("AB","BC","MB","NB","NL","NS","NT","NU","ON","PE","QC","SK","YT")
    Regions$longprovince = c("Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador","Nova Scotia","Northwest Territories","Nunavut","Ontario","Prince Edward Island","Québec","Saskatchewan","Yukon")
    Regions  = as.data.frame(Regions)
    
    CanMap <- dplyr::inner_join(CanMap,Regions,by = c("region.x" = "abbrev"))
    
    CanMap$pInf = CanMap$CaseDiff/CanMap$pop
    CanMap$Country = "Canada"
    CanMap$RegionName = paste(CanMap$name_ccodwg,CanMap$longprovince,"Canada",sep=", ")
    CanMap$geoid = paste0("CAN",CanMap$HR_UID)
    
    CANADA_DATA <- subset(CanMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

    return(CANADA_DATA)
}
