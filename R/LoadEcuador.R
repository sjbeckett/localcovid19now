#' LoadEcuador
#'
#' @description Reads in subnational data for Ecuador to calculate most recent estimate of per capita active COVID-19 cases. Use with LoadData() is recommended.
#'
#' @note
#' Data compiled from the Servicio Nacional de Gestión de Riesgos y Emergencias del Ecuador by Ecuacovid: \url{https://github.com/andrab/ecuacovid}.
#'
#' @return A simple feature returning the date of most recent data (DateReport), a unique region code (geoid), the region name (RegionName) and country name (Country), the number of active cases per capita (pInf) and the regions geometry (geometry).
#'
#' @examples
#' Ecuador <- LoadEcuador()
#' @seealso [LoadData()]
#' @export
LoadEcuador <- function() {
  # Data compiled from the Servicio Nacional de Gestión de Riesgos y Emergencias del Ecuador by Ecuacovid: https://github.com/andrab/ecuacovid

  geomEcuador <- NULL
  utils::data("geomEcuador", envir = environment())
  geomEcuador <- sf::st_as_sf(geomEcuador)

  CaseDate <- vroom::vroom("https://github.com/andrab/ecuacovid/raw/master/datos_crudos/positivas/cantones.csv", show_col_types = FALSE, progress = FALSE) # cantons
  # CaseDate = read.csv("https://github.com/andrab/ecuacovid/raw/master/datos_crudos/positivas/provincias.csv") #provinces

  CaseDiff <- c()
  Population <- c()
  DateReport <- c()
  canton <- c()
  provincia <- c()

  # need to link up Cantons and Provinces as some Cantons share same name
  CaseDate$can_pro <- paste0(CaseDate$canton, ",", CaseDate$provincia)
  Cantons <- unique(CaseDate$can_pro)
  for (aa in 1:length(Cantons)) {
    subsetdata <- CaseDate[which(CaseDate$can_pro == Cantons[aa]), ]
    CaseDiff[aa] <- (10 / 14) * sum(utils::tail(subsetdata$nuevas, 14))
    Population[aa] <- subsetdata$canton_poblacion[1]
    DateReport[aa] <- as.character(as.Date(utils::tail(subsetdata$created_at, 1), format = "%d/%m/%Y"))
    canton[aa] <- subsetdata$canton[1]
    provincia[aa] <- subsetdata$provincia[1]
  }

  ecu_df <- data.frame(DateReport, CaseDiff, Population, canton, provincia)

  # geometry: https://data.humdata.org/dataset/cod-ab-ecu

  # geomEcuador$can_pro <- paste0(geomEcuador$micro_name, ",", geomEcuador$macro_name)
  EcuadorMap <- dplyr::inner_join(geomEcuador, ecu_df, by = c("micro_name" = "canton", "macro_name" = "provincia"))
  EcuadorMap$Country <- EcuadorMap$country_name
  EcuadorMap$RegionName <- paste(EcuadorMap$micro_name, EcuadorMap$macro_name, EcuadorMap$country_name, sep = ", ")
  EcuadorMap$pInf <- as.numeric(EcuadorMap$CaseDiff) / as.numeric(EcuadorMap$Population)
  Ecuador_DATA <- subset(EcuadorMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))
  return(Ecuador_DATA)
}



# EC3 = EC2
# "24 de Mayo" =  "Veinticuatro de Mayo"
# EC3$ADM2_ES[which(EC3$ADM2_ES=="24 de Mayo")] =  "Veinticuatro de Mayo"
# EC3$ADM1_ES[which(EC3$ADM1_ES=="Los Rios")] = "Los Ríos"
# EC3$ADM1_ES[which(EC3$ADM1_ES=="Manabi")] = "Manabí"
# EC3$ADM1_ES[which(EC3$ADM1_ES=="Galapagos")] = "Galápagos"
# EC3$ADM1_ES[which(EC3$ADM1_ES=="Sucumbios")] = "Sucumbíos"
# EC3$ADM1_ES[which(EC3$ADM1_ES=="Bolivar")] = "Bolívar"
# EC3$ADM1_ES[which(EC3$ADM1_ES=="Santo Domingo de los Tsachilas")] = "Sto. Domingo Tsáchilas"

# DAT=unique(paste0(CaseDate$canton,", ",CaseDate$provincia))
# MAP=unique(paste0(EC3$ADM2_ES,", ",EC3$ADM1_ES))

# HH = sort(setdiff(DAT,MAP))
# KK = sort(setdiff(MAP,DAT))

## remove: "El Piedrero, Zona No Delimitada",  "Las Golondrinas, Zona No Delimitada",  "Manga del Cura, Zona No Delimitada"
# cbind(KK[-c(12,21,25)],HH)
# KK2 = KK[-c(12,21,25)]
# KK2 = KK2[c(1:12,14,13,15:27,37,39,28:36,38,40:45)]
# cbind(KK2,HH)
# for(aa in 1:length(KK2)){
# EC3$ADM2_ES[which(MAP == KK2[aa])] = strsplit(HH,",")[[aa]][1]
# }

# DAT=unique(paste0(CaseDate$canton,", ",CaseDate$provincia))
# MAP=unique(paste0(EC3$ADM2_ES,", ",EC3$ADM1_ES))
# HHL = sort(setdiff(DAT,MAP))
# KKL = sort(setdiff(MAP,DAT))
# EC3$ADM1_ES[which(EC3$ADM2_ES == "La Concordia")] = "Sto. Domingo Tsáchilas"
