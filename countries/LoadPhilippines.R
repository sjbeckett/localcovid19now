LoadPhilippines <- function() {

  # Republic of Philippines Department of Health: https://doh.gov.ph/covid19tracker

  if ("oauth" %in% ls()) {
    oauthpath <- oauth
  } else {
    oauthpath <- readline(prompt = "Enter OAuth filepath:  ")
  }

  drive_auth(path = oauthpath)

  url1 <- "bit.ly/DataDropPH"
  req1 <- GET(url1)
  folder_dr <- drive_ls(str_extract(req1$url, "[:graph:]*(?=\\?)"))

  data_link <- drive_read_raw(
    file = paste("https://drive.google.com/file/d", pull(folder_dr["id"]), sep = "/")
  ) %>%
    pdf_text() %>%
    str_extract("(?<=bit.ly)[:graph:]*") %>%
    .[!is.na(.)]

  url2 <- paste0("https://bit.ly", data_link)

  req2 <- GET(url2)
  folder_data <- drive_ls(str_extract(req2$url, "[:graph:]*(?=\\?)"))

  caseinfo_ids <- folder_data %>%
    filter(str_detect(name, "04 Case Information")) %>%
    arrange(name) %>%
    select(id) %>%
    pull()

  purrr::map_df(
    caseinfo_ids,
    function(x) {
      temp <- tempfile()
      drive_download(
        file = paste("https://drive.google.com/file/d", x, sep = "/"),
        path = temp
      )
      A <- vroom(
        temp,
        col_types = cols(
          DateSpecimen = col_date(format = "%Y-%m-%d"),
          DateResultRelease = col_date(format = "%Y-%m-%d"),
          DateRepConf = col_date(format = "%Y-%m-%d"),
          DateDied = col_date(format = "%Y-%m-%d"),
          DateRecover = col_date(format = "%Y-%m-%d"),
          DateOnset = col_date(format = "%Y-%m-%d")
        )
      )
      unlink(temp)
      return(A)
    }
  ) -> case_details

  philippinesData <- case_details %>%
    mutate(
      ProvRes = case_when(
        str_detect(ProvRes, "\\(") == T ~ str_to_title(str_replace(ProvRes, "\\s\\(([:graph:]*[:blank:]?)*\\)", "")),
        RegionRes == "NCR" & is.na(ProvRes) ~ "NCR", # the NCR region doesn't have provinces, so I would assume that if the Region is NCR, then the province would also be NCR.
        TRUE ~ str_to_title(ProvRes)
      ),
      ProvRes = str_replace_all(
        ProvRes,
        "\\sDel\\s",
        " del "
      ),
      ProvRes = str_replace_all(
        ProvRes,
        "\\sDe\\s",
        " de "
      ),
      ProvRes = str_replace_all(
        ProvRes,
        "\\sOf\\s",
        " of "
      ),
      ProvRes = str_replace_all(
        ProvRes,
        "^Ncr$",
        "NCR"
      )
    ) %>%
    group_by(
      DateRepConf,
      RegionRes,
      ProvRes
    ) %>%
    summarise(
      TotalReported = n()
    ) %>%
    ungroup() %>%
    rename(
      Date = DateRepConf,
      Region = RegionRes,
      Province = ProvRes,
      Cases = TotalReported
    )

  ### Population
  philippinesPop <- vroom("countries/data/philippinesPop2015.csv")

  ### Municipalities:
  province <- unique(philippinesData$Province)
  province <- province[is.na(province) == F]
  getData <- function(code) {
    temp <- philippinesData %>% filter(Province == code)
    temp$CumSum <- cumsum(temp$Cases)
    today <- temp$Date[length(temp$Date)]
    past_date <- today - 14
    pastData <- temp[temp$Date <= past_date, ]
    difference <- (temp$CumSum[length(temp$CumSum)] - pastData$CumSum[length(pastData$CumSum)]) / 14 * 10
    vec <- data.frame(Province = code, Date = today, Difference = difference)
    return(vec)
  }

  philippinesTable <- map_df(
    province, ~ getData(.x)
  )

  ### Geometry:

  geomPhilippines <- st_read("countries/data/geom/geomPhilippines.geojson")

  geomPhilippines <- geomPhilippines %>%
    left_join(
      philippinesPop,
      by = c("micro_name" = "Location")
    )

  philippinesMap <- inner_join(geomPhilippines, philippinesTable, by = c("micro_name" = "Province"))
  philippinesMap$RegionName <- paste(philippinesMap$micro_name, philippinesMap$country_name, sep = ", ")
  philippinesMap$Country <- philippinesMap$country_name
  philippinesMap$DateReport <- as.character(philippinesMap$Date)
  philippinesMap$pInf <- philippinesMap$Difference / philippinesMap$Pop2015
  PHILIPPINES_DATA <- subset(philippinesMap, select = c("DateReport", "geoid", "RegionName", "Country", "pInf", "geometry"))

  return(PHILIPPINES_DATA)
}
