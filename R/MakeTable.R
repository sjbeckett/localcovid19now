calc_risk <- function(p_I, g) {
  r <- 1 - (1 - p_I)**g
  return(round(r * 100, 1))
}

create_c19r_data <- function(GLOBALDAT,
                             risk_output = sprintf("world_risk_regions/world_risk_regions_%s.csv", str_replace_all(lubridate::today(), "-", "")),
                             output_prefix = ".",
                             event_size = c(10, 15, 20, 25, 50, 100, 500, 1000, 5000),
                             asc_bias_list = c(3, 4, 5)) {
  library(sf) # needed to make tibble happen for joins

  if (!all(is.numeric(event_size)) & !all(event_size > 0)) {
    stop("'event_size' must be a vector of positive numbers")
  }

  if (!all(is.numeric(asc_bias_list)) & !all(asc_bias_list > 0)) {
    stop("'asc_bias_list' must be a vector of positive numbers")
  }

  risk_output <- file.path(output_prefix, risk_output)
  if (file.access(dirname(risk_output), mode = 2) != 0) {
    stop("Directory for risk_output file does not appear to be writeable.")
  }

  risk_data <- list()

  for (asc_bias in asc_bias_list) {
    data_Nr <- GLOBALDAT %>%
      dplyr::mutate(Nr = pInf * asc_bias)

    # if (dim(data_Nr)[1] > 2000) {
    for (size in event_size) {
      cn <- glue::glue("{asc_bias}_{size}")

      riskdt <- data_Nr %>%
        dplyr::mutate(
          risk = round(calc_risk(
            Nr, size
          ), 0),
          risk = case_when(
            risk < 1 ~ 0,
            TRUE ~ risk
          ),
          "asc_bias" = asc_bias,
          "event_size" = size
        )
      risk_data[[cn]] <- riskdt %>%
        dplyr::select(geoid, "{cn}" := risk)
      id <- paste(asc_bias, size, sep = "_")
    }
    # }
  }

  risk_data_df <- purrr::reduce(.x = append(list(GLOBALDAT), risk_data), .f = left_join) %>%
    dplyr::mutate(updated = lubridate::ymd(gsub("-", "", Sys.Date())))

  utils::write.csv(risk_data_df,
    risk_output,
    quote = T,
    row.names = F
  )
}
