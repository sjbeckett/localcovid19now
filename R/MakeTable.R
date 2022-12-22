#' Calculate Risk
#'
#' @description Calculates the percentage probability that one or more persons in a group, of a particular size g, may be infectious given the underlying prevalence of disease.
#'
#' @param p_I Probability one individual in a population is infectious.
#' @param g Event size.
#'
#' @return The risk (%) one or more individuals at an event of size g will be infectious.
#' @export
#' @seealso [estRisk()]
#' @examples
#' risk <- calcRisk(.001, 50)
#'
calcRisk <- function(p_I, g) {
  stopifnot("`g` must be a positive value." = is.numeric(g) & g>0)

  r <- 1 - (1 - p_I)**g
  return(r * 100)
}

#' Create Table of Risk Estimates
#'
#' @description Creates a table showing the estimated risk that one or more people will be infectious for the given input locations, event sizes and ascertainment biases.
#'
#' @param df_in Input data.
#' @param risk_output Name of output file.
#' @param output_prefix Folder location to store table file.
#' @param event_size Event size(s) to calculate risk for.
#' @param asc_bias_list Ascertainment bias(es) to calculate risk for, must be named.
#'
#' @return Creates, and writes to file, a table showing estimated risk that one or more people will be infectious for the given input locations, event sizes and ascertainment biases.
#' @export
#'
#' @importFrom rlang :=
#'
#' @examples
#' \dontrun{
#' Canada <- LoadData("LoadCanada")
#' create_c19r_data(Canada)
#' }
create_c19r_data <- function(df_in,
                             risk_output = sprintf("world_risk_regions_%s.csv", stringr::str_replace_all(lubridate::today(), "-", "")),
                             output_prefix = ".",
                             event_size = c(10, 15, 20, 25, 50, 100, 500, 1000, 5000),
                             asc_bias_list = cbind(AB1 = 3, AB2 = 4, AB3 = 5)) {
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

  pInf <- Nr <- geoid <- risk <- NULL
  df_in <- data.frame(df_in)
  df_in$geometry <- NULL

  risk_data <- list()


  # bind the ascertainment bias list
  df_in <- cbind(df_in, asc_bias_list)
  CN <- colnames(asc_bias_list)
  asc_bias_list <- as.matrix(df_in[, (ncol(df_in) - ncol(as.matrix(asc_bias_list)) + 1):ncol(df_in)])
  colnames(asc_bias_list) <- CN

  for (aa in 1:ncol(asc_bias_list)) {
    AB <- asc_bias_list[, aa]
    data_Nr <- df_in %>%
      dplyr::mutate(Nr = pInf * AB)

    for (size in event_size) {
      cn <- glue::glue("{colnames(asc_bias_list)[aa]}_{size}")

      riskdt <- data_Nr %>%
        dplyr::mutate(
          risk = round(calcRisk(
            Nr, size
          ), 0),
          risk = dplyr::case_when(
            risk < 1 ~ 0,
            TRUE ~ risk
          ),
          "asc_bias" = aa,
          "event_size" = size
        )
      risk_data[[cn]] <- riskdt %>%
        dplyr::select(geoid, "{cn}" := risk)
      id <- paste(colnames(asc_bias_list)[aa], size, sep = "_")
    }
  }

  risk_data_df <- purrr::reduce(.x = append(list(df_in), risk_data), .f = dplyr::left_join, by = "geoid") %>%
    dplyr::mutate(updated = lubridate::ymd(gsub("-", "", Sys.Date())))

  utils::write.csv(risk_data_df,
    risk_output,
    quote = T,
    row.names = F
  )
}
