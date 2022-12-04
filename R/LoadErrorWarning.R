#' LoadErrorWarning
#'
#' @description Displays warning message to note that loading failed. To be used internally within LoadX type function calls.
#'
#' @param text Text to use in warning message - here, should be of form LoadX, where X is the data that is attempted to be loaded.
#' @keywords internal
#' @return prints warning.
#'
#' @examples
#' LoadErrorWarning("testing_LoadErrorWarning_message")
#' @export
LoadErrorWarning <- function(text){
  
  message(paste0("Unable to load data in the ", text, ".R function.\nThis may be due to changes to, or lack of availability of, the underlying dataset."))
  
}