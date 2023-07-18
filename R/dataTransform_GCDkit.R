########## Functions for data transformation ################
# These require GCDkit

#### Combined Debon calculations
#' This function combines calculation of millications and of Debon parameters
#' @export
#' @param wrdata A matrix with WR data
DebonCalcFull<-function(wrdata){

# This is a pure GCDkit function ! It should fail if GCDkit is not here
if (!requireNamespace("GCDkit", quietly = TRUE)) {
  stop(
    "Package \"GCDkit\" must be installed to use this function.",
    call. = FALSE
  )
}

  return(GCDkit::DebonCalc(GCDkit::millications(wrdata)) )
}
