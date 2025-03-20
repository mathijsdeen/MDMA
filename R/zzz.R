.onAttach <- function(libname, pkgname) {

  # Check if MDMA was installed from CRAN. If not: caveat.
  desc_file <- system.file("DESCRIPTION", package = "MDMA")
  if (file.exists(desc_file)) {
    desc <- read.dcf(desc_file)
    if (!("Repository" %in% colnames(desc) && desc[1, "Repository"] == "CRAN")) {
      packageStartupMessage("This is a beta version of MDMA. Install the latest stable release from CRAN.")
    }
  }
}

.warningDeprecated <- function(newFunc, version){
  oldFunc <- as.character(sys.call(sys.parent()))[[1]] # qui vocavit me

  # Should probably write a wrapper for text color and styling.
  red        <- "\033[31m"
  green      <- "\033[32m"
  blue       <- "\033[34m"
  reset_col  <- "\033[0m"
  bold       <- "\033[1m"
  reset_bold <- "\033[22m"
  italic     <- "\033[3m"
  reset_itlc <- "\033[23m"
  underline  <- "\033[4m"
  reset_ul   <- "\033[24m"

  warningMessage <- paste0(red, bold, "Warning:", reset_bold, " ",
                           oldFunc, "() was ", italic, "deprecated", reset_itlc, " in ",
                           blue, "MDMA ", version, red,
                           " and will be ", underline, "removed", reset_ul,
                           " in a future release. \nPlease use ",
                           green, newFunc, "()", red, " instead.", reset_col)
  message(warningMessage)
}
