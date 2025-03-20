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
  warningMessage <- paste0(oldFunc, "() was deprecated in MDMA ", version,
                           " and will be removed in a future release. Please use ",
                           newFunc, "() instead.")
  warning(warningMessage, call. = FALSE)
}
