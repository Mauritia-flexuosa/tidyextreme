.onAttach <- function(libname, pkgname) {
  pkgVersion <- utils::packageDescription(pkgname, fields = "Version")

  msg <- c(
    paste0("== Welcome to tidyextreme v", pkgVersion, " =="),
    "A tidy toolbox for ETCCDI climate extreme indices",
    " ",
    "* Quick start: browseVignettes('tidyextreme')",
    "* Issues: https://github.com/Mauritia-flexuosa/tidyextreme/issues",
    "* Citation: citation('tidyextreme')",
    " "
  )

  packageStartupMessage(paste(msg, collapse = "\n"))
}
