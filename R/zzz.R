.onLoad <- function(libname, pkgname) {

  shiny::addResourcePath(
    prefix = "img_pd",
    directoryPath = system.file(
      "img_pd",
      package = "PrincessDino"
    )
  )
}

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath(c("img_pd"))
}
