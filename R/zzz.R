.onLoad <- function(libname, pkgname) {
  # shiny::addResourcePath(
  #   prefix = "princess_m",
  #   directoryPath = system.file(
  #     "img_pd/princess_m",
  #     package = "PrincessDino"
  #   )
  # )
  # shiny::addResourcePath(
  #   prefix = "dino_m",
  #   directoryPath = system.file(
  #     "img_pd/dino_m",
  #     package = "PrincessDino"
  #   )
  # )
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
