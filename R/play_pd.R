#' Play_pd
#'
#' Launches the Shiny interface to play Princesses and Dragons
#' The app is developed for toddlers and pre-school age children (ca. 2-6 yr).
#' They can play two games: Memory and Stop!. They can also switch between two
#' themes: "princesses" or "dinosaurs".
#'
#' @param start_theme This argument is not working yet!
#' App will start at default theme "princesses".
#' (String character used to make the game start at the given theme.
#' Either the full theme name or the abbreviation (i.e. "prin", "dino") is
#' valid. Default is princesses.)
#'
#' @return No values are returned to R.
#'
#' @examples
#' \dontrun{
#' play_pd("dino")
#' }
#'
#' @export
play_pd <-
  function(starttheme = "princesses") {
    #?match.arg
    options("pd_starttheme" = starttheme)
  shiny::shinyApp(ui, server)
}

