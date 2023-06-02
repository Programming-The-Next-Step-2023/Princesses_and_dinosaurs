# Imported packages

#'@import shiny
#'@import bslib
#'@import shinyWidgets


# Memory objects
n_memo <- 3

princess_list <- list.files(system.file("img_pd/princess_m",
                                        package = "PrincessDino"))
Princess_sample <- sample(princess_list, n_memo)
princess_png <- sample(rep(Princess_sample, 2))

dino_list <- list.files(system.file("img_pd/dino_m",
                                    package = "PrincessDino"))
dino_sample <- sample(dino_list,n_memo)
dino_png <- sample(rep(dino_sample,2))

get_princess_back <- function() {
  return(system.file("img_pd/princess_back.png",
                     package = "PrincessDino"))
}
get_dino_back <- function() {
  return(system.file("img_pd/dino_back.png",
                     package = "PrincessDino"))
}

get_img_dir <- function() {
  return(system.file("img_pd",
                     package = "PrincessDino"))
}

memo_png <- list()

# Stop! objects
princess_walking_gif <- shiny::img(src = ("img_pd/elsa_walk.gif"),
                                   hight= "640px",
                                   width = "640px")
princess_stopping_img <- shiny::img(src = ("img_pd/Elsa_from_Disney's_Frozen.png"),
                                    hight= "640px",
                                    width = "640px")
princess_stop_now_gif <- shiny::img(src = ("img_pd/stop_now_elsa.gif"),
                                    hight= "640px",
                                    width = "640px")

dino_walking_gif <- shiny::img(src = ("img_pd/dino_walk.gif"),
                               hight= "640px",
                               width = "640px")
dino_stopping_img <- shiny::img(src = ("img_pd/dino_stopped.png"),
                                hight= "640px",
                                width = "640px")
dino_stop_now_gif <- shiny::img(src = ("img_pd/stop_now_dino.gif"),
                                hight= "640px",
                                width = "640px")

btn_img <- shiny::img(src = ("img_pd/stop_button.png"),
                      height = "180px",
                      width = "180px")


# Style settings app:
groupbtn <-
  tags$style(
    HTML(
      ".round-button {
        display: inline-block;
        border-radius: 50%;
        cursor: pointer;
        width: 180px;
        height: 180px;
      }

      .theme-change-button {
        background: url(img_pd/knopthema.png) no-repeat center center;
        background-size: contain;
        width: 200px;
        height: 200px;
      }

      .stop-button {
        background: url(img_pd/stop_button.png) no-repeat center center;
        background-size: contain;
        width: 180px;
        height: 180px;
      }"
    )
  )


princess_ <- bslib::bs_theme(bootswatch = "quartz",
                             base_font = '"Verdana", sans-serif',
                             code_font = '"Verdana", sans-serif',
                             "font-size-base" = "1.8rem",
                             "font-size-code" = "1.6rem")
princess_theme <-
  bslib::bs_add_rules(princess_,
                      "body {
                        background-image:linear-gradient(#9987BE,  #F4D1DA);
                        background-size: contain;
                        background-position: center;
                        min-height: 100vh;
                        width:100%;}")

princess_memorygame_btn = shiny::span(class = "d-flex flex-column align-items-center",
                                      icon("th",
                                           style = "font-size: 7rem;",
                                           lib = "glyphicon"),
                                      "Memory",
                                      style = "border: 5px solid #FFB6C1;
                                               cursor: pointer;",
                                      class = "btn btn-sq-responsive")


princess_stopgame_btn = shiny::span(class = "d-flex flex-column align-items-center",
                                    icon("hand-paper",
                                         style = "font-size: 7rem;"),
                                    "Stop!",
                                    style = "border: 5px solid #FFB6C1;
                                            cursor: pointer;",
                                    class = "btn btn-sq-responsive")

princess_home_btn = shiny::span(class = "d-flex flex-column align-items-center",
                                icon("house",
                                     style = "font-size: 7rem;"),
                                "",
                                style = "border: 5px solid #FFB6C1;
                                         cursor: pointer;",
                                class = "btn btn-sq-responsive")

princess_btn_names = list(princess_home_btn,
                          princess_memorygame_btn,
                          princess_stopgame_btn)

dino_theme <- bslib::bs_theme(bootswatch = "sketchy",
                              bg = "#BFDB38",
                              fg = "#00425A",
                              base_font = '"Verdana", sans-serif',
                              code_font = '"Verdana", sans-serif',
                              "font-size-base" = "1.8rem",
                              "font-size-code" = "1.6rem")



dino_memorygame_btn = shiny::span(class = "d-flex flex-column align-items-center btn btn-sq-responsive",
                                  shiny::icon("th",
                                              style = "font-size: 7rem;",
                                              lib = "glyphicon"),
                                  "Memory",
                                  style = "border: 5px solid #1F8A70;
                                          cursor: pointer;",
                                  class = "btn btn-sq-responsive")


dino_stopgame_btn = shiny::span(class = "d-flex flex-column align-items-center",
                                shiny::icon("hand-paper",
                                            style = "font-size: 7rem;"),
                                "Stop!",
                                style = "border: 5px solid #1F8A70;
                                        cursor: pointer;",
                                class = "btn btn-sq-responsive")

dino_home_btn = shiny::span(class = "d-flex flex-column align-items-center",
                            shiny::icon("house",
                                        style = "font-size: 7rem;"),
                            "",
                            style = "border: 5px solid #1F8A70;
                                    cursor: pointer;",
                            class = "btn btn-sq-responsive")

dino_btn_names = list(dino_home_btn,
                      dino_memorygame_btn,
                      dino_stopgame_btn)

btn_values = c(3, 1, 2)


# this is not working yet! But I want to keep trying:)
#
# starttheme =
#   if (getOption("pd_starttheme", "princesses") == "princesses") {
#     princess_theme
#   } else {
#     dino_theme
#   }


