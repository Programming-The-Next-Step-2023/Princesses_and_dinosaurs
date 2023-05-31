library(shiny)

source("stop_module.R")




princess_walking_gif <- img(src = "elsa_walk.gif")
princess_stopping_img <- img(src = "Elsa_from_Disney's_Frozen.png")
princess_stop_now_gif <- img(src = "stop_now_elsa.gif")

dino_walking_gif <- img(src = "dono_walk.gif")
dino_stopping_img <- img(src = "Elsa_from_Disney's_Frozen.png")
dino_stop_now_gif <- img(src = "stop_now_dino.gif")

btn_img <- img(src = "stop_button.png", height = "160px", width = "160px")



# Style settings app:

groupbtn <- tags$head(
  tags$style(HTML(
    ".btn-with-image {
                background: url('knopthema.png') no-repeat center center;
                background-size: contain;
                width: 160px;
                height: 160px;
                border-radius: 50%;
                margin-top: 60px;
                line-height: 150px;
                cursor: pointer;
                }")))



princess_ <- bslib::bs_theme(bootswatch = "quartz",
                             base_font = '"Verdana", sans-serif',
                             code_font = '"Verdana", sans-serif',
                             "font-size-base" = "1.8rem",
                             "font-size-code" = "1.6rem")
princess_theme <-
  bslib::bs_add_rules(princess_,
                      "body {
                        background-image:linear-gradient(#9987BE, #E3A5D7, #F4D1DA);
                        background-size: contain;
                        background-position: center;
                        min-height: 100vh;
                        width:100%;}")





princess_memorygame_btn = span(class = "d-flex flex-column align-items-center",
                   icon("th", style = "font-size: 7rem;", lib = "glyphicon"),
                   "Memory",
                   style = "height: 180px;
                   width: 180px;
                   border: 5px solid #FFB6C1;
                   margin-bottom: 60px;")


princess_stopgame_btn = span(class = "d-flex flex-column align-items-center",
                 icon("hand-paper", style = "font-size: 7rem;"),
                 "Stop!",
                 style = "height: 180px;
                   width: 180px;
                   border: 5px solid #FFB6C1;
                   margin-bottom: 60px;")

princess_btn_names = list(princess_memorygame_btn, princess_stopgame_btn)



dino_theme <- bslib::bs_theme(bootswatch = "sketchy",
                        bg = "#BFDB38",
                        fg = "#00425A",
                        base_font = '"Verdana", sans-serif',
                        code_font = '"Verdana", sans-serif',
                        "font-size-base" = "1.8rem",
                        "font-size-code" = "1.6rem")

dino_game_btn <-tags$style(
  HTML("game_choice {
    height: 170px;
    width: 170px;
    border: 8px solid #1F8A70;
    margin-bottom: 10px;
    justify-objects: center;
    align-objects: center;
    flex-direction: column;
    display: flex;
       }"
  ))

dino_memorygame_btn = span(class = "d-flex flex-column align-items-center",
                               icon("th", style = "font-size: 7rem;", lib = "glyphicon"),
                               "Memory",
                               style = "height: 180px;
                               width: 180px;
                               border: 5px solid #1F8A70;
                               margin-bottom: 60px;")


dino_stopgame_btn = span(class = "d-flex flex-column align-items-center",
                             icon("hand-paper", style = "font-size: 7rem;"),
                             "Stop!",
                             style = "height: 180px;
                             width: 180px;
                             border: 5px solid #1F8A70;
                             margin-bottom: 60px;")

dino_btn_names = list(dino_memorygame_btn, dino_stopgame_btn)
btn_values = c(1,2)



# Memory--------------

n_memo <- 3
# Images lists:


# Functions:

which_show <- function(l, indice = NULL) {
  l <- filter_found(l)
  if (length(l) == 0) {
    return(NULL)
  }
  res <- lapply(l, `[[`, "show")
  res <- unlist(res)
  if (all(!res)) {
    return(NULL)
  }
  ts <- unlist(lapply(l[res], `[[`, "ts"), use.names = FALSE)
  res <- names(l)[res]
  res <- res[order(ts, decreasing = FALSE)]
  if (is.null(indice)) {
    res
  } else {
    as_null(res[indice])
  }
}

filter_found <- function(l) {
  found <- unlist(lapply(l, `[[`, "found"), use.names = FALSE)
  l[!found]
}

all_found <- function(l) {
  found <- unlist(lapply(l, `[[`, "found"), use.names = FALSE)
  all(found)
}

as_null <- function(x) {
  if (is.na(x)) {
    NULL
  } else {
    x
  }
}

which_memo <- function(l, module) {
  res <- lapply(module, function(x) l[[x]]$memo)
  unlist(res, use.names = FALSE)
}

