n_memo <- 3


princess_list <- list.files(system.file("img_pd/princess_m", package = "PrincessDino"))
Princess_sample <- sample(princess_list, n_memo)
princess_png <- sample(rep(Princess_sample, 2))

dino_list <- list.files(system.file("img_pd/dino_m", package = "PrincessDino"))
dino_sample <- sample(dino_list,n_memo)
dino_png <- sample(rep(dino_sample,2))

princess_back <- ("img_pd/princess_back.png")
dino_back <- ("img_pd/dino_back.png")

memo_png <- list()



princess_walking_gif <- img(src = ("img_pd/elsa_walk.gif"))
princess_stopping_img <- img(src = ("img_pd/Elsa_from_Disney's_Frozen.png"))
princess_stop_now_gif <- img(src = ("img_pd/stop_now_elsa.gif"))

dino_walking_gif <- img(src = ("img_pd/dino_walk.gif"))
dino_stopping_img <- img(src = ("img_pd/Elsa_from_Disney's_Frozen.png"))
dino_stop_now_gif <- img(src = ("img_pd/stop_now_dino.gif"))

btn_img <- img(src = ("img_pd/stop_button.png"), height = "180px", width = "180px")



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


#  tags$head(
#   tags$style(HTML(
#     '.theme-change-button {
#       background: url(img_pd/knopthema.png) no-repeat center center;
#       background-size: contain;
#       width: 160px;
#       height: 160px;
#       border-radius: 50%;
#       margin-top: 60px;
#       line-height: 150px;
#       cursor: pointer;
#
#     }
#
#     .stop-button {
#       background: url(img_pd/stop_button.png) no-repeat center center;
#       background-size: contain;
#       width: 200px;
#       height: 200px;
#       border-radius: 50%;
#       cursor: pointer;
#        border: none !important;
#       box-shadow: none !important;
#
#     }')))




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





princess_memorygame_btn = span(class = "d-flex flex-column align-items-center",
                   icon("th", style = "font-size: 7rem;", lib = "glyphicon"),
                   "Memory",
                   style = "border: 5px solid #FFB6C1;
                   cursor: pointer;",
                   class = "btn btn-sq-responsive")


princess_stopgame_btn = span(class = "d-flex flex-column align-items-center",
                 icon("hand-paper", style = "font-size: 7rem;"),
                 "Stop!",
                 style = "border: 5px solid #FFB6C1;
                 cursor: pointer;",
                 class = "btn btn-sq-responsive")

princess_home_btn = span(class = "d-flex flex-column align-items-center",
                         icon("house", style = "font-size: 7rem;"),
                         "",
                         style = "border: 5px solid #FFB6C1;
                                  cursor: pointer;",
                         class = "btn btn-sq-responsive")

princess_btn_names = list(princess_home_btn, princess_memorygame_btn, princess_stopgame_btn)





dino_theme <- bslib::bs_theme(bootswatch = "sketchy",
                        bg = "#BFDB38",
                        fg = "#00425A",
                        base_font = '"Verdana", sans-serif',
                        code_font = '"Verdana", sans-serif',
                        "font-size-base" = "1.8rem",
                        "font-size-code" = "1.6rem")



dino_memorygame_btn = span(class = "d-flex flex-column align-items-center btn btn-sq-responsive",
                           icon("th", style = "font-size: 7rem;", lib = "glyphicon"),
                           "Memory",
                           style = "border: 5px solid #1F8A70;
                                    cursor: pointer;",
                           class = "btn btn-sq-responsive")


dino_stopgame_btn = span(class = "d-flex flex-column align-items-center",
                         icon("hand-paper", style = "font-size: 7rem;"),
                         "Stop!",
                         style = "border: 5px solid #1F8A70;
                                  cursor: pointer;",
                         class = "btn btn-sq-responsive")

dino_home_btn = span(class = "d-flex flex-column align-items-center",
                     icon("house", style = "font-size: 7rem;"),
                     "",
                     style = "border: 5px solid #1F8A70;
                              cursor: pointer;",
                     class = "btn btn-sq-responsive")

dino_btn_names = list(dino_home_btn, dino_memorygame_btn, dino_stopgame_btn)
btn_values = c(3, 1, 2)



# Memory--------------



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

