# Dialog functions

start_modal <- function () {
  shiny::modalDialog(
    tags$div(
      style = "text-align: center;",
      tags$h3("Stop optijd!")
    ),
    tags$br(), tags$br(),
    shiny::actionButton(
      inputId = "button_start",
      label = "Start!",
      style = "width: 100%;"),
    footer = NULL,
    easyClose = FALSE
  )
}

modal_go <- function() {
  shiny::modalDialog(
    title = "Druk sneller op de knop",
    style = "text-align: center;",
    shiny::actionButton(
      inputId = "button_start",
      label = "Probeer nog een keer!",
      style = "width: 100%;"),
    footer = NULL,
    easyClose = FALSE
  )
}

modal_win <- function() {
  shiny::modalDialog(
    title = "Winnaar!",
    style = "text-align: center;",
    shiny::actionButton(
      inputId = "button_start",
      label = "Nog een keer!",
      style = "width: 100%;"),
    footer = NULL,
    easyClose = FALSE
  )
}

memo_win <- function () {
  shiny::modalDialog(
    tags$div(
      style = "text-align: center;",
      tags$h2(
        tags$span(icon("trophy"), style = "color: #F7E32F;"),
        "Winnaar!",
        tags$span(icon("trophy"), style = "color: #F7E32F;")
      ),
      tags$br(), tags$br(),
      shiny::actionButton(
        inputId = "reload",
        label = "Nog een keer!",
        style = "width: 100%;"
      )
    ),
    footer = NULL,
    easyClose = FALSE
  )
}

found_pair <- function () {
  shiny::showNotification(
    ui = tags$div(
      style = "font-size: 160%; font-weight: bold;",
      sample(
        x = c("Goed gedaan!", "Bravo!", "Geweldig!", "Goed gedaan!",
              "Geweldig!", "Hoera!"),
        size = 1
      )
    ), type = "message"
  )
}


# Functions Memory:

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


