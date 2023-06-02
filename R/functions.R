# Dialog

start_modal <- function () {
  modalDialog(
    tags$div(
      style = "text-align: center;",
      tags$h3("Stop Elsa optijd!")
    ),
    tags$br(), tags$br(),

    actionButton(
      inputId = "button_start",
      label = "Start!",
      style = "width: 100%;"),
    footer = NULL,
    easyClose = TRUE
  )}


modal_win <- function() {
  modalDialog(
    title = "Winnaar!",
    style = "text-align: center;",
    actionButton(
      inputId = "button_start",
      label = "Nog een keer!",
      style = "width: 100%;"),
    footer = NULL,
    easyClose = FALSE
  )
}

memo_win <- function () {
  modalDialog(
    tags$div(
      style = "text-align: center;",
      tags$h2(
        tags$span(icon("trophy"), style = "color: #F7E32F;"),
        "Winnaar!",
        tags$span(icon("trophy"), style = "color: #F7E32F;")
      ),

      tags$br(), tags$br(),
      tags$br(), tags$br(),

      actionButton(
        inputId = "reload",
        label = "Play again !",
        style = "width: 100%;"
      )
    ),
    footer = NULL,
    easyClose = FALSE
  )
}


# Memory
