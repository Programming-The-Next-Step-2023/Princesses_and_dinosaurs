#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# This is the test version of the stop game for the Princesses and Dinosaurs
# shiny app. It runs as an separate shiny app.

library(shiny)

# create basic UI




stopUI <- function(id) {

  ns = NS(id)

  walking_gif <- img(src = "elsa_walk_g.gif")
  stopping_img <- img(src = "Elsa_from_Disney's_Frozen.png")
  stop_sign <- img(src = "hand_stop_roze.png", height = 50, width = 50)
  no_sign <- img(src = "roze.png", height = 50, width = 50)

      tabsetPanel(id = ns("stop_tabs"),
                  tabPanel(ns("walk_tab"), tags$body(walking_gif,
                                                         no_sign)),
                  tabPanel(ns("stopping_tab"), tags$body(stopping_img,
                                                             stop_sign)),
                  tabPanel(ns("stop_now_tab"), tags$body(walking_gif,
                                                             actionButton(ns("button_stop"), label = "Stop!"))))



}


# server------------
stopServer <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {


      # set reactive values
      btn_click <- reactiveValues(count_stop = 0,
                                  count_walk = 0,
                                  walk_start = NULL,
                                  stop_start = NULL,
                                  stop_now_start = NULL,
                                  warning = TRUE)

      stopui <- reactiveValues(walking = TRUE,
                               stopping = FALSE,
                               stop_now = FALSE)

      # functions-----
      run_walking <- function(roundnr) {

        # set time at point of walking
        btn_click$walk_start <- Sys.time()


        # set walking time (random between 7 and 15 seconds)
        observe({
          if (stopui$walking & btn_click$count_stop == (roundnr-1)){
            invalidateLater(1000)
            if (difftime(Sys.time(), btn_click$walk_start , units = "secs") > sample(c(5:15),1)) {
              stopui$stop_now <- TRUE
              stopui$walking <- FALSE
              updateTabsetPanel(inputId = NS(session$id,"stop_tabs"), selected = NS(session$id, "stop_now_tab"))
              btn_click$stop_now_start <- Sys.time()
            }}
        })

        # Game over if stop button is not clicked in time
        observe({
          if (btn_click$warning & stopui$stop_now){
            invalidateLater(1000)
            if (difftime(Sys.time(), btn_click$stop_now_start, units = "secs") > 3) {
              modal_go <- function() {
                ns <- session$ns
                modalDialog(
                  title = "Druk sneller op de knop",
                  style = "text-align: center;",
                actionButton(
                  inputId = ns("button_start"),
                  label = "Probeer nog een keer!",
                  style = "width: 100%;"),
                footer = NULL,
                easyClose = FALSE
                )
              }
              showModal(modal_go())
              btn_click$count_walk <- 0
              btn_click$count_stop <- 0
              btn_click$warning <- FALSE

            }
          }
        })

      }

      run_stop_now <- function(roundnr) {

        # make character stop
        updateTabsetPanel(inputId = NS(session$id,"stop_tabs"), selected = "stopping_tab")

        # count number of stops made
        btn_click$count_stop <- btn_click$count_stop + 1

        # set time at point of stopping
        btn_click$stop_start <- Sys.time()

        stopui$stop_now <- FALSE
        stopui$stopping <- TRUE
      }

      run_stopping <- function(roundnr){
        # start walking after 3 seconds
        observe ({
          if (stopui$stopping & btn_click$count_stop == roundnr) {
            invalidateLater(1000)
            if (difftime(Sys.time(), btn_click$stop_start, units = "secs") > 2) {
              stopui$walking  <- TRUE
              stopui$stopping <- FALSE

              updateTabsetPanel(inputId = NS(session$id,"stop_tabs"), selected = "walk_tab")
              btn_click$count_walk <- btn_click$count_walk + 1
            }}
        })
      }

      # Show introduction-----
      start_modal <- function () {
        ns = session$ns
       modalDialog(
        tags$div(
          style = "text-align: center;",
          tags$h3("Elsa gaat wandelen"),
          tags$h3("Olaf zegt 'Stop!'"),
          tags$br(),
          tags$h3("Kan jij Elsa helpen?")
        ),
        tags$br(), tags$br(),

        actionButton(
          inputId = ns("button_start"),
          label = "Start!",
          style = "width: 100%;"),
        footer = NULL,
        easyClose = TRUE
       )}

      showModal(start_modal())

      # Begin game by pressing start -----
      observeEvent(input$button_start, {

        # remove start button
        removeModal()

        # start counting
        btn_click$count_walk <- btn_click$count_walk + 1
        stopui$walking <- TRUE

        run_walking(1)
      })



      # stop the princes within time round 1 -----
      observeEvent(input$button_stop, priority = 3, {


        # make character stop & remove sign
        updateTabsetPanel(inputId = NS(session$id,"stop_tabs"), selected = "stopping_tab")

        # count number of stops made
        btn_click$count_stop <- btn_click$count_stop + 1


        # set time at point of stopping
        btn_click$stop_start <- Sys.time()

        stopui$stop_now <- FALSE
        stopui$stopping <- TRUE
      })

      observeEvent(input$button_stop, priority = 2, {


        # start walking after 5 seconds
        observe ({
          if (stopui$stopping & btn_click$count_stop == 1) {
            invalidateLater(1000)
            if (difftime(Sys.time(), btn_click$stop_start, units = "secs") > 5) {
              btn_click$count_walk <- btn_click$count_walk + 1
              stopui$walking  <- TRUE
              stopui$stopping <- FALSE

              updateTabsetPanel(inputId = NS(session$id,"stop_tabs"), selected = "walk_tab")
            }}
        })
      })

      # make character walk every round
      observe({
        if (stopui$walking & btn_click$count_walk == 2){
          run_walking(2)
        }
        if (stopui$walking & btn_click$count_walk == 3){
          run_walking(3)
        }
        if (stopui$walking & btn_click$count_walk == 4){
          run_walking(4)
        }
        if (stopui$walking & btn_click$count_walk == 5){
          run_walking(5)
        }
        if (stopui$walking & btn_click$count_walk == 6){
          run_walking(6)
        }
        if (stopui$walking& btn_click$count_walk == 7){
          run_walking(7)
        }
        if (stopui$walking& btn_click$count_walk == 8){
          run_walking(8)
        }
        if (stopui$walking & btn_click$count_walk == 9){
          run_walking(9)
        }
        if (stopui$walking & btn_click$count_walk == 10){
          modal_win <- function() {
            modalDialog(
            title = "Winnaar!",
            style = "text-align: center;",
            actionButton(
              inputId = ns("button_start"),
              label = "Nog een keer!",
              style = "width: 100%;"),
            footer = NULL,
            easyClose = FALSE
            )
          }
          showModal(modal_win())
          btn_click$count_walk <- 0
          btn_click$count_stop <- 0
        }
      })

      # React with stopping if stop button is clicked within time
      observeEvent(input$button_stop, priority = 4,{
        if (stopui$stop_now & btn_click$count_walk == 2){
          run_stop_now(2)
        }
        if (stopui$stop_now & btn_click$count_walk == 3){
          run_stop_now(3)
        }
        if (stopui$stop_now & btn_click$count_walk == 4){
          run_stop_now(4)
        }
        if (stopui$stop_now & btn_click$count_walk == 5){
          run_stop_now(5)
        }
        if (stopui$stop_now & btn_click$count_walk == 6){
          run_stop_now(6)
        }
        if (stopui$stop_now & btn_click$count_walk == 7){
          run_stop_now(7)
        }
        if (stopui$stop_now & btn_click$count_walk == 8){
          run_stop_now(8)
        }
        if (stopui$stop_now & btn_click$count_walk == 9){
          run_stop_now(9)
        }
      })


      # Stop for a while and start new round
      observeEvent(input$button_stop, priority = 3,{
        if (stopui$stopping & btn_click$count_walk == 2){
          run_stopping(2)
        }
        if (stopui$stopping & btn_click$count_walk == 3){
          run_stopping(3)
        }
        if (stopui$stopping & btn_click$count_walk == 4){
          run_stopping(4)
        }
        if (stopui$stopping & btn_click$count_walk == 5){
          run_stopping(5)
        }
        if (stopui$stopping & btn_click$count_walk == 6){
          run_stopping(6)
        }
        if (stopui$stopping & btn_click$count_walk == 7){
          run_stopping(7)
        }
        if (stopui$stopping & btn_click$count_walk == 8){
          run_stopping(8)
        }
        if (stopui$stopping & btn_click$count_walk == 9){
          run_stopping(9)
        }
      })
  }
)}








