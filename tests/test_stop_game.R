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

princes_walking <- img(src = "elsa_walk_g.gif")
princes_stop <- img(src = "Elsa_from_Disney's_Frozen.png")
stop_sign <- img(src = "hand_stop_roze.png", height = 50, width = 50)
no_sign <- img(src = "roze.png", height = 50, width = 50)

ui <-
  fluidPage(
    tabsetPanel(id = "stop_tabs",
                tabPanel("princes_walk_tab", tags$body(princes_walking,
                                                       no_sign)),
                tabPanel("princes_stopping_tab", tags$body(princes_stop,
                                                           stop_sign)),
                tabPanel("princes_stop_now_tab", tags$body(princes_walking,
                                                           stop_sign,
                                                           actionButton("button_stop", label = "Stop!")))
    ),


    fluidRow(column (8, offset = 4,

                     # check the counting
                     h5("walkclick"),
                     textOutput("walk_nr"),
                     h5("stopclick"),
                     textOutput("stop_nr")
    ))
  )



# server------------
server <-  function(input, output, session) {

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

    # test if counted correct
    output$walk_nr <- renderText(as.character(btn_click$count_walk))

    # set time at point of walking
    btn_click$walk_start <- Sys.time()

    # make character walk
    stopui$walking <- TRUE

    # set walking time (random between 7 and 15 seconds)
    observe({
      if (stopui$walking & btn_click$count_stop == (roundnr-1)){
        invalidateLater(1000)
        if (difftime(Sys.time(), btn_click$walk_start , units = "secs") > sample(c(7:15),1)) {
          stopui$stop_now <- TRUE
          stopui$walking <- FALSE
          updateTabsetPanel(inputId = "stop_tabs", selected = "princes_stop_now_tab")
          btn_click$stop_now_start <- Sys.time()
        }}
    })

    # Game over if stop button is not clicked in time
    observe({
      if (btn_click$warning & stopui$stop_now){
        invalidateLater(1000)
        if (difftime(Sys.time(), btn_click$stop_now_start, units = "secs") > 3) {
          showModal(modalDialog(
            title = "Druk sneller op de knop",
            style = "text-align: center;",
            actionButton(
              inputId = "button_start",
              label = "Probeer nog een keer!",
              style = "width: 100%;"),
            footer = NULL,
            easyClose = FALSE
          ))
          btn_click$count_walk <- 0
          btn_click$count_stop <- 0
          btn_click$warning <- FALSE

        }
      }
    })

  }

  run_stop_now <- function(roundnr) {
    # make character stop
    updateTabsetPanel(inputId = "stop_tabs", selected = "princes_stopping_tab")

    # count number of stops made
    btn_click$count_stop <- btn_click$count_stop + 1
    output$stop_nr<-renderText(as.character(btn_click$count_stop))

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
          output$walk_nr <- renderText(as.character(btn_click$count_walk))
          updateTabsetPanel(inputId = "stop_tabs", selected = "princes_walk_tab")
          btn_click$count_walk <- btn_click$count_walk + 1
        }}
    })
  }

  # Show introduction-----
  showModal(modalDialog(
    tags$div(
      style = "text-align: center;",
      tags$h3("Elsa gaat wandelen"),
      tags$h3("Olaf zegt 'Stop!'"),
      tags$br(),
      tags$h3("Kan jij Elsa helpen?")
    ),

    tags$br(), tags$br(),
    tags$br(), tags$br(),

    actionButton(
      inputId = "button_start",
      label = "Start!",
      style = "width: 100%;"),
    footer = NULL,
    easyClose = FALSE
  )
  )


  # Begin game by pressing start -----
  observeEvent(input$button_start, priority=1, {

    updateTabsetPanel(inputId = "stop_tabs", selected = "princes_walk_tab")

    # remove start button
    removeModal()

    # start counting
    btn_click$count_walk <- btn_click$count_walk + 1

    run_walking(1)
  })



  # stop the princes within time round 1 -----
  observeEvent(input$button_stop, priority = 3, {

    # make character stop & remove sign
    updateTabsetPanel(inputId = "stop_tabs", selected = "princes_stopping_tab")

    # count number of stops made
    btn_click$count_stop <- btn_click$count_stop + 1
    output$stop_nr<-renderText(as.character(btn_click$count_stop))

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
          output$walk_nr <- renderText(as.character(btn_click$count_walk))
          updateTabsetPanel(inputId = "stop_tabs", selected = "princes_walk_tab")
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
      showModal(modalDialog(
        title = "Winnaar!",
        style = "text-align: center;",
        actionButton(
          inputId = "button_start",
          label = "Nog een keer!",
          style = "width: 100%;"),
        footer = NULL,
        easyClose = FALSE
      ))
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




shinyApp(ui,server)


