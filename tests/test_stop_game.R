#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# This is the stop game for the Princesses and Dinosaurs shiny app


# create basic UI
ui <-
  fluidPage(
    fluidRow(column(4, uiOutput("timer")),
             column(4, uiOutput("timer2")),
             column(4, uiOutput("stop_sign")),
             column(4, uiOutput("princes"))
    ),
    fluidRow(column (12,
        actionButton("button_stop", label = "Stop!"),
        actionButton("button_start", label = "Start!"),

        # check the counting
        h5("loopclick"),
        textOutput("loop_nr"),
        h5("stopclick"),
        textOutput("stop_nr")
    ))
  )


server <-  function(input, output, session) {

  # define images
  princes_loop <- img(src = "elsa_walk_g.gif")
  princes_stop <- img(src = "Elsa_from_Disney's_Frozen.png")
  stop_sign <- img(src = "hand_stop_roze.png", height = 50, width = 50)

  # create random stop times
  stop_times_sec <- sample(2:5, 20, replace = TRUE)

  # set reactive values
  btn_click <- reactiveValues(stop_click = 0,
                              loop_click = 0,
                              loop_start = NULL,
                              stop_start = NULL,
                              loop_now = FALSE,
                              stop_now = FALSE,
                              id_count = 0,
                              wait = TRUE,
                              stopped = FALSE)


  # Begin game by pressing start
  observeEvent(input$button_start, {

    # start counting
    btn_click$loop_click <- btn_click$loop_click + 1

    # test if counted correct
    output$loop_nr <- renderText(as.character(btn_click$loop_click))

    # set time at point of walking
    btn_click$loop_start <- Sys.time()

    # make character walk
    output$princes <- renderUI(princes_loop)
    btn_click$wait <- TRUE

    # wait to stop
    wait_time <- function(time1) {
      if (difftime(Sys.time(), btn_click$loop_start, units = "secs") >= 3) {
        output$stop_sign <- renderUI(stop_sign)
        btn_click$stop_now <- TRUE
        btn_click$wait <- FALSE
      }
    }
    if (btn_click$wait == TRUE){
      observe({
        invalidateLater(1000, session)
        wait_time()
      })
    }
  })

  # stop the princes within time
  observeEvent(input$button_stop, {
    if (btn_click$stop_now == TRUE & btn_click$loop_click & btn_click$stop_click < 20) {

      # count number of stops made
      btn_click$stop_click <- btn_click$stop_click + 1
      output$stop_nr <- renderText(as.character(btn_click$stop_click))

      # make character stop & remove sign
      output$princes <- renderUI(princes_stop)
      output$stop_sign <- removeUI(stop_sign)  #does not go away

      # set time at point of stopping
      btn_click$stop_start <- Sys.time()

      btn_click$stop_now <- FALSE


      # start walking after 3 seconds                        # Is not happening
      stop_time <- function(time1) {
        if (difftime(Sys.time(), btn_click$stop_start, units = "secs") >= 3) {
          output$princes <- renderUI(princes_walk)
          btn_click$stopped <- FALSE
          btn_click$walking <- TRUE
          btn_click$loop_click <- btn_click$loop_click + 1
        }
      }

      if (btn_click$stopped == TRUE){
        observe({
          invalidateLater(1000, session)
          stop_time()
        })
      }

      # do nothing if the stop button is clicked when not needed
    } else if (btn_click$stop_now == FALSE) {
      btn_click$stop_click = btn_click$stop_click
    }
  })

  # Player did not stop the princes
  #
  #
  #

  # Princes starts walking again (not yet working!)
  # observe({
  #
  #   # start counting
  #   btn_click$loop_click <- btn_click$loop_click + 1
  #
  #   # test if counted correct
  #   output$loop_nr <- renderText(as.character(btn_click$loop_click))
  #
  #   # set time at point of walking
  #   btn_click$loop_start <- Sys.time()
  #
  #   # make character walk
  #   output$princes <- renderUI(princes_loop)
  #   btn_click$wait <- TRUE
  #
  #   # wait to stop
  #   wait_time <- function(time1) {
  #     if (difftime(Sys.time(), btn_click$loop_start, units = "secs") >= 3) {
  #       output$stop_sign <- renderUI(stop_sign)
  #       btn_click$stop_now <- TRUE
  #       btn_click$wait <- FALSE
  #     }
  #   }
  #   if (btn_click$wait == TRUE){
  #     observe({
  #       invalidateLater(1000, session)
  #       wait_time()
  #     })
  #   }
  # })

}

shinyApp(ui,server)

