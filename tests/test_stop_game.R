
ui <-
  fluidPage(
    fluidRow(column(4, uiOutput("timer")),
             column(4, countdown(minutes = 0,seconds = 4, id = "timer2")),
             column(4, uiOutput("stop_sign")),
             column(4, uiOutput("princes"))
    ),
    fluidRow(column (12,
        actionButton("button_stop", label = "Stop!"),
        actionButton("button_loop", label = "Loop"),
        h5("loopclick"),
        textOutput("loop_nr"),
        h5("stopclick"),
        textOutput("stop_nr")
    ))
  )


server <-  function(input, output, session) {

  princes_loop <- img(src = "elsa_walk_g.gif")
  princes_stop <- img(src = "Elsa_from_Disney's_Frozen.png")
  stop_sign <- img(src = "hand_stop_roze.png")


  loop_time <- reactiveVal()

  stop_time <- reactiveVal()

  stop_times_sec <- sample(2:5, 20, replace = TRUE)


  btn_click <- reactiveValues(stop_click = 0,
                              loop_click = 0,
                              loop_start = NULL,
                              loop_now = FALSE,
                              stop_now = FALSE,
                              id_count = 0,
                              wait = NULL)




  observeEvent(input$button_stop, {
    if (btn_click$stop_now == TRUE & (btn_click$stop_click + 1) == btn_click$loop_click & btn_click$stop_click < 20) {
      # count number of stops made
      btn_click$stop_click <- btn_click$stop_click + 1
      output$stop_nr <- renderText(as.character(btn_click$stop_click))
      # make character stop
      output$princes <- renderUI(princes_stop)

      # set time at point of stopping
      btn_click$stop_start <- Sys.time()

      # start walking after 3 seconds
      if (difftime(Sys.time(), btn_click$stop_start, units = "secs") >= 3) {
        output$princes <- renderUI(princes_loop)
        output$stop_sign <- removeUI(stop_sign)
      }

      # do nothing if the stop button is clicked when not needed
    } else if (btn_click$stop_now == FALSE | btn_click$stop_click >= btn_click$loop_click) {
      btn_click$stop_click = btn_click$stop_click
    }
  })


  observeEvent(input$button_loop, {

    # count number of walks
    btn_click$loop_click <- btn_click$loop_click + 1

    # test if counted correct
    output$loop_nr <- renderText(as.character(btn_click$loop_click))

    # set time at point of walking
    btn_click$loop_start <- Sys.time()

    # make character walk
    output$princes <- renderUI(princes_loop)

    # loop_time <- as.numeric(difftime(Sys.time(), btn_click$loop_start, units = "secs"))

    #wait to stop
    wait_time <- function(time1) {
      if (difftime( Sys.time(), btn_click$loop_start, units = "secs") >= 3) {
      output$stop_sign <- renderUI(stop_sign)
      btn_click$stop_now <- TRUE
      }
    }

    observe({
    invalidateLater(1000, session)
    wait_time()
    })

    })

}

shinyApp(ui,server)

