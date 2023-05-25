#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# This is the stop game for the Princesses and Dinosaurs shiny app


# create basic UI

princes_walked <- img(src = "elsa_walk_g.gif")
princes_stop <- img(src = "Elsa_from_Disney's_Frozen.png")
stop_sign <- img(src = "hand_stop_roze.png", height = 50, width = 50)
no_sign <- img(src = "roze.png", height = 50, width = 50)

ui <-
  fluidPage(
    tabsetPanel(id = "stop_tabs",
                tabPanel("princes_walk_tab", tags$body(princes_walked,
                                                           no_sign)),
                tabPanel("princes_stopped_tab", tags$body(princes_stop,
                                                              stop_sign)),
                tabPanel("princes_stop_now_tab", tags$body(princes_walked,
                                                           stop_sign))
    ),


    fluidRow(column (8,
        actionButton("button_stop", label = "Stop!"),
        actionButton("button_start", label = "Start!"),
        column(2, uiOutput("timer")),
        column(2, uiOutput("timer2")),

        # check the counting
        h5("loopclick"),
        textOutput("loop_nr"),
        h5("stopclick"),
        textOutput("stop_nr")
    ))
  )



# server------------
server <-  function(input, output, session) {

  # define images
  princes_walked <- img(src = "elsa_walk_g.gif")
  princes_stop <- img(src = "Elsa_from_Disney's_Frozen.png")
  stop_sign <- img(src = "hand_stop_roze.png", height = 50, width = 50)
  no_sign <- img(src = "roze.png", height = 50, width = 50)



  # create random stop times
  stop_times_sec <- sample(2:5, 20, replace = TRUE)

  # set reactive values
  btn_click <- reactiveValues(stop_click = 0,
                              loop_click = 0,
                              loop_start = NULL,
                              stop_start = NULL
                              )

  stopui <- reactiveValues(walking = TRUE,
                           stopped = FALSE,
                           stop_now = FALSE,
                           wait = FALSE,
                           wait_to_walk = FALSE
                           )

#
#   output_princes <- function(walking) {
#    output$princes <<- renderUI({
#     if (walking == TRUE) {
#       (img(src = "elsa_walk_g.gif"))
#     } else {
#       (img(src = "Elsa_from_Disney's_Frozen.png"))
#     }
#   })
#   }
#
#
#   output_stop <- function(stop_sign) {
#     output$stop_sign <<- renderUI({
#       if (stop_sign == TRUE) {
#       (img(src = "hand_stop_roze.png", height = 50, width = 50))
#     } else {
#       (img(src = "roze.png", height = 50, width = 50))
#     }
#     })
#   }
#
#   selectui <- reactive({
#     if ((stopui$walking | stopui$wait) & !stopui$stop_now) {"princes_walk_tab"}
#     if (stopui$walking & stopui$stop_now) {"princes_stop_now_tab"}
#     if (!stopui$walking) {"princes_stopped_tab"}
#   })


  # Begin game by pressing start -----
  observeEvent(input$button_start, priority = 3, {

    # start counting
    btn_click$loop_click <- btn_click$loop_click + 1

    # test if counted correct
    output$loop_nr <- renderText(as.character(btn_click$loop_click))

    # set time at point of walking
    btn_click$loop_start <- Sys.time()


    # make character walk
    stopui$wait <- TRUE

    wait_time <- function() {
      if (stopui$wait) {
        if (difftime(Sys.time(), btn_click$loop_start , units = "secs") > 5) {
          stopui$stop_now <- TRUE
          stopui$wait <- FALSE
        updateTabsetPanel(inputId = "stop_tabs", selected = "princes_stop_now_tab")
      }
    }
    }

      observe({
        invalidateLater(1000)
        wait_time()
      })

  })


  # stop the princes within time -----
  observeEvent(input$button_stop, priority = 3, {

      # make character stop & remove sign
      updateTabsetPanel(inputId = "stop_tabs", selected = "princes_stopped_tab")

      # count number of stops made
      btn_click$stop_click <- btn_click$stop_click + 1


      # set time at point of stopping
      btn_click$stop_start <- Sys.time()

      stopui$stop_now <- FALSE
      stopui$walking <- FALSE
      stopui$wait_to_walk <- TRUE


  })

  observeEvent(input$button_stop, priority = 2, {

    # start walking after 3 seconds                        # Is not happening!
      stop_time <- function() {
        if (stopui$wait_to_walk & !stopui$walking) {
        if (difftime(Sys.time(), btn_click$stop_start, units = "secs") > 5) {
          stopui$walking  <- TRUE
          btn_click$loop_click <- btn_click$loop_click + 1
          output$loop_nr <- renderText(as.character(btn_click$loop_click))
          updateTabsetPanel(inputId = "stop_tabs", selected = "princes_walk_tab")
        }
        }
      }

      observe({
        invalidateLater(1000)
        stop_time()
        })
    })
}



    #}
    #} else if (btn_click$stop_now == FALSE) {
    #  btn_click$stop_click = btn_click$stop_click

      # do nothing if the stop button is clicked when not needed



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



shinyApp(ui,server)

