#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#library(shiny)

#'@import shiny



# memo module----
# Modified from Memory memo by DreamRs (https://github.com/dreamRs/memory-memo)


# memo server
memoServer <- function(id,
                       memo_logo,
                       reset,
                       block,
                       img_back,
                       theme_memo){
  moduleServer(
    id,
    function(input, output, session) {
      click_status <- reactiveValues(show = FALSE,
                                     ts = Sys.time(),
                                     found = FALSE,
                                     memo = memo_logo)




      observeEvent(input$memo_click, {
        print(str(click_status))
        if (!click_status$found) {
          click_status$show <- !click_status$show
          click_status$ts <- Sys.time()
        }
      })
      observeEvent(block$x, {
        if (memo_logo %in% block$x) {
          click_status$found <- TRUE
        }
      })

      observeEvent(reset$x, {
        if (memo_logo %in% reset$x & !click_status$found) {
          click_status$show <- FALSE
        }
      })

      output$memo <- renderImage({
        if (!click_status$show) {
          list(
            src = img_back,
            contentType = "image/png",
            width = 200,
            hight = 200
          )
        } else {
          list(
            src = paste0("www/",theme_memo,"_m/", memo_logo),
            contentType = "image/png",
            width = 200,
            hight = 200
          )
        }
      },
      deleteFile = FALSE)
      print(click_status)
      return(click_status)
    }
  )
}



#------------------------------------------------------------------------------------




# Define server----
server <- function(input, output, session) {

  # create reactive values needed to change theme
  set_theme <- reactiveValues(theme_val =
                               if (getOption("pd_starttheme", "princesses") == "princesses") {
                                  princess_theme
                                } else {
                                 dino_theme
                                }
                                )

  # Set values for ui to start the app
  observe({session$setCurrentTheme(set_theme$theme_val)},
          priority=10)
  output$walking_gif <- renderUI({princess_walking_gif})
  output$stop_now_gif <- renderUI({princess_stop_now_gif})
  output$stopping_img <- renderUI({princess_stopping_img})


  # change theme values when change button is clicked
  observeEvent(input$theme_change, {
    if (identical(bslib::bs_current_theme(), princess_theme))   {
      set_theme$theme_val <- dino_theme
      output$walking_gif <- renderUI({dino_walking_gif})
      output$stop_now_gif <- renderUI({dino_stop_now_gif})
      output$stopping_img <- renderUI({dino_stopping_img})
      shinyWidgets::updateRadioGroupButtons(inputId = "game_choice",
                                            choiceNames = dino_btn_names,
                                            choiceValues = c(3,1,2))
      memo_theme <- "dino"
      memo_back <- dino_back
      for (x in 1:(n_memo*2))  {
        memo_png[[paste0("nr", x)]] <- dino_png[x]
    }
    } else {
      set_theme$theme_val <- princess_theme
      output$game_btn <- renderUI({princess_gamebtn})
      output$walking_gif <- renderUI({princess_walking_gif})
      output$stop_now_gif <- renderUI({princess_stop_now_gif})
      output$stopping_img <- renderUI({princess_stopping_img})
      shinyWidgets::updateRadioGroupButtons(inputId = "game_choice",
                                            choiceNames = princess_btn_names,
                                            choiceValues = c(3,1,2))
      memo_theme <- "princess"
      memo_back <- princess_back
      for (x in 1:(n_memo*2))  {
        memo_png[[paste0("nr", x)]] <- princess_png[x]
      }
    }
    # Change UI to theme
    session$setCurrentTheme(set_theme$theme_val)
  })




  # select tabppanel
  observeEvent(input$game_choice, {
    updateTabsetPanel(inputId = "game_tabs", selected = input$game_choice)
  })



# Stop! part------------------


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
          updateTabsetPanel(inputId = "stop_tabs", selected ="stop_now_tab")
          btn_click$stop_now_start <- Sys.time()
        }}
    })

    # Game over if stop button is not clicked in time
    observe({
      if (btn_click$warning & stopui$stop_now){
        invalidateLater(1000)
        if (difftime(Sys.time(), btn_click$stop_now_start, units = "secs") > 3) {
          modal_go <- function() {
            modalDialog(
              title = "Druk sneller op de knop",
              style = "text-align: center;",
              actionButton(
                inputId = "button_start",
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
    updateTabsetPanel(inputId = "stop_tabs", selected = "stopping_tab")

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

          updateTabsetPanel(inputId = "stop_tabs", selected = "walk_tab")
          btn_click$count_walk <- btn_click$count_walk + 1
        }}
    })
  }

  # Show introduction-----

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

  observe({if (input$game_choice == "2"){
  showModal(start_modal())
    }
  })

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
    updateTabsetPanel(inputId = "stop_tabs", selected = "stopping_tab")

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

          updateTabsetPanel(inputId = "stop_tabs", selected = "walk_tab")
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
            inputId = "button_start",
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


#Memory part---------------------

  # memo_modal <- function () {
  #   modalDialog(
  #     tags$div(
  #       style = "text-align: center;",
  #       tags$h3("speel memory")
  #     ),
  #     tags$br(), tags$br(),
  #
  #     actionButton(
  #       inputId = "start_memo",
  #       label = "Start!",
  #       style = "width: 100%;"),
  #     footer = NULL,
  #     easyClose = TRUE
  #   )}
  #
  # observeEvent(input$game_choice, {
  #   if (input$game_choice == "1") {
  #   showModal(memo_modal())
  # }
  # })


  princess_list <- list.files("princess_m/")
  print(princess_list)
  Princess_sample <- sample(princess_list, n_memo)
  princess_png <- sample(rep(Princess_sample, 2))

  dino_list <- list.files("dino_m/")
  dino_sample <- sample(dino_list,n_memo)
  dino_png <- sample(rep(dino_sample,2))

  princess_back <- "princess_back.png"
  dino_back <- "dino_back.png"



    reset <- reactiveValues(x = NULL)
    block <- reactiveValues(x = NULL)
    results_mods <- reactiveValues()
    results_mods_parse <- reactiveValues(all = NULL,
                                         show1 = NULL,
                                         show2 = NULL,
                                         show3 = NULL)

    memo_png <- reactiveValues(x=NULL)
    memo_theme <- reactiveVal("princess")
    memo_back <- reactiveVal(princess_back)

    observe({
      for (x in 1:(n_memo*2))  {
      memo_png[[paste0("nr", x)]] <- princess_png[x]
      print(memo_png[[paste0("nr", x)]])
      }
    })


    observe({
     # removeModal()
      for (x in 1:(n_memo*2)) {
        results_mods[[paste0("module", x)]] <-
          memoServer(id = paste0("module", x),
                     memo_logo = memo_png[[paste0("nr", x)]],
                     reset = reset,
                     block = block,
                     theme_memo = memo_theme,
                     img_back = memo_back)

      }
    })



    observeEvent(results_mods,{
      res_mod <- lapply(
        X = reactiveValuesToList(results_mods),
        FUN = reactiveValuesToList
      )
      results_mods_parse$all <- res_mod
      results_mods_parse$show1 <- which_show(res_mod, 1)
      results_mods_parse$show2 <- which_show(res_mod, 2)
      results_mods_parse$show3 <- which_show(res_mod, 3)
    })

    observeEvent(results_mods_parse$show2, {
      memo1 <- which_memo(results_mods_parse$all, results_mods_parse$show1)
      memo2 <- which_memo(results_mods_parse$all, results_mods_parse$show2)
      if (identical(memo1, memo2)) {
        block$x <- memo1
        showNotification(
          ui = tags$div(
            style = "font-size: 160%; font-weight: bold;",
            sample(
              x = c("Well done!", "Bravo!", "Great!", "Good job!",
                    "Amazing!", "That's a match!", "Hooray!"),
              size = 1
            )
          ), type = "message"
        )
      }
    })

    observeEvent(results_mods_parse$show3, {
      reset$x <- which_memo(
        results_mods_parse$all,
        c(results_mods_parse$show1, results_mods_parse$show2)
      )
      results_mods_parse$show1 <- NULL
      results_mods_parse$show2 <- NULL
      results_mods_parse$show1 <- results_mods_parse$show3
      results_mods_parse$show3 <- NULL
    })

    observe({
      allfound <- all_found(results_mods_parse$all)
      if (isTRUE(allfound)) {
        showModal(modalDialog(
          tags$div(
            style = "text-align: center;",
            tags$h2(
              tags$span(icon("trophy"), style = "color: #F7E32F;"),
              "Well done !",
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
        ))
      }
    })

    observeEvent(input$reload, {
       session$reload()
    }, ignoreInit = TRUE)
}



