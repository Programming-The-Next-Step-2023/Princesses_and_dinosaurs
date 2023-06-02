

# Define server----
server <- function(input, output, session) {

  # create reactive values needed to change theme
  set_theme <- shiny::reactiveValues(theme_val = princess_theme)

  # Set values for ui to start the app
  output$walking_gif <- shiny::renderUI({princess_walking_gif})
  output$stop_now_gif <- shiny::renderUI({princess_stop_now_gif})
  output$stopping_img <- shiny::renderUI({princess_stopping_img})

  memo_png <- list()
  memo_theme <- shiny::reactiveVal("princess")
  memo_back <- shiny::reactiveVal(get_princess_back())
  for (x in 1:(n_memo*2))  {
    memo_png[[x]] <- shiny::reactiveVal(princess_png[x])
  }

  # change theme values when change button is clicked
  shiny::observeEvent(input$theme_change, {
    if (identical(bslib::bs_current_theme(), princess_theme))   {
      # main ui
      set_theme$theme_val <- dino_theme
      shinyWidgets::updateRadioGroupButtons(inputId = "game_choice",
                                            choiceNames = dino_btn_names,
                                            choiceValues = c(3,1,2))
      # stop! ui
      output$walking_gif <- shiny::renderUI({dino_walking_gif})
      output$stop_now_gif <- shiny::renderUI({dino_stop_now_gif})
      output$stopping_img <- shiny::renderUI({dino_stopping_img})
      # memory ui
      memo_theme <- "dino"
      memo_back <- get_dino_back()
      for (x in 1:(n_memo*2))  {
        memo_png[[x]] <- dino_png[x]
      }
    } else {
      # main ui
      set_theme$theme_val <- princess_theme
      shinyWidgets::updateRadioGroupButtons(inputId = "game_choice",
                                            choiceNames = princess_btn_names,
                                            choiceValues = c(3,1,2))
      # stop! ui
      output$walking_gif <- shiny::renderUI({princess_walking_gif})
      output$stop_now_gif <- shiny::renderUI({princess_stop_now_gif})
      output$stopping_img <- shiny::renderUI({princess_stopping_img})
      # memory ui
      memo_theme <- "princess"
      memo_back <- get_princess_back()
      for (x in 1:(n_memo*2))  {
        memo_png[[x]] <- princess_png[x]
      }
    }
    session$setCurrentTheme(set_theme$theme_val)
  })

  # show the chosen game
  shiny::observeEvent(input$game_choice, {
    shiny::updateTabsetPanel(inputId = "game_tabs", selected = input$game_choice)
  })


  # Stop!-------------------------

  # set reactive values
  btn_click <- shiny::reactiveValues(count_stop = 0,
                                     count_walk = 0,
                                     walk_start = NULL,
                                     stop_start = NULL,
                                     stop_now_start = NULL,
                                     warning = TRUE)

  stopui <- shiny::reactiveValues(walking = TRUE,
                                  stopping = FALSE,
                                  stop_now = FALSE)


  # functions (have to be inside server because of the many reactive values)

  run_walking <- function(roundnr) {
    btn_click$walk_start <- Sys.time()           # set time at start of walking
    shiny::observe({                             # walk for 5-15 seconds
      if (stopui$walking & btn_click$count_stop == (roundnr-1)){
        shiny::invalidateLater(1000)
        if (difftime(Sys.time(), btn_click$walk_start , units = "secs") > sample(c(5:15),1)) {
          stopui$stop_now <- TRUE
          stopui$walking <- FALSE
          shiny::updateTabsetPanel(inputId = "stop_tabs", selected ="stop_now_tab")
          btn_click$stop_now_start <- Sys.time()
        }}
    })
    observe({                 # Game over if stop button is not clicked in time
      if (btn_click$warning & stopui$stop_now){
        shiny::invalidateLater(1000)
        if (difftime(Sys.time(), btn_click$stop_now_start, units = "secs") > 5) {
          shiny::showModal(modal_go())
          btn_click$count_walk <- 0
          btn_click$count_stop <- 0
          btn_click$warning <- FALSE
        }
      }
    })
  }

  run_stop_now <- function(roundnr) {
    shiny::updateTabsetPanel(inputId = "stop_tabs", selected = "stopping_tab")
    btn_click$count_stop <- btn_click$count_stop + 1
    btn_click$stop_start <- Sys.time()
    stopui$stop_now <- FALSE
    stopui$stopping <- TRUE
  }

  run_stopping <- function(roundnr){
    shiny::observe ({
      if (stopui$stopping & btn_click$count_stop == roundnr) {
        shiny::invalidateLater(1000)
        if (difftime(Sys.time(), btn_click$stop_start, units = "secs") > 2) {
          stopui$walking  <- TRUE
          stopui$stopping <- FALSE
          shiny::updateTabsetPanel(inputId = "stop_tabs", selected = "walk_tab")
          btn_click$count_walk <- btn_click$count_walk + 1
        }}
    })
  }

  # Show introduction
  shiny::observe({
    if (input$game_choice == "2") {
      shiny::showModal(start_modal())
    }
  })

  # Begin game by pressing start
  shiny::observeEvent(input$button_start, {
    shiny::removeModal()                               # remove start button
    btn_click$count_walk <- btn_click$count_walk + 1   # start counting rounds
    stopui$walking <- TRUE
    run_walking(1)
  })

  # stop the princes within time round 1
  shiny::observeEvent(input$button_stop, priority = 3, {
    shiny::updateTabsetPanel(inputId = "stop_tabs", selected = "stopping_tab")
    btn_click$count_stop <- btn_click$count_stop + 1    # count number of stops
    btn_click$stop_start <- Sys.time()
    stopui$stop_now <- FALSE
    stopui$stopping <- TRUE
  })

  # start walking after 5 seconds round 1
  shiny::observeEvent(input$button_stop, priority = 2, {
    shiny::observe ({
      if (stopui$stopping & btn_click$count_stop == 1) {
        shiny::invalidateLater(1000)
        if (difftime(Sys.time(), btn_click$stop_start, units = "secs") > 5) {
          btn_click$count_walk <- btn_click$count_walk + 1
          stopui$walking  <- TRUE
          stopui$stopping <- FALSE
          shiny::updateTabsetPanel(inputId = "stop_tabs", selected = "walk_tab")
        }}
    })
  })

  # make character walk every round
  shiny::observe({
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
      shiny::showModal(modal_win())
      btn_click$count_walk <- 0
      btn_click$count_stop <- 0
    }
  })

  # React with stopping if stop button is clicked within time
  shiny::observeEvent(input$button_stop, priority = 4,{
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
  shiny::observeEvent(input$button_stop, priority = 3,{
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


  # Memory part---------------------

  reset <- shiny::reactiveValues(x = NULL)
  block <- shiny::reactiveValues(x = NULL)

  results_mods <- shiny::reactiveValues()
  results_mods_parse <- shiny::reactiveValues(all = NULL,
                                              show1 = NULL,
                                              show2 = NULL,
                                              show3 = NULL)
  # Show the selected images
  shiny::observe({
    lapply(
      X = seq_len(n_memo * 2),
      FUN = function(x) {
        results_mods[[paste0("module", x)]] <-
          shiny::callModule(
            module = memo,
            id = paste0("module", x),
            memo_dir <- get_img_dir(),
            memo_logo = memo_png[[x]](),
            reset = reset,
            block = block,
            theme_memo = memo_theme(),
            img_back = memo_back())
      }
    )
  })

  # Check if two images are the same
  shiny::observe({
    res_mod <- lapply(
      X = shiny::reactiveValuesToList(results_mods),
      FUN = shiny::reactiveValuesToList
    )
    results_mods_parse$all <- res_mod
    results_mods_parse$show1 <- which_show(res_mod, 1)
    results_mods_parse$show2 <- which_show(res_mod, 2)
    results_mods_parse$show3 <- which_show(res_mod, 3)
  })

  shiny::observeEvent(results_mods_parse$show2, {
    memo1 <- which_memo(results_mods_parse$all, results_mods_parse$show1)
    memo2 <- which_memo(results_mods_parse$all, results_mods_parse$show2)
    if (identical(memo1, memo2)) {
      block$x <- memo1
      found_pair()
    }
  })

  shiny::observeEvent(results_mods_parse$show3, {
    reset$x <- which_memo(
      results_mods_parse$all,
      c(results_mods_parse$show1, results_mods_parse$show2)
    )
    results_mods_parse$show1 <- NULL
    results_mods_parse$show2 <- NULL
    results_mods_parse$show1 <- results_mods_parse$show3
    results_mods_parse$show3 <- NULL
  })

  shiny::observe({
    allfound <- all_found(results_mods_parse$all)
    if (isTRUE(allfound)) {
      shiny::showModal(memo_win())
    }
  })

  shiny::observeEvent(input$reload, {
    session$reload()
  }, ignoreInit = TRUE)
}



