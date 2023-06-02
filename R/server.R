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

#------------------------------------------------------------------------------------




# Define server----
server <- function(input, output, session) {

  # create reactive values needed to change theme
  set_theme <- reactiveValues(theme_val = princess_theme
                              )


  # Set values for ui to start the app
  output$walking_gif <- renderUI({princess_walking_gif})
  output$stop_now_gif <- renderUI({princess_stop_now_gif})
  output$stopping_img <- renderUI({princess_stopping_img})

  memo_png <- list()
  memo_theme <- reactiveVal("princess")
  memo_back <- reactiveVal(get_princess_back())
  for (x in 1:(n_memo*2))  {
    memo_png[[x]] <- reactiveVal(princess_png[x])
  }


  # change theme values when change button is clicked
  observeEvent(input$theme_change, {
    if (identical(bslib::bs_current_theme(), princess_theme))   {
      set_theme$theme_val <- dino_theme
      shinyWidgets::updateRadioGroupButtons(inputId = "game_choice",
                                            choiceNames = dino_btn_names,
                                            choiceValues = c(3,1,2))
      output$walking_gif <- renderUI({dino_walking_gif})
      output$stop_now_gif <- renderUI({dino_stop_now_gif})
      output$stopping_img <- renderUI({dino_stopping_img})

      memo_theme <- "dino"
      memo_back <- get_dino_back()
      for (x in 1:(n_memo*2))  {
        memo_png[[x]] <- dino_png[x]
    }
    } else {
      set_theme$theme_val <- princess_theme
      shinyWidgets::updateRadioGroupButtons(inputId = "game_choice",
                                            choiceNames = princess_btn_names,
                                            choiceValues = c(3,1,2))

      output$walking_gif <- renderUI({princess_walking_gif})
      output$stop_now_gif <- renderUI({princess_stop_now_gif})
      output$stopping_img <- renderUI({princess_stopping_img})

      memo_theme <- "princess"
      memo_back <- get_princess_back()
      for (x in 1:(n_memo*2))  {
        memo_png[[x]] <- princess_png[x]
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
        if (difftime(Sys.time(), btn_click$stop_now_start, units = "secs") > 5) {
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
    updateTabsetPanel(inputId = "stop_tabs", selected = "stopping_tab")
    btn_click$count_stop <- btn_click$count_stop + 1
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

  observe({if (input$game_choice == "2") {
  showModal(start_modal())
    }
  })

  # Begin game by pressing start -----
  observeEvent(input$button_start, {
    removeModal()                                      # remove start button
    btn_click$count_walk <- btn_click$count_walk + 1   # start counting rounds
    stopui$walking <- TRUE
    run_walking(1)
  })



  # stop the princes within time round 1 -----
  observeEvent(input$button_stop, priority = 3, {
    updateTabsetPanel(inputId = "stop_tabs", selected = "stopping_tab")
    btn_click$count_stop <- btn_click$count_stop + 1    # count number of stops
    btn_click$stop_start <- Sys.time()
    stopui$stop_now <- FALSE
    stopui$stopping <- TRUE
  })

  # start walking after 5 seconds
  observeEvent(input$button_stop, priority = 2, {
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


# Memory part---------------------

    reset <- reactiveValues(x = NULL)
    block <- reactiveValues(x = NULL)



    results_mods <- reactiveValues()
       results_mods_parse <- reactiveValues(all = NULL,
                                            show1 = NULL,
                                            show2 = NULL,
                                            show3 = NULL)
 observe({
      lapply(
        X = seq_len(n_memo * 2),
        FUN = function(x) {
        results_mods[[paste0("module", x)]] <-
            callModule(
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



    observe({
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
              x = c("Goed gedaan!", "Bravo!", "Geweldig!", "Goed gedaan!",
                    "Geweldig!", "Hoera!"),
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
        showModal(memo_win())
      }
    })

    observeEvent(input$reload, {
       session$reload()
    }, ignoreInit = TRUE)
}



