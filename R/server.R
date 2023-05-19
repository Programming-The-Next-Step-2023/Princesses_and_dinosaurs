#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(glue)


# Hex module----
# Modified from Memory Hex by DreamRs (https://github.com/dreamRs/memory-hex)

# Hex formula's
n_hex <- 3

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

which_hex <- function(l, module) {
  res <- lapply(module, function(x) l[[x]]$hex)
  unlist(res, use.names = FALSE)
}

# hex server

hex_server <- function(id, hex_logo, reset = reactiveValues(x = NULL), block = reactiveValues(x = NULL)) {
  moduleServer(id, function(input, output, session) {

  click_status <- reactiveValues(show = FALSE, hex = hex_logo, ts = Sys.time(), found = FALSE)

  observeEvent(input$hex_click, {
    if (!click_status$found) {
      click_status$show <- !click_status$show
      click_status$ts <- Sys.time()
    }
  })

  observeEvent(block$x, {
    if (hex_logo %in% block$x) {
      click_status$found <- TRUE
    }
  })

  observeEvent(reset$x, {
    if (hex_logo %in% reset$x & !click_status$found) {
      click_status$show <- FALSE
    }
  })

  output$hex <- renderImage({
    if (!click_status$show) {
      list(
        src = "www/rstats-hex.png",
        width = 120,
        height = 139,
        contentType = "image/png"
      )
    } else {
      list(
        src = paste0("www/hex/", hex_logo),
        width = 120,
        height = 139,
        contentType = "image/png"
      )
    }
  }, deleteFile = FALSE)

  return(click_status)
  }
)}


#------------------------------------------------------------------------------------



# Create UI for the games----


dino_stop <- img(src = "dino_walk.gif", height="200%", width="200%")
princes_stop <- img(src = "elsa_walk_g.gif")


# Create html for ui
princes <- bslib::bs_theme(bootswatch = "quartz",
                           base_font = '"Verdana", sans-serif',
                           code_font = '"Verdana", sans-serif',
                           "font-size-base" = "1.8rem",
                           "font-size-code" = "1.6rem")

princes_gamebtn <- tags$style(
  HTML(".btn {
    height: 180px;
    width: 180px;
    border: 5px solid #FFB6C1;
    margin-bottom: 30px;
    justify-self: center;
    align-self: center;
    direction: vertical;
    display: flex;}"))

princes_groupbtn <- tags$head(
  tags$style(HTML(
    ".btn-with-image {
                background: url('knopthema.png') no-repeat center center;
                background-size: contain;
                width: 60px;
                height: 60px;
                border-radius: 50%;
                font-size: 18px;
                line-height: 150px;
                cursor: pointer;
                }")))

dino <- bslib::bs_theme(bootswatch = "sketchy",
                        bg = "#BFDB38",
                        fg = "#00425A",
                        base_font = '"Verdana", sans-serif',
                        code_font = '"Verdana", sans-serif',
                        "font-size-base" = "1.8rem",
                        "font-size-code" = "1.6rem")

dino_gamebtn <- tags$style(
  HTML(".btn {
    height: 170px;
    width: 170px;
    border: 8px solid #1F8A70;
    margin-bottom: 10px;
    justify-objects: center;
    align-objects: center;
    direction: vertical;
    display: flex;
       }"
  )
)

dino_groupbtn <- tags$head(
  tags$style(HTML(
    ".btn-with-image {
                background: url('knopthema.png') no-repeat center center;
                background-size: contain;
                width: 60px;
                height: 60px;
                border-radius: 50%;
                font-size: 18px;
                line-height: 150px;
                cursor: pointer;
                }"
  ))
)


# Define server----
server <- function(input, output, session) {

  # create reactive values needed to change theme
  set_theme <- reactiveValues(theme_val = princes,
                              game_btn = princes_gamebtn,
                              group_btn = princes_groupbtn)

  # Set values for ui to start the app
  session$setCurrentTheme(princes)
  output$game_btn <- renderUI({princes_gamebtn})
  output$group_btn <- renderUI({princes_groupbtn})


  # change theme values when change button is clicked
  observeEvent(input$theme_change, {
    if (identical(bslib::bs_current_theme(), princes))   {
      set_theme$theme_val <- dino
      set_theme$game_btn <- dino_gamebtn
      set_theme$group_btn <- dino_groupbtn
    } else {
      set_theme$theme_val <- princes
      set_theme$game_btn <- princes_gamebtn
      set_theme$group_btn <- princes_groupbtn

    }
    # Change UI to theme
    session$setCurrentTheme(set_theme$theme_val)
    output$game_btn <- renderUI({set_theme$game_btn})
    output$group_btn <- renderUI({set_theme$group_btn})
  })

  # Logic to change the main panel to the selected game----  NOT WORKING

  game_type <- reactiveValues(memory_id = "princes",
                              memory_theme = "princes",
                              stop_game = princes_stop)


  # Create output per game
  observeEvent(input$theme_change, {
    if (identical(bslib::bs_current_theme(), princes))   {
      memory_id <- "dinoid"
      memory_theme <- "dino"
      stop_game <- dino_stop
    } else{
      memory_id <- "princesid"
      memory_theme <- "princes"
      stop_game <- princes_stop
    }
  })



  # select tabppanel
  observeEvent(input$game_choice, {
    updateTabsetPanel(inputId = "game_tabs", selected = input$game_choice)
  })

  output$stop_tab <- renderUI({game_type$stop_game})
  output$nummers_tab <- renderUI("test")

    hex_png <- sample(list.files(path = "www/hex/", pattern = "png$"), n_hex)
    hex_png <- sample(rep(hex_png, 2))
    results_mods <- reactiveValues()
    results_mods_parse <- reactiveValues(
      all = NULL,
      show1 = NULL,
      show2 = NULL,
      show3 = NULL)
    reset <- reactiveValues(x = NULL)
    block <- reactiveValues(x = NULL)
    lapply(
      X = seq_len(n_hex * 2),
      FUN = function(x) {
        results_mods[[paste0("module", x)]] <- hex_server(
          id = (paste0("module", x)),
          hex_logo = hex_png[x],
          reset = reset,
          block = block
        )
      }
    )

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
      hex1 <- which_hex(results_mods_parse$all, results_mods_parse$show1)
      hex2 <- which_hex(results_mods_parse$all, results_mods_parse$show2)
      if (identical(hex1, hex2)) {
        block$x <- hex1
        showNotification(
          ui = tags$div(
            style = "font-size: 160%; font-weight: bold;",
            sample(
              x = c(
                "Well done!",
                "Bravo!",
                "Great!",
                "Good job!",
                "Amazing!",
                "That's a match!",
                "Hooray!"
              ),
              size = 1
            )
          ),
          type = "message"
        )
      }
    })

    observeEvent(results_mods_parse$show3, {
      reset$x <- which_hex(
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

            tags$br(),
            tags$br(),
            tags$br(),
            tags$br(),

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


    eventReactive(input$reload, {
      shinyjs::reset("game_tabs")
    })
}

