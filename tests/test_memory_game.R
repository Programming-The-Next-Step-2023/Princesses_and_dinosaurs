#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# This is the memory game for the Princesses and Dinosaurs shiny app

# Based on Memory Hex by DreamRs (https://github.com/dreamRs/memory-hex)

library(shiny)

# global object
n_hex <- 3


# functions
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

# module UI
hex_UI <- function(id) {
  ns <- NS(id)
  tagList(
    imageOutput(
      outputId = ns("hex"),
      click = clickOpts(id = ns("hex_click"), clip = FALSE),
      width = 120,
      height = 139,
      inline = TRUE
    )
  )
}

# module server
hex <- function(input, output, session, hex_logo, reset = reactiveValues(x = NULL), block = reactiveValues(x = NULL)) {

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

UI <- fluidPage(

    tags$head(
      tags$link(href="styles.css", rel="stylesheet", type="text/css")
    ),

    tags$div(
      class = "title-app",
      tags$h1("Hex memory game"),
      tags$h4("Find matching hex!")
    ),
    tags$br(),

    # verbatimTextOutput("test_res_show"),

  tags$div(
    style = "width: 650px; margin: auto;",
    tags$br(),
    lapply(
      X = seq_len(n_hex * 2),
      FUN = function(x) {
        hex_UI(id = paste0("module", x))
      }
    )
  )
)





server <- function(input, output, session) {



    hex_png <- sample(list.files(path = "www/hex/", pattern = "png$"), n_hex)
    hex_png <- sample(rep(hex_png, 2))

    results_mods <- reactiveValues()
    results_mods_parse <- reactiveValues(all = NULL, show1 = NULL, show2 = NULL, show3 = NULL)
    reset <- reactiveValues(x = NULL)
    block <- reactiveValues(x = NULL)

  lapply(
    X = seq_len(n_hex * 2),
    FUN = function(x) {
      results_mods[[paste0("module", x)]] <- callModule(
        module = hex,
        id = paste0("module", x),
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
            x = c("Well done!", "Bravo!", "Great!", "Good job!",
                  "Amazing!", "That's a match!", "Hooray!"),
            size = 1
          )
        ), type = "message"
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

shinyApp(UI,server)
