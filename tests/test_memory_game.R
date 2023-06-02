#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# This is the memory game for the Princesses and Dinosaurs shiny app

# Based on Memory memo by DreamRs (https://github.com/dreamRs/memory-memo)

library(shiny)

# global object
n_memo <- 3


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

which_memo <- function(l, module) {
  res <- lapply(module, function(x) l[[x]]$memo)
  unlist(res, use.names = FALSE)
}

# module UI
memoUI <- function(id) {
  ns <- NS(id)
  tagList(
    imageOutput(
      outputId = ns("memo"),
      click = clickOpts(id = ns("memo_click"), clip = FALSE),
      inline = TRUE
    )
  )
}

# module server
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
        src = paste0("C:/Users/marro/OneDrive - UvA/Studie/Master/Programming/App Package/Princesses_and_dinosaurs/R/www/",theme_memo(),"_m/", memo_logo),
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

UI <- fluidPage(

    tags$head(
      tags$link(href="styles.css", rel="stylesheet", type="text/css")
    ),

    actionButton("theme_change",label = "change theme"),
    tags$br(),

    verbatimTextOutput("test_theme"),

  tags$div(
    style = "width: 650px; margin: auto;",
    tags$br(),
    lapply(
      X = seq_len(n_memo * 2),
      FUN = function(x) {
        memoUI(id = paste0("module", x))
      }
    )
  )
)





server <- function(input, output, session) {

  set_theme <- reactiveValues(theme_val = "princess_theme")

  observeEvent(input$theme_change, {
    if (identical(set_theme$theme_val, "princess_theme"))   {
      set_theme$theme_val <- "dino_theme"
    } else {
      set_theme$theme_val <- "princess_theme"
    }
  })

  output$test_theme <- renderText(set_theme$theme_val)

  memo_theme <- reactiveValues()

  princess_list <- list.files("C:/Users/marro/OneDrive - UvA/Studie/Master/Programming/App Package/Princesses_and_dinosaurs/R/www/princess_m"
  )
  Princess_sample <- sample(princess_list,n_memo)
  princess_png <- sample(rep(Princess_sample,2))

  dino_list <- list.files("C:/Users/marro/OneDrive - UvA/Studie/Master/Programming/App Package/Princesses_and_dinosaurs/R/www/dino_m"
  )
  dino_sample <- sample(dino_list,n_memo)
  dino_png <- sample(rep(dino_sample,2))


princess_back <- "C:/Users/marro/OneDrive - UvA/Studie/Master/Programming/App Package/Princesses_and_dinosaurs/R/www/princess_back.png"
dino_back <- "C:/Users/marro/OneDrive - UvA/Studie/Master/Programming/App Package/Princesses_and_dinosaurs/R/www/dino_back.png"


    results_mods <- reactiveValues()
    results_mods_parse <- reactiveValues(all = NULL, show1 = NULL, show2 = NULL, show3 = NULL)

    reset <- reactiveValues(x = NULL)
    block <- reactiveValues(x = NULL)

    memo_png <- reactiveValues(x=NULL)
    theme_memo <- reactiveVal("princess")
    memo_back <- reactiveVal(princess_back)

    observe({
        if (identical(set_theme$theme_val, "princess_theme")){
          theme_memo <- "princess"
          memo_back <- princess_back
          for (x in 1:(n_memo*2))  {
            memo_png[[paste0("nr", x)]] <- princess_png[x]
          }
        } else {
          memo_png <- dino_png
          theme_memo <- "dino"
          memo_back <- dino_back
          for (x in 1:(n_memo*2))  {
          memo_png[[paste0("nr", x)]] <- dino_png[x]
          }
        }
    })

    observe({
      for (X in 1:(n_memo*2)) {
          results_mods[[paste0("module", X)]] <-
            memoServer(id = paste0("module", X),
                       memo_logo = memo_png[[paste0("nr", X)]],
                       reset = reset,
                       block = block,
                       theme_memo = theme_memo,
                       img_back = memo_back())
        }
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

shinyApp(UI,server)
