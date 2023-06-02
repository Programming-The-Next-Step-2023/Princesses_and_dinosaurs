


# memo module----
# Modified from Memory memo by DreamRs (https://github.com/dreamRs/memory-memo)


# memo server


memo <- function(input, output, session, memo_logo,
                reset = reactiveValues(x = NULL), block = reactiveValues(x = NULL),
                img_back, theme_memo, memo_dir) {

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
            src = paste0(memo_dir,"/", theme_memo, "_m/", memo_logo),
            contentType = "image/png",
            width = 200,
            hight = 200
          )
        }
      },
      deleteFile = FALSE)
      return(click_status)

    }


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
