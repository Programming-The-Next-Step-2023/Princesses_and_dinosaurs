#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)




memory_name = span(class = "d-flex flex-column align-items-center",
                   icon("th", style = "font-size: 7rem;", lib = "glyphicon"),
                   "Memory",
                   style = "height: 180px;
                   width: 180px;
                   border: 5px solid #FFB6C1;
                   margin-bottom: 60px;")


stop_name = span(class = "d-flex flex-column align-items-center",
                 icon("hand-paper", style = "font-size: 7rem;"),
                 "Stop!",
                 style = "height: 180px;
                   width: 180px;
                   border: 5px solid #FFB6C1;
                   margin-bottom: 60px;")


btn_names = list(memory_name, stop_name)
btn_values = c(1,2)

# Create html for ui
princess_ <- bslib::bs_theme(bootswatch = "quartz",
                             base_font = '"Verdana", sans-serif',
                             code_font = '"Verdana", sans-serif',
                             "font-size-base" = "1.8rem",
                             "font-size-code" = "1.6rem")



princess_theme <-
  bslib::bs_add_rules(princess_,
                      "body {
                        background-image:linear-gradient(#9987BE, #E3A5D7, #F4D1DA);
                        background-size: contain;
                        background-position: center;
                        min-height: 100vh;
                        width:100%;}")


n_hex <- 3

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

# Define UI
fluidPage(theme = princess_theme,

          # Header
          fluidRow(

          # Theme button
            column(
              width = 2,
              offset = 10,
              htmlOutput("group_btn"),
              actionButton(
                inputId = "theme_change",
                label =  "",
                class = "btn-with-image"
              )
            )
          ),

          # sidepanel with game options
          fluidRow(
            column(3,

              # change style of buttons
              htmlOutput("game_btn"),

              shinyWidgets::radioGroupButtons(
                inputId = "game_choice",
                choiceNames = btn_names,
                choiceValues = btn_values,
                individual = TRUE,
                direction = "vertical",
                status = "custom-class"
              )

            ),

            # Mainpanel for the games
            (column(9,
            tabsetPanel(
              shinyjs::useShinyjs(),
              id = "game_tabs",
              type = "hidden",
              header = NULL,
              footer = NULL,
              tabPanelBody(
                value = "1",
                tags$head(
                  tags$link(href="styles.css", rel="stylesheet", type="text/css")
                ),
                  tags$div(
                  class = "title-app",
                  tags$h4("Hex memory game"),
                  tags$h4("Find matching hex!")
                ),
                lapply(
                  X = seq_len(n_hex * 2),
                  FUN = function(x) {
                    hex_UI(paste0("module", x))
                  })),
              tabPanelBody("2", stopUI("test-id")

              ))
            ))

          )
          )

