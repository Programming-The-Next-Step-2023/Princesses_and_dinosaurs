#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)







# # Create html for ui
# princess_ <- bslib::bs_theme(bootswatch = "quartz",
#                              base_font = '"Verdana", sans-serif',
#                              code_font = '"Verdana", sans-serif',
#                              "font-size-base" = "1.8rem",
#                              "font-size-code" = "1.6rem")
#
#
#
# princess_theme <-
#   bslib::bs_add_rules(princess_,
#                       "body {
#                         background-image:linear-gradient(#9987BE, #E3A5D7, #F4D1DA);
#                         background-size: contain;
#                         background-position: center;
#                         min-height: 100vh;
#                         width:100%;}")



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


# Define UI
fluidPage(theme = princess_theme,

          # Header
          fluidRow(

          # Theme button
            column(
              width = 2,
              offset = 10,
              groupbtn,
              actionButton(
                inputId = "theme_change",
                label =  "",
                class = "btn-with-image"
              )
            )
          ),

          # sidepanel with game options
          sidebarLayout(

            sidebarPanel(
              width=3,
              id = "sidepanel",
                   tags$div(
                     tags$style(
                       HTML(
                         "#game_choice {
                          height: 100hv;
                          display: flex;
                          align-items: center;
                          }"
                       )
                     ),
                     shinyWidgets::radioGroupButtons(
                       inputId = "game_choice",
                       choiceNames = princess_btn_names,
                       choiceValues = btn_values,
                       individual = TRUE,
                       direction = "vertical"
                     )

            )),

            # Mainpanel for the games
            mainPanel(
              width=9,
            tabsetPanel(
              id = "game_tabs",
              type = "hidden",
              header = NULL,
              footer = NULL,

              # Tab for Memory
              tabPanelBody(
                value = "1",
                tags$head(
                  tags$link(href="styles.css", rel="stylesheet", type="text/css")
                )
                ,
                lapply(
                  X = seq_len(n_memo * 2),
                  FUN = function(x) {
                    memoUI(paste0("module", x))
                  })),

              # Tab for Stop!
              tabPanelBody(
                value = "2",
                tabsetPanel(id = "stop_tabs",
                            type = "hidden",
                            tags$style(
                              HTML(".image-container {
                                   display: flex;
                                   justify-content: center;
                                    align-items: center;
                                    }
                                   .image-container img {
                                    margin: 5px;}"
                              )
                            ),
                            tabPanelBody("walk_tab",
                                     tags$body(
                                       div(
                                         class = "image-container",
                                         uiOutput("walking_gif"),
                                         btn_img))),
                            tabPanelBody("stopping_tab",
                                     tags$body(
                                       div(
                                         class = "image-container",
                                         uiOutput("stopping_img"),
                                         btn_img))),
                            tabPanelBody("stop_now_tab",
                                     tags$body(
                                       div(
                                         class = "image-container",
                                         uiOutput("stop_now_gif"),
                                         actionButton("button_stop",
                                         label = "",
                                         style =  "background: url('stop_button.png') no-repeat center center;
                                         background-size: contain;
                                         width: 160px;
                                         height: 160px;
                                         border-radius: 50%;
                                         cursor: pointer;"))))
                )


              ))
            ))
)



