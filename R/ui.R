#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)




# Define UI
ui <- fluidPage(
 theme = princess_theme,

  # Header
  fluidRow(

    # Theme button
    column(
      width = 2,
      offset = 10,
      tags$div(groupbtn,
        actionButton(
          inputId = "theme_change",
          label = "",
          class = "round-button theme-change-button"
        )
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
                value = "3",
               "kies een spel"),

              tabPanelBody(
                value = "1",
                tags$body(
                 lapply(
                  X = seq_len(n_memo * 2),
                  FUN = function(x) {
                    memoUI(paste0("module", x))
                  }))),


              # Tab for Stop!
              tabPanelBody(
                value = "2",
                #suppressWarnings( # to silence tabpanel
                tabsetPanel(id = "stop_tabs",
                            type = "hidden",
                            tags$style(
                              HTML(".image-container {
                                   display: flex;
                                   justify-content: center;
                                    align-items: center;
                                    width: 70wv;
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
                                            tags$div(
                                              groupbtn,
                                            actionButton(
                                              "button_stop",
                                              label = "",
                                              class =  "round-button stop-button"
                                            )
                                          )
                                        )
                                      )
                                    ),
                  # ...
                )
              )
            )
            )
          )
)


