

# Define UI
ui <- shiny::fluidPage(
  theme = princess_theme,

  shiny::fluidRow(
    # Theme change button
    shiny::column(
      width = 2,
      offset = 10,
      tags$div(groupbtn,
               shiny::actionButton(
                 inputId = "theme_change",
                 label = "",
                 class = "round-button theme-change-button"
               )
      )
    )
  ),

  # sidepanel with game options
  shiny::sidebarLayout(
    shiny::sidebarPanel(
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
      )
    ),

    # Mainpanel for the games
    shiny::mainPanel(
      width=9,
      shiny::tabsetPanel(
        id = "game_tabs",
        type = "hidden",
        header = NULL,
        footer = NULL,

        # Home tab
        shiny::tabPanelBody(
          value = "3",
          "kies een spel"),

        # Memory tab
        shiny::tabPanelBody(
          value = "1",
          tags$body(
            lapply(
              X = seq_len(n_memo * 2),
              FUN = function(x) {
                memoUI(paste0("module", x))
              }))),


        # Stop! tab
        shiny::tabPanelBody(
          value = "2",
          suppressWarnings(                          # silence warning tabpanel
            shiny::tabsetPanel(id = "stop_tabs",
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
                               shiny::tabPanelBody("walk_tab",
                                                   tags$body(
                                                     div(
                                                       class = "image-container",
                                                       uiOutput("walking_gif"),
                                                       btn_img))),
                               shiny::tabPanelBody("stopping_tab",
                                                   tags$body(
                                                     div(
                                                       class = "image-container",
                                                       uiOutput("stopping_img"),
                                                       btn_img))),
                               shiny::tabPanelBody("stop_now_tab",
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
            )
          )
        )
      )
    )
  )
)


