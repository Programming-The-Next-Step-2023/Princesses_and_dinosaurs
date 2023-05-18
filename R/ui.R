#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)

# Define UI
fluidPage(

    # Theme specifications
    bslib::bs_theme(bootswatch = "quartz"),

    # Title/welcome
    fluidRow(column(10),

    # Button to switch theme
            column(2)

    ),

    # Sidebar with group-buttons to choose game
    fluidRow(column(3),


             # Mainpanel with picture or game
             column(9)


             # picture

             # game..
    ),
)

