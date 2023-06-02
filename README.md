# Princesses and Dinosaurs

A shiny app for children who stay in a shelter, with games that are fun and (maybe) teach some valuable lessons too!

## Start app

This is an app for young children (2-6 yr.) to play the games "Memory" or "Stop!". There are two theme's to choose from: a princess theme and a dinosaurs theme, these can be changes while playing.

`play_pd()` will start the shiny app. No additional arguments are needed, the "princess theme" will start as default. In the future, it may be possible to choose your own starting theme with the argument `starttheme`.

## Code structure

The package is structured with the most important files and directories being:

*inst/img_pd*: The directory containing all the images\
*global.R*: The objects used in the shiny app\
*functions.R*: Most functions used in the app, including messages\
*memo_module.R*: The module for the memory game\
*zzz.R* : The resource path to the package directory for shiny\
*server.R* and *ui.R*: The shiny app R.-scripts

## Play the games

As the games are quite simple, there wont be a standard step-by-step follow trough. Parents can find examples for explaining the games to a young child in the vignette. It also offers fun suggestions to play together! This information is provided in both Dutch and English.
