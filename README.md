# Olympic Games Shiny app
This repository contains R code for a shiny app featuring data from the modern Olympic games. The dataset used in the app contains information on the Olympic games from 1896 to 2016 and was obtained from the #TidyTuesday project located here: https://github.com/rfordatascience/tidytuesday/blob/master/data/2021/2021-07-27/readme.md

To use this app locally, you can download the `shiny` app in R, clone or download this repository, and run `shiny::runApp("app.R")`.
 
Once the app is opened, it should look like this:
![screenshot of app](https://i.imgur.com/mFeQrnz.png)

The main panel of the app displays a bar chart showcasing the top teams of the chosen games, in order of number of medals won. Below the bar chart is a table summarizing the medal winners of a specific event. The side panel allows users to select the season of the games (summer or winter), the year, the sport, and the event. 
