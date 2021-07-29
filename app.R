library(shiny)
library(tidyverse)
library(here)
library(emo)
library(DT)
library(shinythemes)
library(htmltools)

olympics <-
    readr::read_rds(here::here('data/data.rds'))

countries <- maps::world.cities %>% 
    select(city = name,
           country = country.etc) %>% 
    distinct()
    
olympics_countries <- 
    olympics %>% 
    left_join(countries, by = "city") %>%
    mutate(country = case_when(
        city == "Barcelona" ~ "Spain",
        city == "London" ~ "United Kingdom",
        city == "Paris" ~ "France",
        city == "Los Angeles" ~ "California, USA",
        city == "Salt Lake City" ~ "Utah, USA",
        city == "Antwerpen" ~ "Belgium",
        city == "Lake Placid" ~ "New York, USA",
        city == "Sydney" ~ "Australia",
        city == "Atlanta" ~ "Georgia, USA",
        city == "Torino" ~ "Italy",
        city == "Athina" ~ "Greece",
        city == "Squaw Valley" ~ "California, USA",
        city == "Seoul" ~ "South Korea",
        city == "Berlin" ~ "Germany",
        city == "Cortina d'Ampezzo" ~ "Italy",
        city == "Melbourne" ~ "Australia",
        city == "Roma" ~ "Italy",
        city == "Moskva" ~ "Soviet Union",
        city == "Vancouver" ~ "Canada",
        city == "Chamonix" ~ "France",
        city == "St. Louis" ~ "Missouri, USA",
        TRUE ~ country
    )) %>% 
    distinct(city, country, year, season)

    # Define UI for application that draws a histogram
    ui <- fluidPage(
        
        tags$head(
            tags$link(rel = "stylesheet", type = "text/css", href = "olympics.css")
        ),

        # Application title
        titlePanel("Historical Data from the Olympic Games"),

        # Sidebar with various radioButtons and selectInputs
        sidebarLayout(
            sidebarPanel(
                img(src = "olympic-rings.png", height = 128, width = 277),
                radioButtons(inputId = "season",
                             label = "Select season",
                             choices = c("Summer", "Winter")),
                selectInput(inputId = "year",
                            label = "Select year",
                            choices = sort(unique(olympics$year))),
                selectInput(inputId = "sport", label = "Select a sport",
                            choices = sort(unique(olympics$sport))),
                selectInput(inputId = "event", label = "Select an event",
                            choices = sort(unique(olympics$event)))
            ),

            # Show a plot of the top medal winning teams and a medal summary table
            mainPanel(
                textOutput("intro"),
                plotOutput(outputId = "barchart"),
                dataTableOutput('table')
            )
        )
    )

# Define server logic 
server <- function(input, output, session) {
    observeEvent(input$season,
                 {
                     
                     updateSelectInput(session, "year",
                                       choices = olympics %>% filter(season == input$season) %>% 
                                           distinct(year) %>% arrange(year) %>% pull()
                     )
                     updateSelectInput(session, "sport",
                                       choices = olympics %>% filter(season == input$season) %>%  
                                           distinct(sport) %>% arrange(sport) %>% pull(sport))
                     updateSelectInput(session, "event",
                                       choices = olympics %>% filter(season == input$season) %>%  
                                           distinct(event) %>% arrange(event) %>% pull(event))
                 }
    )
    
    observeEvent(input$year,
                 {
                     
                     updateSelectInput(session, "sport",
                                       choices = olympics %>% filter(year == input$year,
                                                                     season == input$season) %>%  
                                           distinct(sport) %>% arrange(sport) %>% pull(sport))
                     updateSelectInput(session, "event",
                                       choices = olympics %>% filter(year == input$year,
                                                                     season == input$season) %>%  
                                           distinct(event) %>% arrange(event) %>% pull(event))
                 }
    
    )
    observeEvent(input$sport,
                 {
                     updateSelectInput(session, "event",
                                       choices = olympics %>% filter(year == input$year,
                                                                     season == input$season,
                                                                     sport == input$sport) %>%  
                                           distinct(event) %>% arrange(event) %>% pull(event))
                 }
    )
    output$barchart <- renderPlot({
        barchartdata <-
            olympics %>%
            filter(!is.na(medal),
                   year == input$year, 
                   season == input$season) %>%
            mutate(team = gsub('-[0-9]+', '', team)) %>%
            distinct(team, event, medal) %>%
            count(team, medal) %>% 
            mutate(medal = factor(medal, levels = c("Gold", "Silver", "Bronze"))) %>% 
            group_by(team) %>%
            arrange(medal) %>% 
            ungroup() %>% 
            add_count(team, wt = n, name = "num_medals") %>% 
            mutate(rank = dense_rank(desc(num_medals))) 
        
        barchartdatatop <-
            barchartdata %>%
            filter(rank %in% 1:5) 
        
        if (length(unique(barchartdatatop$team)) > 10){
            barchartdatatop <-
                barchartdata %>%
                filter(rank %in% 1:3) 
        }
        
        barchartdatatop %>% 
            ggplot(aes(x = reorder(team, -num_medals), y = n, fill = medal)) +
            geom_bar(stat = "identity", position = "stack") +
            theme_minimal() +
            geom_text(aes(label = paste0(n, " ", medal)), position = position_stack(vjust = 0.5), 
                      color = "white", size = 4, fontface = "bold") +
            stat_summary(fun = sum, aes(label = paste(..y.., emo::ji("sports medal")), group = team), 
                         geom = "text", vjust = -.3, size = 5) +
            labs(title = paste0("Top ", length(unique(barchartdatatop$team)), " Teams in the ", input$year, " ", input$season, 
                                " Olympic Games"),
                 x = "Team",
                 y = "Number of Medals Won") +
            scale_fill_manual("legend", values = c("Gold" = "gold", "Silver" = "#C0C0C0", 
                                                   "Bronze" = "#cd7f32")) + 
            theme(legend.position = "none") 
        
    })
    
    # reactive data set
    event_table <- reactive({
        olympics %>%
            filter(!is.na(medal),
                   year == input$year,
                   season == input$season,
                   event == input$event) %>% 
            mutate(Team = gsub('-[0-9]+', '', team),
                   Medal = factor(medal, levels = c("Gold", "Silver", "Bronze"))) %>%
            distinct(event, Team, Medal) %>% 
            arrange(Medal) %>% 
            rename(Event = event)
    })
    
    output$table <- renderDataTable({
        DT::datatable(event_table(), 
                      options = list(dom = 't',
                                     initComplete = JS(
                                         "function(settings, json) {",
                                         "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
                                         "}")
                      ),
                      caption = htmltools::tags$caption(
                          paste(input$year, input$season, input$event,
                                "Olympics Medal Summary", sep = " "),
                          style = "color:white"))
    })
    
    olympic_location <- reactive({
        olympics_countries %>% 
            filter(year == input$year,
                   season == input$season) %>% 
            distinct(city, country) %>%
            mutate(location = paste0(city, ", ", country)) %>% 
            pull(location)
    })
    
    olympic_teams <- reactive({
        olympics %>% 
            filter(year == input$year,
                   season == input$season) %>% 
            mutate(team = gsub('-[0-9]+', '', team)) %>% 
            distinct(team) %>% 
            nrow()
    })
        
    output$intro <- renderText({ 
        paste0("The ", input$year, " ", input$season, 
               " Olympic Games were held in ", 
              olympic_location(), ".",
              " A total of ", olympic_teams(), " teams participated in the games."
              )
    })
}

# Run the application
shinyApp(ui = ui, server = server)

