library(shiny)
library(leaflet)
library(BH)

shinyUI(navbarPage("HCAHPS Explorer",
                   
                   tabPanel("Around the States",
                            tags$head(includeCSS("style.css")),
                            div(class="outer",
                                leafletMap("map", width="100%", height="100%",
                                           initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
                                           initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
                                           options=list(
                                                   center = c(37.45, -93.85),
                                                   zoom = 4
                                                   ##maxBounds = list(list(15.961329,-129.92981), list(52.908902,-56.80481)) # Show US only
                                           )
                                )
                            )
                   ),                   
                   
                   tabPanel("By Location",

                            fluidRow(
                                    column(4,
                                           uiOutput("choose_state")),
                                    column(4,
                                           uiOutput("choose_cities")),
                                    column(4,
                                           uiOutput("choose_rating"))),
                            fluidRow(column(6, align = "center", uiOutput("show_rating_header")),
                                     column(6, align = "center", uiOutput("show_recommend_header"))),                            
                            fluidRow(
                                    column(3,
                                           plotOutput("show_rating")),
                                    column(3, align = "center",
                                           tableOutput("show_rating_txt")),
                                    column(3,
                                           plotOutput("show_recommend")),
                                    column(3, align = "center",
                                           tableOutput("show_recommend_txt"))),
                            fluidRow(
                                    column(12,
                                           plotOutput("show_top5_questions"))),
                            fluidRow(
                                    column(12,
                                           dataTableOutput("show_selected_hosp")))                            ),
                   tabPanel("What's Important",
                            fluidRow(
                                    column(4,
                                           uiOutput("choose_clean"),
                                           uiOutput("choose_quiet"),
                                           uiOutput("choose_on_time"),
                                           uiOutput("choose_ncom"),
                                           uiOutput("choose_dcom")),
                                    column(8,
                                           dataTableOutput("show_whats_important_table"))))                          
                  
                   
                   
))