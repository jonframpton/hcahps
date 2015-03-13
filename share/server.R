library(shiny)
library(dplyr)
library(ggplot2)
library(reshape2)
library(leaflet)
library(RColorBrewer)
## helpers will load and carry all the necessary data elements
## source("helper.R")

d <- read.csv("data/hcahps.csv")


## build some used data elements
states <- as.character(unique(d$State))
cities <- d %>% select(State, City) %>% distinct(State, City)
ttl <- d %>% summarize(MeanOverallRating = round(mean(rating, na.rm = T),2),
                               PercentHighRating = round(mean(H_HSP_RATING_9_10, na.rm = T),2),
                               PercentMediumRating = round(mean(H_HSP_RATING_7_8, na.rm = T),2),
                               PercentLowRating = round(mean(H_HSP_RATING_0_6, na.rm = T),2),
                               MeanRecommendRating = round(mean(recommend, na.rm = T),2),
                               PercentHighRecommend = round(mean(H_RECMND_DY, na.rm = T),2),
                               PercentMediumRecommend = round(mean(H_RECMND_PY, na.rm = T),2),
                               PercentLowRecommend = round(mean(H_RECMND_DN, na.rm = T),2),
                       MaxClean = max(clean, na.rm = T), MinClean = min(clean, na.rm = T), MeanClean = mean(clean, na.rm = T),
                       MaxQuiet = max(quiet, na.rm = T), MinQuiet = min(quiet, na.rm = T), MeanQuiet = mean(quiet, na.rm = T),
                       MaxOnTime = max(on_time, na.rm = T), MinOnTime = min(on_time, na.rm = T), MeanOntime = mean(on_time, na.rm = T),
                       MaxNurseCom = max(nurse_com, na.rm = T), MinNurseCom = min(nurse_com, na.rm = T), MeanNurseCome = mean(nurse_com, na.rm = T),
                       MaxDocCom = max(doc_com, na.rm = T), MinDocCom = min(doc_com, na.rm = T), MeanDocCome = mean(doc_com, na.rm = T))


shinyServer(
        function(input, output, session) {
                
                ################  by location page #################
                
                ## drop down selection for states
                output$choose_state <- renderUI({
                        selectInput("state","State", c(Choose = "", states), selectize = TRUE)
                })
                
                ## drop down for cities
                output$choose_cities <- renderUI({
                        # If missing input, return to avoid error later in function
                        if(is.null(input$state))
                                return()
                        
                        ## print(input$state)
                        selectedCities <- cities[cities$State == input$state,2]
                        selectInput("selectedcity", "SelectedCity", c(Choose = "", as.vector(selectedCities)), multiple = TRUE, selectize = TRUE)
                })
                
                ## slider for rating
                output$choose_rating <- renderUI({
                        if(is.null(input$selectedcity))
                                return()
                        
                        scr <- round(as.vector(d[d$City %in% input$selectedcity,"rating"]),3)
                        ## print(scr)
                        sliderInput("availratings", "Overall Ratings", min = min(scr, na.rm = TRUE),
                                    max = max(scr, na.rm = TRUE), value = max(scr, na.rm = TRUE), round = -2, sep = "", step = .001)
                })
                
                ### rating top section
                output$show_rating_header <- renderUI({
                        if(is.null(input$selectedcity))
                                return() 
                        h3("Overall Rating")
                        })
                
                output$show_rating <- renderPlot({
                        if(is.null(input$selectedcity))
                                return()                        
                        
                        ggplot(d[d$City %in% input$selectedcity & d$rating <= input$availratings,], aes(x=rating)) + geom_histogram(binwidth=.1, colour="black", fill="white") +
                                geom_vline(aes(xintercept=mean(rating, na.rm=T)),   # Ignore NA values for mean
                                           color="red", linetype="dashed", size=1) + 
                                labs(title = "", y = "", x = "") +
                                theme(rect = element_rect(size = 0.25))
                })
                output$show_rating_txt <- renderTable({
                        if(is.null(input$selectedcity))
                                return()

                        d.s <- d %>% 
                                filter(City %in% input$selectedcity & rating <= input$availratings) %>% 
                                summarize(HospitalCount = n(),
                                          MeanRating = round(mean(rating, na.rm = T),2),
                                          RatingVsAll = paste(round(((MeanRating / ttl$MeanOverallRating) - 1)*100,2), "%", sep = ""),
                                          PercentHigh = round(mean(H_HSP_RATING_9_10, na.rm = T),2),
                                          PrecentHighVsAll = paste(round(((PercentHigh / ttl$PercentHighRating) - 1)*100,2), "%", sep = ""),
                                          PercentMedium = round(mean(H_HSP_RATING_7_8, na.rm = T),2),
                                          PrecentMedVsAll = paste(round(((PercentMedium / ttl$PercentMediumRating) - 1)*100,2), "%", sep = ""),
                                          PercentLow = round(mean(H_HSP_RATING_0_6, na.rm = T),2),
                                          PrecentLowVsAll = paste(round(((PercentLow / ttl$PercentLowRating) - 1)*100,2), "%", sep = "")
                                          )

                        d.st <- as.data.frame(t(d.s))
                        colnames(d.st) <- c("Value")
                        
                        d.st
                        
                })
                
                ### recommend top section
                output$show_recommend_header <- renderUI({
                        if(is.null(input$selectedcity))
                                return() 
                        h3("Would You Recommend?")
                        })
                
                output$show_recommend <- renderPlot({
                        if(is.null(input$selectedcity))
                                return()                        
                        
                        ggplot(d[d$City %in% input$selectedcity & d$rating <= input$availratings,], aes(x=recommend)) + geom_histogram(binwidth=.1, colour="black", fill="white") +
                                geom_vline(aes(xintercept=mean(recommend, na.rm=T)),   # Ignore NA values for mean
                                           color="red", linetype="dashed", size=1) + 
                                labs(title = "", y = "", x = "") +
                                theme(rect = element_rect(size = 0.25))
                })
                output$show_recommend_txt <- renderTable({
                        if(is.null(input$selectedcity))
                                return()
                        
                        d.s <- d %>% 
                                filter(City %in% input$selectedcity & rating <= input$availratings) %>% 
                                summarize(HospitalCount = n(),
                                          MeanRating = round(mean(recommend, na.rm = T),2),
                                          RatingVsAll = paste(round(((MeanRating / ttl$MeanRecommendRating) - 1)*100,2), "%", sep = ""),
                                          PercentHigh = round(mean(H_RECMND_DY, na.rm = T),2),
                                          PrecentHighVsAll = paste(round(((PercentHigh / ttl$PercentHighRecommend) - 1)*100,2), "%", sep = ""),
                                          PercentMedium = round(mean(H_RECMND_PY, na.rm = T),2),
                                          PrecentMedVsAll = paste(round(((PercentMedium / ttl$PercentMediumRecommend) - 1)*100,2), "%", sep = ""),
                                          PercentLow = round(mean(H_RECMND_DN, na.rm = T),2),
                                          PrecentLowVsAll = paste(round(((PercentLow / ttl$PercentLowRecommend) - 1)*100,2), "%", sep = "")
                                )
                        
                        d.st <- as.data.frame(t(d.s))
                        colnames(d.st) <- c("Value")
                        
                        d.st
                        
                })     
                
                ### question distributions
                output$show_top5_questions <- renderPlot({
                        if(is.null(input$selectedcity))
                                return()
                        
                        d.s <- d %>% 
                                filter(City %in% input$selectedcity & rating <= input$availratings) %>% 
                                select(Provider.ID, 
                                       HowClean = clean, 
                                       HowQuiet = quiet, 
                                       NurseCommunication = nurse_com, 
                                       DoctorCommunication = doc_com, 
                                       OnTime = on_time, 
                                       PainManagement = pain_mgd, 
                                       ExplainedMedicine = med_expl, 
                                       AtHomeRecoveryInfo = home_reco_info, 
                                       UnderstoodPostCare = understood_care_post)
                        d.m <- melt(d.s, id.vars = c("Provider.ID"))
                        ggplot(d.m, aes(x=variable, y=value, fill=variable)) + geom_boxplot() + guides(fill=FALSE) + coord_flip()
                })
                
                ### hospital data table
                output$show_selected_hosp <- renderDataTable({
                        if(is.null(input$selectedcity))
                                return()
                        
                        d.s <- d %>% 
                                filter(City %in% input$selectedcity & rating <= input$availratings) %>% 
                                select(Hospital = Hospital.Name,
                                       OverallRating = rating,
                                       WouldYouRecommend = recommend,
                                       HowClean = clean, 
                                       HowQuiet = quiet, 
                                       NurseCommunication = nurse_com, 
                                       DoctorCommunication = doc_com, 
                                       OnTime = on_time, 
                                       PainManagement = pain_mgd, 
                                       ExplainedMedicine = med_expl, 
                                       AtHomeRecoveryInfo = home_reco_info, 
                                       UnderstoodPostCare = understood_care_post)
                        return(d.s)
                }, options = list(pageLength = 10))
                
                ################  what's important page #################
                output$choose_clean <- renderUI({
                        sliderInput("clean", "How Clean", min = ttl$MinClean,
                                    max = ttl$MaxClean, value = ttl$MaxClean, round = -2, sep = "", step = .001)
                })
                output$choose_quiet <- renderUI({
                        sliderInput("quiet", "How Quiet", min = ttl$MinQuiet,
                                    max = ttl$MaxQuiet, value = ttl$MaxQuiet, round = -2, sep = "", step = .001)
                })                
                output$choose_on_time <- renderUI({
                        sliderInput("on_time", "On Time", min = ttl$MinOnTime,
                                    max = ttl$MaxOnTime, value = ttl$MaxOnTime, round = -2, sep = "", step = .001)
                })
                output$choose_ncom <- renderUI({
                        sliderInput("ncom", "Nurse's Communication", min = ttl$MinNurseCom,
                                    max = ttl$MaxNurseCom, value = ttl$MaxNurseCom, round = -2, sep = "", step = .001)
                })
                output$choose_dcom <- renderUI({
                        sliderInput("dcom", "Doctor's Communication", min = ttl$MinDocCom,
                                    max = ttl$MaxDocCom, value = ttl$MaxDocCom, round = -2, sep = "", step = .001)
                }) 
                output$show_whats_important_table <- renderDataTable({
                        d %>% filter(clean <= input$clean,
                                     quiet <= input$quiet,
                                     on_time <= input$on_time,
                                     nurse_com <= input$ncom,
                                     doc_com <= input$dcom) %>%
                                select(Hospital.Name,
                                       OverallRating = rating,
                                       WouldYouRecommend = recommend) %>%
                                arrange(desc(OverallRating, WouldYouRecommend))
                }, options = list(pageLength = 25))
                
                ################  overall page #################
                output$slide_rating <- renderUI({
                        scr <- round(as.vector(d[,"rating"]),3)
                        ## print(scr)
                        sliderInput("rat", "Overall Ratings", min = min(scr, na.rm = TRUE),
                                    max = max(scr, na.rm = TRUE), value = max(scr, na.rm = TRUE), round = -2, sep = "", step = .001)
                })
                output$slide_recommend <- renderUI({
                        scr <- round(as.vector(d[,"recommend"]),3)
                        ## print(scr)
                        sliderInput("rec", "Overall Recommend", min = min(scr, na.rm = TRUE),
                                    max = max(scr, na.rm = TRUE), value = max(scr, na.rm = TRUE), round = -2, sep = "", step = .001)
                })
                
                map <- createLeafletMap(session, "map")
                
                session$onFlushed(once=TRUE, function() {
                        paintObs <- observe({
                                
                                # Clear existing circles before drawing
                                map$clearShapes()
                                # Draw in batches of 1000; makes the app feel a bit more responsive
                                colors <- brewer.pal(4, "Reds")[cut(d$rating, 4, labels = FALSE)]
                                chunksize <- 1000
                                for (from in seq.int(1, nrow(d), chunksize)) {
                                        to <- min(nrow(d), from + chunksize)
                                        zipchunk <- d[from:to,]
                                        # Bug in Shiny causes this to error out when user closes browser
                                        # before we get here
                                        try(
                                                map$addCircle(
                                                        zipchunk$lat, zipchunk$lon,
                                                        d$rating * 500,
                                                        zipchunk$Hospital.Name,
                                                        list(stroke=FALSE, fill=TRUE, fillOpacity=d$rating -.5),
                                                        list(color = colors)
                                                )
                                        )
                                }
                        })
                })
                
                # Show a popup at the given location
                showHospPopup <- function(hosp, lat, lng) {
                        sh <- d[d$Hospital.Name == hosp,]
                        content <- as.character(tagList(
                                tags$h4(hosp),
                                tags$strong(HTML(sprintf("%s, %s %s",
                                                         sh$City, sh$State, sh$Zip.Code
                                ))), tags$br(),
                                sprintf("Overall Rating: %s%%", sh$rating), tags$br(),
                                sprintf("Percent would Recommend: %s%%", sh$recommend), tags$br()
                                
                        ))
                        map$showPopup(lat, lng, content, hosp)
                }
                
                # When map is clicked, show a popup with city info
                clickObs <- observe({
                        map$clearPopups()
                        event <- input$map_shape_click
                        if (is.null(event))
                                return()
                        
                        isolate({
                                showHospPopup(event$id, event$lat, event$lng)
                        })
                })
                
                session$onSessionEnded(clickObs$suspend)                
                
                
        }
)