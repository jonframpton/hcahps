## exploring the hcaps data set


library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape2)
library(stringr)

## lead the initial data set
h <- read.csv("hcahps/HCAHPS_-_Hospital.csv")

h.contact <- h[,c(1:8, 13:14, 18)]
h.contact <- unique(h.contact)

ll.pattern <- "\\(.+?\\)"

ll.start <- gsub("^[^,]*,", "", h.contact$Location)

ll.pattern <- "\\(.+\\d\\)"
ll.single <- str_extract(ll.start, ll.pattern)
ll.replaced <- str_replace(ll.single, fixed("("), "")
ll.replaced.both <- str_replace(ll.replaced, fixed(")"), "")
ll.repalced.both.t <- str_replace(ll.replaced.both, " ", "")
ll <- data.frame(lat = round(as.numeric(str_split_fixed(ll.replaced.both, ",", 2)[,1]),6), 
                 lon = round(as.numeric(str_split_fixed(ll.replaced.both, ",", 2)[,2]),6))
ll.na <- ll[is.na(ll$lat),]
ll.na$row.names
h.contact[c(104,157,2562,4399,4149),]
h.hosp <- cbind(h.contact[1:10], ll)
## head(h.con)

## now lets create an id and question based set
h.question <- h[,c(1, 9:17)]
h.qs <- data.frame(i = h.question$HCAHPS.Measure.ID, d = h.question$HCAHPS.Question, a = h.question$HCAHPS.Answer.Description)
h.qs <- unique(h.qs)
h.qna <- dcast(data = h.question, formula = Provider.ID ~ HCAHPS.Measure.ID, value.var = "HCAHPS.Answer.Percent")

## so we have three main data sets, the rest will be removed
## h.hosp is the hospital level data
## h.qs are all the question descriptions and ids
## h.qna are the questions and answers by provider id
rm(h);rm(h.contact);rm(h.question);rm(ll);rm(ll.pattern);rm(ll.replaced);rm(ll.replaced.both);rm(ll.single);rm(ll.na)

## scoring by question
## two type of elements, 3 level and Y/N
## basic scoring with a possible of 10 total points
h <- .1         ## high levels
m <- .075       ## med
l <- .05        ## low

y <- .1         ## yes gets total possible
n <- 0          ## no gets zero

## add question scores to dataset
h.qna.scored <- h.qna %>% mutate(clean = (H_CLEAN_HSP_A_P * h + H_CLEAN_HSP_U_P * m + H_CLEAN_HSP_SN_P * l),
                                 quiet = (H_QUIET_HSP_A_P * h + H_QUIET_HSP_U_P * m + H_QUIET_HSP_SN_P * l),
                                 nurse_com = (H_COMP_1_A_P * h + H_COMP_1_U_P * m + H_COMP_1_SN_P * l),
                                 doc_com = (H_COMP_2_A_P * h + H_COMP_2_U_P * m + H_COMP_2_SN_P * l),
                                 on_time = (H_COMP_3_A_P * h + H_COMP_3_U_P * m + H_COMP_3_SN_P * l),
                                 pain_mgd = (H_COMP_4_A_P * h + H_COMP_4_U_P * m + H_COMP_4_SN_P * l),
                                 med_expl = (H_COMP_5_A_P * h + H_COMP_5_U_P * m + H_COMP_5_SN_P * l),
                                 home_reco_info = (H_COMP_6_Y_P * y + H_COMP_6_N_P * n),
                                 understood_care_post = (H_COMP_7_SA * h + H_COMP_7_A * m + H_COMP_7_D_SD * l),
                                 rating = (H_HSP_RATING_9_10 * h + H_HSP_RATING_7_8 * m + H_HSP_RATING_0_6 * l),
                                 recommend = (H_RECMND_DY * h + H_RECMND_PY * m + H_RECMND_DN * l))
## bring the names back in
d <- inner_join(h.qna.scored, h.hosp)

ApplyQuintiles <- function(x) {
        cut(x, breaks=c(quantile(d$rating, probs = seq(0, 1, by = 0.25), na.rm = T)), 
            labels=c("1","2","3","4"), include.lowest=TRUE)
}
d$q <- sapply(d$rating, ApplyQuintiles)

## write the d file for the shinyapp
write.csv(d,"hcahps.csv")

## get rid of some of the others
rm(h.hosp);rm(h.qna);rm(h.qna.scored)

## how does the overall rating compare to the cleanliness score
ggplot(h.qna.scored, aes(x=clean, y=rating)) +
        geom_point(shape=1) +  
        geom_smooth(method=lm)

## how does the quiet score relate to the overall rating
ggplot(h.qna.scored, aes(x=quiet, y=rating)) +
        geom_point(shape=1) +  
        geom_smooth(method=lm)

## view distribution of recommend scores
ggplot(h.qna.scored, aes(x=recommend)) + geom_histogram(binwidth=.1, colour="black", fill="white") +
        geom_vline(aes(xintercept=mean(recommend, na.rm=T)),   # Ignore NA values for mean
                   color="red", linetype="dashed", size=1)

## add location based stats for h.hosp set
## http://notesofdabbler.bitbucket.org/2013_12_censusBlog/censusHomeValueExplore_wdesc.html
## requires uscensus api key but can get one


## build a basic shiny app
## 3 tabs (by state, by question, overall)
## 

scity = c("FLORENCE")
d.s <- d %>% filter(City %in% scity) %>% summarize(MeanRating = round(mean(rating, na.rm = T),2),
                                                                PercentHigh = round(mean(H_HSP_RATING_9_10, na.rm = T),2),
                                                                PercentMedium = round(mean(H_HSP_RATING_7_8, na.rm = T),2),
                                                                PercentLow = round(mean(H_HSP_RATING_0_6, na.rm = T),2))

d.st <- as.data.frame(t(d.s))
colnames(d.st) <- c("Value")
d.st

## build a condition set
## clean, quiet, nurse_com, doc_com, on_time, pain_mgd, med_expl, home_reco_info, understood_care_post
d.tst <- d %>% select(Provider.ID, clean, quiet, nurse_com, doc_com, on_time, pain_mgd, med_expl, home_reco_info, understood_care_post)
d.tst2 <- melt(d.tst, id.vars = c("Provider.ID"))


################################### mapping it out
install.packages("devtools")
devtools::install_github("rstudio/leaflet")
library(leaflet)

d <- read.csv("hcahps/data/hcahps.csv")
d.t <- d[is.na(d$lon),]

m <- leaflet(d) %>% addTiles() %>% addCircles()
m
m <- leaflet()