library(ggvis)
library(dplyr)
library(lubridate) # remove if don't end up using
library(rvest) # for web scraping
library(reshape2)
source('computationFunctions.R')

ws <- 'http://www.freddiemac.com/pmms/pmms30.htm'
cssSelector <- 'tbody+ tbody td'

# read full website
ws.html <- read_html(ws)
# there are 11 columns on the website including the month labels
ws.ncol <- 11

# parse table
mtg <- ws.html %>% 
  rvest::html_nodes(cssSelector) %>%
  html_text() %>% matrix(ncol = ws.ncol, byrow = T)

# mtg now looks like the table on the website, but we want to get it in a long table
# format; we know that every 13 rows new years start; we also know that the current year
# is the first set of data and 1971 is the last set of data

# drop the pts, which is every second column
mtg <- mtg[,-seq(3,ncol(mtg),2)]
# loop over every 13 rows and cbind them to the original data to create a matrix of 
# months x years
tmpMtg <- mtg[1:13,]
mtg <- mtg[-(1:13),]
for(i in 1:(nrow(mtg)/13)){
  tmpMtg <- cbind(tmpMtg,mtg[1:13,2:ncol(mtg)])
  mtg <- mtg[-(1:13),]
}

# remove the annual average row, and the months as a column of data
mtg <- tmpMtg[1:12,2:ncol(tmpMtg)]
rownames(mtg) <- 1:12 # january - december
colnames(mtg) <- 2015:1971 # should probably make this so it is not hard coded
rm(tmpMtg)

# melt to long format and make month-year into single variable
mtg <- mtg %>% reshape2::melt(varnames = c('month','year'), value.name = 'rate') %>%
  dplyr::mutate(yearMon = paste(year,month,'01',sep = '-')) %>% 
  dplyr::select(one_of('yearMon','rate'))

last_day <- function(date) {
  ceiling_date(date, "month") + months(1) - days(1)
}
mtg$rate <- as.numeric(as.character(mtg$rate))
mtg$yearMon <- last_day(lubridate::ymd(mtg$yearMon))

# remove na's from data, and then arrange from oldest to newest
mtg <- mtg %>% filter(!is.na(rate)) %>% arrange(yearMon)

# and save the data so I don't have to load it from the web every time
save(mtg, file = 'mortgageData.RData')
