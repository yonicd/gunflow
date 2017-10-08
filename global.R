library(reshape2)
library(geojson)
library(readxl)
library(leaflet)
library(httr)
library(rgeolocate)
library(shiny)
library(dplyr)

#f0 <- tempfile()
#download.file("https://raw.githubusercontent.com/rstudio/leaflet/gh-pages/json/us-states.geojson",destfile = f0)
#states <- geojsonio::geojson_read(f0, what = "sp")

Sys.setenv(MAPBOX_ACCESS_TOKEN=read.dcf('www/.MYTOKEN')[1])

#f1 <- tempfile()
#download.file('https://www.atf.gov/docs/undefined/sourcerecoverybystatecy2016xlsx/download',destfile = f1)

states <- geojsonio::geojson_read('www/us-states.geojson', what = "sp")

gun_mat <- readxl::read_xlsx('www/sourcerecoverybystatecy2016.xlsx',col_names = TRUE,range = 'B2:BD56')

gun_mat <- gun_mat%>%
  reshape2::melt(.,'X__1')

names(gun_mat) <- c('from','to','value')

gun_mat <- gun_mat %>%
  dplyr::filter(!(grepl('^GUAM|^US VIRGIN',to)|grepl('^GUAM|^US VIRGIN',from)))

whereami <- ip_api(httr::content(httr::GET('https://api.ipify.org?format=json'))[1])

thisstate <- 'ILLINOIS'

if(whereami$country_code=='US'){
  thisstate <- toupper(whereami$region_name)
}