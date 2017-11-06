library(reshape2)
library(geojson)
library(readxl)
library(leaflet)
library(httr)
library(rgeolocate)
library(shiny)
library(ggplot2)
library(sp)
library(widyr)
library(igraph)
library(slickR)
library(ggraph)
library(svglite)
library(dplyr)

source('funs.R')

plot_size = 16

capitalize=function(x){
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)
}

#f0 <- tempfile()
#download.file("https://raw.githubusercontent.com/rstudio/leaflet/gh-pages/json/us-states.geojson",destfile = f0)
#states <- geojsonio::geojson_read(f0, what = "sp")

Sys.setenv(MAPBOX_ACCESS_TOKEN=read.dcf('www/MYTOKEN')[1])

#f1 <- tempfile()
#download.file('https://www.atf.gov/docs/undefined/sourcerecoverybystatecy2016xlsx/download',destfile = f1)

states <- geojsonio::geojson_read('www/us-states.geojson', what = "sp")

# gun_mat <- plyr::ddply(data.frame(year=2011:2016),c('year'),.fun=function(x){
#   gun_mat0 <- readxl::read_xlsx(sprintf('www/final_source_recovery_by_state-cy_%s.xlsx',x),col_names = TRUE,range = 'B2:BD56')
#   
#   gun_mat <- gun_mat0%>%
#     reshape2::melt(.,'X__1')
#   
#   names(gun_mat) <- c('from','to','value')
#   
#   gun_mat %>%
#     dplyr::filter(!(grepl('^GUAM|^US VIRGIN',to)|grepl('^GUAM|^US VIRGIN',from)))%>%
#     dplyr::mutate_at(.vars = vars(to,from),.funs=funs(capitalize(tolower(.))))%>%
#     dplyr::mutate(to=ifelse(to=='Dst Of Columbia','District Of Columbia',to),
#                   from=ifelse(from=='Dst Of Columbia','District Of Columbia',from))
# })
                  

load('www/gun_mat.rda')

whereami <- rgeolocate::ip_api(httr::content(httr::GET('https://api.ipify.org?format=json'))[1])

thisstate <- 'Illinois'

if(whereami$country_code=='US'){
  thisstate <- whereami$region_name
}

net_flow <- calc(side = 'from')%>%
  left_join(calc(side = 'to'),by=c('year','state'))%>%
  mutate(net=state_sum_from-state_sum_to,
         ratio_net=ratio_from-ratio_to)%>%
  arrange(desc(ratio_net))

# p <- net_flow%>%mutate(check=state=='California')%>%
#   ggplot(aes(x=ratio_from,y=ratio_to,label=state,frame=year))+
#   geom_label(aes(fill=check),show.legend = FALSE)+
#   geom_abline(aes(intercept=0,slope=1),linetype=2)

network_dat <- net_dat(gun_mat)

tot <- scatter_fun(gun_mat)

# gun_ranking <- (xml2::read_html('http://smartgunlaws.org/scorecard/')%>%rvest::html_nodes(xpath = '//*[@id="rankingsTable"]')%>%rvest::html_table())[[1]]
# 
# names(gun_ranking) <- c('rank','state','grade','death_rate','smart_law')
# 
# gun_ranking$smart_law[nzchar(gun_ranking$smart_law)] <- '*'

load('www/gun_ranking.rda')

load('www/atf_data.rda')

tot <- tot%>%mutate(state=as.character(state))%>%left_join(gun_ranking,by=c('year','state'))

tot$state_grade <- gsub('NA','',paste(tot$state,tot$grade,tot$smart_law))
tot$grade_round <- gsub('[+-]','',tot$grade)
