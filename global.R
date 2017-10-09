library(reshape2)
library(geojson)
library(readxl)
library(leaflet)
library(httr)
library(rgeolocate)
library(shiny)
library(ggplot2)
library(sp)
library(dplyr)

capitalize=function(x){
  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", x, perl=TRUE)
}

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
  dplyr::filter(!(grepl('^GUAM|^US VIRGIN',to)|grepl('^GUAM|^US VIRGIN',from)))%>%
  dplyr::mutate_at(.vars = vars(to,from),.funs=funs(capitalize(tolower(.))))

whereami <- ip_api(httr::content(httr::GET('https://api.ipify.org?format=json'))[1])

thisstate <- 'Illinois'

if(whereami$country_code=='US'){
  thisstate <- whereami$region_name
}

calc <- function(side){
  total <- gun_mat%>%
    group_by_(side)%>%
    summarise(total_sum=sum(value))%>%
    rename_('state'=side)
  
  nonstate <- gun_mat%>%
    filter(to!=from)%>%
    group_by_(side)%>%
    summarise(state_sum=sum(value))%>%
    rename_('state'=side)
  
  ret <- nonstate%>%
    left_join(total,by='state')%>%
    mutate(ratio=100*state_sum/total_sum)
  
  names(ret)[-1] <- paste(names(ret)[-1],side,sep = '_')
  
  ret
}

net_flow <- calc(side = 'from')%>%
  left_join(calc(side = 'to'),by='state')%>%
  mutate(net=state_sum_from-state_sum_to,
         ratio_net=ratio_from-ratio_to)%>%
  arrange(desc(ratio_net))

net_flow$state <- factor(net_flow$state,levels = net_flow$state)

net_plot <- ggplot2::ggplot(net_flow,ggplot2::aes(x=state,y=ratio_net,
                                      fill=cut(ratio_net,breaks = 10,include.lowest = TRUE)))+
  ggplot2::geom_bar(stat='identity')+
  scale_fill_brewer(palette = "RdYlBu",direction = -1,name=NULL)+
  theme_bw()+
  labs(title='Net Firearm Flow per 100 Firearms Between States',
       subtitle='High is Net Exporter, Low is Net Importer',
       y='Net Ratio per 100 Firearms',x='State')+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90),legend.position = 'bottom')
