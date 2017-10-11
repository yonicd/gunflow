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

data(state)

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

gun_mat0 <- readxl::read_xlsx('www/sourcerecoverybystatecy2016.xlsx',col_names = TRUE,range = 'B2:BD56')

gun_mat <- gun_mat0%>%
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
  theme_minimal()+
  labs(title='Net Firearm Flow per 100 Firearms Between States',
       subtitle='High is Net Exporter, Low is Net Importer',
       caption = "Source: Bureau of Alcohol, Firearms and Explosives (2016)",
       y='Net Ratio per 100 Firearms',x='State')+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90),legend.position = 'bottom')

gun_mat_cors<- x%>%
  widyr::pairwise_cor(from, to, sort = TRUE)

filtered_cors <- gun_mat_cors%>%
  dplyr::filter(correlation > 0.50)

vertices <-gun_mat%>%
  filter(from %in% filtered_cors$item1)%>%
  group_by(from)%>%do(.,head(.,1))

suppressWarnings({
  net_flow <- net_flow%>%
    dplyr::filter(state %in% filtered_cors$item1)%>%
    dplyr::left_join(data.frame(state=state.name,
                                Region=as.character(state.region),
                                Division=as.character(state.division),stringsAsFactors = FALSE),by='state')
})


set.seed(1234)

network_plot <- filtered_cors %>%
  igraph::graph_from_data_frame(vertices = net_flow,directed = TRUE) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), width = 0.5,
                 arrow = arrow(type = "closed", ends = "first",
                               length = unit(0.10, "inches"))) +
  geom_node_point(aes(size = ratio_net,colour=Division)) +
  theme_graph() +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  #scale_size_continuous(range = c(1, 15)) +
  scale_color_discrete(na.value = 'black')+
  labs(size = "Net Flow",
       edge_alpha = "Correlation",
       title = "Directed Graph of Interstate Firearms Source and Recovery",
       caption = "Source: Bureau of Alcohol, Firearms and Explosives (2016)")

tot_in <- gun_mat%>%dplyr::filter(to!=from)%>%dplyr::group_by(from)%>%dplyr::summarise(from_total=sum(value))%>%rename(state=from)
tot_out <- gun_mat%>%dplyr::filter(to!=from)%>%dplyr::group_by(to)%>%dplyr::summarise(to_total=sum(value))%>%rename(state=to)
tot_within <- gun_mat%>%dplyr::filter(to==from)%>%rename(within_total=value)%>%select(state=from,within_total)

tot <- tot_within%>%dplyr::left_join(tot_in,by='state')%>%dplyr::left_join(tot_out,by='state')

tot$total <- apply(tot[,-1],1,sum)

tot <- tot%>%dplyr::mutate(within_pct=100*within_total/total,from_pct=1000*from_total/total,to_pct=1000*to_total/total)

tot$state <- factor(tot$state,levels=tot$state[order(apply(tot[,c(6:8)],1,sum),decreasing = TRUE)])

tot%>%dplyr::select(state,ends_with('pct'))%>%
  reshape2::melt(.,id='state')%>%
  ggplot(aes(x=state,y=value,fill=variable))+geom_bar(stat='identity')+facet_wrap(~variable,ncol=1)

tot$total_pct <- apply(tot[,c(6:8)],1,sum)

scatter_plot <- 
  tot%>%
  ggplot(aes(x=from_pct,y=to_pct,fill=cut(within_pct,5,include.lowest = TRUE)))+
  ggrepel::geom_label_repel(aes(label=state),alpha=.7)+
  scale_fill_brewer(palette = "RdYlBu",direction = -1,name='Within Percent')+
  theme_minimal()+
  labs(title='Rate per 1000 Firearms Inflow, Outflow and Within',
       caption = "Source: Bureau of Alcohol, Firearms and Explosives (2016)",
       x='Outflow Rate',y='Inflow Rate')


# g <- gun_mat0[,-1]
# row.names(g) <- gun_mat0[,1]
# 
# mg <- as.matrix(g)
# 
# diag(mg) <- NA
# 
# mg <- mg[!grepl('^GUAM|^US VIRGIN',rownames(mg)),!grepl('^GUAM|^US VIRGIN',colnames(mg))]
# 
# heat_plot <- heatmaply::heatmaply(heatmaply::percentize(mg),
#                      main='Firearms Sourced and Recovered in the United States and Territories 2016',
#                      xlab = 'Source',
#                      ylab = 'Recovered',
#                      colors = rev(heatmaply::RdYlBu(256)),
#                      k_row = 5,
#                      k_col = 5,
#                      fontsize_row = 6,
#                      fontsize_col = 6,file='www/heat_plot.html')