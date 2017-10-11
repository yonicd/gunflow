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

plot_size = 20

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
  theme_minimal(base_size = plot_size)+
  labs(title='Net Firearm Flow per 100 Firearms Between States',
       subtitle='High is Net Exporter, Low is Net Importer',
       caption = "Source: Bureau of Alcohol, Firearms and Explosives (2016)",
       y='Net Ratio per 100 Firearms',x='State')+
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90),legend.position = 'bottom')

#network_dat <- net_dat(gun_mat)

load('www/network_dat.rda')

set.seed(1234)

network_plot <- network_dat$filtered_cors %>%
  igraph::graph_from_data_frame(vertices = network_dat$net_flow,directed = TRUE) %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = correlation), width = 0.5,
                 arrow = arrow(type = "closed", ends = "first",
                               length = unit(0.10, "inches"))) +
  geom_node_point(aes(size = ratio_net,colour=Division)) +
  theme_graph(base_size = plot_size) +
  geom_node_text(aes(label = name), repel = TRUE, point.padding = unit(0.2, "lines")) +
  #scale_size_continuous(range = c(1, 15)) +
  scale_color_discrete(na.value = 'black')+
  labs(size = "Net Flow",
       edge_alpha = "Correlation",
       title = "Directed Graph of Interstate Firearms Source and Recovery",
       caption = "Source: Bureau of Alcohol, Firearms and Explosives (2016)")


power_plot <- network_dat$alpha_pow%>%
  ggplot(aes(x=neg,y=pos,label=state,fill=Division))+
  ggrepel::geom_label_repel()+theme_minimal(base_size = plot_size)+
  labs(x='Level of Antagonistic Relations',
       y='Level of Cooperative Relations',
       title = "State Power Centrality of Interstate Firearms Directed Graph",
       subtite = "Bonacich Exponent Levels (-0.5 , 0.5)",
       caption = "Source: Bureau of Alcohol, Firearms and Explosives (2016)")

scatter_plot <- scatter_fun(gun_mat)

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