net_dat <- function(gun_mat_in=gun_mat,min_n=30,min_corr=.5){

  gun_mat_out <- gun_mat_in %>%
    dplyr::filter(!(grepl('^GUAM|^US VIRGIN',to)|grepl('^GUAM|^US VIRGIN',from)))%>%
    dplyr::mutate_at(.vars = vars(to,from),.funs=funs(capitalize(tolower(.))))%>%
    dplyr::filter(to!=from)%>%
    dplyr::filter(value>min_n)

  
  x <- plyr::mdply(1:nrow(gun_mat_out),
                   function(i) data.frame(t(replicate(gun_mat_out[i,3],as.character(gun_mat_out[i,c(1,2)])))),
                   .progress = 'text')
  
  names(x) <- c('from','to')
  
  gun_mat_cors<- x %>%
    widyr::pairwise_cor(from, to, sort = TRUE)
  
  filtered_cors <- gun_mat_cors%>%
    dplyr::filter(correlation > min_corr)
  
  vertices <-gun_mat_out%>%
    filter(from %in% filtered_cors$item1)%>%
    group_by(from)%>%do(.,head(.,1))
  
  data(state)
  
  suppressWarnings({
    net_flow_out <- net_flow%>%
      dplyr::filter(state %in% filtered_cors$item1)%>%
      dplyr::left_join(data.frame(state=state.name,
                                  Region=as.character(state.region),
                                  Division=as.character(state.division),stringsAsFactors = FALSE),by='state')
  })
  
  set.seed(1234)
  
  network_plot <- filtered_cors %>%
    igraph::graph_from_data_frame(vertices = net_flow_out,directed = TRUE) %>%
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
  
  g <- igraph::graph_from_data_frame(filtered_cors,vertices = net_flow_out,directed = TRUE)
  
  alpha_pow <- plyr::ddply(data.frame(e=c(-0.5,0.5)),c('e'),.fun=function(df){
    alpha_pow <-power_centrality(g,exponent = df$e)
    data.frame(state=names(alpha_pow),pow=alpha_pow,stringsAsFactors = FALSE)
  })%>%mutate(alpha=factor(e,labels=c('neg','pos')))%>%reshape2::dcast(state~alpha,value.var = 'pow')
  
  
  alpha_pow <- alpha_pow%>%
    dplyr::left_join(data.frame(state=state.name,
                                Region=as.character(state.region),
                                Division=as.character(state.division),stringsAsFactors = FALSE),by='state')
  
  
  return(list(net_flow=net_flow_out,vertices=vertices,filtered_cors=filtered_cors,alpha_pow=alpha_pow))
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

scatter_fun <- function(gun_mat){
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
  
  tot%>%
    ggplot(aes(x=from_pct,y=to_pct,fill=cut(within_pct,5,include.lowest = TRUE)))+
    ggrepel::geom_label_repel(aes(label=state),alpha=.7)+
    scale_fill_brewer(palette = "RdYlBu",direction = -1,name='Within Percent')+
    theme_minimal(base_size = plot_size)+
    labs(title='Rate per 1000 Firearms Inflow, Outflow and Within',
         caption = "Source: Bureau of Alcohol, Firearms and Explosives (2016)",
         x='Outflow Rate',y='Inflow Rate')
}

