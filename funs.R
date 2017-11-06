net_dat <- function(gun_mat_in=gun_mat,min_n=30,min_corr=.5){

  gun_mat_out <- gun_mat_in %>% 
    group_by(year)%>%
    dplyr::filter(!(grepl('^Puerto|^District',to)|grepl('^Puerto|^District',from)))%>%
    dplyr::mutate_at(.vars = vars(to,from),.funs=funs(capitalize(tolower(.))))%>%
    dplyr::filter(to!=from)%>%
    dplyr::filter(value>min_n)

  
  x <- gun_mat_out[rep(seq_len(nrow(gun_mat_out)), times=gun_mat_out$value),c('year','from','to')]
  
  gun_mat_cors<- plyr::ddply(x,c('year'),.fun=function(x0) x0%>%select(-year)%>%widyr::pairwise_cor(from, to, sort = TRUE))
  
  filtered_cors <- gun_mat_cors%>%
    group_by(year) %>%
    dplyr::filter(correlation > min_corr)%>%
    select(item1,item2,correlation,year)
  
  data(state)
  
  net_flow_out <- net_flow%>%
      dplyr::filter(state %in% filtered_cors$item1)%>%
      dplyr::left_join(data.frame(state=state.name,
                                  Region=as.character(state.region),
                                  Division=as.character(state.division),stringsAsFactors = FALSE),by='state')%>%
      select(state,everything())
  
  
  lout <- plyr::dlply(data.frame(year=2011:2016),c('year'),.fun=function(yr){
    set.seed(1234)
    network_plot <- filtered_cors %>% filter(year==yr)%>%
      igraph::graph_from_data_frame(vertices = net_flow_out%>%filter(year==yr),directed = TRUE) %>%
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
           title = sprintf("Directed Graph of Interstate Firearms Source and Recovery (%s)",yr),
           caption = "Source: Bureau of Alcohol, Firearms and Explosives (2016)")  
    
    l <- list(network_plot=network_plot)
    
    g <- igraph::graph_from_data_frame(filtered_cors%>% filter(year==yr),vertices = net_flow_out%>% filter(year==yr),directed = TRUE)
    
    alpha_pow <- try(plyr::ddply(data.frame(e=c(-0.5,0.5)),c('e'),.fun=function(df){
      alpha_pow <-power_centrality(g,exponent = df$e)
      data.frame(state=names(alpha_pow),pow=alpha_pow,stringsAsFactors = FALSE)
    })%>%mutate(alpha=factor(e,labels=c('neg','pos')))%>%reshape2::dcast(state~alpha,value.var = 'pow'))
    
    if(class(alpha_pow)!='try-error'){
      alpha_pow <- alpha_pow%>%
        dplyr::left_join(data.frame(state=state.name,
                                    Region=as.character(state.region),
                                    Division=as.character(state.division),stringsAsFactors = FALSE),by='state')  
      
      l$alpha_pow=alpha_pow
    }
    
    return(l)    
  })

  lout
  
}

calc <- function(side){
  total <- gun_mat%>%
    group_by_('year',side)%>%
    summarise(total_sum=sum(value))%>%
    rename_('state'=side)
  
  nonstate <- gun_mat%>%
    filter(to!=from)%>%
    group_by_('year',side)%>%
    summarise(state_sum=sum(value))%>%
    rename_('state'=side)
  
  ret <- nonstate%>%
    left_join(total,by=c('year','state'))%>%
    mutate(ratio=100*state_sum/total_sum)
  
  names(ret)[-c(1,2)] <- paste(names(ret)[-c(1,2)],side,sep = '_')
  
  ret
}

scatter_fun <- function(gun_mat){
  tot_in <- gun_mat%>%dplyr::filter(to!=from)%>%dplyr::group_by(year,from)%>%dplyr::summarise(from_total=sum(value))%>%rename(state=from)
  tot_out <- gun_mat%>%dplyr::filter(to!=from)%>%dplyr::group_by(year,to)%>%dplyr::summarise(to_total=sum(value))%>%rename(state=to)
  tot_within <- gun_mat%>%dplyr::filter(to==from)%>%dplyr::group_by(year)%>%rename(within_total=value)%>%select(year,state=from,within_total)
  
  tot <- tot_within%>%dplyr::left_join(tot_in,by=c('year','state'))%>%dplyr::left_join(tot_out,by=c('year','state'))
  
  tot$total <- apply(tot[,-c(1,2)],1,sum)
  
  tot <- tot%>%dplyr::mutate(within_pct=100*within_total/total,from_pct=100*from_total/total,to_pct=100*to_total/total)
  
  tot$total_pct <- apply(tot[,c(6:8)],1,sum)
  
  tot
}

