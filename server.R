shinyServer(function(input, output) {

    datin <- shiny::eventReactive(c(input$thisstate,input$type,input$year),{

      gun_mat1 <- switch(input$type,
                         Inflow={
                           gun_mat%>%
                             dplyr::filter(year==input$year)%>%
                             dplyr::group_by(to)%>%
                             dplyr::mutate(value1=ifelse(to==from,NA,value),pct=100*value1/sum(value1,na.rm = TRUE))%>%
                             dplyr::filter(to==input$thisstate)%>%
                             dplyr::rename(state=from)       
                         },
                         Outflow={
                           gun_mat%>%
                             dplyr::filter(year==input$year)%>%
                             dplyr::group_by(from)%>%
                             dplyr::mutate(value1=ifelse(to==from,NA,value),pct=100*value1/sum(value1,na.rm = TRUE))%>%
                             dplyr::filter(from==input$thisstate)%>%
                             dplyr::rename(state=to)
                         })
      
      mydata <- states@data   
      mydata <- mydata%>%
        rename(state=name)%>%
        mutate(state=as.character(state))%>%
        left_join(gun_mat1%>%ungroup%>%select(state,value1,value,pct),by='state')
      
      states@data$pct <- mydata$pct
      states@data$level <- mydata$value1
      states@data$value <- mydata$value
      states@data$density <- NULL
      
      states
    })
    
    observeEvent(c(datin(),input$scale),{
      
      d <- switch(input$scale,
                  National={
                    seq(0,35)
                  },
                  State={
                    datin()$pct
                  })
      
      pal <- colorNumeric(
        palette = "RdYlBu",
        domain = d,na.color = 'black',reverse = TRUE)
      
      
      output$leaf <- leaflet::renderLeaflet({
        
        df <- datin()
        
        m <- leaflet(df) %>%
          setView(-96, 37.8, 4) %>%
          addProviderTiles("MapBox", options = providerTileOptions(
            id = "mapbox.light",
            accessToken = Sys.getenv('MAPBOX_ACCESS_TOKEN')))
        
        
        labels <- switch (input$type,
                          Inflow={
                            sprintf(
                              "Of the %s Out of State Firearms Recovered in <strong>%s</strong><br/>%g%% of them originating from <strong>%s</strong><br/>Total Firearms Recovered in <strong>%s</strong> : %s",
                              sum(df$level,na.rm = TRUE),
                              input$thisstate,
                              round(df$pct,2),
                              states$name,
                              input$thisstate,
                              sum(df$value,na.rm = TRUE)
                            )
                          },
                          Outflow={
                            sprintf(
                              'Of the %s Out of State Firearms Originating from <strong>%s</strong><br/>%g%% were Recovered in <strong>%s</strong><br/>Total Firearms Originating from <strong>%s</strong> : %s',
                              sum(df$level,na.rm = TRUE),
                              input$thisstate,
                              round(df$pct,2),
                              states$name,
                              input$thisstate,
                              sum(df$value,na.rm = TRUE)
                            ) 
                          }
        )%>% lapply(htmltools::HTML)
        
        m %>% addPolygons(
          fillColor = ~pal(pct),
          weight = 2,
          smoothFactor = 0.2,
          stroke=FALSE,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 1,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"))%>% 
          addLegend(pal = pal, values = switch(input$scale,National=0:35,State=~pct), opacity = 0.7, title = 'Percent',
                    position = "bottomright",na.label = 'Selected State') 

      })  
      
      output$tbl <- renderDataTable({datin()@data})

      output$inset_plot <- renderPlot({
        
        idx <- which(net_flow$state%in%c(input$thisstate))
        
        net_plot +
          geom_segment(x= idx, 
                       xend=idx,
                       y=ceiling(max(net_flow$ratio_net))+5,
                       yend=pmax(0,net_flow$ratio_net[idx]), 
                       arrow = arrow(length = unit(0.5, "cm")))
      })
      
    })
    
    
    # shiny::observeEvent(c(input$leaf_shape_mouseover),{
    #   hovering <- input$leaf_shape_mouseover
    #   state.names <- as.character(states@data$name)
    #   hover.state <- state.names[which(sapply(states@polygons,function(x) point.in.polygon(hovering$lng,hovering$lat,x@Polygons[[1]]@coords[,1],x@Polygons[[1]]@coords[,2]))==1)]
    #   
      

    atf_plot_base <- eventReactive(input$thisstate,{
      atf_marginal <- atf_data%>%
        filter(year==2017)%>%
        filter(base_rate>1|state==input$thisstate)%>%
        mutate(chosen=state==input$thisstate)
      
      atf_data%>%
        ggplot(aes(x=year,y=base_rate,group=state.abb))+
        geom_line()+
        geom_point(data=atf_marginal)+
        ggrepel::geom_label_repel(aes(label=state.abb,fill=chosen),
                                  data=atf_marginal,
                                  show.legend = FALSE,
                                  segment.alpha = .3,
                                  segment.colour = 'blue')+
        facet_wrap(~Division)+
        scale_x_continuous(breaks=2010:2017,limits = c(2010,2018))+
        theme_minimal()+
        labs(title = 'Rate of Firearm Registration Per 100 individuals (age>17,Base year 2010)',
             subtitle = 'Label indicates states in 2017 with rate of change above 100% (Blue is selected state)',
             caption = 'Source: Bureau of Alcohol, Firearms and Explosives',
             x = 'Year',
             y = 'Rate of Change (Base year 2010)')
    })
    
    atf_plot <- eventReactive(input$thisstate,{
      
      this_atf <- atf_data
      
      if(input$thisstate!='Wyoming')
        this_atf <- this_atf%>%filter(state!='Wyoming')
      
      this_atf$chosen <- this_atf$state==input$thisstate
      
      atf_marginal <- this_atf%>%
        filter(year==2017)%>%
        filter(rate>3|state==input$thisstate)%>%
        mutate(chosen=state==input$thisstate)
      
      this_atf%>% 
        ggplot(aes(x=year,y=rate,group=state.abb))+
        geom_line()+
        ggrepel::geom_label_repel(aes(label=state.abb,fill=chosen),
                                  data=atf_marginal,
                                  show.legend = FALSE,
                                  segment.alpha = .3,
                                  segment.colour = 'blue')+
        facet_wrap(~Division)+
        scale_x_continuous(breaks=2010:2017,limits = c(2010,2018))+
        theme_minimal()+
        labs(title = 'Rate of Firearm Registration Per 100 individuals (age>17)',
             subtitle = 'Label indicates states in 2017 with rate above 3 Firearms per 100 (Blue is selected state)',
             caption = 'Source: Bureau of Alcohol, Firearms and Explosives',
             x = 'Year',
             y = 'Rate per 100 Individuals (age>17)')
    })
    
    power_plot <- eventReactive(c(input$year,input$thisstate),{
      
      this_net_dat <- network_dat[[input$year]]
      
      this_net_dat$alpha_pow$chosen <- as.numeric((this_net_dat$alpha_pow$state==input$thisstate))
      
      this_net_dat$alpha_pow%>%
      ggplot(aes(x=neg,y=pos,label=state,fill=Division))+
      geom_hline(yintercept = 0,linetype=2) + 
      geom_vline(xintercept = 0,linetype=2) + 
      ggrepel::geom_label_repel()+theme_minimal(base_size = plot_size)+
      geom_point(aes(size=chosen),show.legend = FALSE,data=this_net_dat$alpha_pow) +
        scale_y_continuous(limits = c(-4,4)) +
        scale_x_continuous(limits = c(-4,4)) + 
      labs(x='(<== WEAKER | STRONGER ==>)\nLevel of Antagonistic Relations',
           y='Level of Cooperative Relations\n(<== STRONGER | WEAKER ==>)',
           title = "State Power Centrality of Interstate Firearms Directed Graph",
           subtitle=paste(c("Cooperative Relations: If ego has neighbors who have many connections to others,",
                            "making ego more powerful, because it has the 'right' connections.",
                            "\nAntagonistic Relations: If ego has weak neighbors it increases the ego centrality power"),
                          collapse='\n'),
           caption = sprintf("Source: Bureau of Alcohol, Firearms and Explosives (%s)",input$year))
      })
      
    
    network_plot <- eventReactive(c(input$year,input$thisstate),{
      
      this_net_flow <- net_flow%>%filter(year==input$year)
      
      this_net_flow$state <- factor(this_net_flow$state,levels = this_net_flow$state)
      
      idx1 <- which(this_net_flow$state==c(input$thisstate))
      
      this_net_flow$chosen <- ifelse(this_net_flow$state==input$thisstate,'State Selected','State Not Selected')
      
      this_net_plot <- ggplot2::ggplot(this_net_flow,
                      ggplot2::aes(x=state,y=ratio_net,
                                   fill=cut(ratio_net,
                                            breaks = 10,
                                            include.lowest = TRUE)))+
        ggplot2::geom_bar(stat='identity')+
        scale_fill_brewer(palette = "RdYlBu",direction = -1,name=NULL)+
        theme_minimal(base_size = plot_size)+
        labs(title='Net Firearm Flow per 100 Firearms Between States',
             subtitle='High is Net Exporter, Low is Net Importer',
             caption = sprintf("Source: Bureau of Alcohol, Firearms and Explosives (%s)",input$year),
             y='Net Ratio per 100 Firearms',x='State')+
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90),legend.position = 'bottom')
      
      this_net_plot +
        geom_segment(x= idx1, 
                     xend=idx1,
                     y=ceiling(max(this_net_flow$ratio_net))+5,
                     yend=pmax(0,this_net_flow$ratio_net[idx1]), 
                     arrow = arrow(length = unit(0.5, "cm")))
    })
    
    
    scatter_plot <- eventReactive(c(input$year,input$thisstate),{
      this_tot <- tot%>%ungroup%>%filter(year==input$year)
      
      this_tot$chosen=as.numeric((this_tot$state==input$thisstate))  
      
      this_tot%>%
        ggplot(aes(x=from_pct,y=to_pct,fill=cut(within_pct,5,include.lowest = TRUE)))+
        ggrepel::geom_label_repel(aes(label=state_grade),alpha=.7)+
        scale_fill_brewer(palette = "RdYlBu",direction = -1,name='Internal Rate')+
        geom_point(aes(size=chosen),show.legend = FALSE,data=this_tot)+
        theme_minimal(base_size = plot_size)+
        labs(title='Inflow, Outflow and Internal Firearms Rate per 100',
             subtitle='Label Attributes: Higher grades reflect stricter gunlaws, * reflects state with background checks',
             caption = sprintf("Sources: Bureau of Alcohol, Firearms and Explosives (%s)\n Law Center To Prevent Gun Violence",input$year),
             x='Outflow Rate',y='Inflow Rate')
    })
    
    
    
    
      inset_plot <- eventReactive(c(input$year,input$thisstate),{
        
      plotsToSVG=list(
        svglite::xmlSVG({show(atf_plot())},standalone=TRUE,width = 12),
        svglite::xmlSVG({show(atf_plot_base())},standalone=TRUE,width = 12),
        svglite::xmlSVG({show(network_plot())},standalone=TRUE,width = 12),
        svglite::xmlSVG({show(scatter_plot())},standalone=TRUE,width = 12),
        svglite::xmlSVG({show(network_dat[[input$year]]$network_plot)},standalone=TRUE,width = 12),
        svglite::xmlSVG({show(power_plot())},standalone=TRUE,width = 12)
      )
      
      sapply(plotsToSVG,function(sv){paste0("data:image/svg+xml;utf8,",as.character(sv))})
      
    })

      output$slick <- slickR::renderSlickR({
        slickR::slickR(inset_plot(),
                       slideId = 'gg',
                       slickOpts = list(autoplay=TRUE,dots=TRUE,autoplaySpeed=7000))
      })
            
})
