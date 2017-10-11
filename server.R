shinyServer(function(input, output) {

    
    datin <- shiny::eventReactive(c(input$thisstate,input$type),{

      gun_mat1 <- switch(input$type,
                         Inflow={
                           gun_mat%>%
                             dplyr::group_by(to)%>%
                             dplyr::mutate(value1=ifelse(to==from,NA,value),pct=100*value1/sum(value1,na.rm = TRUE))%>%
                             dplyr::filter(to==input$thisstate)%>%
                             dplyr::rename(state=from)       
                         },
                         Outflow={
                           gun_mat%>%
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
                    position = "bottomleft",na.label = 'Selected State') 

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
      

      inset_plot <- reactive({
        
      idx1 <- which(net_flow$state==c(input$thisstate))
      #idx2 <- which(net_flow$state==c(hover.state))
        
      p <-   net_plot +
          geom_segment(x= idx1, 
                       xend=idx1,
                       y=ceiling(max(net_flow$ratio_net))+5,
                       yend=pmax(0,net_flow$ratio_net[idx1]), 
                       arrow = arrow(length = unit(0.5, "cm")))
      
      # if(length(idx2)==1){
      #  p <- p + geom_segment(x= idx2, 
      #                xend=idx2,
      #                y=ceiling(max(net_flow$ratio_net))+5,
      #                yend=pmax(0,net_flow$ratio_net[idx2]),linetype=2,
      #                arrow = arrow(length = unit(0.5, "cm")))
      # }
      
      tot$chosen=as.numeric((tot$state==input$thisstate))
      net_flow$chosen <- ifelse(net_flow$state==thisstate,'State Selected','State Not Selected')
      
        
      plotsToSVG=list(
        svglite::xmlSVG({show(p)},standalone=TRUE,width = 12),
        svglite::xmlSVG({show(network_plot)},standalone=TRUE,width = 12),
        svglite::xmlSVG({
          show(scatter_plot+geom_point(aes(size=chosen),show.legend = FALSE,data=tot))
          },standalone=TRUE,width = 12)
      )
      
      sapply(plotsToSVG,function(sv){paste0("data:image/svg+xml;utf8,",as.character(sv))})
      
    })

      output$slick <- slickR::renderSlickR({
        slickR::slickR(inset_plot(),
                       slideId = 'gg',
                       slickOpts = list(autoplay=TRUE,dots=TRUE,autoplaySpeed=7000))
      })
            
    # })
    
})
