shinyServer(function(input, output) {

    
    datin <- shiny::eventReactive(c(input$thisstate,input$type),{

      gun_mat1 <- switch(input$type,
                         Inflow={
                           gun_mat%>%
                             dplyr::group_by(to)%>%
                             dplyr::mutate(value=ifelse(to==from,NA,value),pct=100*value/sum(value,na.rm = TRUE))%>%
                             dplyr::filter(to==input$thisstate)%>%
                             dplyr::rename(state=from)       
                         },
                         Outflow={
                           gun_mat%>%
                             dplyr::group_by(from)%>%
                             dplyr::mutate(value=ifelse(to==from,NA,value),pct=100*value/sum(value,na.rm = TRUE))%>%
                             dplyr::filter(from==input$thisstate)%>%
                             dplyr::rename(state=to)
                         })
      
      mydata <- states@data   
      mydata <- mydata%>%
        rename(state=name)%>%
        mutate(state=as.character(state))%>%
        left_join(gun_mat1%>%ungroup%>%select(state,value,pct),by='state')
      
      states@data$pct <- mydata$pct
      states@data$level <- mydata$value
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
                              "Of the %s Out of State Firearms Recovered in <strong>%s</strong><br/> %g%% of them originating from <strong>%s</strong>",
                              sum(df$level,na.rm = TRUE),
                              input$thisstate,
                              round(df$pct,2),
                              states$name
                            )
                          },
                          Outflow={
                            sprintf(
                              'Of the %s Out of State Firearms Originating from <strong>%s</strong><br/> %g%% were Recovered in <strong>%s</strong>',
                              sum(df$level,na.rm = TRUE),
                              input$thisstate,
                              round(df$pct,2),
                              states$name
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
    
    
    shiny::observeEvent(c(input$leaf_shape_mouseover),{
      hovering <- input$leaf_shape_mouseover
      state.names <- as.character(states@data$name)
      hover.state <- state.names[which(sapply(states@polygons,function(x) point.in.polygon(hovering$lng,hovering$lat,x@Polygons[[1]]@coords[,1],x@Polygons[[1]]@coords[,2]))==1)]
      
      

      output$inset_plot <- renderPlot({
        
      idx1 <- which(net_flow$state==c(input$thisstate))
      idx2 <- which(net_flow$state==c(hover.state))
        
      p <-   net_plot +
          geom_segment(x= idx1, 
                       xend=idx1,
                       y=ceiling(max(net_flow$ratio_net))+5,
                       yend=pmax(0,net_flow$ratio_net[idx1]), 
                       arrow = arrow(length = unit(0.5, "cm")))
      
      if(length(idx2)==1){
       p <- p + geom_segment(x= idx2, 
                     xend=idx2,
                     y=ceiling(max(net_flow$ratio_net))+5,
                     yend=pmax(0,net_flow$ratio_net[idx2]),linetype=2,
                     arrow = arrow(length = unit(0.5, "cm")))
      }
      p
    })
      
    })
    
})
