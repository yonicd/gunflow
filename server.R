shinyServer(function(input, output) {

    
    datin <- shiny::eventReactive(c(input$thisstate,input$type),{

      gun_mat1 <- switch(input$type,
                         Inflow={
                           gun_mat%>%
                             dplyr::group_by(to)%>%
                             dplyr::mutate(value=ifelse(to==from,NA,value),pct=100*value/sum(value,na.rm = TRUE))%>%
                             dplyr::filter(to==input$thisstate)%>%
                             dplyr::mutate(from=tolower(from))%>%
                             dplyr::rename(state=from)       
                         },
                         Outflow={
                           gun_mat%>%
                             dplyr::group_by(from)%>%
                             dplyr::mutate(value=ifelse(to==from,NA,value),pct=100*value/sum(value,na.rm = TRUE))%>%
                             dplyr::filter(from==input$thisstate)%>%
                             dplyr::mutate(to=tolower(to))%>%
                             dplyr::rename(state=to)
                         })
      
      mydata <- states@data   
      mydata <- mydata%>%mutate(state=tolower(name))%>%left_join(gun_mat1%>%ungroup%>%select(state,value,pct),by='state')
      
      states@data$pct <- mydata$pct
      states@data$level <- mydata$value
      states@data$density <- NULL
      
      states
    })
    
    observeEvent(datin(),{
      
      pal <- colorNumeric(
        palette = "RdYlBu",
        domain = datin()$pct,na.color = 'black',reverse = TRUE)
      
      
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
                              'Of the %s Firearms Recovered in <strong>%s</strong><br/> %g%% of them originating from <strong>%s</strong>',
                              sum(df$level,na.rm = TRUE),
                              states$name,
                              round(df$pct,2),
                              input$thisstate
                            )
                          },
                          Outflow={
                            sprintf(
                              "Of the %s Firearms Recovered in <strong>%s</strong><br/> %g%% of them originating from <strong>%s</strong>",
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
          addLegend(pal = pal, values = ~pct, opacity = 0.7, title = NULL,
                    position = "bottomleft",na.label = 'Selected State') 
      })  
      
      output$tbl <- renderDataTable({datin()@data})
    })
    
})
