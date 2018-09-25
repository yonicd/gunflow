ui <- bootstrapPage(
  h3('Firearms Sourced and Recovered in the United States and Territories 2017'),
  p('Interstate Firearms flow, Choose a state in the drop down menu to the left and the direction of flow'),
  p('Source: ',
    shiny::a(href="https://www.atf.gov/resource-center/firearms-trace-data-2017",'Bureau of Alcohol, Firearms and Explosives'),
    ', Data: ',
    shiny::a(href="https://www.atf.gov/docs/undefined/sourcerecoverybystatecy2017xlsx/download",'Excel Spreadsheet')
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:80%}"),
  leaflet::leafletOutput('leaf',height = '500px'),
  absolutePanel(top = 180, left = 10,
                shiny::selectInput('year','Select Year',choices = 2017:2011,selected = 2017,width = '90%'),
                shiny::selectInput('thisstate','Select state',choices = states$name,selected = thisstate,width = '90%'),
                shiny::radioButtons('type','Direction',c('Inflow','Outflow'),'Inflow',inline = TRUE),
                shiny::radioButtons('scale','Scale Type',c('National','State'),inline=TRUE)
  ),
  slickR::slickROutput('slick',width='100%',height='400px')
)