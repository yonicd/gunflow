ui <- bootstrapPage(
  h3('Firearms Sourced and Recovered in the United States and Territories 2016'),
  p('Interstate Firearms flow, Choose a state in the drop down menu to the left and the direction of flow'),
  p('Source: ',
    shiny::a(href="https://www.atf.gov/resource-center/firearms-trace-data-2016",'Bureau of Alcohol, Firearms and Explosives'),
    ', Data: ',
    shiny::a(href="https://www.atf.gov/docs/undefined/sourcerecoverybystatecy2016xlsx/download",'Excel Spreadsheet')
  ),
  tags$style(type = "text/css", "html, body {width:100%;height:80%}"),
  leaflet::leafletOutput('leaf',height = '500px'),
  absolutePanel(top = 180, left = 10,
                shiny::radioButtons('type','',c('Inflow','Outflow'),'Inflow',inline = TRUE),
                shiny::selectInput('thisstate','select state',choices = states$name,selected = thisstate,width = '90%')
  ),
  shiny::plotOutput('inset_plot')
)