# Firearms Sourced and Recovered in the United States and Territories 2016

Shinyapp that uses leaflets to visualize direction of firearms flow between states.

Choose a state in the drop down menu to the left and the direction of flow

R users - **please** deploy from R console as to not deplete the account on shinyapps.io. Thank you!

```r

pkgs <- c('reshape2','geojson','readxl','ggplot2',
'leaflet','httr','rgeolocate','shiny','sp','dplyr')

check <- sapply(pkgs,require,warn.conflicts = TRUE,character.only = TRUE)

if(any(!check)){
  pkgs.missing <- pkgs[!check]
  install.packages(pkgs.missing)
  check <- sapply(pkgs.missing,require,warn.conflicts = TRUE,character.only = TRUE)
  }

shiny::runGitHub('yonicd/gunflow')

```

![](https://github.com/yonicd/gunflow/blob/master/gunflow.gif?raw=true)

![](https://github.com/yonicd/gunflow/blob/master/gunflow2.gif?raw=true)