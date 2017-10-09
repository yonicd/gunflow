gun_mat <- readxl::read_xlsx('www/sourcerecoverybystatecy2016.xlsx',
                             col_names = TRUE,range = 'B2:BD56')%>%data.frame()
g <- gun_mat[,-1]
row.names(g) <- gun_mat[,1]

mg <- as.matrix(g)
diag(mg) <- NA

library(viridis)
shinyHeatmaply::launch_heatmaply(mg)
