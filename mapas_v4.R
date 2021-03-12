library(sf)
library(ggplot2)
library(tmap)
library(tmaptools)
library(leaflet)
library(dplyr)
library(odbc)

con <- dbConnect(odbc(), 
                 Driver = "SQL Server", 
                 Server = "BODCLU17", 
                 Database = "VP_INT_NEG", 
                 Trusted_Connection = "True")

data_est <- dbGetQuery(con,"SELECT * FROM [VP_INT_NEG].[dbo].[MAPAS_GLOBAL_EST]")
data_mun2 <- dbGetQuery(con,"SELECT * FROM [VP_INT_NEG].[dbo].[MAPAS_GLOBAL_MUN]")

data_est <- dbGetQuery(con,"SELECT * FROM [VP_INT_NEG].[dbo].[MAPAS_GLOBAL_EST_piv]")
data_mun <- dbGetQuery(con,"SELECT * FROM [VP_INT_NEG].[dbo].[MAPAS_GLOBAL_MUN_piv2]")

mapa_est <- st_read("C:\\Users\\jnelo\\Documents\\Estados_Venezuela.shp")
mapa_mun <- st_read("C:\\Users\\jnelo\\Documents\\Municipios_Venezuela.shp")


var_temp_sal <- filter(variable_list, FECHA == as.date8('28-02-2021'), VARIABLE == 'SAL_PROM_USD')
var_temp_clien <- filter(variable_list, FECHA == '28-02-2021', VARIABLE == 'CANT_CLIEN')
var_temp_ent <- filter(variable_list, FECHA == '28-02-2021', VARIABLE == ' MONTO_CRED_USD_AVG')

variable_list$FECHA <- as.Date(variable_list$FECHA, )
  

mapa_mun$MUNICIPIO <- gsub("á","a", mapa_mun$MUNICIPIO)
mapa_mun$MUNICIPIO <- gsub("é","e", mapa_mun$MUNICIPIO)
mapa_mun$MUNICIPIO <- gsub("í","i", mapa_mun$MUNICIPIO)
mapa_mun$MUNICIPIO <- gsub("ó","o", mapa_mun$MUNICIPIO)
mapa_mun$MUNICIPIO <- gsub("ú","u", mapa_mun$MUNICIPIO)
mapa_mun$MUNICIPIO <- gsub("ñ","n", mapa_mun$MUNICIPIO)

mapa_mun$ESTADO <- gsub("á","a", mapa_mun$ESTADO)
mapa_mun$ESTADO <- gsub("é","e", mapa_mun$ESTADO)
mapa_mun$ESTADO <- gsub("í","i", mapa_mun$ESTADO)
mapa_mun$ESTADO <- gsub("ó","o", mapa_mun$ESTADO)
mapa_mun$ESTADO <- gsub("ú","u", mapa_mun$ESTADO)
mapa_mun$ESTADO <- gsub("ñ","n", mapa_mun$ESTADO)

mapa_est$ESTADO <- gsub("á","a", mapa_est$ESTADO)
mapa_est$ESTADO <- gsub("é","e", mapa_est$ESTADO)
mapa_est$ESTADO <- gsub("í","i", mapa_est$ESTADO)
mapa_est$ESTADO <- gsub("ó","o", mapa_est$ESTADO)
mapa_est$ESTADO <- gsub("ú","u", mapa_est$ESTADO)
mapa_est$ESTADO <- gsub("ñ","n", mapa_est$ESTADO)

mapa_mun$indice = paste(mapa_mun$ESTADO," - ",mapa_mun$MUNICIPIO)
mapa_mun$indice2 = paste(mapa_mun$FECHA,"-",mapa_mun$ESTADO,"-",mapa_mun$MUNICIPIO)


mapa_data_est <- inner_join(mapa_est, data_est, by = "ESTADO")

mapa_data_mun <- inner_join(mapa_mun, data_mun, by = "indice")

lat_data_mun <- inner_join(lat_lon, data_mun, by = "indice")

mapa_data_mun2 <- inner_join(mapa_mun, data_mun2, by = "indice")
lat_data_mun2 <- inner_join(lat_lon, data_mun2, by = "indice")

sum.formula  = JS("function (cluster) {
    var r0 = 255;
    var g0 = 253;
    var b0 = 0;
    
    var r1 = 255;
    var g1 = 0;
    var b1 = 0;
    
    var dr = ( r1 - r0 ) / 77963
    var dg = ( g1 - g0 ) / 77963
    var db = ( b1 - b0 ) / 77963
    
    var fr = (s) => dr*s + r0
    var fg = (s) => dg*s + g0
    var fb = (s) => db*s + b0
    
    var markers = cluster.getAllChildMarkers();
    var sum = 0; 
    for (i = 0; i < markers.length; i++) {
      sum += Number(markers[i].options.mag);
    }
    
    var ra = fr(sum);
    var ga = fg(sum);
    var ba = fb(sum);

    function logn(val) {
      return Math.log(val) / Math.log(2);
    }
    
    var size = logn(sum)*1.75+2;
    var formatted_number = Math.round(sum).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, ' ');

    return new L.DivIcon({ className: 'custom-cluster-icon', html: '<div style=\"line-height:'+size+'px; white-space: nowrap;background-color: rgb('+ra+','+ga+','+ba+', 0.6);border-radius:50%\">'+formatted_number+'</div>'   , iconSize: L.point(size, size) });
  }")



options(OutDec=",")
options(scipen=999)

pal_pag_exp <- colorQuantile(palette = "RdYlBu", n = 5,
                             domain = unique(mapa_data_mun$NETO_Pago_Express_20210228))

pal_sal_usd <- colorQuantile(palette = "YlGn", n = 5,
                             domain = unique(mapa_data_mun$SAL_PROM_USD_20210228))

pal_cant_clien <- colorQuantile(palette = "Blues", n = 5,
                                domain = unique(mapa_data_mun$CANT_CLIEN_20210228))

pal_ent_prom <- colorQuantile(palette = "Oranges", n = 5,
                              domain = unique(mapa_data_mun$MONTO_CRED_USD_AVG_20210228))

pal_cant_clien2 <- colorNumeric(palette = "Blues", n = 5,
                                domain = unique(lat_data_mun$CANT_CLIEN_20210228))

pal_fluj_net <- colorQuantile(palette = "RdYlGn", n = 5,
                              domain = unique(mapa_data_mun$NETO_TOTAL_20210228))

pal_com_pos <- colorQuantile(palette = "Reds", n = 5,
                             domain = unique(mapa_data_mun$NETO_Compra_POS_Total_20210228))

pal_ban_ext <- colorQuantile(palette = "PiYG", n = 5,
                             domain = unique(mapa_data_mun$NETO_Bcos_Externos_20210228))

pal_op_usd <- colorQuantile(palette = "Spectral", n = 5,
                            domain = unique(mapa_data_mun$NETO_Divisas_efectivo_20210228))

pal_liq_pos <- colorQuantile(palette = "BuGn", n = 5,
                            domain = unique(mapa_data_mun$NETO_Liquidación_POS_20210228))



mapa_leaflet <- leaflet(mapa_data_mun) %>%
  
  addProviderTiles("Esri", group = "Esri") %>%
  addProviderTiles("CartoDB", group = "Carto") %>%
  
  addPolygons(data = mapa_data_mun, 
              weight = 1, 
              fillOpacity = .75,
              color = pal_sal_usd(mapa_data_mun$SAL_PROM_USD_20210228),
              label = paste0(mapa_data_mun$indice,": ", format(mapa_data_mun$SAL_PROM_USD_20210228, nsmall=2, big.mark=".")),
              popup = paste0("<b>",mapa_data_mun$indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Saldo promedio USD: ", format(mapa_data_mun$SAL_PROM_USD_20210228, nsmall=2, big.mark="."),"</b>","</p>",
                             "Cantidad clientes: ", format(mapa_data_mun$CANT_CLIEN_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Entradas promedio al mes USD: ", format(mapa_data_mun$MONTO_CRED_USD_AVG_20210228, nsmall=2, big.mark=".")),
              group = "Saldo promedio USD",
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
  addPolygons(data = mapa_data_mun, 
              weight = 1, 
              fillOpacity = .75,
              color = pal_cant_clien(mapa_data_mun$CANT_CLIEN_20210228),
              label = paste0(mapa_data_mun$indice,": ", format(mapa_data_mun$CANT_CLIEN_20210228, nsmall=2, big.mark=".")),
              popup = paste0("<b>",mapa_data_mun$indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Cantidad clientes: ", format(mapa_data_mun$CANT_CLIEN_20210228, nsmall=2, big.mark="."),"</b>","</p>",
                             "Saldo promedio USD: ", format(mapa_data_mun$SAL_PROM_USD_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Entradas promedio al mes USD: ", format(mapa_data_mun$MONTO_CRED_USD_AVG_20210228, nsmall=2, big.mark=".")),
              group = "Cantidad clientes",
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
  addPolygons(data = mapa_data_mun, 
              weight = 1, 
              fillOpacity = .75,
              color = pal_ent_prom(mapa_data_mun$MONTO_CRED_USD_AVG_20210228),
              label = paste0(mapa_data_mun$indice,": ", format(mapa_data_mun$MONTO_CRED_USD_AVG_20210228, nsmall=2, big.mark=".")),
              popup = paste0("<b>",mapa_data_mun$indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Entradas promedio al mes USD: ", format(mapa_data_mun$MONTO_CRED_USD_AVG_20210228, nsmall=2, big.mark="."),"</b>","</p>",
                             "Saldo promedio USD: ", format(mapa_data_mun$SAL_PROM_USD_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(mapa_data_mun$CANT_CLIEN_20210228, nsmall=2, big.mark=".")),
              group = "Entradas promedio al mes USD",
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
  addPolygons(data = mapa_data_mun, 
              weight = 1, 
              fillOpacity = .75,
              color = pal_fluj_net(mapa_data_mun$NETO_TOTAL_20210228),
              label = paste0(mapa_data_mun$indice,": ", format(round(mapa_data_mun$NETO_TOTAL_20210228/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = paste0("<b>",mapa_data_mun$indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo de Fondos Neto en MM Bs.: ", format(round(mapa_data_mun$NETO_TOTAL_20210228/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(mapa_data_mun$MONTO_CRED_USD_AVG_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(mapa_data_mun$SAL_PROM_USD_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(mapa_data_mun$CANT_CLIEN_20210228, nsmall=2, big.mark=".")),
              group = "Flujo Neto",
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
  addPolygons(data = mapa_data_mun, 
              weight = 1, 
              fillOpacity = .75,
              color = pal_com_pos(mapa_data_mun$NETO_Compra_POS_Total_20210228),
              label = paste0(mapa_data_mun$indice,": ", format(round(mapa_data_mun$NETO_Compra_POS_Total_20210228/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = paste0("<b>",mapa_data_mun$indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Compra de POS total en MM Bs.: ", format(round(mapa_data_mun$NETO_Compra_POS_Total_20210228/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(mapa_data_mun$MONTO_CRED_USD_AVG_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(mapa_data_mun$SAL_PROM_USD_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(mapa_data_mun$CANT_CLIEN_20210228, nsmall=2, big.mark=".")),
              group = "Compra POS",
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
  addPolygons(data = mapa_data_mun, 
              weight = 1, 
              fillOpacity = .75,
              color = pal_ban_ext(mapa_data_mun$NETO_Bcos_Externos_20210228),
              label = paste0(mapa_data_mun$indice,": ", format(round(mapa_data_mun$NETO_Bcos_Externos_20210228/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = paste0("<b>",mapa_data_mun$indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo Neto de Bancos Externos en MM Bs.: ", format(round(mapa_data_mun$NETO_Bcos_Externos_20210228/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(mapa_data_mun$MONTO_CRED_USD_AVG_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(mapa_data_mun$SAL_PROM_USD_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(mapa_data_mun$CANT_CLIEN_20210228, nsmall=2, big.mark=".")),
              group = "Bancos Externos",
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
  addPolygons(data = mapa_data_mun, 
              weight = 1, 
              fillOpacity = .75,
              color = pal_op_usd(mapa_data_mun$NETO_Divisas_efectivo_20210228),
              label = paste0(mapa_data_mun$indice,": ", format(round(mapa_data_mun$NETO_Divisas_efectivo_20210228/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = paste0("<b>",mapa_data_mun$indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo Neto de Operaciones en ME en MM Bs.: ", format(round(mapa_data_mun$NETO_Divisas_efectivo_20210228/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(mapa_data_mun$MONTO_CRED_USD_AVG_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(mapa_data_mun$SAL_PROM_USD_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(mapa_data_mun$CANT_CLIEN_20210228, nsmall=2, big.mark=".")),
              group = "Operaciones Moneda Extranjera",
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
  addPolygons(data = mapa_data_mun, 
              weight = 1, 
              fillOpacity = .75,
              color = pal_liq_pos(mapa_data_mun$NETO_Liquidación_POS_20210228),
              label = paste0(mapa_data_mun$indice,": ", format(round(mapa_data_mun$NETO_Liquidación_POS_20210228/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = paste0("<b>",mapa_data_mun$indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Liquidación POS en MM Bs.: ", format(round(mapa_data_mun$NETO_Liquidación_POS_20210228/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(mapa_data_mun$MONTO_CRED_USD_AVG_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(mapa_data_mun$SAL_PROM_USD_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(mapa_data_mun$CANT_CLIEN_20210228, nsmall=2, big.mark=".")),
              group = "Liquidación POS",
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
  addPolygons(data = mapa_data_mun, 
              weight = 1, 
              fillOpacity = .75,
              color = pal_pag_exp(mapa_data_mun$NETO_Pago_Express_20210228),
              label = paste0(mapa_data_mun$indice,": ", format(round(mapa_data_mun$NETO_Pago_Express_20210228/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = paste0("<b>",mapa_data_mun$indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo Neto Pago Express en MM Bs.: ", format(round(mapa_data_mun$NETO_Pago_Express_20210228/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(mapa_data_mun$MONTO_CRED_USD_AVG_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(mapa_data_mun$SAL_PROM_USD_20210228, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(mapa_data_mun$CANT_CLIEN_20210228, nsmall=2, big.mark=".")),
              group = "Pago Express",
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
  addLabelOnlyMarkers(data = lat_data_mun,
                      options = markerOptions(mag = lat_data_mun$CANT_CLIEN_20210228),
                      label =  scales::number(lat_data_mun$CANT_CLIEN_20210228),
                      labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T),
                      clusterOptions = markerClusterOptions(iconCreateFunction=JS(sum.formula)),
                      group = "Circulos - Cantidad de Clientes") %>% 
  addLayersControl(baseGroups = c("Carto", "Esri"),
                   overlayGroups = c("Saldo promedio USD", "Cantidad clientes", "Entradas promedio al mes USD", "Flujo Neto", "Compra POS", "Bancos Externos", "Operaciones Moneda Extranjera", "Liquidación POS", "Pago Express", "Circulos - Cantidad de Clientes"),
                   options = layersControlOptions(collapsed = FALSE,autoZIndex = TRUE)) %>%
  setMaxBounds(lng1 = -78.624807, lat1 = 15.638759,
               lng2 = -51.78926, lat2 = -1.97570) %>%
  hideGroup(c("Cantidad clientes", "Entradas promedio al mes USD", "Flujo Neto", "Compra POS", "Bancos Externos", "Operaciones Moneda Extranjera", "Liquidación POS", "Pago Express"))
