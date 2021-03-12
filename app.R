library(leaflet)
library(stringr)
library(ggplot2)
library(waiter)
library(rlist)
library(lubridate)
library(shiny)
library(sf)
library(tibble)
library(plotly)
library(dplyr)
load("./data_mapas_v5.RData")
source("./mapa_animacion.R")

timeline <-
  seq(as.Date("2020-04-01"), length = 12, by = "1 month") - 1

last_updated_date <- timeline[1]
last_divn <- "SAL_PROM_USD"
last_div <- "CANT_CLIEN"

all_data <- tibble(
  Date = timeline,
  MapaMun = mapply(function(fecha) mapa_data_mun2[mapa_data_mun2$FECHA == fecha, ] %>% st_as_sf(), timeline, SIMPLIFY = FALSE),
  LatMun = mapply(function(fecha) lat_data_mun2[lat_data_mun2$FECHA == fecha, ], timeline, SIMPLIFY = FALSE)
)

columnInformation <- 
  data.frame(
    DisplayName = c("Saldo promedio USD", "Cantidad clientes", "Entradas promedio al mes USD", "Flujo Neto", "Compra POS", "Bancos Externos", "Operaciones Moneda Extranjera", "Liquidación POS", "Pago Express"),
    InternalName = c("SAL_PROM_USD", "CANT_CLIEN", "MONTO_CRED_USD_AVG", "NETO_TOTAL", "NETO_Compra_POS_Total" ,"NETO_Bcos_Externos", "NETO_Divisas_efectivo", "NETO_Liquidación_POS", "NETO_Pago_Express"),  
    PaletteName = c("YlGn", "Blues", "Oranges", "RdYlGn", "Reds", "PiYG", "Spectral", "BuGn", "RdYlBu") 

  )

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


ui <- navbarPage("Dashboard Mapas",
  tabPanel("Inicio",
  use_waiter(),
  waiter_show_on_load(
    html = spin_dots(),
    color = "rgb(76, 175, 80)"
  ),
  tags$head(
    tags$style(
      HTML(
        "
          body, html {
            background: rgb(155, 155, 155);
            height: 100%;
            padding: 10px;
          }
          #mymap {
            padding-top: 15px;
            padding-bottom: 15px;
          }
          #by_year_comparative {
            padding-top: 15px;
            padding-bottom: 15px;
          }
          #selectedMun {
            width: 100%;
            height: 435px;
            border: 0;
          }
          "
       )
    ),
    tags$script(leaflet_js)
  ),
  sidebarLayout(
    sidebarPanel(
      sliderInput("fecha",
                  "Fecha: ",
                  min = as.Date("31-03-2020", "%d-%m-%Y"),
                  max = as.Date("28-02-2021", "%d-%m-%Y"),
                  value = as.Date("31-03-2020", "%d-%m-%Y"),
                  timeFormat = "%b %Y",
		              animate = TRUE,
		              step = 3,
                  ticks = TRUE),
      selectInput("control_fecha",
                  "Seleccionar Fecha de Corte",
                  paste(
                    toupper(
                      substr(
                        format(timeline, "%d %b %Y"), 1, 1)
                    ),
                    substring(
                        format(timeline, "%d %b %Y"), 2
                    ),
                    sep = ""
                  )
      ),
      selectInput("p_ind",
                  "Seleccionar índice dividendo", 
                  columnInformation$DisplayName),
      selectInput("s_ind",
                  "Seleccionar índice divisor",
                  columnInformation$DisplayName,
                  selected = "Cantidad clientes"),
      uiOutput("selectedMun")
    ),
    mainPanel(
      fluidRow(
        leafletOutput("mymap", height = "404")
      ),
      fluidRow(
        plotlyOutput("by_year_comparative", height = "403")
      ),
    )
  )
  )
)

options(OutDec = ",")
options(scipen = 999)
options(shiny.fullstacktrace = TRUE, shiny.port = 5050L)

pal_pag_exp <- colorQuantile(palette = "RdYlBu", n = 5,
                             domain = unique(mapa_data_mun2$NETO_Pago_Express))

pal_sal_usd <- colorQuantile(palette = "YlGn", n = 5,
                             domain = unique(mapa_data_mun2$SAL_PROM_USD))

pal_cant_clien <- colorQuantile(palette = "Blues", n = 5,
                                domain = unique(mapa_data_mun2$CANT_CLIEN))

pal_ent_prom <- colorQuantile(palette = "Oranges", n = 5,
                              domain = unique(mapa_data_mun2$MONTO_CRED_USD_AVG))

pal_cant_clien2 <- colorNumeric(palette = "Blues", n = 5,
                                domain = unique(lat_data_mun2$CANT_CLIEN))

pal_fluj_net <- colorQuantile(palette = "RdYlGn", n = 5,
                              domain = unique(mapa_data_mun2$NETO_TOTAL))

pal_com_pos <- colorQuantile(palette = "Reds", n = 5,
                             domain = unique(mapa_data_mun2$NETO_Compra_POS_Total))

pal_ban_ext <- colorQuantile(palette = "PiYG", n = 5,
                             domain = unique(mapa_data_mun2$NETO_Bcos_Externos))

pal_op_usd <- colorQuantile(palette = "Spectral", n = 5,
                            domain = unique(mapa_data_mun2$NETO_Divisas_efectivo))

pal_liq_pos <- colorQuantile(palette = "BuGn", n = 5,
                            domain = unique(mapa_data_mun2$NETO_Liquidación_POS))
     
server <- function(input, output, session) {
  observeEvent(input$control_fecha, {
    input_date <- as.Date(input$control_fecha, "%d %b %Y")
    updateSliderInput(
      session,
      "fecha",
      value = input_date,
      timeFormat = "%b %Y"
    )
  })
  year_data <- eventReactive(input$fecha, {
    input_date <- as.Date(input$fecha, "%d-%m-%Y")
    date <- timeline[month(timeline) == month(input_date)]
    return_data_mun <- filter(all_data, Date == date) %>%
                       select(MapaMun) %>%
                       first()
    return_lat_mun <- filter(all_data, Date == date) %>%
                      select(LatMun) %>%
                      first()
    list(
      pol_data = return_data_mun[[1]],
      lat_data = return_lat_mun[[1]]
    )
  })
  quotient_data <- eventReactive(c(input$s_ind, input$p_ind), {
    p_interno <-
      columnInformation[columnInformation$DisplayName == input$p_ind, "InternalName"]
    s_interno <-
      columnInformation[columnInformation$DisplayName == input$s_ind, "InternalName"]
    list(
          ColP = p_interno,
          ColS = s_interno,
          ColPLabel = input$p_ind,
          ColSLabel = input$s_ind
        )
  })
  observe({
    if (!is.null(input$mymap_shape_click)) {
     point <- c(input$mymap_shape_click$lng, input$mymap_shape_click$lat)
     point <- st_sfc(st_point(point), crs = st_crs(mapa_data_mun2))
     d_name <- input$mymap_shape_click$group
     state_clicked <-
       filter(mapa_data_mun2,
              st_intersects(
                point,
                geometry, sparse = FALSE))
     internal_name <-
       columnInformation[columnInformation$DisplayName == d_name, "InternalName"]
     palette_name <-
       columnInformation[columnInformation$DisplayName == d_name, "PaletteName"]
     if (!is.na(str_extract(d_name, " por "))) {
       split_data <- str_split(d_name, " por ")[[1]]
       p_col <-
         columnInformation[columnInformation$DisplayName == split_data[1], "InternalName"]
       s_col <-
         columnInformation[columnInformation$DisplayName == split_data[2], "InternalName"]
       output$by_year_comparative <- renderPlotly({
         plot <-
         ggplot(data = state_clicked) +
         geom_bar(
           mapping = aes(
             x = .data[["FECHA"]],
             y = .data[[p_col]] / .data[[s_col]],
             fill = .data[[p_col]] / .data[[s_col]]),
           stat = "identity") +
         labs(x = "Fecha de cierre", y = d_name, fill = str_trunc(d_name, 20)) +
         scale_fill_distiller(
           palette = "YlOrRd",
           direction = 1
         )

         ggplotly(plot)
       })
     } else {
       output$by_year_comparative <- renderPlotly({
         plot <-
         ggplot(data = state_clicked) +
         geom_bar(
           mapping = aes(
             x = .data[["FECHA"]],
             y = .data[[internal_name]],
             fill = .data[[internal_name]]),
           stat = "identity") +
         labs(x = "Fecha de cierre", y = d_name, fill = str_trunc(d_name, 20)) +
         scale_fill_distiller(
           palette = palette_name,
           direction = 1
         )

         ggplotly(plot)
       })
     }
     output$selectedMun <- renderUI({
        lng <- input$mymap_shape_click$lng[1] %>% toString() %>% str_replace(",", ".")
        lat <- input$mymap_shape_click$lat[1] %>% toString() %>% str_replace(",", ".")
        mun <- unique(state_clicked$MUNICIPIO)
        estado <- unique(state_clicked$ESTADO)
        url <- str_interp("https://www.google.com/maps/embed/v1/search?q=BOD+in+${mun},${estado}&center=${lat},${lng}&zoom=7&key=AIzaSyBeEp6L_J2GCjBEbKF9wg7cnr1a6LNXaj0")
        tags$iframe(
          src = url,
          style = "width: 100%; height: 415px; border: none;"
        )
      })
    }
  })

  output$mymap <- renderLeaflet({
    data <- all_data$MapaMun[1][[1]]
    lat_data <- all_data$LatMun[1][[1]]
    p_interno <- columnInformation$InternalName[1]
    s_interno <- columnInformation$InternalName[2]
    p_ind <- columnInformation$DisplayName[1]
    s_ind <- columnInformation$DisplayName[2]
    dynamic_pal <- colorNumeric("YlOrRd",
                        unique((data[, p_interno] / data[, s_interno])[[1]]),
                        n = 5)
    on.exit({
      waiter_hide()
    })
    leaflet(data, options = leafletOptions(preferCanvas = TRUE)) %>%
    addProviderTiles("Esri", group = "Esri") %>%
    addProviderTiles("CartoDB", group = "Carto") %>%
    addPolygons(weight = 1, 
              fillOpacity = .75,
              color = ~pal_sal_usd(SAL_PROM_USD),
              label = ~paste0(indice,": ", format(SAL_PROM_USD, nsmall=2, big.mark=".")),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"</b>","</p>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark="."),"<br/>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark=".")),
              group = "Saldo promedio USD",
	      layerId = ~paste0("SAL_", indice),
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
    addPolygons(weight = 1, 
              fillOpacity = .75,
              color = ~pal_cant_clien(CANT_CLIEN),
              label = ~paste0(indice,": ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark="."),"</b>","</p>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark=".")),
              group = "Cantidad clientes",
	      layerId = ~paste0("CANT_", indice),
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
    addPolygons(weight = 1, 
              fillOpacity = .75,
              color = ~pal_ent_prom(MONTO_CRED_USD_AVG), label = ~paste0(indice,": ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark=".")),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"</b>","</p>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
              group = "Entradas promedio al mes USD",
	      layerId = ~paste0("MONTO_", indice),
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
    addPolygons(weight = 1, 
              fillOpacity = .75,
              color = ~pal_fluj_net(NETO_TOTAL),
              label = ~paste0(indice,": ", format(round(NETO_TOTAL/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo de Fondos Neto en MM Bs.: ", format(round(NETO_TOTAL/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
              group = "Flujo Neto",
	      layerId = ~paste0("NETO_TOTAL_", indice),
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
    addPolygons(weight = 1, 
              fillOpacity = .75,
              color = ~pal_com_pos(NETO_Compra_POS_Total),
              label = ~paste0(indice,": ", format(round(NETO_Compra_POS_Total/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Compra de POS total en MM Bs.: ", format(round(NETO_Compra_POS_Total/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
              group = "Compra POS",
	      layerId = ~paste0("NETO_Compra_", indice),
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
    addPolygons(weight = 1, 
              fillOpacity = .75,
              color = ~pal_ban_ext(NETO_Bcos_Externos),
              label = ~paste0(indice,": ", format(round(NETO_Bcos_Externos/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo Neto de Bancos Externos en MM Bs.: ", format(round(NETO_Bcos_Externos/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
              group = "Bancos Externos",
	      layerId = ~paste0("NETO_Bcos_", indice),
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
    addPolygons(weight = 1, 
              fillOpacity = .75,
              color = ~pal_op_usd(NETO_Divisas_efectivo),
              label = ~paste0(indice,": ", format(round(NETO_Divisas_efectivo/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo Neto de Operaciones en ME en MM Bs.: ", format(round(NETO_Divisas_efectivo/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
              group = "Operaciones Moneda Extranjera",
	      layerId = ~paste0("NETO_Divisas_", indice),
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
    addPolygons(weight = 1, 
              fillOpacity = .75,
              color = ~pal_liq_pos(NETO_Liquidación_POS),
              label = ~paste0(indice,": ", format(round(NETO_Liquidación_POS/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Liquidación POS en MM Bs.: ", format(round(NETO_Liquidación_POS/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
              group = "Liquidación POS",
	      layerId = ~paste0("NETO_Liquidación_", indice),
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
    addPolygons(weight = 1, 
              fillOpacity = .75,
              color = ~pal_pag_exp(NETO_Pago_Express),
              label = ~paste0(indice,": ", format(round(NETO_Pago_Express/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo Neto Pago Express en MM Bs.: ", format(round(NETO_Pago_Express/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
              group = "Pago Express",
	      layerId = ~paste0("NETO_Pago_", indice),
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
    addPolygons(weight = 1, 
              fillOpacity = .75,
              color = ~dynamic_pal(data[[p_interno]] / data[[s_interno]]),
              label = ~paste0(indice,": ", data[[p_interno]] / data[[s_interno]]),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                              "<b>","<p style=color:blue>", p_ind, " por ", s_ind, ": ",
      				data[[p_interno]] / data[[s_interno]],"</b>","</p>",
                             "Saldo promedio USD: ", SAL_PROM_USD,"<br/>",
                             "Cantidad clientes: ", CANT_CLIEN),
              group = paste(p_ind, " por ", s_ind, sep = ""),
	      layerId = ~paste0("Dynamic_", indice),
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
    addLabelOnlyMarkers(data = lat_data,
                       options = ~markerOptions(mag = CANT_CLIEN),
                       label =  ~scales::number(CANT_CLIEN),
	      	       layerId = ~paste0("Circles_", indice),
		       clusterId = "circle_cluster",
                       labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T),
                       clusterOptions = markerClusterOptions(iconCreateFunction=JS(sum.formula)),
                       group = "Circulos - Cantidad de clientes") %>%
    addLayersControl(baseGroups = c("Carto", "Esri"),
                     overlayGroups = c(columnInformation$DisplayName,
                                       "Circulos - Cantidad de clientes",
                                        paste(p_ind," por ", s_ind, sep = "")),
                     options = layersControlOptions(collapsed = FALSE,autoZIndex = TRUE)) %>%
    setMaxBounds(lng1 = -78.624807, lat1 = 15.638759,
                 lng2 = -51.78926, lat2 = -1.97570) %>%
    hideGroup(columnInformation
	   	[! columnInformation$DisplayName %in% c("Saldo promedio USD"), "DisplayName"] )
 })
 observe({
   q_data <- quotient_data()
   w_data <- year_data()
   data <- w_data$pol_data
   lat_data <- w_data$lat_data
   p_interno <- q_data$ColP
   s_interno <- q_data$ColS
   p_ind <- q_data$ColPLabel
   s_ind <- q_data$ColSLabel
   if (month(last_updated_date) == month(input$fecha) && last_divn == p_interno && last_div == s_interno) {
	    print("exiting")
   }
   else {
	    last_updated_date <<- input$fecha
      quotient <- data[[p_interno]] / data[[s_interno]]
      quotient[quotient == Inf] <- 0
      dynamic_pal <- colorNumeric("YlOrRd",
                       unique(
                         quotient
                       ),
                       n = 5)
   	  proxy <- leafletProxy("mymap", data = data)
   	  proxy %>%
    	setShapeStyle(fillColor = ~pal_sal_usd(SAL_PROM_USD),
              label = ~paste0(indice,": ", format(SAL_PROM_USD, nsmall=2, big.mark=".")),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"</b>","</p>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark="."),"<br/>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark=".")),
	      layerId = ~paste0("SAL_", indice)) %>%
    	setShapeStyle(fillColor = ~pal_cant_clien(CANT_CLIEN),
              label = ~paste0(indice,": ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark="."),"</b>","</p>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark=".")),
	      layerId = ~paste0("CANT_", indice)) %>%
    	setShapeStyle(fillColor = ~pal_ent_prom(MONTO_CRED_USD_AVG),
	      label = ~paste0(indice,": ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark=".")),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"</b>","</p>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
	      layerId = ~paste0("MONTO_", indice)) %>%
    	setShapeStyle(fillColor = ~pal_fluj_net(NETO_TOTAL),
              label = ~paste0(indice,": ", format(round(NETO_TOTAL/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo de Fondos Neto en MM Bs.: ", format(round(NETO_TOTAL/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
	      layerId = ~paste0("NETO_TOTAL_", indice)) %>%
    	setShapeStyle(fillColor = ~pal_com_pos(NETO_Compra_POS_Total),
              label = ~paste0(indice,": ", format(round(NETO_Compra_POS_Total/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Compra de POS total en MM Bs.: ", format(round(NETO_Compra_POS_Total/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
	      layerId = ~paste0("NETO_Compra_", indice)) %>%
    	setShapeStyle(fillColor = ~pal_ban_ext(NETO_Bcos_Externos),
              label = ~paste0(indice,": ", format(round(NETO_Bcos_Externos/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo Neto de Bancos Externos en MM Bs.: ", format(round(NETO_Bcos_Externos/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
	      layerId = ~paste0("NETO_Bcos_", indice)) %>%
    	setShapeStyle(fillColor = ~pal_op_usd(NETO_Divisas_efectivo),
              label = ~paste0(indice,": ", format(round(NETO_Divisas_efectivo/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo Neto de Operaciones en ME en MM Bs.: ", format(round(NETO_Divisas_efectivo/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
	      layerId = ~paste0("NETO_Divisas_", indice)) %>%
    	setShapeStyle(fillColor = ~pal_liq_pos(NETO_Liquidación_POS),
              label = ~paste0(indice,": ", format(round(NETO_Liquidación_POS/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Liquidación POS en MM Bs.: ", format(round(NETO_Liquidación_POS/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
	      layerId = ~paste0("NETO_Liquidación_", indice)) %>%
    	setShapeStyle(fillColor = ~pal_pag_exp(NETO_Pago_Express),
              label = ~paste0(indice,": ", format(round(NETO_Pago_Express/1000000,digits=2), nsmall=2, big.mark="."), " MM"),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Flujo Neto Pago Express en MM Bs.: ", format(round(NETO_Pago_Express/1000000,digits=2), nsmall=2, big.mark="."),"</b>","</p>",
                             "Entradas promedio al mes USD: ", format(MONTO_CRED_USD_AVG, nsmall=2, big.mark="."),"<br/>",
                             "Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"<br/>",
                             "Cantidad clientes: ", format(CANT_CLIEN, nsmall=2, big.mark=".")),
	      layerId = ~paste0("NETO_Pago_", indice)) %>%
	    removeMarker(layerId = ~paste0("Dynamic_", indice)) %>%
    	addPolygons(weight = 1, 
              fillOpacity = .75,
              color = ~dynamic_pal(quotient),
              label = ~paste0(indice,": ", data[[p_interno]] / data[[s_interno]]),
              popup = ~paste0("<b>",indice,"</b>","<br/>",
                              "<b>","<p style=color:blue>", p_ind, " por ", s_ind, ": ",
      				data[[p_interno]] / data[[s_interno]],"</b>","</p>",
                             "Saldo promedio USD: ", SAL_PROM_USD,"<br/>",
                             "Cantidad clientes: ", CANT_CLIEN),
              group = paste(p_ind, " por ", s_ind, sep = ""),
	      layerId = ~paste0("Dynamic_", indice),
              highlight = highlightOptions(weight = 3, color = "blue",
                                           bringToFront = TRUE)) %>%
    	removeMarker(layerId = ~paste0("Circles_", indice)) %>%
    	addLabelOnlyMarkers(data = lat_data,
                       options = ~markerOptions(mag = CANT_CLIEN),
                       label =  ~scales::number(CANT_CLIEN),
	      	       layerId = ~paste0("Circles_", indice),
		       clusterId = "circle_cluster",
                       labelOptions = labelOptions(noHide = T, direction = 'center', textOnly = T),
                       clusterOptions = markerClusterOptions(iconCreateFunction=JS(sum.formula)),
                       group = "Circulos - Cantidad de clientes")
	if (last_divn != p_interno || last_div != s_interno) {
		proxy %>%
		removeLayersControl() %>%
		addLayersControl(baseGroups = c("Carto", "Esri"),
                     overlayGroups = c(columnInformation$DisplayName,
                                       "Circulos - Cantidad de clientes",
                                        paste(p_ind," por ", s_ind, sep = "")),
                     options = layersControlOptions(collapsed = FALSE,autoZIndex = TRUE))
	}
   	last_divn <<- p_interno
	  last_div <<- s_interno
   }
 })
}

shinyApp(ui, server)
