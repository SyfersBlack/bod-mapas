library(leaflet)
setShapeStyle <- function( map, data = getMapData(map), layerId,
                           stroke = NULL, color = NULL,
                           weight = NULL, opacity = NULL,
			   category = "shape",
                           fill = NULL, fillColor = NULL,
                           fillOpacity = NULL, dashArray = NULL,
                           smoothFactor = NULL, noClip = NULL, label = NULL,
			   popup = NULL, layerGroup = category,
                           options = NULL){
  
  options <- c(list(layerId = layerId),
               options,
               filterNULL(list(stroke = stroke, color = color,
                               weight = weight, opacity = opacity,
                               fill = fill, fillColor = fillColor,
                               fillOpacity = fillOpacity, dashArray = dashArray,
                               smoothFactor = smoothFactor, noClip = noClip, label = label,
			       popup = popup, layerGroup = layerGroup
               )))

  options <- evalFormula(options, data = data)
  options <- do.call(data.frame, c(options, list(stringsAsFactors=FALSE)))
  
  layerId <- options[[1]]
  style <- options[-1]
  if("label" %in% colnames(style)){
    labelData = style[,"label", FALSE]
    style = style[,-which(colnames(style)=="label"), FALSE]
    leaflet::invokeMethod(map, data, "setLabel", layerGroup, layerId, label)
  }
  if ("popup" %in% colnames(style)) {
    popupData = style[,"popup", FALSE]
    style = style[,-which(colnames(style)=="popup"), FALSE]
    leaflet::invokeMethod(map, data, "setPopup", category, layerId, popup)
  }
  leaflet::invokeMethod(map, data, "setStyle", category, layerId, style);
}

leaflet_js <- HTML('
window.LeafletWidget.methods.setStyle = function(category, layerId, style){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  style = HTMLWidgets.dataframeToD3(style);
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.setStyle(style[i]);
    }
  });
};
window.LeafletWidget.methods.setLabel = function(category, layerId, label){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = null;
    if (category == "shape") {
    	layer = map.layerManager.getLayer(category, d);
    } else {
	var group = map.layerManager.getLayerGroup(category);
	layer = map.layerManager.getLayerGroup(category).getLayer(d);
    }
    if (layer){
      layer.unbindTooltip();
      layer.bindTooltip(label[i])
    }
  });
};

window.LeafletWidget.methods.setPopup = function(category, layerId, popup){
  var map = this;
  if (!layerId){
    return;
  } else if (!(typeof(layerId) === "object" && layerId.length)){
    layerId = [layerId];
  }
  layerId.forEach(function(d,i){
    var layer = map.layerManager.getLayer(category, d);
    if (layer){
      layer.unbindPopup();
      layer.bindPopup(popup[i])
    }
  });
};
') 

renderFirstMap <- function(data, initial_palette) {
    leaflet(data) %>%
    addProviderTiles("CartoDB", group = "Carto") %>%
    setView(lng = 46.5, lat = -26, zoom = 8) %>%
    setMaxBounds(lng1 = -78.624807, lat1 = 15.638759,
                 lng2 = -51.78926, lat2 = -1.97570) %>%
    addPolygons(weight = 1,
		fillOpacity = 1,
		layerId = ~indice,
    		color = ~initial_palette(SAL_PROM_USD),
		popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>","Saldo promedio USD: ", format(SAL_PROM_USD, nsmall=2, big.mark="."),"</b>","</p>"),
              	label = ~paste0(indice,": ", format(SAL_PROM_USD, nsmall=2, big.mark=".")),
		highlight = highlightOptions(weight = 3, color = "blue"))
}


onInputsChange <- function(input, data, columnInformation, timeline) {
    selected_col <- columnInformation[columnInformation$DisplayName == input$animation_col, "InternalName"]
    palette <- columnInformation[columnInformation$DisplayName == input$animation_col, "PaletteName"] 
    date <- timeline[month(timeline) == month(as.Date(input$animation_date, "%d-%m-%Y"))]
    new_data <- filter(data, Date == date) %>% select(MapaMun) %>% first()
    new_data <- new_data[[1]] %>% as.data.frame()
    new_palette <- colorQuantile(palette = palette, domain = unique(new_data[, selected_col]), n = 5)
    leafletProxy("animateMap") %>%
    clearMarkers() %>%
    setShapeStyle(weight = 0,
	      data = st_as_sf(new_data),
              fillOpacity = 1,
	      layerId = ~indice,
              fillColor = ~new_palette(new_data[[selected_col]]),
	      popup = ~paste0("<b>",indice,"</b>","<br/>",
                             "<b>","<p style=color:blue>", input$animation_col, ": ", format(new_data[[selected_col]], nsmall=2, big.mark="."),"</b>","</p>"),
              label = ~paste0(indice,": ", format(new_data[[selected_col]], nsmall=2, big.mark=".")))
}
