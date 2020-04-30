SelectCharacteristic <- function(data, characteristic, type.filter = NA) {
  data %<>% 
    dplyr::filter(grepl(characteristic, `Key 1`))
                  
  if (!is.na(type.filter)) {
    data %<>% 
      dplyr::filter(grepl(paste0("\\b",type.filter,"\\b"), `Key 3`)) 
  } 
  data %<>% 
    dplyr::select(-`Key 1`, -`Key 3`) %>% 
    dplyr::group_by(Date) %>% 
    dplyr::mutate(Ratio = Value/sum(Value)) %>% 
    dplyr::select(Date, `Key 2`, Value, Ratio) %>% 
    dplyr::ungroup(.)
  
  title <- paste(characteristic, 
                 "decomposition of", 
                 ifelse(is.na(type.filter),
                        "total",
                        stringr::str_to_lower(type.filter)),
                 "positives")
  
  title.ts <- paste(characteristic, 
                 "time series of", 
                 ifelse(is.na(type.filter),
                        "total",
                        stringr::str_to_lower(type.filter)),
                 "positives")
  
  return(list(data = data, title = title, title.ts = title.ts))
} 

PlotCharacteristic <- function(input) {
  names <- unique(input$data$`Key 2`)
  
  data <- input$data %>% 
    dplyr::select(-Value) %>% 
    tidyr::pivot_wider(names_from = `Key 2`, values_from = Ratio)
  
  p <- plotly::plot_ly(data,
                    type = 'scatter', mode = 'none', stackgroup = 'one', 
                    groupnorm = 'percent') %>% 
    plotly::config(displayModeBar = FALSE) %>% 
    plotly::layout(
      legend = list(orientation = 'h'),
      hovermode = "x unified",
      yaxis = list(title = "% of cases"),
      xaxis = list(title = "")
    )
  
  for (i in seq_along(names)) {
    y <- data[[names[i]]]
    p <- p %>% plotly::add_trace(x = ~Date, y = y, name = names[i])
  }

  p
}

PlotCharacteristicTimeSeries <- function(input) {
  names <- unique(input$data$`Key 2`)
  
  data <- input$data %>% 
    dplyr::select(-Ratio) %>% 
    tidyr::pivot_wider(names_from = `Key 2`, values_from = Value)
  
  p <- plotly::plot_ly(data, type = 'scatter', 
                       mode = "lines+markers") %>% 
    plotly::config(displayModeBar = FALSE) %>% 
    plotly::layout(
      legend = list(orientation = 'h'),
      hovermode = "x unified",
      yaxis = list(title = "Number of cases"),
      xaxis = list(title = "")
    )

  for (i in seq_along(names)) {
    y <- data[[names[i]]]
    p <- p %>% plotly::add_trace(x = ~Date, y = y, name = names[i])
  }
  
  p
}