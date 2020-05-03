library(magrittr)

PlotConfidenceIntervals <- function(data) {
  
  data.nowcast <- data %>% 
    dplyr::filter(grepl("nowcast", rt_type)) %>% 
    dplyr::select(-rt_type)
  
  data.forecast <- data %>% 
    dplyr::filter(grepl("forecast", rt_type)) %>% 
    dplyr::select(-rt_type)
  
  plotly::plot_ly() %>% 
    plotly::add_lines(x = data.nowcast$date, y = data.nowcast$point, 
                      line = list(color = "15C9A9", width = 4), 
                      name = "Nowcast") %>% 
    plotly::add_ribbons(x = data.nowcast$date, ymin = data.nowcast$lower, 
                        ymax = data.nowcast$upper, color = I("#cbe0dc"), 
                        name = "95% confidence", 
                        line = list(color = 'transparent'),
                        showlegend = FALSE) %>% 
    plotly::add_ribbons(x = data.nowcast$date, ymin = data.nowcast$mid_lower, 
                        ymax = data.nowcast$mid_upper, color = I("#86DBCD"), 
                        name = "90% confidence",
                        line = list(color = 'transparent'),
                        showlegend = FALSE) %>%
    plotly::add_lines(x = data.forecast$date, y = data.forecast$point, 
                      line = list(color = "A15400", width = 4), 
                      name = "Forecast") %>% 
    plotly::add_ribbons(x = data.forecast$date, ymin = data.forecast$lower, 
                        ymax = data.forecast$upper, color = I("#EFC095"), 
                        name = "95% confidence", 
                        line = list(color = 'transparent'),
                        showlegend = FALSE) %>% 
    plotly::add_ribbons(x = data.forecast$date, ymin = data.forecast$mid_lower, 
                        ymax = data.forecast$mid_upper, color = I("#EC8126"), 
                        name = "90% confidence", 
                        line = list(color = 'transparent'),
                        showlegend = FALSE) %>% 
    plotly::config(displayModeBar = FALSE) %>% 
    plotly::layout(
      legend = list(orientation = 'h'),
      yaxis = list(title = "Reproduction Rate"),
      xaxis = list(title = "")
    )
}

file.R0 <- "https://github.com/epiforecasts/covid-global/raw/master/national/Ireland/latest/time_varying_params.rds"

file.cases <- "https://github.com/epiforecasts/covid-global/raw/master/national/Ireland/latest/plot_cases.rds"

rt <- readRDS(url(file.R0))

R0.data <- rt$R0 %>% 
  tibble::as_tibble(.) %>% 
  dplyr::select(date, rt_type, R0_range) %>% 
  tidyr::unnest_wider(R0_range) 
  
cases <- readRDS(url(file.cases))

cases.estimates <- cases$data %>% 
  dplyr::filter(grepl("nowcast",type)) %>% 
  dplyr::select(date = date_onset, rt_type = type, lower = bottom, upper = top, 
                mid_lower = lower, mid_upper = upper, point = mean)

cases.forecast <-  rt$case_forecast %>% 
  dplyr::select(date = date, rt_type, lower = bottom, upper = top, mid_lower = lower, 
                mid_upper = upper, point = mean)

cases.data <- cases.estimates %>% 
  dplyr::bind_rows(cases.forecast)

actual.cases <- cases$data %>% 
  dplyr::filter(grepl("Observed by report date",type)) %>% 
  dplyr::select(date, median)
  

p1 <- PlotConfidenceIntervals(R0.data) %>% 
  layout(showlegend = FALSE)
p2 <- PlotConfidenceIntervals(cases.data) %>% 
  plotly::add_trace(type='bar', x = actual.cases$date, 
                    y = actual.cases$median, 
                    marker = list(color = '#C4D6E0'))
                    
plotly::subplot(p2, p1, nrows = 2)
