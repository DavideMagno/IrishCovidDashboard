source(here::here("R/IrishMap.R"))
source(here::here("R/Analysis.R"))
source(here::here("R/MultipleGraphs.R"))
source(here::here("R/Data_Wrangling.R"))
source(here::here("R/Plot_Characteristics.R"))

Data <- GetRawData()
last.date <- dplyr::last(Data$covid.regions$Date)
data("economics", package = "ggplot2")