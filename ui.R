library(shiny)
library(shinyjs)
library(billboarder)
library(shinycssloaders)
library(billboarder)

# Define UI for application that draws a histogram
navbarPage("Irish Covid19 Dashboard", id="nav",
           tabPanel("Interactive Map", 
                    fluidRow(
                      column(3, h4("Select the date for the regional map and statistics"),
                             dateInput("date", "", 
                                       last.date, 
                                       min = as.Date("2020-03-23"),
                                       max = last.date),
                             align = 'center'),
                      column(3, h4("Change in Total Positives"), 
                             wellPanel (
                               div(textOutput("total.ratio"),style = "font-size:150%"),
                               style="padding: 15px;padding-top: 0px;padding-bottom: 0px;margin-top: 0px;"
                             ), align = 'center', style="margin-top: 0px;"),
                      column(3, h4("Change in ICU Admission"), 
                             wellPanel (
                               div(textOutput("icu.ratio"),style = "font-size:150%"),
                               style="padding: 15px;padding-top: 0px;padding-bottom: 0px;margin-top: 0px;"
                             ), align = 'center', style="margin-top: 0px;"),
                      column(3, h4("Change in Deaths"),
                             wellPanel (
                               div(textOutput("dead.ratio"),style = "font-size:150%"),
                               style="padding: 15px;padding-top: 0px;padding-bottom: 0px;margin-top: 0px;"
                             ), align = 'center', style="margin-top: 0px;"),
                    ),
                    fluidRow(
                      column(12,
                             includeCSS(here::here("Dashboard/styles.css")),
                        leaflet::leafletOutput("map", height = 710),
                        absolutePanel(id = "controls", class = "panel panel-default",
                                      fixed = FALSE, top = 15, draggable = TRUE,
                                      left = "auto", right = 60, bottom = "auto",
                                      width = 330, height = "auto", cursor = "default",
                                      p(),
                                      # Change Condition
                                      conditionalPanel(condition = "output.type == true",
                                                       selectInput("field", "Data for the national graph",
                                                                   choices = c("Admitted to Hospital" = "Hospitalised", 
                                                                               "Admitted to ICU" = "In ICU",
                                                                               "Dead", "Clusters","In Clusters",
                                                                               "Imported", "Healthcare",
                                                                               "Total Infections" = "Total"),
                                                                   selected = "Total")),
                                      p(),
                                      plotly::plotlyOutput("dynamic", height = "300px"),
                                      checkboxInput("log", "Log scale", value = TRUE)
                        ),
                        absolutePanel(id = "bestworst", class = "panel panel-default",
                                      fixed = FALSE, top = 15, draggable = TRUE,
                                      left = 60, right = "auto", bottom = "auto",
                                      width = 400, height = "auto", cursor = "default",
                                      p(), p(),
                                      plotly::plotlyOutput("best.worst.plot", height = "450px")
                        )
                      )
                    ),
                    fluidRow(
                      p("For the dashboard ©Davide Magno")
                    ),
                   fluidRow(
                      p("Data source are the daily HPSC reports available at ",
                        tags$a(href="https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/",
                               "https://www.hpsc.ie/a-z/respiratory/coronavirus/novelcoronavirus/"))
                    ),
                   fluidRow(
                     p("Full data in csv format is at ",
                       tags$a(href="https://github.com/DavideMagno/IrelandCovidData",
                              "https://github.com/DavideMagno/IrelandCovidData"))
                    ),
                   fluidRow(
                     p("Data is reported in line with events created as of the midnight of each date. This justifies the 2 days lag.")
                   )
           ),
           tabPanel("Data Explorer by Geography",
                    fluidRow(
                      column(4,
                             selectInput("regions", "Select the Regions of analysis",
                                         c("Ireland"= "",
                                           unique(Data$covid.regions$County)),
                                         multiple=TRUE),
                             align="center"),
                      column(4,
                             dateRangeInput("date.range", "Select the date range of analysis",
                                            start = as.Date("2020-03-23"),
                                            end = last.date,
                                            min = as.Date("2020-03-23"),
                                            max = last.date),
                             align="center"
                      ),
                      column(4,
                             selectInput("data.field", "Select the fields to analyse",
                                         c("Total" = ""),
                                         multiple=TRUE, selected = "Total"),
                             align="center"
                      )
                    ),
                    fluidRow(
                      column(1,
                             p(strong("Absolute figures"))
                      ),
                      column(1,
                             checkboxInput("data_increments", "Show daily increments", value = TRUE)
                      ),
                      column(1,
                             conditionalPanel(condition = "!input.data_increments",
                                              checkboxInput("data.log", "log scale", value = TRUE))
                      )
                    ),
                    fluidRow(
                      uiOutput("plots")
                    ),
                    fluidRow(
                      column(1,
                             p(strong("Relative increments"))
                      )
                    ),
                    fluidRow(
                      uiOutput("plots.ratio")
                    )
           ),
           tabPanel("Epidemiological Data Explorer", 
                    fluidRow(
                      column(width = 6,p(strong("Total positives data split by Sex")))
                    ),
                    fluidRow(
                      column(width = 6, plotly::plotlyOutput("g1")),
                      column(width = 6, plotly::plotlyOutput("g1.ts"))
                      ),
                    fluidRow(
                      column(width = 6,p(strong("Total positives data split by HSE Area of reporting")))
                    ),
                    fluidRow(
                      column(width = 6, plotly::plotlyOutput("g2")),
                      column(width = 6, plotly::plotlyOutput("g2.ts"))
                    ),
                    fluidRow(
                      column(width = 6, p(strong("Total positives data split by transmission type")))
                    ),
                    fluidRow(
                      column(width = 6, plotly::plotlyOutput("g3")),
                      column(width = 6, plotly::plotlyOutput("g3.ts"))
                    ),
                    fluidRow(
                      column(width = 3, p(strong("Epidemiological data split by age group"))),
                      column(width = 3, selectInput("g.field", "Filter by type of ",
                                                    choices = c("Admitted to Hospital" = "Hospitalised", 
                                                                "Admitted to ICU" = "In ICU",
                                                                "Dead" = "Died", "Total"),
                                                    selected = "Total"))
                    ),
                    fluidRow(
                      column(width = 6, plotly::plotlyOutput("g4")),
                      column(width = 6, plotly::plotlyOutput("g4.ts"))
                    ),
                    fluidRow(
                      column(width = 3, p(strong("Information about HSE workers who are positive split by HSE area"))),
                      column(width = 3, selectInput("g.workers", "Filter by place of contagion",
                                                    choices = c("Foreign travel", 
                                                                "Local/Community transmission",
                                                                "Not specified"),
                                                    selected = "Not specified"))
                    ),
                    fluidRow(
                      column(width = 6, plotly::plotlyOutput("g5")),
                      column(width = 6, plotly::plotlyOutput("g5.ts"))
                    )
           )
)