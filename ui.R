library(shiny)
library(ggplot2)
library(plotly)
library(stringr)
library(purrr)
library(dplyr)
library(xml2)
library(markdown)
library(shinythemes)
library(tidyr)
library(leaflet.extras)
library(htmltools)
library(lubridate)
library(httr)

shinyUI(navbarPage("Gallicapresse",
                   tabPanel("Graphique",fluidPage(),
                            tags$head(
                              tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                            pageWithSidebar(headerPanel(''),
                                            sidebarPanel(
                                              textInput("mot","Terme(s) à chercher","Abel Bonnard"),
                                              p('Utiliser "a+b" pour rechercher a OU b'),
                                              dateRangeInput('dateRange',
                                                             label = '\n',
                                                             start = as.Date.character("1913-01-01"), end = as.Date.character("1914-12-31"),
                                                             separator="à", startview = "century"),
                                              p(textOutput("message")),
                                              conditionalPanel(condition = "(output.message.includes('Gallica')==false) || (output.target_upload==true)",div(style="display: inline-block;vertical-align:bottom",actionButton("do","Générer les graphiques"))),
                                              div(style="display: inline-block;vertical-align:bottom",actionButton("gallica","Ouvrir dans Gallica")),
                                              uiOutput("ui_gallica"),
                                              fileInput('target_upload','', 
                                                        accept = c(
                                                          'text/csv',
                                                          'text/comma-separated-values',
                                                          '.csv'
                                                        ),buttonLabel='Importer', placeholder='un rapport de recherche'),
                                              checkboxInput("relative", "Afficher les résultats en valeurs relatives", value = FALSE),
                                              checkboxInput("mois_pub", "Supprimer les numéros sans mois de publication enregistré", value = FALSE),
                                              radioButtons("structure", "Données à analyser :",choices = list("Titre de presse" = 1, "Ville de publication" = 2,"Classement thématique de Dewey" = 3,"Périodicité" = 4),selected = 1),
                                              div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadData', 'Télécharger les données')),
                                              div(style="display: inline-block;vertical-align:bottom",downloadButton('downloadRR', 'Télécharger le rapport de recherche'))
                                            ),
                                            
                                            mainPanel(
                                              plotlyOutput("plot1"),
                                              fluidRow(textOutput("legende1"),align="right"),
                                              fluidRow(textOutput("legende2"),align="right"),
                                              downloadButton('downloadPlot1', 'Télécharger le graphique interactif'),
                                              p(""),
                                              plotlyOutput("plot2"),
                                              fluidRow(textOutput("legende3"),align="right"),
                                              fluidRow(textOutput("legende4"),align="right"),
                                              downloadButton('downloadPlot2', 'Télécharger le graphique interactif'),
                                              p(""),
                                              conditionalPanel(condition="input.structure==2",leafletOutput("plot7")),
                                              conditionalPanel(condition="input.structure==2",fluidRow(textOutput("legende5"),align="right")),
                                              conditionalPanel(condition="input.structure==2",fluidRow(textOutput("legende6"),align="right")),
                                              conditionalPanel(condition="input.structure==2",downloadButton('downloadPlot7', 'Télécharger la carte interactive')),
                                              p(""),
                                              h2(textOutput("currentTime"), style="color:white")
                                            ))),
                   tabPanel("Notice",shiny::includeMarkdown("Notice.md")),
                   tabPanel("Corpus",fluidPage(),
                            tags$head(
                              tags$style(HTML(".shiny-output-error-validation{color: red;}"))),
                            pageWithSidebar(headerPanel(''),
                                            sidebarPanel(checkboxInput("corpus_relative_p", "Afficher les résultats en valeurs relatives", value = FALSE),
                                                         radioButtons("corpus_structure_p", "Données à analyser :",choices = list("Ville de publication" = 2,"Droits d'auteur"=3,"Bibliothèque d'origine"=4, "Classement thématique de Dewey" = 5,"Périodicité" = 6,"Titre de presse" = 7),selected = 2),
                                            ),
                                            mainPanel(
                                              fluidRow(plotlyOutput("corpus1")),
                                              p("")
                                            )
                            )
                   ),
                   tabPanel(title=HTML("<li><a href='https://shiny.ens-paris-saclay.fr/gallicagram_app' target='_blank'>Gallicagram"))
))