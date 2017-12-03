library(shiny)

shinyUI(fluidPage(theme = "bootstrap.css",
  titlePanel(""),
  sidebarLayout(
    sidebarPanel(width = 3,
      fileInput('file1', 'Uploader vos données',
                accept=c('text/csv', 
                         'text/comma-separated-values,text/plain', 
                         '.csv','application/vnd.openxmlformats-officedocument.spreadsheetml.sheet','xlsx')),
      checkboxInput('header', 'Entête', TRUE),
      radioButtons('sep', 'Séparateur',
                   c(Virgule=',',
                     `Point virgule`=';',
                     Tabulation='\t'),
                   ','),
      radioButtons('na', 'Symbole des valeurs manquantes',
                   c(`Cellule vide`=' ',
                     `NA`='NA',
                     `?`='?',`NaN`='NaN'),
                   ' '),tags$hr(),strong("Votre rapport"),
      radioButtons('format', 'Format', c('Word','PDF'),
                   inline = TRUE),
      downloadButton('downloadReport',label = "Télécharger")),
  
    mainPanel(
              tabsetPanel(type="tabs",
                  tabPanel(strong("Tableur"), dataTableOutput("table")), 
                  tabPanel(strong("Valeurs manquantes"),br(),h5(strong("Exploration des valeurs manquantes")),div("Tableau des valeurs manquantes",h5(p("La table suivante résume le pourcentage de valeurs manquantes par colonne (variable) dans toute la base de données.")),tableOutput("na_table1")),div(strong("Histogramme"),plotOutput('plot_na',width = "500px", height = "300px")),tags$hr(),h5(strong("Traitement des valeurs manquantes")),
                           fluidRow(column(width=6,p(width=NULL,status = "info",title = "Remarques",textOutput("na_text1"),strong(htmlOutput("na_text2")))),column(width=4,div(
                            wellPanel("Pourcentage toléré",br(),sliderInput("missinrate",label = "", min = 0,max = 70, value = 9))))),fluidRow(div("Les variable suivantes ont été omises de la base de données à cause de leurs taux de non réponse très élevé par rapport au seuil",strong(htmlOutput("na_text3")))),tags$hr(),fluidRow(div(strong("Procéder à un traitement simple de la base de donnée?",br(),p(actionButton("treat","Traitement")),htmlOutput("warning_na"), tags$head(tags$style("#warning_na{color: green;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            font-style: bold;
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                            }")))))
                           #,helpText('Cette table est provisoire, cest pour montrer que le traitement a bien marché')
                           #,tableOutput("na_table2")
                  ),
                  tabPanel(strong("Résumer"), p(tableOutput("summary"),width=NULL,status = "info",title = "Aperçu")), 
                  tabPanel(strong("Visualiser"),div(strong("Analyse univariée")),div(column(width=8,plotOutput("hist")),column(width=4,actionButton("saveplot","Enregistrer"))),column(width=4,tags$hr(),strong("Boite de contrôle"),wellPanel(p(selectInput("columname", "Sélectionner votre variable",choices = NULL)),h5("Les labels ne sont pas lisibles?"),checkboxInput("fliplab", "Aménager le graphique", value = FALSE, width = NULL))),
                  column(width=4,wellPanel(sliderInput("bins",
                                                                                                                             "Nombre de batôn",
                                                                                                                             min = 1,
                                                                                                                             max = 100,
                                                                                                                             value = 30),uiOutput("slider"))
                        )))
  )
)
))