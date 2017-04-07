
# Librairies à installer/charger:
list.of.packages = c('shiny','leaflet','shinydashboard')

# # Packages à installer
# new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)){install.packages(new.packages)}

# Chargement des librairies
lapply(list.of.packages, require, character.only = TRUE)



            ###############################################################
            ###                     User Interface                      ###
            ###############################################################

# USER INTERFACE REALISEE AVEC LE PACKAGE SHINYDASHBAORD      
# Toutes les images sont stockées dans le dossier 'www'

# HEADER (Titre en haut de l'application) :

header = dashboardHeader(title = "Géomarketing - Carte interactive", titleWidth = 450
                         
                         ,
                         tags$li(a(href = 'https://www.institutlouisbachelier.org',
                                   img(src = 'bachelier.png',
                                       title = "Company Home", height = "30px"),
                                   style = "padding-top:10px; padding-bottom:10px;"),
                                 class = "dropdown"),
                         tags$li(a(href = 'https://www.ca-briepicardie.fr',
                                   img(src = 'logo_crca.png',
                                       title = "Company Home", height = "30px"),
                                   style = "padding-top:10px; padding-bottom:10px;"),
                                 class = "dropdown")
                         
                         )

# SIDEBAR (Menu, un onglet par catégorie) :

sidebar = dashboardSidebar(width = 250, 
                            sidebarMenu(id = "tabs",
                                        menuItem("Informations",tabName = "Accueil", icon = shiny::icon("info")),
                                        br(),
                                        div(h5("Catégories de variables :"), style = "text-align: center; font-weight : bold;"),
                                        menuItem("Données sociodémographiques",tabName = "Démographie", icon = shiny::icon("users")),
                                        menuItem("Données socioéconomiques", tabName = "Economie", icon = shiny::icon("line-chart")),
                                        menuItem("Logement",tabName = "Logement", icon = shiny::icon("home")),
                                        menuItem("Services",tabName = "Services", icon = shiny::icon("handshake-o")),
                                        menuItem("Couverture Numérique",tabName = "Numérique", icon = shiny::icon("at")),
                                        menuItem("Environnement",tabName = "Environnement", icon = shiny::icon("pagelines")),
                                        menuItem("Transport",tabName = "Transport", icon = shiny::icon("car")),
                                        menuItem("Politique",tabName = "Politique", icon = shiny::icon("university")),
                                        menuItem("Combinaison de variables",tabName = "Combi", icon = shiny::icon("arrows-h"))                            ) #/sidebarMenu
) # /dashboardSidebar

# BODY

body = dashboardBody(
  
  tags$style("
             
             .nav-tabs-custom .nav-tabs li.active:hover a, .nav-tabs-custom .nav-tabs li.active a {
             background-color: rgb(229, 242, 252);

             } "
)
  
  ,

  tabItems(
    
    
    
    ###########################
    #         Accueil         #
    ###########################
    
    
    tabItem(
      tabName = "Accueil",
      br(), br(),
      h1("Plateforme de visualisation des données IRIS"),
      div(p("Cette application permet de visualiser sur une carte interactive de la région Brie-Picardie les données publiques rassemblées par l'Institut Louis Bachelier.")
          ,style = "text-indent: 30px; font-size : 16px;"),
      fluidRow(

        br(),br(), br(),
        div(shinydashboard::box(title = div(p("Informations"), style = 'font-size = 16px; font-weight : bold'),
                status = "primary", solidHeader = FALSE,
          "La carte de la région Brie-Picardie est découpée en  \"Ilots regroupés pour l'information statistique\" (IRIS).",
          "L'IRIS est un niveau géographique infra-communal utilisé par l'INSEE pour la collecte et la diffusion de données statistiques.",
          br(), br(),
          "Sur la carte, les IRIS sont colorés de manière à représenter une mesure statistique (par exemple, le revenu médian).",
          "Les jeux de données disponibles sont classés en Catégories & en Sous-Catégories.",
          br(), br(), "Cette application a été réalisée grâce au logiciel", icon("registered"), "(packages Shiny & Leaflet).",
          br(), br(), br()
        ), style = "text-align: justify;"),
        div(shinydashboard::box(title = div(p("Instructions"), style = 'font-size = 16px; font-weight : bold'),
            status = 'primary', solidHeader = FALSE,
          "1. Cliquer sur une catégorie de données dans le menu à gauche.",
          br(), br(),
          "2. Choisir une sous-catégorie dans la fenêtre à droite.",
          br(), br(),
          "3. Choisir enfin une variable dans le menu déroulant.",
          br(), br(),
          "4. Si la variable représentée sur la carte est une variable quantitative,
          alors les IRIS sont colorés selon  une échelle de couleurs correspondant aux
          quantiles de la variable. Si la variable est qualitative, chaque modalité de la variable est
          représentée avec une couleur spécifique.", "Les 'NA' (colorés en gris) correspondent soit aux valeurs manquantes, soit aux valeurs abberrantes.",
          br()
        ), style = "text-align: justify;")
      )
    )


    ,

    
    ###############################################
    #         Données sociodémographiques         #
    ###############################################
    
    shinydashboard::tabItem(tabName = "Démographie", class = 'active',
            
            leaflet::leafletOutput("Carte_Pop", height = 600)
            
            ,
            
            shiny::fluidPage(
              
              shiny::absolutePanel(
                draggable = TRUE,
                fixed = FALSE, 
                top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                wellPanel(
                  shiny::h4("Jeux de données"),
                  
                  radioButtons(inputId = "choix_categ_pop", label = "",
                               choices = liste_sous_categ_pop, selected = liste_sous_categ_pop[1])
                  ,
                  
                  selectInput(inputId = "choix_var_pop", label = "",
                              choices = liste_Population)
                  
                  , 
                  style = "background: rgba(34,45,50, 0.92); color: rgba(219, 236, 245,1);"
                  ) # /wellpanel       
         ) # /absolutepanel
      ) # /fluidPage
    ) # /tabitem  
    
    
    
    ,
    
    
    
    ###############################################
    #           Données socioéconomiques          #
    ###############################################
    
    shinydashboard::tabItem(tabName = "Economie", class = 'active',

            leaflet::leafletOutput("Carte_Eco", height = 600)

            ,

            shiny::fluidPage(

              shiny::absolutePanel(
                draggable = TRUE,
                fixed = FALSE,
                top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                wellPanel(
                  shiny::h4("Jeux de données"),

                  radioButtons(inputId = "choix_categ_eco", label = "",
                               choices = liste_sous_categ_socioeco)
                  ,

                  selectInput(inputId = "choix_var_eco", label = "",
                              choices = c())

                  ,
                  style = "background: rgba(34,45,50, 0.92); color: rgba(219, 236, 245,1);"
                  ) # /wellpanel
              ) # /absolutepanel
            ) # /fluidPage
    ) # /tabitem


    ,
    
    ######################################
    #           Données Logement         #
    ######################################
    
    tabItem(tabName = "Logement", class = 'active',
            
            leafletOutput("Carte_log", height = 600)
            
            ,
            
            shiny::fluidPage(
              
              shiny::absolutePanel(
                draggable = TRUE,
                fixed = FALSE,
                top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                wellPanel(
                  shiny::h4("Jeux de données"),
                  
                  radioButtons(inputId = "choix_categ_log", label = "",
                               choices = liste_sous_categ_log)
                  ,
                  
                  selectInput(inputId = "choix_var_log", label = "",
                              choices = c())
                  ,
                  style = "background: rgba(34,45,50, 0.92); color: rgba(219, 236, 245,1);") # /wellpanel       
              ) # /absolutepanel
            ) # /fluidPage
    ) # /tabitem  
    
    
    ,
    
    ######################################
    #           Données Services         #
    ######################################
    
    tabItem(tabName = "Services", class = 'active',
            
            leafletOutput("Carte_services", height = 600)
            
            ,
            
            shiny::fluidPage(
              
              shiny::absolutePanel(
                draggable = TRUE,
                fixed = FALSE,
                top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                wellPanel(
                  shiny::h4("Jeux de données"),
                  
                  radioButtons(inputId = "choix_categ_services", label = "",
                               choices = liste_sous_categ_serv)
                  ,
                  
                  selectInput(inputId = "choix_var_services", label = "",
                              choices = c())
                  ,
                  style = "background: rgba(34,45,50, 0.92); color: rgba(219, 236, 245,1);") # /wellpanel       
              ) # /absolutepanel
            ) # /fluidPage
    ) # /tabitem  
    
    ,
    
    ######################################
    #         Données Numerique         #
    ######################################
    
    tabItem(tabName = "Numérique", class = 'active',
            
            leafletOutput("Carte_numerique", height = 600)
            
            ,
            
            shiny::fluidPage(
              
              shiny::absolutePanel(
                draggable = TRUE,
                fixed = FALSE,
                top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                wellPanel(
                  shiny::h4("Jeux de données"),
                  
                  radioButtons(inputId = "choix_categ_numerique", label = "",
                               choices = liste_sous_categ_numeric)
                  ,
                  
                  selectInput(inputId = "choix_var_numerique", label = "",
                              choices = c())
                  ,
                  style = "background: rgba(34,45,50, 0.92); color: rgba(219, 236, 245,1);") # /wellpanel       
              ) # /absolutepanel
            ) # /fluidPage
    ) # /tabitem  
    
    ,
    
    
    #########################################
    #         Données Environnement         #
    #########################################
    
    tabItem(tabName = "Environnement", class = 'active',
            
            leafletOutput("Carte_envir", height = 600)
            
            ,
            
            shiny::fluidPage(
              
              shiny::absolutePanel(
                draggable = TRUE,
                fixed = FALSE,
                top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                wellPanel(
                  shiny::h4("Jeux de données"),
                  
                  radioButtons(inputId = "choix_categ_envir", label = "",
                               choices = liste_sous_categ_environnement)
                  ,
                  
                  selectInput(inputId = "choix_var_envir", label = "",
                              choices = c())
                  ,
                  style = "background: rgba(34,45,50, 0.92); color: rgba(219, 236, 245,1);") # /wellpanel       
              ) # /absolutepanel
            ) # /fluidPage
    ) # /tabitem  
    
    ,
    
    
    #####################################
    #         Données Transport         #
    #####################################
    
    tabItem(tabName = "Transport", class = 'active',
            
            leafletOutput("Carte_transport", height = 600)
            
            ,
            
            shiny::fluidPage(
              
              shiny::absolutePanel(
                draggable = TRUE,
                fixed = FALSE,
                top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                wellPanel(
                  shiny::h4("Jeux de données"),
                  
                  radioButtons(inputId = "choix_categ_transport", label = "",
                               choices = liste_sous_categ_transp)
                  ,
                  
                  selectInput(inputId = "choix_var_transport", label = "",
                              choices = c())
                  ,
                  style = "background: rgba(34,45,50, 0.92); color: rgba(219, 236, 245,1);") # /wellpanel       
              ) # /absolutepanel
            ) # /fluidPage
    ) # /tabitem  
    
    ,

    
    #####################################
    #         Données Politique         #
    #####################################
    
    tabItem(tabName = "Politique", class = 'active',
            
            leafletOutput("Carte_pol", height = 600)
            
            ,
            
            shiny::fluidPage(
              
              shiny::absolutePanel(
                draggable = TRUE,
                fixed = FALSE,
                top = 60, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                wellPanel(
                  shiny::h4("Jeux de données"),
                  
                  radioButtons(inputId = "choix_categ_pol", label = "",
                               choices = liste_sous_categ_pol)
                  ,
                  
                  selectInput(inputId = "choix_var_pol", label = "",
                              choices = c())
                  ,
                  style = "background: rgba(34,45,50, 0.92); color: rgba(219, 236, 245,1);") # /wellpanel       
              ) # /absolutepanel
            ) # /fluidPage
    ) # /tabitem  
    
    ,
    
    ###############################
    #         Combinaisons        #
    ###############################
    
    tabItem(tabName = "Combi", class = 'active',
            
            leafletOutput("Carte_Combi", height = 600)
            
            ,
            
            shiny::fluidPage(

              shiny::absolutePanel(
                draggable = TRUE,
                fixed = FALSE,
                top = 75, left = 310, right = 'auto', bottom = "auto",
                width =  800, height = "auto",
                
                shinydashboard::box(title = p(shiny::icon("info-circle"),'Instructions'),
                                    collapsible = TRUE, status = 'danger', solidHeader = TRUE,
                div(p("Choisir deux critères :", style = "font-size : 20px"), p(
                  "- Pour les variables", strong("numériques"), ": le critère est un intervalle de valeurs au sein duquel la variable doit se trouver",
                  br(), "- Pour les variabales", strong("qualitatives"),": le critère est une modalité à laquelle la variable doit appartenir",
                  br(), br(),
                  strong("Les IRIS sont colorés :"),
                  br(),
                  "- en JAUNE si le", strong("1er critère"),"seulement est rempli",
                  br(),
                  "- en ORANGE si le", strong("2nd critère"), "seulement est rempli",
                  br(),
                  "- en BLEU si les", strong("2 critères"), "sont remplis",
                  br(),
                  "- en ROUGE si", strong("aucun"), "des 2 critères n'est rempli"
              ), style = 'text-align : justify;'))),
              
              shiny::absolutePanel(
                draggable = TRUE,
                fixed = FALSE,
                top = 75, left = 930, right = 'auto', bottom = "auto",
                width =  830, height = "auto",
              
                  
              shinydashboard::tabBox(
                
                tabPanel(
                  div(p("Choix du 1er critère"), style = "color: black; font-weight : bold"),
                  
                  selectInput(inputId = "categ1", label = "Choix de la catégorie", width = "70%", choices = liste_categ, selected = liste_categ[2]),
                  selectInput(inputId = "subcateg1", label = "Choix de la sous-catégorie",  width = "70%", choices = c()),
                  selectInput(inputId = "variable1", label = "Choix de la variable",  width = "70%", choices = c()),
                  uiOutput("choixcritere1")
                  
                  ) # /tabpanel       
                
                ,
                
                tabPanel(
                  div(p("Choix du 2eme critère"), style = "color: black;font-weight : bold"),

                  selectInput(inputId = "categ2", label = "Choix de la catégorie",  width = "70%", choices = liste_categ, selected = liste_categ[3]),
                  selectInput(inputId = "subcateg2", label = "Choix de la sous-catégorie",  width = "70%", choices = c()),
                  selectInput(inputId = "variable2", label = "Choix de la variable",  width = "70%", choices = c()),
                  uiOutput("choixcritere2")
                  
                  ) # /tabpanel       
              
                
              ) # /tabBox
              )#/absolutePanel
         
            ) # /fluidPage
    ) # /tabitem  
    
    
    
  ) # /tabitems
) # /dashboardbody



dashboardPage(skin = "black",
              header,
              sidebar,
              body
)





