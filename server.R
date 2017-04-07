
# Librairies à installer/charger:
list.of.packages = c('shiny','leaflet','leaflet.extras', 'readr', 'rgdal', 'sp')

# # Packages à installer
# new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)){install.packages(new.packages)}

# Chargement des librairies
lapply(list.of.packages, require, character.only = TRUE)

            
            ######################################################
            ###                   Serveur                      ###
            ######################################################


# Création d'un Output (une carte Leaflet) par catégorie de données
# Seule la première carte (données sociodémographiques) est
# commentée de manière détaillée car la marche à suivre
# selon les cartes est relativement la même

# En revanche la carte générée dans l'onglet 'combinaison de variables'
# est vraiment spécifique; Elle est codée tout en bas de ce script

function(input, output, session){
  
  
  ###############################################
  #         Données sociodémographiques         #
  ###############################################
  
  # L'utilisateur a cliqué sur l'onglet 'Données sociodémographiques'
  # Maintenant il doit choisir une sous-catégorie de variables (par exemple, 'population')
  # On observe son choix de sous-catégorie de variables (input$choix_categ_pop)
  # et on retourne la liste des variables appartenant à cette sous-catégorie
  # dans un menu déroulant, càd qu'on update le SelectInput
  # Les valeurs du menu déroulant correspondent aux descriptions de chacune
  # des variables et non au nom exact des variables pour plus de lisibilité. 
  # Cette description est contenue dans le dico des variables
  # On accède à cette liste (dico$Label) d'une variable en appelant
  # dico$Label[dico$Sous_Categ == sous-categ choisie par l'utilisateur)]
  
  observeEvent(input$choix_categ_pop, {
    categ = input$choix_categ_pop
    updateSelectInput(session = session, inputId = "choix_var_pop",label = "",
                      choices = dico$Label[dico$Sous_Categ == as.character(categ)])

  })
  
  # On crée maintenant les objets qui réagiront à l'interface et aux choix
  # de l'utilisateur :
  
  # renvoie la description (= label) de la variable que l'utilisateur a choisi dans le menu
  # déroulant :
  label_reac_pop = reactive({as.character(input$choix_var_pop)})
  
  # renvoie le nom exact de la variable que l'utilisateur a choisi dans le menu
  # déroulant. On obtient le nom exact de la variable en cherchant le label
  # correspondant dans le dico : 
  # dico$Variable[dico$Label == le label de la  variable choisie] :
  
  var_reac_pop = reactive({dico$Variable[dico$Label == label_reac_pop()]})
  
  # renvoie la palette de couleurs qui devra être utilisée pour représenter la 
  # variable choisie sur la carte :
  col_reac_pop = reactive({as.character(dico$Couleur[dico$Label == label_reac_pop()])})
  
  # renvoie le type de la variable choisie, ce qui va déterminer l'unité de la variable
  # qui sera utilisée sur la carte : par exemple le taux de chômage est en % :
  
  type_reac_pop = reactive({as.character(dico$Type[dico$Label == label_reac_pop()])})
  
  # renvoie l'unité de la variable selon son type : si le type est 'Pct' alors l'unité
  # est %, et pour tous les autres types il n'y a pas d'unité spécifique.
  unite_reac_pop = reactive({ifelse(as.character(type_reac_pop()) == "Pct", " %", "")})
  
  
      ##############################################
      ##      Création de la carte Leaflet        ##
      ##############################################
  
 
    output$Carte_Pop <- renderLeaflet({
      
      # On alloue d'abord à des variables les éléments réactifs 
      # Pour plus de lisbilité
      
      label = label_reac_pop() # label de la variable choisie
      var = var_reac_pop() # nom exact de la variable choisie
      col = col_reac_pop() # palette de couleurs
      type = type_reac_pop() # type
      unite = unite_reac_pop() # unité
      values_var = map_WGS84@data[,var] # vecteur des valeurs de la variable
      
      # Le nombre de quantiles qui devra être utilisé pour la légende
      # en fonction de la variable choisie 
      # nb_quantile est définie dans le script global
      
      nb_quant = nb_quantile(map_WGS84@data[,var])
      
      # Création de la POP-UP (la fenêtre qui s'ouvre lorsque l'on clique sur un IRIS) :
      
          # Pour les variables continues 
          # Ecart entre la valeur de la variable et la moyenne sur la région Brie-Picardie :
          x = round(as.numeric(map_WGS84@data[,var] - mean(map_WGS84@data[,var], na.rm = TRUE)),2)
          
          # La valeur moyenne de la variable pour la région Brie-Picardie :
          mean = round(mean(map_WGS84@data[,var], na.rm = TRUE),2)
          
          # La couleur de la police de la valeur de la variable dans la POPUP : 
          # Rouge si la valeur de la variable est inférieure à la moyenne BP
          # Vert si la valeur de la variable est supérieure à la moyenne BP
          color = ifelse(x > 0,"green","red")
          
          # Création de la variable popup : si la variable est en %, l'unité derrière la valeur
          # est %, et la différence entre la valeur et la moyenne est exprimée en points de pourcentage
          # Si la variable choisie n'est pas en %, pas d'unité derrière la valeur

          if (type == "Pct")
          {popup =  paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                           map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", round(map_WGS84@data[,var],2),unite,
                           "<br><i><font color=",color,">", ifelse(x>0, paste0("+", x), x), 
                           " points</font> par rapport à la moyenne Brie-Picardie ( = ", mean, unite," )</i>")}
          else {
            popup =  paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                            map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", round(map_WGS84@data[,var],2),unite,
                            "<br><i><font color=",color,">", ifelse(x>0, paste0("+", x), x), 
                            "</font> par rapport à la moyenne Brie-Picardie ( = ", mean, " )</i>")
          }
      
      
      
      # Création de la palette de couleurs à utiliser pour la variable choisie :
      
          # Palette du clair au foncé et nombre de quantiles max > 2 : 
          # on utilise un coloriage du type ColorQuantile  
          if (col == "Clair_Fonce" & nb_quant > 2){
            pal = colorQuantile(Clair_Fonce, n = nb_quant, map_WGS84@data[,var], na.color = "grey")
          } 
          
          # Palette du rouge au vert + colorQuantile
          else if (col == "Rouge_Vert" & nb_quant > 2){
            pal = colorQuantile(Rouge_Vert, n = nb_quant, map_WGS84@data[,var],na.color = "grey")
          } 
          
          # Palette du rouge au vert + colorNumeric
          else if (col == "Rouge_Vert" & nb_quant <= 2){ 
            pal = colorNumeric(Rouge_Vert, map_WGS84@data[,var], na.color = "grey") 
          }
          
          # Palette du clair au foncé + colorNumeric
          else { 
            pal = colorNumeric(Clair_Fonce, map_WGS84@data[,var], na.color = "grey") 
          }
          
      
      # Création de la carte Leaflet en tant que telle
      # avec tous les objets créées précédemment
          
      leaflet(map_WGS84) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>% # fond de carte sans labels de villes
      addProviderTiles("CartoDB.PositronOnlyLabels") %>% # couche qui contient juste les labels
      setView(lng = 2.468738900000062, lat = 49.19316, zoom = 7)  %>% # on fixe le zoom initial 
      clearShapes() %>% # permet d'enlever tous les polygones tracés sur la carte à chaque fois 
                        # que la carte se met à jour
      clearPopups() %>% # permet d'enlever les popups
        
        # Ajout des polygones :
      addPolygons(weight = 1, # épaisseur de la frontière entre les IRIS
                  color = ~pal(map_WGS84@data[,var]), # couleur de remplissage des IRIS
                  popup = popup, # Popup
                  fillOpacity = 0.7, # Opacité du remplissage des IRIS
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE) 
                  # les frontières de l'IRIS
                  # se colorent en blanc quand on passe la souris dessus
                    
      ) %>%
      
        # Ajout des marqueurs pour les grandes villes :
        # les latitudes/longitudes des grandes villes sont contenues dans
        # le dataframe 'villes'
        # on ajoute l'argument group pour pouvoir choisir ou non d'afficher
        # ces marqueurs sur la carte
      addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
        
        # Ajout des noms des grandes villes 
      addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
        
        # Ajout des marqueurs pour les agences CA, l'icône du marqueur est défini dans le scirpt
        # global, il s'agit d'un icône de  banque
      addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
      
        # Permet d'avoir une fenêtre pour contrôler les marqueurs que l'on affiche
      addLayersControl(
        overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
        options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
      ) %>%
      
        # Par défaut les marqueurs des agences CA ne sont pas affichés
      hideGroup("Agences CA")%>%
    
        # Ajout d'une barre de recherche de communes
      addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
        
        # Ajout de la légende de la carte
        # Le format de la légende dépend du type de légende et du type de la variable
        # Lorsque la variable est découpée en quantiles, l'affichage par défaut de leaflet
        # correspond au pourcentage des quantiles et non aux valeurs des quantiles :
        # par exemple pour les quartiles la légende est par défaut :
        # 0-25%, 25%-50%, 50%-75%, 75%-100% 
        # alors qu'on voudrait plutôt les intervalles correspondant aux valeurs des quantiles
        # l'argument labFormat dans la légende permet de gérer ces problèmes
        
      addLegend("bottomleft",
                pal=pal,
                values=values_var,
                labFormat = function(type, cuts, p) {
                suffix = unite
                n = length(cuts)
                  if (type == 'bin'){paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'factor'){
                  paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'numeric'){
                  paste0(round(cuts, 2), suffix)
                  } 
                  else if (type == 'quantile'){
                  paste0(round(cuts[-n]), suffix, " &ndash; ", round(cuts[-1]), suffix)
                  } 
                  else {}
                },
                title = ifelse(nb_quant > 2, "Quantiles", "Valeurs"),
                layerId="colorLegend")

  })
  
  
  
  ###############################################
  #           Données socioéconomiques          #
  ###############################################
  
  
  observeEvent(input$choix_categ_eco, {
    categ = input$choix_categ_eco
    updateSelectInput(session = session, inputId = "choix_var_eco",label = "",
                      choices = dico$Label[dico$Sous_Categ == as.character(categ)])
    
  })

  label_reac_eco = reactive({as.character(input$choix_var_eco)})
  
  var_reac_eco = reactive({dico$Variable[dico$Label == label_reac_eco()]})
  
  col_reac_eco = reactive({as.character(dico$Couleur[dico$Label == label_reac_eco()])})
  
  type_reac_eco = reactive({as.character(dico$Type[dico$Label == label_reac_eco()])})
  
  unite_reac_eco = reactive({ifelse(as.character(type_reac_eco()) == "Pct", " %", "")})

  output$Carte_Eco <- renderLeaflet({
    
    label = label_reac_eco()
    
    var = var_reac_eco()
    
    col = col_reac_eco()
    
    type = type_reac_eco()
    
    unite = unite_reac_eco()
    
    nb_quant = nb_quantile(map_WGS84@data[,var])
    
    x = round(as.numeric(map_WGS84@data[,var] - mean(map_WGS84@data[,var], na.rm = TRUE)),2)
    mean = round(mean(map_WGS84@data[,var], na.rm = TRUE),2)
    color = ifelse(x > 0,"green","red")
    
    if (type == "Pct")
    {popup =  paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                     map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", round(map_WGS84@data[,var],2),unite,
                     "<br><i><font color=",color,">", ifelse(x>0, paste0("+", x), x), 
                     " points</font> par rapport à la moyenne Brie-Picardie ( = ", mean, "% )</i>")}
    else {
      popup =  paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                      map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", round(map_WGS84@data[,var],2),unite,
                      "<br><i><font color=",color,">", ifelse(x>0, paste0("+", x), x), 
                      "</font> par rapport à la moyenne Brie-Picardie ( = ", mean, " )</i>")
    }
    values_var = map_WGS84@data[,var]
    
  
    
    
    if (col == "Clair_Fonce" & nb_quant > 2){
      pal = colorQuantile(Clair_Fonce, n = nb_quant, map_WGS84@data[,var], na.color = "grey")
    } 
    
    else if (col == "Vert_Rouge" & nb_quant > 2){
      pal = colorQuantile(Vert_Rouge, n = nb_quant, map_WGS84@data[,var],na.color = "grey")
    } 
    
    else if (col == "Vert_Rouge" & nb_quant <= 2){ 
      pal = colorNumeric(Vert_Rouge, map_WGS84@data[,var], na.color = "grey") 
    }
    
    else if (col == "Clair_Fonce" & nb_quant <= 2){ 
      pal = colorNumeric(Clair_Fonce, map_WGS84@data[,var], na.color = "grey") 
    }
    
    
    leaflet(map_WGS84) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      setView(lng = 2.468738900000062, lat = 49.19316, zoom = 7) %>%
      clearShapes() %>%
      clearPopups() %>% 
      addPolygons(weight = 1, 
                  color = ~pal(map_WGS84@data[,var]),
                  popup = popup,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>%
      
      addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
      
      addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
      
      addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
      
      addLayersControl(
        overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
        options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
      ) %>%
      
      hideGroup("Agences CA")%>%
      
      addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
      
      addLegend("bottomleft",
                pal=pal,
                values=values_var,
                labFormat = function(type, cuts, p) {
                  suffix = unite
                  n = length(cuts)
                  if (type == 'bin'){paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'factor'){
                    paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'numeric'){
                    paste0(round(cuts, 2), suffix)
                  } else if (type == 'quantile'){
                    paste0(round(cuts[-n],2), suffix, " &ndash; ", round(cuts[-1],2), suffix)
                  } else {}
                },
                title = ifelse(nb_quant > 2, "Quantiles", "Valeurs"),
                layerId="colorLegend")
    
  })
  
  ######################################
  #           Données Logement         #
  ######################################
  
  observeEvent(input$choix_categ_log, {
    categ = input$choix_categ_log
    updateSelectInput(session = session, inputId = "choix_var_log",label = "",
                      choices = dico$Label[dico$Sous_Categ == as.character(categ)])
    
  })
  
  
  label_reac_log = reactive({as.character(input$choix_var_log)})
  
  var_reac_log = reactive({dico$Variable[dico$Label == label_reac_log()]})
  
  col_reac_log = reactive({as.character(dico$Couleur[dico$Label == label_reac_log()])})
  
  type_reac_log = reactive({as.character(dico$Type[dico$Label == label_reac_log()])})
  
  unite_reac_log = reactive({ifelse(as.character(type_reac_log()) == "Pct", " %", "")})
  
  
  output$Carte_log <- renderLeaflet({
    
    label = label_reac_log()
    
    var = var_reac_log()
    
    col = col_reac_log()
    
    type = type_reac_log()
    
    unite = unite_reac_log()
    
    nb_quant = nb_quantile(map_WGS84@data[,var])
    
    x = round(as.numeric(map_WGS84@data[,var] - mean(map_WGS84@data[,var], na.rm = TRUE)),2)
    mean = round(mean(map_WGS84@data[,var], na.rm = TRUE),2)
    color = ifelse(x > 0,"green","red")
    if (type == "Pct")
    {popup =  paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                     map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", round(map_WGS84@data[,var],2),unite,
                     "<br><i><font color=",color,">", ifelse(x>0, paste0("+", x), x), 
                     " points</font> par rapport à la moyenne Brie-Picardie ( = ", mean, "% )</i>")}
    else {
      popup =  paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                      map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", round(map_WGS84@data[,var],2),unite,
                      "<br><i><font color=",color,">", ifelse(x>0, paste0("+", x), x), 
                      "</font> par rapport à la moyenne Brie-Picardie ( = ", mean, " )</i>")
    }
    values_var = map_WGS84@data[,var]
    
    
    
    if (col == "Clair_Fonce" & nb_quant > 2){
      pal = colorQuantile(Clair_Fonce, n = nb_quant, map_WGS84@data[,var], na.color = "grey")
    } 
    

    else { 
      pal = colorNumeric(Clair_Fonce, map_WGS84@data[,var], na.color = "grey") 
    }
    
    
    
    leaflet(map_WGS84) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      setView(lng = 2.468738900000062, lat = 49.19316, zoom = 7) %>%
      clearShapes() %>%
      clearPopups() %>% 
      addPolygons(weight = 1, 
                  color = ~pal(map_WGS84@data[,var]),
                  popup = popup,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>%
      
      addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
      
      addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
      
      addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
      
      addLayersControl(
        overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
        options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
      ) %>%
      
      hideGroup("Agences CA")%>%
      
      
      addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
      
      addLegend("bottomleft",
                pal=pal,
                values=values_var,
                labFormat = function(type, cuts, p) {
                  suffix = unite
                  n = length(cuts)
                  if (type == 'bin'){paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'factor'){
                    paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'numeric'){
                    paste0(round(cuts, 2), suffix)
                  } else if (type == 'quantile'){
                    paste0(round(cuts[-n]), suffix, " &ndash; ", round(cuts[-1]), suffix)
                  } else {}
                },
                title = ifelse(nb_quant > 2, "Quantiles", "Valeurs"),
                layerId="colorLegend")
  })

  
  ######################################
  #           Données Services         #
  ######################################
  
  
  observeEvent(input$choix_categ_services, {
    categ = input$choix_categ_services
    updateSelectInput(session = session, inputId = "choix_var_services",label = "",
                      choices = dico$Label[dico$Sous_Categ == as.character(categ)])
    
  })
  
  
  label_reac_services = reactive({as.character(input$choix_var_services)})
  
  var_reac_services = reactive({dico$Variable[dico$Label == label_reac_services()]})
  
  col_reac_services = reactive({as.character(dico$Couleur[dico$Label == label_reac_services()])})
  
  type_reac_services = reactive({as.character(dico$Type[dico$Label == label_reac_services()])})
  
  unite_reac_services = reactive({ifelse(as.character(type_reac_services()) == "Pct", " %", "")})
  
  
  output$Carte_services <- renderLeaflet({
    
    label = label_reac_services()
    
    var = var_reac_services()
    
    col = col_reac_services()
    
    type = type_reac_services()
    
    unite = unite_reac_services()
    
    x = round(as.numeric(map_WGS84@data[,var] - mean(map_WGS84@data[,var], na.rm = TRUE)),2)
    mean = round(mean(map_WGS84@data[,var], na.rm = TRUE),2)
    color = ifelse(x > 0,"green","red")
    
    if (type == "Abs")
    {popup =  paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                     map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", round(map_WGS84@data[,var],2),unite,
                     "<br><i><font color=",color,">", ifelse(x>0, paste0("+", x), x), 
                     " points</font> par rapport à la moyenne Brie-Picardie ( = ", mean, "% )</i>")}
    else {
      popup =  paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                      map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", map_WGS84@data[,var])
    }
    
    values_var = map_WGS84@data[,var]
    
    if(col == "Clair_Fonce"){
      pal = colorNumeric(Clair_Fonce, map_WGS84@data[,var], na.color = "grey") 
      
    }
    else {
    pal = colorFactor(rainbow(12, start = 0.6, end = 1), map_WGS84@data[,var], na.color = "grey") 
    }
  
    
    
    leaflet(map_WGS84) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      setView(lng = 2.468738900000062, lat = 49.19316, zoom = 7) %>%
      clearShapes() %>%
      clearPopups() %>% 
      addPolygons(weight = 1, 
                  color = ~pal(map_WGS84@data[,var]),
                  popup = popup,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>%
      
      addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
      
      addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
      
      addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
      
      addLayersControl(
        overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
        options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
      ) %>%
      
      hideGroup("Agences CA")%>%
      
      
      addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
      
      addLegend("bottomleft",
                pal=pal,
                values=values_var,
                labFormat = function(type, cuts, p) {
                  suffix = unite
                  n = length(cuts)
                  if (type == 'bin'){paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'factor'){
                    paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'numeric'){
                    paste0(round(cuts, 2), suffix)
                  } else if (type == 'quantile'){
                    paste0(round(cuts[-n]), suffix, " &ndash; ", round(cuts[-1]), suffix)
                  } else {}
                },
                title = "Valeurs",
                layerId="colorLegend")
    
  })
  
  ######################################
  #         Données Numérique         #
  ######################################
  
  
  observeEvent(input$choix_categ_numerique, {
    categ = input$choix_categ_numerique
    updateSelectInput(session = session, inputId = "choix_var_numerique",label = "",
                      choices = dico$Label[dico$Sous_Categ == as.character(categ)])
    
  })
  
  
  label_reac_numerique = reactive({as.character(input$choix_var_numerique)})
  
  var_reac_numerique = reactive({dico$Variable[dico$Label == label_reac_numerique()]})
  
  col_reac_numerique = reactive({as.character(dico$Couleur[dico$Label == label_reac_numerique()])})
  
  type_reac_numerique = reactive({as.character(dico$Type[dico$Label == label_reac_numerique()])})
  
  unite_reac_numerique = reactive({ifelse(as.character(type_reac_numerique()) == "Pct", " %", "")})
  
  output$Carte_numerique <- renderLeaflet({
    
    label = label_reac_numerique()
    
    var = var_reac_numerique()
    
    col = col_reac_numerique()
    
    type = type_reac_numerique()
    
    unite = unite_reac_numerique()
    
    nb_quant = nb_quantile(map_WGS84@data[,var])
    
    x = round(as.numeric(map_WGS84@data[,var] - mean(map_WGS84@data[,var], na.rm = TRUE)),2)
    mean = round(mean(map_WGS84@data[,var], na.rm = TRUE),2)
    color = ifelse(x > 0,"green","red")
    if (type == "Pct")
    {popup =  paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                     map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", round(map_WGS84@data[,var],2),unite,
                     "<br><i><font color=",color,">", ifelse(x>0, paste0("+", x), x), 
                     " points</font> par rapport à la moyenne Brie-Picardie ( = ", mean, "% )</i>")}
    else {
      popup =  paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                      map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", round(map_WGS84@data[,var],2),unite,
                      "<br><i><font color=",color,">", ifelse(x>0, paste0("+", x), x), 
                      "</font> par rapport à la moyenne Brie-Picardie ( = ", mean, " )</i>")
    }
    values_var = map_WGS84@data[,var]
    
    
    
    
    if (col == "Clair_Fonce" & nb_quant > 2){
      pal = colorQuantile(Clair_Fonce, n = nb_quant, map_WGS84@data[,var], na.color = "grey")
    } 
    
    
    else if (col == "Clair_Fonce" & nb_quant <= 2){ 
      pal = colorNumeric(Clair_Fonce, map_WGS84@data[,var], na.color = "grey") 
    }
    
    
    
    leaflet(map_WGS84) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      setView(lng = 2.468738900000062, lat = 49.19316, zoom = 7) %>%
      clearShapes() %>%
      clearPopups() %>% 
      addPolygons(weight = 1, 
                  color = ~pal(map_WGS84@data[,var]),
                  popup = popup,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>%
      
      addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
      
      addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
      
      addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
      
      addLayersControl(
        overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
        options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
      ) %>%
      
      hideGroup("Agences CA")%>%
      
      
      addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
      
      addLegend("bottomleft",
                pal=pal,
                values=values_var,
                labFormat = function(type, cuts, p) {
                  suffix = unite
                  n = length(cuts)
                  if (type == 'bin'){paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'factor'){
                    paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'numeric'){
                    paste0(round(cuts, 2), suffix)
                  } else if (type == 'quantile'){
                    paste0(round(cuts[-n]), suffix, " &ndash; ", round(cuts[-1]), suffix)
                  } else {}
                },
                title = ifelse(nb_quant > 2, "Quantiles", "Valeurs"),
                layerId="colorLegend")
    
  })
  
  #########################################
  #         Données Environnement         #
  #########################################
  
  
  
  observeEvent(input$choix_categ_envir, {
    categ = input$choix_categ_envir
    updateSelectInput(session = session, inputId = "choix_var_envir",label = "",
                      choices = dico$Label[dico$Sous_Categ == as.character(categ)])
    
  })
  
  
  label_reac_envir = reactive({as.character(input$choix_var_envir)})
  
  var_reac_envir = reactive({dico$Variable[dico$Label == label_reac_envir()]})
  
  col_reac_envir = reactive({as.character(dico$Couleur[dico$Label == label_reac_envir()])})
  
  type_reac_envir = reactive({as.character(dico$Type[dico$Label == label_reac_envir()])})
  
  unite_reac_envir = reactive({ifelse(as.character(type_reac_envir()) == "Pct", " %", "")})
  
  
  output$Carte_envir <- renderLeaflet({
    
    label = label_reac_envir()
    
    var = var_reac_envir()
    
    col = col_reac_envir()
    
    type = type_reac_envir()
    
    unite = unite_reac_envir()
    
    popup = paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                     map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", map_WGS84@data[,var])
    
    values_var = map_WGS84@data[,var]

    
    if(type == "Factor"){
      pal = colorFactor(rainbow(12, start = 0.6, end = 1), map_WGS84@data[,var])
    }else if(type == 'Abs'){
      pal = colorNumeric(rainbow(12, start = 0.6, end = 1), map_WGS84@data[,var])
    }

    
    leaflet(map_WGS84) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      setView(lng = 2.468738900000062, lat = 49.19316, zoom = 7) %>%
      clearShapes() %>%
      clearPopups() %>% 
      addPolygons(weight = 1, 
                  color = ~pal(map_WGS84@data[,var]),
                  popup = popup,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>%
      
      addCircleMarkers(radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
      
      
      addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
      
      addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
      
      addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
      
      addLayersControl(
        overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
        options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
      ) %>%
      
      hideGroup("Agences CA")%>%
      
      
      addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
      
      addLegend("bottomleft",
                pal=pal,
                values=values_var,
                labFormat = function(type, cuts, p) {
                  suffix = unite
                  n = length(cuts)
                  if (type == 'bin'){paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'factor'){
                    paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'numeric'){
                    paste0(round(cuts, 2), suffix)
                  } else if (type == 'quantile'){
                    paste0(round(cuts[-n]), suffix, " &ndash; ", round(cuts[-1]), suffix)
                  } else {}
                },
                title = "Valeurs",
                layerId="colorLegend")
    
  })
  
  
  #####################################
  #         Données Transport         #
  #####################################
  
  
  observeEvent(input$choix_categ_transport, {
    categ = input$choix_categ_transport
    updateSelectInput(session = session, inputId = "choix_var_transport",label = "",
                      choices = dico$Label[dico$Sous_Categ == as.character(categ)])
    
  })
  
  
  label_reac_transport = reactive({as.character(input$choix_var_transport)})
  
  var_reac_transport = reactive({dico$Variable[dico$Label == label_reac_transport()]})
  
  col_reac_transport = reactive({as.character(dico$Couleur[dico$Label == label_reac_transport()])})
  
  type_reac_transport = reactive({as.character(dico$Type[dico$Label == label_reac_transport()])})
  
  unite_reac_transport = reactive({ifelse(as.character(type_reac_transport()) == "Pct", " %", "")})
  
  output$Carte_transport <- renderLeaflet({
    
    label = label_reac_transport()
    
    var = var_reac_transport()
    
    col = col_reac_transport()
    
    type = type_reac_transport()
    
    unite = unite_reac_transport()
    
    nb_quant = nb_quantile(map_WGS84@data[,var])
    
    x = round(as.numeric(map_WGS84@data[,var] - mean(map_WGS84@data[,var], na.rm = TRUE)),2)
    mean = round(mean(map_WGS84@data[,var], na.rm = TRUE),2)
    color = ifelse(x > 0,"green","red")

    popup =  paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                      map_WGS84$NOM_IRIS,"<br><strong>",label,"</strong> : ", round(map_WGS84@data[,var],2),unite,
                      "<br><i><font color=",color,">", ifelse(x>0, paste0("+", x), x), 
                      "</font> par rapport à la moyenne Brie-Picardie ( = ", mean, " )</i>")
    
    values_var = map_WGS84@data[,var]
    
    
    
    
    if (col == "Clair_Fonce" & nb_quant > 2){
      pal = colorQuantile(Clair_Fonce, n = nb_quant, map_WGS84@data[,var], na.color = "grey")
    } 
    
    
    else if (col == "Clair_Fonce" & nb_quant <= 2){ 
      pal = colorNumeric(Clair_Fonce, map_WGS84@data[,var], na.color = "grey") 
    }
    
    else {
      pal = colorFactor(rainbow(12, start = 0.6, end = 1), map_WGS84@data[,var], na.color = "grey")
    }
    
    leaflet(map_WGS84) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      setView(lng = 2.468738900000062, lat = 49.19316, zoom = 7) %>%
      clearShapes() %>%
      clearPopups() %>% 
      addPolygons(weight = 1, 
                  color = ~pal(map_WGS84@data[,var]),
                  popup = popup,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>%
      
      addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
      
      addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
      
      addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
      
      addLayersControl(
        overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
        options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
      ) %>%
      
      hideGroup("Agences CA")%>%
      
      
      addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
      
      addLegend("bottomleft",
                pal=pal,
                values=values_var,
                labFormat = function(type, cuts, p) {
                  suffix = unite
                  n = length(cuts)
                  if (type == 'bin'){paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'factor'){
                    paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'numeric'){
                    paste0(round(cuts, 2), suffix)
                  } else if (type == 'quantile'){
                    paste0(round(cuts[-n]), suffix, " &ndash; ", round(cuts[-1]), suffix)
                  } else {}
                },
                title = ifelse(nb_quant > 2, "Quantiles", "Valeurs"),
                layerId="colorLegend")
    
  })
  
 
  #####################################
  #         Données Politique         #
  #####################################
  
  
  observeEvent(input$choix_categ_pol, {
    categ = input$choix_categ_pol
    updateSelectInput(session = session, inputId = "choix_var_pol",label = "",
                      choices = dico$Label[dico$Sous_Categ == as.character(categ)])
    
  })
  
  
  label_reac_pol = reactive({as.character(input$choix_var_pol)})
  
  var_reac_pol = reactive({dico$Variable[dico$Label == label_reac_pol()]})
  
  col_reac_pol = reactive({as.character(dico$Couleur[dico$Label == label_reac_pol()])})
  
  type_reac_pol = reactive({as.character(dico$Type[dico$Label == label_reac_pol()])})
  
  unite_reac_pol = reactive({ifelse(as.character(type_reac_pol()) == "Pct", " %", "")})
  
  
  output$Carte_pol <- renderLeaflet({
    
    label = label_reac_pol()
    
    var = var_reac_pol()
    
    col = col_reac_pol()
    
    type = type_reac_pol()
    
    unite = unite_reac_pol()
    
    
    popup = paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                   map_WGS84$NOM_IRIS,"<br><strong>",label, "</strong> : ", as.character(map_WGS84@data[,var]))
    values_var = map_WGS84@data[,var]
 
    
    
    if (col == "Factor_1") {
      pal = colorFactor(c("orange", "pink", "navy", "red","dodgerblue"), domain = c("Bayrou","Hollande","Le Pen","Mélenchon","Sarkozy"),na.color = "grey")
    }
    
    else if (col == 'Factor_3'){
      pal = colorFactor(c('magenta4','lightskyblue','lightpink',
                          'midnightblue','orangered1','turquoise','salmon',
                          'orangered1','skyblue2'), 
                        domain = c('Debout la France','Divers Droite',
                                   'Divers Gauche','Front National',
                                   'Parti Socialiste','Union de la Droite',
                                   'Union des Démocrates et Indépendants',
                                   'Union de la Gauche',
                                   'Union pour un Mouvement Populaire'),
                        na.color = "grey")
    }
    
    else if (col == 'Factor_4'){
      pal = colorFactor(c('lightskyblue','lightpink','red3',
                          'midnightblue','orangered1','slategray','turquoise','salmon','orange1','skyblue2'), 
                        c('Divers Droite','Divers Gauche','Front de Gauche',
                          'Front National','Parti Socialiste','Sans Etiquette',
                          'Union de la Droite','Union de la Gauche','Union des Démocrates et Indépendants',
                          'Union pour un Mouvement Populaire'),
                        na.color = "grey")
    }
    
    else if (col == 'Factor_5'){
      pal = colorFactor(c('orangered','midnightblue',
                          'turquoise','salmon'), 
                        c('Exaequo','Front National',
                          'Union de la Droite','Union de la Gauche'),
                        na.color = "grey")
    }
    
    else if (col == 'Factor_6'){
      pal = colorFactor(c('lightskyblue','orangered','midnightblue',
                          'turquoise','salmon'), c('Divers Droite','Exaequo',
                                                   'Front National',
                                                   'Union de la Droite','Union de la Gauche'),na.color = "grey")
    }
    
    else {# Factor2
      pal = colorFactor(c("pink", "dodgerblue"), domain = c("Hollande", "Sarkozy"),na.color = "grey")
    }
    
    
    
    leaflet(map_WGS84) %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      setView(lng = 2.468738900000062, lat = 49.19316, zoom = 7) %>%
      clearShapes() %>%
      clearPopups() %>% 
      addPolygons(weight = 1, 
                  color = ~pal(map_WGS84@data[,var]),
                  popup = popup,
                  fillOpacity = 0.7,
                  highlightOptions = highlightOptions(color = "white", weight = 2,
                                                      bringToFront = TRUE)
      ) %>%
      
      addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
      
      addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                       data = villes, lng = villes$Long, lat = villes$Lat, 
                       label = villes$Ville, 
                       labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
      
      addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
      
      addLayersControl(
        overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
        options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
      ) %>%
      
      hideGroup("Agences CA")%>%
      
      
      addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
      
      addLegend("bottomleft",
                pal=pal,
                values=values_var,
                labFormat = function(type, cuts, p) {
                  suffix = unite
                  n = length(cuts)
                  if (type == 'bin'){paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'factor'){
                    paste0(as.character(cuts), suffix)
                  }
                  else if (type == 'numeric'){
                    paste0(round(cuts, 2), suffix)
                  } else if (type == 'quantile'){
                    paste0(round(cuts[-n]), suffix, " &ndash; ", round(cuts[-1]), suffix)
                  } else {}
                },
                title = "Valeurs",
                layerId="colorLegend")
    
  })
  
  
  
        ##################################
        #### COMBINAISON DE VARIABLES ####
        ##################################
  
  # CHOIX CATEGORIE ET SOUS CATEGORIE :
  
  # Update select input après choix de la catégorie 1
  
  observeEvent(input$categ1, {
    
    updateSelectInput(session, "subcateg1", 
                      choices = dico$Sous_Categ[dico$Categorie == input$categ1]
    )
    
  })
  
  # Update select input après choix de la catégorie 2
  
  observeEvent(input$categ2, {
    
    updateSelectInput(session, "subcateg2", 
                      choices = dico$Sous_Categ[dico$Categorie == input$categ2]
    )
    
  })
  
  
  # Update select input après choix sous-catégorie 1
  
  observeEvent(input$subcateg1, {
    updateSelectInput(session, "variable1", 
                      choices = dico$Label[dico$Sous_Categ == input$subcateg1]
    )
  })
  
  # Update select input après choix sous-catégorie 2
  
  observeEvent(input$subcateg2, {
    updateSelectInput(session, "variable2", 
                      choices = dico$Label[dico$Sous_Categ == input$subcateg2]
    )
  })
  
  # REACTIVE VALUES :
  
  # Prend le label de la variable en input et retourne le nom de la variable
  
  var_1 = reactive({
    as.character(dico$Variable[dico$Label == input$variable1])
  })
  
  var_2 = reactive({
    as.character(dico$Variable[dico$Label == input$variable2])
  })
  
  # Prend le label de la variable en input et retourne le type de la variable
  
  type_var_1 = reactive({
    as.character(dico$Type[dico$Label == input$variable1])
  })
  
  type_var_2 = reactive({
    as.character(dico$Type[dico$Label == input$variable2])
  })
  
  # Renvoie le label la variable (écriture plus pratique que input$variable)
  
  label_1 = reactive({
    as.character(input$variable1)
  })
  
  label_2 = reactive({
    as.character(input$variable2)
  })
  
  # WIDGETS :
  
  # Création du widget qui va servir au choix du critère :
  #     - un Slider Input pour les variables quantitatives (sélection d'un intervalle)
  #     - des Radio Buttons pour les variables qualitatives (sélection  d'une modalité)
  
  output$choixcritere1 = renderUI({
    
    var1 = as.character(dico$Variable[dico$Label == input$variable1])
    type1 = as.character(dico$Type[dico$Label == input$variable1])
    
    # Variable qualitative :
    if (is.factor(data_BP[,var1]) == TRUE){
      levels = levels(data_BP[,var1])
      radioButtons(inputId = "choixmoda1",label = "Choix de la modalité",
                   choices = levels)
    }
    # variable quantitative :
    else if (is.numeric(data_BP[,var1]) == TRUE){
      sliderInput("slidervariable1", label = "Choix du 1er critère",
                  min = round(min(data_BP[,var1], na.rm = TRUE)),
                  max = round(max(data_BP[,var1], na.rm = TRUE)),
                  value = c((min(data_BP[,var1], na.rm = TRUE) 
                             + max(data_BP[,var1], na.rm = TRUE))/4,
                            3*(min(data_BP[,var1], na.rm = TRUE) 
                               + max(data_BP[,var1], na.rm = TRUE))/4),
                  sep = " ",
                  post = ifelse(type1 == "Pct", " %", ""))
    }
  })
  
  output$choixcritere2 = renderUI({
    
    var2 = as.character(dico$Variable[dico$Label == input$variable2])
    type2 =  as.character(dico$Type[dico$Label == input$variable2])
    
    # Variable qualitative :
    if (is.factor(data_BP[,var2]) == TRUE){
      levels = levels(data_BP[,var2])
      radioButtons(inputId = "choixmoda2",label = "Choix de la modalité",
                   choices = levels)
    }
    # variable quantitative :
    else if (is.numeric(data_BP[,var2]) == TRUE){
      sliderInput("slidervariable2", label = "Choix du 2nd critère",
                  min = round(min(data_BP[,var2], na.rm = TRUE)),
                  max = round(max(data_BP[,var2], na.rm = TRUE)),
                  value = c((min(data_BP[,var2], na.rm = TRUE) 
                             + max(data_BP[,var2], na.rm = TRUE))/4,
                            3*(min(data_BP[,var2], na.rm = TRUE) 
                               + max(data_BP[,var2], na.rm = TRUE))/4),
                  sep = " ",
                  post = ifelse(type2 == "Pct", " %", ""))
    }
  })
  
  
  # CARTE INTERACTIVE :
  
  # Base qui va servir à tous les cas de figure :
  
  output$Carte_Combi <- renderLeaflet({
    
    leaflet() %>%
      addProviderTiles("CartoDB.PositronNoLabels") %>%
      addProviderTiles("CartoDB.PositronOnlyLabels") %>%
      setView(lng = 2.468738900000062, lat = 49.19316, zoom = 7)
    
  })
  
  # Création de la variable indicatrice qui va servir à colorier les IRIS :
  # Modalités : - 2 critères OK
  #             - 1er critère rempli
  #             - 2ème critère rempli
  #             - Aucun critère rempli
  
  
  indic = reactive({
    
    # Si les deux variables sont numériques :
    
    if(is.numeric(map_WGS84@data[,var_1()]) == TRUE & is.numeric(map_WGS84@data[,var_2()]) == TRUE){
      
      
      ifelse(in_interval(map_WGS84@data[,var_1()], c(input$slidervariable1[1],input$slidervariable1[2])) == TRUE 
             & in_interval(map_WGS84@data[,var_2()], c(input$slidervariable2[1],input$slidervariable2[2])) == TRUE, 
             "2 critères OK",
             
             ifelse((in_interval(map_WGS84@data[,var_1()], c(input$slidervariable1[1],input$slidervariable1[2]))) == FALSE 
                    & (in_interval(map_WGS84@data[,var_2()], c(input$slidervariable2[1],input$slidervariable2[2]))) == TRUE, 
                    "2ème critère rempli",
                    
                    ifelse((in_interval(map_WGS84@data[,var_1()], c(input$slidervariable1[1],input$slidervariable1[2]))) == TRUE
                           & (in_interval(map_WGS84@data[,var_2()], c(input$slidervariable2[1],input$slidervariable2[2]))) == FALSE, 
                           "1er critère rempli",
                           
                           ifelse((in_interval(map_WGS84@data[,var_1()], c(input$slidervariable1[1],input$slidervariable1[2]))) == FALSE 
                                  & (in_interval(map_WGS84@data[,var_2()], c(input$slidervariable2[1],input$slidervariable2[2]))) == FALSE,
                                  "Aucun critère rempli","PB"))))
    }
    
    # Si les variables sont qualitatives :
    
    else if (is.factor(map_WGS84@data[,var_1()]) == TRUE & is.factor(map_WGS84@data[,var_2()]) == TRUE){
      
      ifelse(map_WGS84@data[,var_1()] == input$choixmoda1 & map_WGS84@data[,var_2()] == input$choixmoda2, 
             "2 critères OK",
             
             ifelse(map_WGS84@data[,var_1()] != input$choixmoda1 & map_WGS84@data[,var_2()] == input$choixmoda2, 
                    "2ème critère rempli",
                    
                    ifelse(map_WGS84@data[,var_1()] == input$choixmoda1 & map_WGS84@data[,var_2()] != input$choixmoda2, 
                           "1er critère rempli",
                           
                           "Aucun critère rempli")))
      
    }
    
    # Var1 : quali, Var2 : quanti
    
    else if(is.factor(map_WGS84@data[,var_1()]) == TRUE & is.numeric(map_WGS84@data[,var_2()]) == TRUE){
      
      ifelse(map_WGS84@data[,var_1()] == input$choixmoda1 
             & in_interval(map_WGS84@data[,var_2()], c(input$slidervariable2[1],input$slidervariable2[2])), 
             "2 critères OK",
             
             ifelse(map_WGS84@data[,var_1()] != input$choixmoda1 
                    & in_interval(map_WGS84@data[,var_2()], c(input$slidervariable2[1],input$slidervariable2[2])), 
                    "2ème critère rempli",
                    
                    ifelse(map_WGS84@data[,var_1()] == input$choixmoda1 
                           & xor(map_WGS84@data[,var_2()] < input$slidervariable2[2],map_WGS84@data[,var_2()] > input$slidervariable2[2]), 
                           "1er critère rempli",
                           
                           "Aucun critère rempli")))
      
    }
    
    # Var 1 : quanti, Var2: quali
    
    else if(is.factor(map_WGS84@data[,var_2()]) == TRUE & is.numeric(map_WGS84@data[,var_1()]) == TRUE){
      
      ifelse(map_WGS84@data[,var_2()] == input$choixmoda2 
             & in_interval(map_WGS84@data[,var_1()], c(input$slidervariable1[1],input$slidervariable1[2])), 
             "2 critères OK",
             
             ifelse(map_WGS84@data[,var_2()] != input$choixmoda2 
                    & in_interval(map_WGS84@data[,var_1()], c(input$slidervariable1[1],input$slidervariable1[2])), 
                    "2ème critère rempli",
                    
                    ifelse(map_WGS84@data[,var_2()] == input$choixmoda2 
                           & xor(map_WGS84@data[,var_1()] < input$slidervariable1[2],map_WGS84@data[,var_1()] > input$slidervariable1[2]), 
                           "1er critère rempli",
                           
                           "Aucun critère rempli")))
      
    }
    
  })
  
  # Carte_Combi : Si les deux variables sont numériques
  
    
    observe({
            
      

            label1 = label_1()
            label2 = label_2()
            var1 = var_1()
            var2 = var_2()
            indicatrice = indic()
            type1 = type_var_1()
            type2 = type_var_2()
            
            
            pal = colorFactor(levels = c("2 critères OK", "1er critère rempli",
                                         "2ème critère rempli", "Aucun critère rempli"),
                              palette = Combi, na.color = "grey")
            
            unite1 = ifelse(type1 == "Pct", " %", "")
            unite2 = ifelse(type2 == "Pct"," %","")
      
    
            
              
            if (is.numeric(map_WGS84@data[,var1]) == TRUE & is.numeric(map_WGS84@data[,var2]) == TRUE) 
              
            {
          
              
                  leafletProxy("Carte_Combi", data = map_WGS84) %>%
                    
                    clearShapes() %>%
                    clearPopups() %>% 
                    addPolygons(weight = 1,
                                color = ~pal(indicatrice),
                                fillOpacity = 0.8,
                                highlightOptions = highlightOptions(color = "white", weight = 2,
                                                                    bringToFront = TRUE),
                                popup = paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                                               map_WGS84$NOM_IRIS,"<br><strong>", label1, "</strong> : ", round(map_WGS84@data[,var1],2), unite1,
                                               "<br><strong>", label2, "</strong> : ", round(map_WGS84@data[,var2],2), unite2))%>%
                    
                addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                                 data = villes, lng = villes$Long, lat = villes$Lat, 
                                 label = villes$Ville, 
                                 labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
                
                addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                                 data = villes, lng = villes$Long, lat = villes$Lat, 
                                 label = villes$Ville, 
                                 labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
                
                addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
                
                addLayersControl(
                  overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
                  options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
                ) %>%
                
                hideGroup("Agences CA")%>%
                
                addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
                addLegend("bottomleft", pal = pal, labels = c("2 critères OK", "1er critère rempli",
                                                                  "2ème critère rempli", "Aucun critère rempli"),title="Valeurs",
                              values  = ~indicatrice, layerId="colorLegend", opacity = 1)
                    
            }
            
    })
        
    
      
    
  
  # Carte_Combi : Si les deux variables sont qualitatives 
  
    observe({
    
    label1 = label_1()
    label2 = label_2()
    var1 = var_1()
    var2 = var_2()
    indicatrice = indic()
    type1 = type_var_1()
    type2 = type_var_2()
    
    pal = colorFactor(levels = c("2 critères OK", "1er critère rempli",
                                 "2ème critère rempli", "Aucun critère rempli"),
                      palette = Combi, na.color = "grey")  
    
    unite1 = ifelse(type1 == "Pct", " %", "")
    unite2 = ifelse(type2 == "Pct"," %","")
    
    if (is.factor(map_WGS84@data[,var1]) == TRUE & is.factor(map_WGS84@data[,var2]) == TRUE)
    {
      leafletProxy("Carte_Combi", data = map_WGS84) %>%
        
        clearShapes() %>%
        clearPopups() %>% 
        addPolygons(weight = 1,
                    color = ~pal(indicatrice),
                    fillOpacity = 0.8,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup = paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                                   map_WGS84$NOM_IRIS,"<br><strong>", label1, "</strong> : ", map_WGS84@data[,var1], unite1,
                                   "<br><strong>", label2, "</strong> : ", map_WGS84@data[,var2], unite2))%>%
        
        addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                         data = villes, lng = villes$Long, lat = villes$Lat, 
                         label = villes$Ville, 
                         labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
        
        addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                         data = villes, lng = villes$Long, lat = villes$Lat, 
                         label = villes$Ville, 
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
        
        addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
        
        addLayersControl(
          overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
          options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
        ) %>%
        
        hideGroup("Agences CA")%>%
        
        addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
        addLegend("bottomleft", pal = pal, labels = c("2 critères OK", "1er critère rempli",
                                                      "2ème critère rempli", "Aucun critère rempli"),title="Valeurs",
                  values  = ~indicatrice, layerId="colorLegend", opacity = 1)
      
    }})
  
  # Carte_Combi : Si la variable 1 est qualitative et la variable 2 est quantitative
  
    observe({
    
    label1 = label_1()
    label2 = label_2()
    var1 = var_1()
    var2 = var_2()
    indicatrice = indic()
    type1 = type_var_1()
    type2 = type_var_2()
    
    pal = colorFactor(levels = c("2 critères OK", "1er critère rempli",
                                 "2ème critère rempli", "Aucun critère rempli"),
                      palette = Combi, na.color = "grey")
    
    unite1 = ifelse(type1 == "Pct", " %", "")
    unite2 = ifelse(type2 == "Pct"," %","")
    
    if (is.factor(map_WGS84@data[,var1]) == TRUE & is.numeric(map_WGS84@data[,var2]) == TRUE)
    {
      leafletProxy("Carte_Combi", data = map_WGS84) %>%
        
        clearShapes() %>%
        clearPopups() %>% 
        addPolygons(weight = 1,
                    color = ~pal(indicatrice),
                    fillOpacity = 0.8,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup = paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                                   map_WGS84$NOM_IRIS,"<br><strong>", label1, "</strong> : ", map_WGS84@data[,var1],unite1,
                                   "<br><strong>", label2, "</strong> : ", round(map_WGS84@data[,var2],2), unite2))%>%
        
        addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                         data = villes, lng = villes$Long, lat = villes$Lat, 
                         label = villes$Ville, 
                         labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
        
        addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                         data = villes, lng = villes$Long, lat = villes$Lat, 
                         label = villes$Ville, 
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
        
        addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
        
        addLayersControl(
          overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
          options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
        ) %>%
        
        hideGroup("Agences CA")%>%
        
        addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
        addLegend("bottomleft", pal = pal, labels = c("2 critères OK", "1er critère rempli",
                                                      "2ème critère rempli", "Aucun critère rempli"),title="Valeurs",
                  values  = ~indicatrice, layerId="colorLegend", opacity = 1)
      
    }})
  
  # Carte_Combi : Si la variable 1 est quantitative et la variable 2 est qualitative
  
    observe({
    
    label1 = label_1()
    label2 = label_2()
    var1 = var_1()
    var2 = var_2()
    indicatrice = indic()
    type1 = type_var_1()
    type2 = type_var_2()
    
    pal = colorFactor(levels = c("2 critères OK", "1er critère rempli",
                                 "2ème critère rempli", "Aucun critère rempli"),
                      palette = Combi, na.color = "grey")
    
    unite1 = ifelse(type1 == "Pct", " %", "")
    unite2 = ifelse(type2 == "Pct"," %","")  
    
    if (is.factor(map_WGS84@data[,var2]) == TRUE & is.numeric(map_WGS84@data[,var1]) == TRUE)
    {
      leafletProxy("Carte_Combi", data = map_WGS84) %>%
        
        clearShapes() %>%
        clearPopups() %>% 
        addPolygons(weight = 1,
                    color = ~pal(indicatrice),
                    fillOpacity = 0.8,
                    highlightOptions = highlightOptions(color = "white", weight = 2,
                                                        bringToFront = TRUE),
                    popup = paste0("<strong>Commune: </strong>",map_WGS84$NOM_COM,"<br><strong>Nom de l'IRIS: </strong>",
                                   map_WGS84$NOM_IRIS,"<br><strong>", label1, "</strong> : ",round(map_WGS84@data[,var1],2), unite1,
                                   "<br><strong>", label2, "</strong> : ",map_WGS84@data[,var2], unite2 ))%>%
        
        addCircleMarkers(group = 'Grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                         data = villes, lng = villes$Long, lat = villes$Lat, 
                         label = villes$Ville, 
                         labelOptions = labelOptions(noHide = F, textOnly = TRUE,textsize = '14px')) %>%
        
        addCircleMarkers(group = 'Noms des grandes villes', radius = 2.5, stroke = FALSE, fillColor = 'black', fillOpacity = 1,
                         data = villes, lng = villes$Long, lat = villes$Lat, 
                         label = villes$Ville, 
                         labelOptions = labelOptions(noHide = T, textOnly = TRUE,textsize = '14px')) %>%
        
        addMarkers(group = 'Agences CA', lng = agences$Long, lat = agences$Lat, icon = icon_bank) %>%
        
        addLayersControl(
          overlayGroups = c("Grandes villes", "Noms des grandes villes", "Agences CA"),
          options = layersControlOptions(collapsed = FALSE, position = 'bottomright')
        ) %>%
        
        hideGroup("Agences CA")%>%
        
        addSearchOSM(options = searchOSMOptions(position = 'topleft',autoCollapse = TRUE)) %>%
        addLegend("bottomleft", pal = pal, labels = c("2 critères OK", "1er critère rempli",
                                                      "2ème critère rempli", "Aucun critère rempli"),title="Valeurs",
                  values  = ~indicatrice, layerId="colorLegend", opacity = 1)
      
    }})
  

  
}


