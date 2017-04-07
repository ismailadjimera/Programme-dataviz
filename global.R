
# Installation des librairies, chargement des données, traitement préablable des données,
# construction de la base de données finale contenant les contours + les données à représenter
# création de divers objets pour la carte, créations de fonctions  outils utiles

setwd('S:/2403/3509/T8 - FICHIERS PERSO/Ismaila/Shiny ILB/Application')

  #################################
  #  Installation des librairies  #
  #################################

# Librairies à installer/charger:
list.of.packages = c('readr','rgdal','sp','data.table','leaflet')

# # Packages à installer
# new.packages = list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# if(length(new.packages)){install.packages(new.packages)}

# Chargement des librairies
lapply(list.of.packages, require, character.only = TRUE)


  ############################
  #  Chargement des données  #
  ############################

# Ouverture du Shapefile des contours IRIS
contours_IRIS <- readOGR(
  dsn = "contours_IRIS_BP.shp",
  layer = "contours_IRIS_BP",
  verbose = FALSE,
  encoding = 'UTF-8'
)


# Ouverture des données à représenter sur la carte
data_BP = read_delim(
  "base_finale.csv",
  ",",
  na = "empty",
  quote = "\"",
  locale = locale(encoding = 'windows-1252')
)

# Ouverture du dictionnaire des variables (table qui donne pour chaque
# variable sa catégorie/sous-catégorie/description et ses attributs pour la représentation sur
# la carte (type, couleur à utliser))
dico = read_delim(
  'dico_variables.csv',
  ',' ,
  quote = "\"",
  locale = locale(encoding = 'windows-1252'),
  escape_double = TRUE,
  na = "empty",
  trim_ws = TRUE
)

# Chargement des marqueurs
villes = read_delim('Villes.csv', ";", locale = locale(encoding = 'windows-1252')) # Grandes villes
agences = read_delim('Agences.csv', ";", locale = locale(encoding = 'windows-1252')) # Agences CA
#criminalite = read_delim('criminalite_BP.csv', ',')


    ######################################
    #  Traitement préalable des données  #
    ######################################


# Création de la liste des variables à représenter sur la carte 
liste = c('IRIS', dico$Variable)

# On enlève de la base de données les variables à ne pas représenter sur la carte
data_BP = data_BP[, liste[liste %in% names(data_BP)]]


## Valeurs problématiques pour la représentation sur la carte (Inf, Outliers) :

# On remplace les 'Inf' par des 0 :
data_BP = do.call(data.frame, lapply(data_BP, function(x)
  replace(x, is.infinite(x), 0)))

# Fonction pour remplacer les outliers par des NA
remove_outliers = function(x,
                           na.rm = TRUE,
                           probs = c(.25, .75),
                           ...) {
  qnt = quantile(x, probs, na.rm = na.rm, ...)
  H = 1.5 * IQR(x, na.rm = na.rm)
  y = x
  y[x < (qnt[1] - H)] = NA
  y[x > (qnt[2] + H)] = NA
  y
}

  # Liste des variables contenant des outliers : (les variables de taux de  variation)
list_var_outliers = c('croissance_demo_0613', 'croissance_demo_1213', 
                      'evo_taux_cho_1213_1564','evo_taux_cho_1213_1524',
                      'evo_taux_cho_0613_1564', 'evo_taux_cho_0613_1524')

  # Application de la fonction
data_BP[list_var_outliers] = lapply(data_BP[list_var_outliers], remove_outliers, probs = c(0.01, 0.99))


# ?
indx = which(sapply(data_BP, is.factor))
for (j in indx){
  set(data_BP,
      i = grep("^$|^ $", data_BP[[j]]),
      j = j,
      value = NA_integer_)}


    ###############################################
    #  Construction de la base de données finale  #
    ###############################################


# On merge les données avec le fond de carte :
map_WGS84 = spTransform(
  merge(contours_IRIS, data_BP, by.x = 'CODE_IRIS', by.y = 'IRIS'),
  CRS("+init=epsg:4326")
)

# Correction de nom :
names(map_WGS84)[names(map_WGS84) == "TYP_IRIS.x"] <- "TYP_IRIS"


    ############################################
    #  Création d'objets utiles pour la carte  #
    ############################################


    # Créations de listes pour les menus déroulants :

# Listes des sous-catégories :

liste_categ = unique(dico$Categorie)

# Liste des variables par sous-catégorie :

# Données sociodémographiques
liste_sous_categ_pop = levels(as.factor(dico$Sous_Categ[dico$Categorie == 'Donn�es sociodémographiques']))[c(4, 3, 1, 2, 6, 5)]
liste_Population = dico$Label[dico$Sous_Categ == 'Population']

# Données socio-économiques
liste_sous_categ_socioeco = levels(as.factor(dico$Sous_Categ[dico$Categorie == 'Données socioéconomiques']))

# Logement
liste_sous_categ_log = levels(as.factor(dico$Sous_Categ[dico$Categorie == 'Logement']))[c(1, 4, 3, 2)]

# Services
liste_sous_categ_serv = levels(as.factor(dico$Sous_Categ[dico$Categorie == 'Services']))

# Numérique
liste_sous_categ_numeric = levels(as.factor(dico$Sous_Categ[dico$Categorie == 'Numérique']))

# Environnement
liste_sous_categ_environnement = levels(as.factor(dico$Sous_Categ[dico$Categorie == 'Environnement']))

# Politique
liste_sous_categ_pol = unique(dico$Sous_Categ[dico$Categorie == 'Politique'])

# Transport
liste_sous_categ_transp = unique(dico$Sous_Categ[dico$Categorie == 'Transport'])

# Flux de personnes
liste_sous_categ_flux = unique(dico$Sous_Categ[dico$Categorie == 'Flux de personnes'])


    # Palettes de couleur:

# Palette du jaune au bordeaux :
Clair_Fonce = c('#FFC300',
                '#FF5733',
                '#C70039',
                '#900C3F')

# Palette du rouge au vert :
Rouge_Vert = c('#ff4c00',
               '#ff8900',
               '#ffd900',
               '#b7ff00',
               '#1acf2c')

# Palette du vert au rouge :
Vert_Rouge = rev(c('#ff4c00',
                   '#ff8900',
                   '#ffd900',
                   '#b7ff00',
                   '#1acf2c'))

# Palette pour la combinaison de variables :
Combi = c("#60E5D3", "#FFDD38", "#FFAD2B", "#FF614C")

# Icônes pour les agences:

icon_bank = makeIcon("bank-building.png", 9, 9)

    

    ###############################################
    #  Création de fonctions utiles pour la carte #
    ###############################################


  # Fonction qui renvoie TRUE si x appartient à intervalle :
  # Utile pour l'onglet de combinaison de variables avec filtres sur les variables

in_interval = function(x, interval) {
  {
    #stopifnot(length(interval) == 2L)
    interval[1] <= x & x <= interval[2]
  }
}


  # Fonction qui renvoie le nombre maximum de quantiles entre 1 et 5
  # à calculer pour chaque variable. En effet, parfois la distribution 
  # d'une variable est telle que plusieurs quantiles peuvent être égaux 
  # (par exemple si la variable est continue mais qu'elle ne prend 
  # que très peu de valeurs différentes)
  # Dans ce cas le découpage en quantiles n'a plus de sens et donc :
  # Si la fonction renvoie un nombre de quantiles maximal inférieur à 2, 
  # on va choisir une légende de type ColorNumeric et non ColorQuantile.


nb_quantile = function(var) {
  quantileNum = 1
  probs = seq(0, 1, length.out = quantileNum)
  bins = quantile(var, probs = probs, na.rm = TRUE)
  continue = TRUE
  while (continue) {
    quantileNum = quantileNum + 1
    probs = seq(0, 1, length.out = quantileNum + 1)
    bins = quantile(var, probs = probs, na.rm = TRUE)
    if (length(bins) > length(unique(bins)) | quantileNum + 1 > 6) {
      continue = FALSE
      quantileNum = quantileNum - 1
    }
  }
  return(quantileNum)
}


