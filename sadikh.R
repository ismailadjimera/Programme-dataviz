if (!require(plotly)){install.packages("haven")} ; library(haven) 

## Import en R si tu as téléchargé le doc sav
merged_r5_data <- read_sav("C:/Users/X097430/Desktop/Script R/merged_r5_data.sav") # mets ton repertoire ici

## Import en R si tu as depuis internet du doc sav il faut choisir l'un ou l'autre
merged_r5_data <- read_sav("http://afrobarometer.org/sites/default/files/data/round-5/merged_r5_data.sav")

# Export en CSV par contre tu n'auras pas les métadonnées de description des variables
write.csv(merged_r5_data,"C:/Users/X097430/Desktop/Script R/afrobaro.csv")# met ton repertoire ici