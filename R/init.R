
# Packages ----------------------------------------------------------------

library('dplyr')
library('questionr')
library('ggplot2')
library('gt')


# Données -----------------------------------------------------------------

data <- read.csv2("menus_tgv.csv")

# Thèmes et couleurs ------------------------------------------------------

palettes <- 
  list(
    categorie = c('Boissons' = 'slateblue', 'Salé' = 'bisque1', 'Sucré' = 'chocolate1'),
    ouinon = c('OUI' = 'palegreen', 'NON' = "salmon")
  )


# Fonctions ---------------------------------------------------------------
tableau_categorie <- function(cat){
  data %>% 
    filter(Catégorie_produit == cat) %>% 
    summarise(Nombre = n(),
              'Prix moyen' = mean(Prix_au_produit, na.rm = TRUE),
              'Pourcentage de BIO' = sum(BIO == "OUI", na.rm = TRUE)/Nombre,
              'Taux moyen de Glucides (en g/ml)' = mean(Glucides_100g_100ml_, na.rm = TRUE)) %>% 
    gt() %>% 
    fmt_currency('Prix moyen', locale = "fr", placement = "right") %>% 
    fmt_percent('Pourcentage de BIO', decimals = 1, locale = "fr") %>% 
    fmt_number('Taux moyen de Glucides (en g/ml)', decimals = 1, locale = 'fr')
}

  
