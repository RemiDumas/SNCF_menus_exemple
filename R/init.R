
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

graph_caracteristiques <- function(carac, lab, tit, pal) {
  data %>%
    ggplot(aes(fill = Catégorie_produit, x = !!sym(carac))) +
    geom_density() +
    scale_fill_brewer(palette = pal) +
    scale_y_continuous(labels = scales::percent_format(accuracy = .1)) +
    theme_minimal() +
    labs(title = tit, x = lab, y = "en %")
}

# graph_caracteristiques(carac = "Kcal_pour_100_ml_ou_100g", lab = "en Kcal pour 100ml ou 100g", tit = "Apports caloriques des catégories de produits", pal = "Set3")

tab_caracteristiques <- function(carac, lab, tit, col) {
  entete <- glue::glue("**{tit}**\  
                       *({lab})*")
  data %>%
    group_by(Catégorie_produit) %>%
    summarise(Moyenne = mean(!!sym(carac), na.rm = TRUE)) %>%
    bind_rows(data %>% summarise(Moyenne = mean(!!sym(carac), na.rm = TRUE)) %>%
                mutate(Catégorie_produit = "Ensemble")) %>%
    gt(rowname_col = "Catégorie_produit") %>%
    cols_label(Moyenne = md(entete)) %>%
    fmt_number(decimals = 0, locale = "fr") %>%
    tab_style(
      style = list(cell_fill(color = col), cell_text(weight =  "bold")),
      locations = list(
        cells_body(rows = Catégorie_produit == "Ensemble"),
        cells_stub(rows = Catégorie_produit == "Ensemble")
      )
    )
}

# tab_caracteristiques(carac = "Kcal_pour_100_ml_ou_100g", lab = "en Kcal pour 100ml ou 100g", tit = "Apports caloriques moyens", col = "#7993E6")
# tab_caracteristiques(carac = "Protéines_100g_100ml_", lab = "en g pour 100ml ou 100g", tit = "Apports proteïniques moyens", col = "#A24545")
# tab_caracteristiques(carac = "Glucides_100g_100ml_", lab = "en g pour 100ml ou 100g", tit = "Apports en sucres moyens", col = "#14A636")
