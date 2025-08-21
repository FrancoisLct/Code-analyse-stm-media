# ---- 1. CHARGEMENT DES PACKAGES ----
library(readxl)
library(tm)
library(udpipe)
library(quanteda)
library(stm)
library(dplyr)
library(wordcloud)
library(ggplot2)
library(scales)
library(lubridate)
library(patchwork)
library(quanteda.textplots)
library(quanteda.textstats)
library(gridExtra)

# ---- 2. IMPORTATION DES DONNÉES ----
data <- read_excel("C:/Users/francois/Documents/Stm/Base de donnée clean.xlsx")

# Renommer les colonnes proprement
names(data) <- gsub(" ", "", names(data))
names(data) <- gsub("é", "e", names(data))
names(data) <- gsub("è", "e", names(data))
names(data) <- gsub("à", "a", names(data))

# Vérification du nom des colonnes
# print(names(data))

# ---- 3. CONVERSION DE LA DATE ----
# Renommer correctement la colonne si nécessaire
names(data)[names(data) == "date"] <- "Date"
data$Date <- as.Date(data$Date)

# ---- 4. NETTOYAGE DU TEXTE ----
corpus_raw <- data$Content
corpus_clean <- tolower(corpus_raw)
corpus_clean <- gsub("[[:punct:]]", " ", corpus_clean)
corpus_clean <- gsub("[[:digit:]]", " ", corpus_clean)
corpus_clean <- gsub("\\s+", " ", corpus_clean)
corpus_clean <- trimws(corpus_clean)

# ---- 5. LEMMATISATION AVEC UDPIPE ----
ud_model_info <- udpipe_download_model(language = "french")
ud_model <- udpipe_load_model(file = ud_model_info$file_model)

annotations <- udpipe_annotate(ud_model, x = corpus_clean)
anno_df <- as.data.frame(annotations)

lemmatized_text <- anno_df %>%
  filter(!is.na(lemma)) %>%
  filter(nchar(lemma) >= 3) %>%
  group_by(doc_id) %>%
  summarise(text = paste(lemma, collapse = " "))

# ---- 6. CRÉATION DU CORPUS ----
text_lemmatized <- lemmatized_text$text
corp <- corpus(text_lemmatized)
toks <- tokens(corp, remove_punct = TRUE)
toks <- tokens_remove(toks, stopwords("fr"))
toks <- tokens_remove(toks, c("wc", "p", "al", "ceb", "luc", "for", "dr", "agp", "nez", "whire", "beur", "iolig", "etre"))
toks <- tokens_keep(toks, min_nchar = 3)

# ---- 7. CRÉATION DU DFM ET CONVERSION STM ----
dfm <- dfm(toks)
dfm_trimmed <- dfm_trim(dfm, min_termfreq = 5, min_docfreq = 10)
stm_input <- convert(dfm_trimmed, to = "stm")

# ---- 8. AJOUT DES MÉTADONNÉES ----
# On ajoute ici les colonnes utiles
meta_data <- data[, c("Identifiant", "Media", "Date", "Originegeographie", "MedianationalGabonais")]
names(meta_data) <- c("Identifiant", "Media", "Date", "OrigineGeo", "MediaNationalGabonais") 
stm_input$meta <- meta_data
stm_input$meta$OrigineGeoSimplifiee <- ifelse(stm_input$meta$OrigineGeo == "Gabon", "Gabonais", "Non-Gabonais")






table(stm_input$meta$OrigineGeoSimplifiee)


# ---- 9. LANCEMENT DU MODÈLE STM ----
set.seed(420)
stm_model <- stm(
  documents = stm_input$documents,
  vocab = stm_input$vocab,
  data = stm_input$meta,
  K = 10,  # À ajuster selon ton besoin
  init.type = "LDA"
)
labelTopics(stm_model, n = 10)



# ---- ANALYSE DES CONTRIBUTIONS AU TOPIC PAR ORIGINE ----

topic_num <- 10  # change ici le numéro du topic si besoin

# Extraire les proportions du topic
topic_prop <- stm_model$theta[, topic_num]
origines <- stm_input$meta$OrigineGeoSimplifiee

# Créer le dataframe
contrib_data <- data.frame(OrigineGeo = origines, Proportion = topic_prop)

# Calculer les moyennes par origine
topic_share <- contrib_data %>%
  group_by(OrigineGeo) %>%
  summarise(
    n_articles = n(),
    SommeProportion = sum(Proportion),
    PartTopicSurTotalOrigine = mean(Proportion) * 100
  ) %>%
  arrange(desc(PartTopicSurTotalOrigine))




# ---- EXTRACTION ET ANALYSE D'UN TOPIC ----

topic_num <- 10  # Choisis ton topic ici

# 1. Trouver les articles les plus représentatifs
thoughts <- findThoughts(
  stm_model,
  texts = data$Content,
  topics = topic_num,
  n = 10
)

indices <- thoughts$docs[[1]]


# 2. Calcul des contributions pondérées par OrigineGeo
topic_prop <- stm_model$theta[, topic_num]
origines <- stm_input$meta$OrigineGeoSimplifiee

contrib_data <- data.frame(OrigineGeo = origines, Proportion = topic_prop)

# Moyenne pondérée
topic_share <- contrib_data %>%
  group_by(OrigineGeo) %>%
  summarise(
    n_articles = n(),
    SommeProportion = sum(Proportion),
    PartTopicSurTotalOrigine = mean(Proportion) * 100
  ) %>%
  arrange(desc(PartTopicSurTotalOrigine))

print(topic_share)



ggplot(topic_share, aes(x = reorder(OrigineGeo, PartTopicSurTotalOrigine), 
                        y = PartTopicSurTotalOrigine)) +
  geom_col(fill = "black") +
  geom_text(aes(label = sprintf("%.1f%%", PartTopicSurTotalOrigine)), 
            hjust = -0.1, size = 4) +
  coord_flip() +
  labs(title = paste("Part des articles liés au topic", topic_num, "par origine"),
       x = "Origine géographique",
       y = "Part des articles (%)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text(size = 12)) +
  scale_y_continuous(limits = c(0, max(topic_share$PartTopicSurTotalOrigine) * 1.1))


#Evolution des topics dans le temps 

# --- Police Trébuchet (si dispo) ---
if (requireNamespace("showtext", quietly = TRUE)) {
  library(showtext)
  font_add(family = "Trebuchet MS", regular = "Trebuchet MS")
  showtext_auto()
}

topic_num <- 10  # <- change ici le topic à tracer

meta <- stm_input$meta
meta$Proportion <- stm_model$theta[, topic_num]
meta$DateMois   <- floor_date(meta$Date, unit = "month")
meta$Groupe     <- ifelse(meta$Media == "L'Union", "L'Union", "Autres")

# 1) Somme des proportions par mois et par groupe (même norme pour tous)
grp <- meta %>%
  group_by(DateMois, Groupe) %>%
  summarise(TotalVolume = sum(Proportion, na.rm = TRUE), .groups = "drop")

# 2) Série "Tous médias" calculée avec LA MÊME FORMULE
tous <- meta %>%
  group_by(DateMois) %>%
  summarise(TotalVolume = sum(Proportion, na.rm = TRUE), .groups = "drop") %>%
  mutate(Groupe = "Tous médias")

evo3 <- bind_rows(grp, tous) %>%
  mutate(Groupe = factor(Groupe, levels = c("Tous médias","Autres","L'Union")))

# 3) Graphique sans points, uniquement traits
p3 <- ggplot(evo3, aes(x = DateMois, y = TotalVolume, linetype = Groupe)) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.9, span = 0.5) +
  geom_vline(xintercept = as.Date("2019-01-01"), linetype = "dashed",
             color = "black", linewidth = 0.3) +
  scale_linetype_manual(values = c("solid","longdash","dotted"), name = "Série") +
  scale_x_date(date_breaks = "4 months", date_labels = "%b %Y") +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "Date", y = "Volume total des contenus liés au topic") +
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Trebuchet MS"),
    plot.title = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank(),
    legend.title = element_text(face = "bold"),
    legend.position = "right"
  )

print(p3)

# 4) Export vectoriel PDF + SVG
ggsave("evolution_topic_3courbes.pdf", plot = p3,
       device = cairo_pdf, width = 180, height = 120, units = "mm", bg = "white")
if (requireNamespace("svglite", quietly = TRUE)) {
  svglite::svglite("evolution_topic_3courbes.svg", width = 180/25.4, height = 120/25.4)
  print(p3); dev.off()
}





# Choisir le topic à comparer
topic_num <- 10

# Extraire les proportions du topic pour chaque article
topic_prop <- stm_model$theta[, topic_num]

# Ajouter info média
media <- stm_input$meta$Media
is_union <- ifelse(media == "L'Union", "L'Union", "Autres")

# Créer un dataframe
df <- data.frame(Media = is_union, Proportion = topic_prop)

# Calculer les moyennes par groupe
library(dplyr)
comparaison_media <- df %>%
  group_by(Media) %>%
  summarise(
    n_articles = n(),
    MoyenneProportion = mean(Proportion),
    Pourcentage = mean(Proportion) * 100
  )

print(comparaison_media)



ggplot(comparaison_media, aes(x = Media, y = Pourcentage)) +
  geom_col(width = 0.6, fill = "black", show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.1f%%", Pourcentage)), 
            vjust = -0.5, size = 4.5, color = "black") +
  labs(title = paste("Proportion moyenne du topic", topic_num, "par L’Union vs. autres médias"),
       x = "", y = "Part moyenne du topic (%)") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(color = "black")
  )






topic_num <- 10
theta <- stm_model$theta[, topic_num]

# --- Test 1 : L'Union vs Autres ---
meta$GroupeMedia <- ifelse(meta$Media == "L'Union", "L'Union", "Autres")
res_union <- test_permutation(as.factor(meta$GroupeMedia), theta)
df_union  <- data.frame(Diff = as.numeric(res_union$perm_diffs))

ggplot(df_union, aes(x = Diff)) +
  geom_histogram(binwidth = 0.002, fill = "lightgrey", color = "black") +
  geom_vline(xintercept = res_union$observed_diff, color = "black", linetype = "dashed") +
  labs(
    title = paste("Test de permutation – Médias (L’Union vs Autres) (Topic", topic_num, ")"),
    subtitle = paste("Différence observée =", round(res_union$observed_diff, 4),
                     "| p-value =", round(res_union$p_val, 4)),
    x = "Différence des moyennes de proportion",
    y = "Fréquence"
  ) +
  theme_minimal()

# --- Test 2 : Gabonais vs Non-Gabonais ---
# Reconstruire explicitement la variable ici
meta$GroupeOrigine <- ifelse(
  !is.na(meta$OrigineGeo) & grepl("^gabon", trimws(meta$OrigineGeo), ignore.case = TRUE),
  "Gabonais", "Non-Gabonais"
)
meta$GroupeOrigine <- factor(meta$GroupeOrigine, levels = c("Gabonais", "Non-Gabonais"))

res_orig <- test_permutation(as.factor(meta$GroupeOrigine), theta)
df_orig  <- data.frame(Diff = as.numeric(res_orig$perm_diffs))

ggplot(df_orig, aes(x = Diff)) +
  geom_histogram(binwidth = 0.002, fill = "lightgrey", color = "black") +
  geom_vline(xintercept = res_orig$observed_diff, color = "black", linetype = "dashed") +
  labs(
    title = paste("Test de permutation – Origines (Gabonais vs Non-Gabonais) (Topic", topic_num, ")"),
    subtitle = paste("Différence observée =", round(res_orig$observed_diff, 4),
                     "| p-value =", round(res_orig$p_val, 4)),
    x = "Différence des moyennes de proportion",
    y = "Fréquence"
  ) +
  theme_minimal()









# ---- ESTIMATION DES INTERVALLES DE CONFIANCE POUR DEUX VARIABLES ----

# 1. Choisir le topic à analyser
selected_topic <- 10  # Change ici pour le topic que tu veux

# 2. Vérifier que les variables sont bien des facteurs
stm_input$meta$OrigineGeoSimplifiee <- as.factor(stm_input$meta$OrigineGeoSimplifiee)
set.seed(9632)
stm_input$meta$GroupeMedia <- ifelse(stm_input$meta$Media == "L'Union", "L'Union", "Autres")
stm_input$meta$GroupeMedia <- as.factor(stm_input$meta$GroupeMedia)

# 3. Estimer les effets pour OrigineGeoSimplifiee
effet_geo <- estimateEffect(
  formula = selected_topic ~ OrigineGeoSimplifiee,
  stmobj = stm_model,
  metadata = stm_input$meta,
  uncertainty = "Global"
)

# 4. Estimer les effets pour Media (Union vs autres)
effet_media <- estimateEffect(
  formula = selected_topic ~ GroupeMedia,
  stmobj = stm_model,
  metadata = stm_input$meta,
  uncertainty = "Global"
)

# 5. Graphique - Origine géographique
plot(effet_geo, 
     covariate = "OrigineGeoSimplifiee",
     topics = selected_topic, 
     model = stm_model,
     method = "pointestimate",
     xlab = "Proportion estimée du topic",
     main = paste("🧠 Topic", selected_topic, "- Origine géographique"),
     labeltype = "custom",
     custom.labels = levels(stm_input$meta$OrigineGeoSimplifiee))

# 6. Graphique - Média (L'Union vs Autres)
plot(effet_media, 
     covariate = "GroupeMedia",
     topics = selected_topic, 
     model = stm_model,
     method = "pointestimate",
     xlab = "Proportion estimée du topic",
     main = paste("🧠 Topic", selected_topic, "- Média (L'Union vs Autres)"),
     labeltype = "custom",
     custom.labels = levels(stm_input$meta$GroupeMedia))

save(stm_model, stm_input, file = "C:/Users/francois/Documents/Stm/modele_stm.RData")

load("C:/Users/francois/Documents/Stm/modele_stm.RData")





# ====================== VISU TYPE LDAvis (NIVEAUX DE GRIS, SANS TRAITS) ======================
if (!requireNamespace("ggplot2", quietly = TRUE)) stop("ggplot2 requis.")
suppressPackageStartupMessages({
  library(ggplot2)
  library(svglite)
})

# 1) Matrices du modèle
phi <- exp(stm_model$beta$logbeta[[1]])          # K x V
phi <- sweep(phi, 1, rowSums(phi), "/")          # normalisation par sujet
theta <- stm_model$theta                         # D x K
K <- nrow(phi)
prev <- colMeans(theta)

# 2) Étiquettes : 7 mots FREX / sujet
frex_mat <- labelTopics(stm_model, n = 7)$frex
frex_lab <- apply(frex_mat, 1, function(x) paste(x, collapse = ", "))

# 3) Distances inter-sujets (Jensen–Shannon)
jsd <- function(p, q) { m <- 0.5 * (p + q); 0.5 * sum(p * log(p/m)) + 0.5 * sum(q * log(q/m)) }
eps <- 1e-12
phi_eps <- sweep(pmax(phi, eps), 1, rowSums(pmax(phi, eps)), "/")

distJS <- matrix(0, nrow = K, ncol = K)
if (K >= 2) {
  pairs <- utils::combn(K, 2)
  for (c in seq_len(ncol(pairs))) {
    i <- pairs[1, c]; j <- pairs[2, c]
    d <- jsd(phi_eps[i, ], phi_eps[j, ])
    distJS[i, j] <- d; distJS[j, i] <- d
  }
}

# 4) Coordonnées 2D : MDS, sinon PCA
coords <- NULL; mds_ok <- TRUE
if (K >= 2) {
  suppressWarnings({
    tryCatch({
      mds <- cmdscale(as.dist(distJS), k = 2, eig = TRUE)
      coords <- as.data.frame(mds$points); names(coords) <- c("Dim1", "Dim2")
    }, error = function(e) { mds_ok <<- FALSE })
  })
}
if (K < 2 || !mds_ok) {
  pc <- prcomp(phi_eps, center = TRUE)$x[, 1:2, drop = FALSE]
  coords <- as.data.frame(pc); names(coords) <- c("Dim1", "Dim2")
}

coords$Sujet <- factor(seq_len(K))
coords$Prev  <- prev
coords$Frex  <- frex_lab

# 5) Plot en niveaux de gris avec ronds (contour noir) et SANS traits ggrepel
use_repel <- requireNamespace("ggrepel", quietly = TRUE)

p_map <- ggplot(coords, aes(x = Dim1, y = Dim2)) +
  geom_point(aes(size = Prev), shape = 21, fill = "grey60", color = "black") +  # ronds avec contour noir
  {if (use_repel) ggrepel::geom_text_repel(
    aes(label = Sujet),
    box.padding = 0.3, point.padding = 0.2,
    size = 4, fontface = "bold", color = "black",
    max.overlaps = Inf,
    segment.color = NA, segment.size = 0, min.segment.length = Inf
  ) else geom_text(aes(label = Sujet), vjust = -1.1, size = 4, fontface = "bold", color = "black")} +
  {if (use_repel) ggrepel::geom_text_repel(
    aes(label = Frex),
    box.padding = 0.2, point.padding = 0.2,
    size = 3, color = "black",
    max.overlaps = Inf, nudge_y = 0.02,
    segment.color = NA, segment.size = 0, min.segment.length = Inf
  ) else geom_text(aes(label = Frex), vjust = 1.6, size = 3, color = "black")} +
  scale_size_continuous(range = c(3.5, 10), guide = "none") +
  coord_fixed() +  # ratio 1:1 pour éviter distorsion
  theme_minimal(base_size = 16) +  # texte plus lisible
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "grey85"),
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(
    title = "Carte des sujets (STM) – distances Jensen–Shannon",
    x = "Dimension 1",
    y = "Dimension 2"
  )

print(p_map)

# 6) Option : barres de mots p(w|sujet)
plot_bar_sujet <- function(s, n_terms = 15) {
  stopifnot(s >= 1, s <= K)
  pw <- phi[s, ]; ord <- order(pw, decreasing = TRUE)[seq_len(min(n_terms, length(pw)))]
  vocab <- if (!is.null(stm_model$vocab)) stm_model$vocab else stm_input$vocab
  df_b <- data.frame(Terme = factor(vocab[ord], levels = rev(vocab[ord])), Proba = pw[ord])
  ggplot(df_b, aes(x = Terme, y = Proba)) +
    geom_col(fill = "grey40", width = 0.8) +
    coord_flip() +
    theme_minimal(base_size = 13) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "grey85"),
      axis.text  = element_text(color = "black"),
      axis.title = element_text(color = "black"),
      plot.title = element_text(hjust = 0.5, face = "bold")
    ) +
    labs(title = paste0("Mots les plus probables – Sujet ", s),
         x = "Terme", y = "p(terme | sujet)")
}

# 7) Fonctions d'export haute qualité
export_all <- function(plot, basefile = "carte_sujets", w_mm = 160, h_mm = 120, dpi = 600) {
  # PDF vectoriel
  ggplot2::ggsave(paste0(basefile, ".pdf"), plot = plot, device = cairo_pdf,
                  width = w_mm, height = h_mm, units = "mm", bg = "white")
  # SVG vectoriel
  svglite::svglite(paste0(basefile, ".svg"), width = w_mm/25.4, height = h_mm/25.4)
  print(plot)
  dev.off()
  # PNG haute résolution
  if (requireNamespace("ragg", quietly = TRUE)) {
    ggplot2::ggsave(paste0(basefile, "_", dpi, "dpi.png"), plot = plot, device = ragg::agg_png,
                    dpi = dpi, width = w_mm, height = h_mm, units = "mm", bg = "white")
  } else {
    warning("Package 'ragg' non installé : PNG exporté avec moteur par défaut")
    ggplot2::ggsave(paste0(basefile, "_", dpi, "dpi.png"), plot = plot,
                    dpi = dpi, width = w_mm, height = h_mm, units = "mm", bg = "white")
  }
}

# Exemple d'export :
# export_all(p_map)
# Exemple de plot bar :
# print(plot_bar_sujet(1))
