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
library(igraph)
library(ggrepel)

# ---- 2. IMPORTATION DES DONNÉES ----
data <- read_excel("C:/Users/francois/Documents/Stm/Base de donnée clean.xlsx")

# Renommer les colonnes proprement
names(data) <- gsub(" ", "", names(data))
names(data) <- gsub("é", "e", names(data))
names(data) <- gsub("è", "e", names(data))
names(data) <- gsub("à", "a", names(data))


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
toks <- tokens_remove(toks, c("wc", "p", "al", "ceb", "luc", "for", "dr", "agp", "nez", "whire", "beur", "iolig", "etre")) # Adapter en fonction du corpus 
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
labelTopics(stm_model, n = 10) #Visualise les 10 mots les plus représentatifs



# - - - - - - - - - - DEUXIEME PARTIE - - - - - - - - - -


# ---1. Graphique barres : Moyenne de theta par topic avec noms personnalisés ---

# 1) Moyenne de theta par topic
theta <- stm_model$theta
df_bar <- data.frame(
  Topic = paste0("T", seq_len(ncol(theta))),
  Mean  = colMeans(theta, na.rm = TRUE)
)

# 2) Dictionnaire de noms à la place de T1..T10
topic_labels <- c(
  "T1"  = "Arrestation",
  "T2"  = "Recensement",
  "T3"  = "Braconnage",
  "T4"  = "Trafic",
  "T5"  = "Barrière",
  "T6"  = "Attaque",
  "T7"  = "Carbonne",
  "T8"  = "Battue",
  "T9"  = "Consultation",
  "T10" = "Déclin"
)

# 3) Appliquer les libellés tout en conservant l'ordre T1..T10
df_bar <- df_bar %>%
  mutate(TopicName = factor(Topic,
                            levels = names(topic_labels),
                            labels = unname(topic_labels)))

# 4) Plot (barres noires, valeurs au-dessus) + étiquettes inclinées pour éviter le chevauchement
p_bar <- ggplot(df_bar, aes(x = TopicName, y = Mean)) +
  geom_col(fill = "black") +
  geom_text(aes(label = sprintf("%.3f", Mean)), vjust = -0.5, size = 3) +
  labs(x = "Topic", y = "Moyenne de theta") +
  coord_cartesian(ylim = c(0, max(df_bar$Mean) * 1.12)) +
  scale_x_discrete(guide = guide_axis(angle = 45)) +  # <= évite le chevauchement
  theme_minimal(base_size = 14) +
  theme(
    text = element_text(family = "Trebuchet MS"),
    axis.title = element_text(face = "bold"),
    plot.margin = margin(10, 10, 20, 10),            # un peu plus de place en bas
    axis.text.x = element_text(hjust = 1)            # bon ancrage pour l'angle
  )

print(p_bar)



# ----2. ANALYSE DES CONTRIBUTIONS AU TOPIC PAR ORIGINE ----
# - Calcul les moyennes de theta

topic_num <- 10  # changer le numéro du topic si besoin


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

print(topic_share)#Afficher tableau

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



# ----3. Evolution des topics dans le temps — "Tous médias" uniquement, à partir de 2019
topic_num <- 6  # <- change ici le topic à tracer

meta <- stm_input$meta
meta$Proportion <- stm_model$theta[, topic_num]
meta$DateMois   <- floor_date(meta$Date, unit = "month")

# 1) Série "Tous médias" uniquement, filtrée pour commencer en 2019
tous <- meta %>%
  filter(DateMois >= as.Date("2019-01-01")) %>%
  group_by(DateMois) %>%
  summarise(TotalVolume = sum(Proportion, na.rm = TRUE), .groups = "drop")

# 2) Graphique (même style : lissage loess, trait noir, ligne verticale en 2019)
p_tous <- ggplot(tous, aes(x = DateMois, y = TotalVolume)) +
  geom_smooth(method = "loess", se = FALSE, color = "black", linewidth = 0.9, span = 0.5) +
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
    legend.position = "none"
  )

print(p_tous)

# 3) Export vectoriel PDF + SVG
ggsave("evolution_topic_tousmedias_2019.pdf", plot = p_tous,
       device = cairo_pdf, width = 180, height = 120, units = "mm", bg = "white")

if (requireNamespace("svglite", quietly = TRUE)) {
  svglite::svglite("evolution_topic_tousmedias_2019.svg", width = 180/25.4, height = 120/25.4)
  print(p_tous); dev.off()
}




# ----4. Proportion moyenne des topics entre L'Union et les autres médias

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

print(comparaison_media) #Afficher tableau

#Afficher graphique
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




# ----5. Test de permutation

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




# ----6. ESTIMATION DES INTERVALLES DE CONFIANCE POUR DEUX VARIABLES ----

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


# ----7. CARTE JS + LIENS + OFFSETS FORTS (numéros fixes) 
suppressPackageStartupMessages({
  library(ggplot2)
  have_repel <- requireNamespace("ggrepel", quietly = TRUE)
})

# --- Étiquettes MANUELLES par numéro de topic (1..K) ---
# (Doivent correspondre aux numéros affichés sur la carte)
# 1=Arrestation, 2=Recensement, 3=Braconnage, 4=Trafic, 5=Barrière,
# 6=Attaque, 7=Carbone, 8=Battue, 9=Consultation, 10=Déclin
noms_topics <- c("Arrestation","Recensement","Braconnage","Trafic","Barrière",
                 "Attaque","Carbone","Battue","Consultation","Déclin")

# On suppose que 'distJS' et 'coords' existent déjà (créés plus haut)
stopifnot(exists("distJS"), exists("coords"))
K <- nrow(distJS)
stopifnot(length(noms_topics) >= K)

coords$Sujet <- factor(seq_len(K))
coords$Mot   <- noms_topics[as.integer(coords$Sujet)]

# Taille des points = prévalence (si déjà calculée plus haut sous 'prev', on la reprend)
if (!exists("prev")) {
  # prev = prévalence (moyenne de theta par topic) si besoin
  theta <- stm_model$theta
  prev  <- colMeans(theta)
}
coords$Prev <- prev

# ----- Paramètres liens -----
k_neighbors <- 2
use_quantile <- TRUE
q_thresh    <- 0.25
d_thresh    <- 0.50   # ignoré si use_quantile = TRUE

# ----- OFFSETS MANUELS (unités = axes) -----
num_offsets <- list(
  `1` = c(x = -0.020, y =  -0.015),
  `6` = c(x = -0.010, y =  +0.015),
  `8` = c(x =  +0.025, y =  +0.005),
  `2` = c(x =  +0.015, y =  +0.000),
  `7` = c(x =  -0.020, y =  +0.010),
  `4` = c(x =  -0.010, y =  +0.010),
  `3` = c(x =  -0.015, y =  -0.010),
  `5` = c(x =  +0.010, y =  +0.010),
  `9` = c(x =  +0.015, y =  +0.010),
  `10` = c(x =  +0.020, y =  +0.000)
)
frex_offsets <- list(
  `1` = c(x = -0.019, y = +0.020),
  `2` = c(x =  +0.0150, y =  +0.015),
  `7` = c(x =  +0.010, y =  +0.015),
  `5` = c(x =  -0.020, y =   -0.000),
  `8` = c(x =  -0.015, y =   0.015),
  `10` = c(x =  -0.015, y =  -0.025),
  `3` = c(x =  -0.045, y =  +0.015),
  `6` = c(x =  -0.010, y =  -0.015),
  `9` = c(x =  -0.045, y =  -0.015),
  `4` = c(x =  -0.010, y =  -0.010)
)

# ----- Arêtes (réciprocité k-NN + seuil JS) -----
D <- distJS; diag(D) <- Inf
A <- matrix(FALSE, nrow = K, ncol = K)
for (i in 1:K) A[i, head(order(D[i, ]), k_neighbors)] <- TRUE
A_mut <- A & t(A)
pairs <- which(A_mut, arr.ind = TRUE); pairs <- pairs[pairs[,1] < pairs[,2], , drop = FALSE]

edges <- data.frame()
if (nrow(pairs)) {
  d_vals <- mapply(function(a,b) distJS[a,b], pairs[,1], pairs[,2])
  d_cut  <- if (use_quantile) unname(quantile(D[upper.tri(D)], q_thresh, na.rm = TRUE)) else d_thresh
  keep   <- d_vals <= d_cut
  edges  <- data.frame(i = pairs[keep,1], j = pairs[keep,2], d = d_vals[keep])
}
if (nrow(edges)) {
  ln2 <- log(2)
  edges$sim  <- pmax(0, 1 - pmin(edges$d, ln2)/ln2)
  edges$x    <- coords$Dim1[edges$i];  edges$y    <- coords$Dim2[edges$i]
  edges$xend <- coords$Dim1[edges$j];  edges$yend <- coords$Dim2[edges$j]
}

# ----- Appliquer offsets -----
coords$nudge_num_x  <- 0; coords$nudge_num_y  <- 0
coords$nudge_frex_x <- 0; coords$nudge_frex_y <- 0
apply_offsets <- function(df, lst, fx, fy){
  if (length(lst)) for (id in names(lst)) {
    idx <- which(as.integer(df$Sujet) == as.integer(id))
    if (length(idx)) { df[idx, fx] <- lst[[id]]["x"]; df[idx, fy] <- lst[[id]]["y"] }
  }
  df
}
coords <- apply_offsets(coords, num_offsets,  "nudge_num_x",  "nudge_num_y")
coords <- apply_offsets(coords, frex_offsets, "nudge_frex_x", "nudge_frex_y")

coords_num_fixed  <- transform(coords,  x_lab = Dim1 + nudge_num_x,  y_lab = Dim2 + nudge_num_y)
coords_frex_start <- transform(coords,  x_lab = Dim1 + nudge_frex_x, y_lab = Dim2 + nudge_frex_y)

# ----- Plot (étiquette = noms_topics, SANS traits de fond) -----
p_js_links <- ggplot(coords, aes(Dim1, Dim2)) +
  { if (nrow(edges))
    geom_segment(data = edges,
                 aes(x = x, y = y, xend = xend, yend = yend,
                     linewidth = sim, alpha = sim),
                 inherit.aes = FALSE, color = "grey25", lineend = "round")
    else NULL } +
  scale_linewidth(range = c(0.6, 3.2), guide = "none") +
  scale_alpha(range = c(0.30, 0.9), guide = "none") +
  geom_point(aes(size = Prev), shape = 21, fill = "grey60", color = "black") +
  geom_text(data = coords_num_fixed, aes(x = x_lab, y = y_lab, label = Sujet),
            size = 4, fontface = "bold", color = "black") +
  { if (have_repel) ggrepel::geom_text_repel(
    data = coords_frex_start, aes(x = x_lab, y = y_lab, label = Mot),
    box.padding = 0.25, point.padding = 0.1, force = 1.5, max.time = 1,
    size = 3, color = "black", max.overlaps = Inf, seed = 456,
    segment.color = NA, segment.size = 0, min.segment.length = Inf
  ) else geom_text(data = coords_frex_start, aes(x = x_lab, y = y_lab, label = Mot),
                   size = 3, color = "black", vjust = 1.6) } +
  scale_size_continuous(range = c(3.5, 10), guide = "none") +
  coord_fixed() +
  theme_minimal(base_size = 16) +
  theme(
    panel.grid.major = element_blank(),   # retire la grille
    panel.grid.minor = element_blank(),   # retire la grille
    axis.text  = element_text(color = "black"),
    axis.title = element_text(color = "black"),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(
    title = "Carte des topics (JS) + liens réciproques",
    x = "Dimension 1", y = "Dimension 2",
    caption = if (nrow(edges)) {
      paste0("Liens : réciprocité k=", k_neighbors, " et distance JS ≤ Q",
             round(q_thresh*100), "% (",
             signif(if (exists("d_cut")) d_cut else unname(quantile(D[upper.tri(D)], q_thresh, na.rm = TRUE)), 3),
             ")")
    } else "Aucun lien ne satisfait la réciprocité + seuil."
  )

print(p_js_links)

