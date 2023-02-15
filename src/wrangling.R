################################################################################
# @roman_avj                           09/02/23
# pricing script to solve dd360 challenge | data wrangle
# eda
################################################################################
# libraries
library(tidyverse)
library(stringi)      # regex functions
library(tm)           # for text mining
library(stopwords)    # stop words in spanish
library(SnowballC)    # stemmizer
library(wordcloud)    # world cloud
library(syuzhet)      # sentiment analysis
library(sf)           # spatial analysis

# function ----------------------------------------------------------------
#### helpers ####
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}
#### nlp ####
tidy_text_mining_nlp <- function(s){
  # replace special chars 2 space
  to_rpl <- content_transformer(
    function(x, pattern, ch){
      str_replace_all(
        pattern = pattern, 
        replacement = ch, 
        string = x)
  })
  without_accents <- content_transformer(
    function(x) stri_trans_general(x, id='Latin-ASCII')
  )
  
  # get corpus
  corpus <- Corpus(VectorSource(s))
  
  # remove special chars
  corpus <- tm_map(corpus, to_rpl, '/', ' ')
  corpus <- tm_map(corpus, to_rpl, '\\·', ' ')
  corpus <- tm_map(corpus, to_rpl, '\\.', ' ')
  corpus <- tm_map(corpus, to_rpl, '\\,', ' ')
  corpus <- tm_map(corpus, to_rpl, '²', ' ')
  corpus <- tm_map(corpus, to_rpl, '•', ' ')
  corpus <- tm_map(corpus, content_transformer(tolower)) # convert to lower case
  corpus <- tm_map(corpus, without_accents)

  # transform text
  corpus <- tm_map(corpus, removeNumbers) # remove numbers
  corpus <- tm_map(corpus, removePunctuation) # remove punctuartion
  corpus <- tm_map(corpus, to_rpl, '(?<=\\s)[a-z](?=\\s)', ' ')  # for m2 
  corpus <- tm_map(corpus, to_rpl, '(?<=\\s)id(?=\\s)', ' ')  # for id's
  corpus <- tm_map(corpus, to_rpl, '(?<=\\s)mas(?=\\s)', ' ')  # stopword
  corpus <- tm_map(corpus, removeWords, stopwords(language = 'es', source = 'snowball'))
  corpus <- tm_map(corpus, removeWords, stopwords('spanish'))
  corpus <- tm_map(corpus, stripWhitespace)
  
  # text stemming
  corpus <- tm_map(corpus, function(x) wordStem(x, language = 'spanish')) # try another stemm

    # manual change plural 2 singular
  corpus <- tm_map(corpus, to_rpl, "rom", "roma")
  corpus <- tm_map(corpus, to_rpl, "romaa", "roma")
  corpus <- tm_map(corpus, to_rpl, "nort", "norte")
  corpus <- tm_map(corpus, to_rpl, "nortee", "norte")
  corpus <- tm_map(corpus, to_rpl, "col", "colonia")
  corpus <- tm_map(corpus, to_rpl, "vent", "venta")
  corpus <- tm_map(corpus, to_rpl, "ventaa", "venta")
  corpus <- tm_map(corpus, to_rpl, "coloniaonia", "colonia")
  corpus <- tm_map(corpus, to_rpl, "prevent", "preventa")
  corpus <- tm_map(corpus, to_rpl, "preventaa", "preventa")
  corpus <- tm_map(corpus, to_rpl, "departamentos", "departamento")
  corpus <- tm_map(corpus, to_rpl, "dpto", "departamento")
  corpus <- tm_map(corpus, to_rpl, "depto", "departamento")
  corpus <- tm_map(corpus, to_rpl, "departament", "departamento")
  corpus <- tm_map(corpus, to_rpl, "departamentoo", "departamento")
  corpus <- tm_map(corpus, to_rpl, "roof garden", "roofgarden")
  
  # final white space strip
  corpus <- tm_map(corpus, stripWhitespace)
  
  # return
  return(unlist(as.list(corpus)))
}

important_terms <- function(s){
  text_dtm <- TermDocumentMatrix(s) |> as.matrix()
  
  # sort by freq
  vector_dtm <- sort(rowSums(text_dtm), decreasing = TRUE)
  df_dtm <- tibble(
    word = names(vector_dtm),
    freq = vector_dtm
  )
}

plt_logfreq <- function(df, sbt){
  df |> 
    mutate(
      id_word = row_number(),
      log_freq = log(freq)
    ) |> 
    ggplot(aes(id_word, log_freq)) +
    geom_line(color = "steelblue", linewidth = 0.5) +
    geom_point(color = "steelblue", size = 1, shape = 1) +
    xlab("Rango Palabra") + ylab("Log Frecuencia") +
    theme_bw() +
    ggtitle(
      "Log Frecuencias",
      sbt
    ) +
    theme(
      title = element_text(size = 23),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 17),
      text = element_text(size = 17),
      legend.position = "none"
    )
  
}

sbst_logfreq <- function(df, mini, max){
  df |> 
    filter(
      between(log(freq), mini, max)
    )
}

get_emotion <- function(s){
  # get emotions by columns
  df_sentiment_score <- get_nrc_sentiment(char_v = s, language = 'spanish')
  
  # names
  emotion_names <- colnames(df_sentiment_score)
  
  # emotion
  emotion_index <- apply(df_sentiment_score, 1, which.max)
  no_emotion <- apply(df_sentiment_score, 1, sum)
  
  # get emotion
  emotion_raw <- emotion_names[emotion_index]
  emotion <- ifelse(no_emotion <= 0, 'no_emotion', emotion_raw)
  
  return(emotion)
}

#### spatial analysis ####
read_censo2020 <- function(folder_name){
  # get all the names of the databases
  filenames <- list.files(folder_name, pattern="*.csv", full.names=TRUE)
  # read all the info and append it to a list
  df_work <- do.call(rbind, lapply(filenames, read_mexican_db)) |> 
    # stay with ageb data
    filter(
      str_detect(tolower(nom_loc), pattern = "total ageb urbana")
    )
  
  return(df_work)
} 

tidy_mexican_databases_names <- function(df){
  df_work <- df |> 
    set_names(
      colnames(df) |> 
        str_to_lower() |> 
        stri_trans_general(id='Latin-ASCII') |> #no special chars
        str_replace_all((" |\n"), "_") |> 
        str_remove_all("[\\.,;:]")
    )
  return(df_work)
}

read_mexican_db <- function(file_name, ...){
  df_work <- read_csv(file_name, ...) |> 
    tidy_mexican_databases_names()
}

get_agebs_data <- function(file_name_ageb, file_name_coneval, folder_censo2020){
  # 2022 marco geostadistico
  df_ageb_r <- st_read(
    file_name_ageb, 
    stringsAsFactors=FALSE
  ) 
  
  # coneval 
  df_coneval <- readxl::read_excel(
    file_name_coneval
  ) |> 
    tidy_mexican_databases_names() |> 
    select(
      clave_de_la_ageb, grado_de_rezago_social
    )
  
  # censo 2020
  df_censo2020 <- read_censo2020(folder_censo2020) |> 
    mutate(
      CVEGEO = str_c(entidad, mun, loc, ageb)
    ) |> 
    select(
      CVEGEO,
      pobtot, p_18a24, prom_hnv, pnacent, pnacoe,
      pres2015, presoe15, p18a24a, graproes, pea,
      pocupada, pafil_ipriv, p12ym_solt, p12ym_casa, tothog,
      pobhog, vivtot, tvivhab, tvivpar, vivpar_hab, prom_ocup,
      vph_sincint, vph_autom
    )
  
  # join
  df_main <- df_ageb_r |>
    left_join(
      df_coneval,
      by = c("CVEGEO" = 'clave_de_la_ageb')
    ) |> 
    left_join(
      df_censo2020,
      by = "CVEGEO"
    )
  
  return(df_main)
}

plt_map_points <- function(sf_poly, sf_points, subtitle_name){
  # plot
  ggplot() + 
    geom_sf(data = sf_poly, fill = NA) +
    geom_sf(data = sf_points, size = 1, color = 'red', show.legend = FALSE) +
    labs(x='Longitud', y='Latitud', 
         title='Derpartamentos en Venta',
         subtitle = subtitle_name,
         caption = 'Fuente: Datos del Marco Geostadístico Nacional 2022. INEGI.') +
    theme_minimal() +
    theme(
      title = element_text(size = 23),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      plot.caption = element_text(face = 'italic', size = 8),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 17),
      text = element_text(size = 17),
      legend.position = "none"
    )
}

plt_map_prices <- function(sf_poly, sf_points, subtitle_name){
  # plot
  ggplot() + 
    geom_sf(data = sf_poly, fill = NA) +
    geom_sf(
      data = sf_points, 
      mapping = aes(color = price_square_meter/1e+3),
      size = 1.5, alpha = 0.9
    ) +
    scale_color_viridis_c(option = 'H', name = 'Precio por m2. En miles de pesos.') +
    labs(x='Longitud', y='Latitud', 
         title='Precios de Departamentos en Venta',
         subtitle = subtitle_name,
         caption = 'Fuente: Datos del Marco Geostadístico Nacional 2022. INEGI.') +
    theme_minimal() +
    theme(
      title = element_text(size = 23),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      plot.caption = element_text(face = 'italic', size = 8),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 17),
      text = element_text(size = 17),
      legend.position = "top"
    )
}
# read & wrangle ----------------------------------------------------------
#### read main data ####
df_r <- read_csv("data/reto_precios.csv")

# get agebs with census and coneval polygon data
df_ageb <- get_agebs_data(
  file_name_ageb = "data/inegi/889463770541_s/mg2022_integrado/conjunto_de_datos/00a.shp",
  file_name_coneval = "data/coneval/grs_ageb_urbana_tidy.xlsx",
  folder_censo2020 = "data/inegi/censo2020"
)

# set main df
df_main <- df_r

#### nlp ####
# clean descriptions
df_main <- df_main |>
  mutate(
    main_text_minned = tidy_text_mining_nlp(df_main$main_name),
    description_minned = tidy_text_mining_nlp(df_main$description)
  )

# get doc matrix
main_text_docmatrix <- important_terms(df_main$main_text_minned)
description_docmatrix <- important_terms(df_main$description_minned)

# associations
list_ass_main <- findAssocs(
  TermDocumentMatrix(df_main$main_text_minned),
  terms = c("preventa", "terraza", "tec", "nuevo", "roma"),
  corlimit = 0.25
)
list_ass_descr <- findAssocs(
  TermDocumentMatrix(df_main$description_minned),
  terms = c("roofgarden", "terraza", "lujo", "balcon", "vigilancia",
            "comerciales", "restaurantes", "centros", "elevador", "ubicacion",
            "nocnok"
            ),
  corlimit = 0.4
)

# generate importants covars for descriptions
df_main <- df_main |>
  mutate(
    # for the main text
    main_has_preventa = str_detect(main_text_minned, "preventa") |> as.integer(),
    main_has_roof = str_detect(main_text_minned, "roofgarden|terraza|balcon") |> as.integer(),
    main_has_tecmty = str_detect(main_text_minned, "tec|tecnologico") |> as.integer(),
    main_has_new = str_detect(main_text_minned, "nuevo") |> as.integer(),
    # for the description
    desc_has_roof = str_detect(description_minned, "roofgarden|terraza|balcon") |> as.integer(),
    desc_has_luxury = str_detect(description_minned, "lujo") |> as.integer(),
    desc_has_secure = str_detect(description_minned, "vigilancia") |> as.integer(),
    desc_has_centric = str_detect(description_minned, "comerciales|restaurantes|centros|escuelas|ubicacion") |> as.integer(),
    desc_has_elevator = str_detect(description_minned, "elevador") |> as.integer(),
    desc_has_gym = str_detect(description_minned, 'gimnasios') |> as.integer(),
  )

# generate sentiments & emotions
df_main <- df_main |>
  mutate(
    # sentiments
    sentiment_main_text = get_sentiment(
      char_v = main_text_minned,
      method = 'afinn',
      language = 'spanish'
    ),
    sentiment_description = get_sentiment(
      char_v = description_minned,
      method = 'afinn',
      language = 'spanish'
    ),
    #emotions
    emotion_main_text = get_emotion(main_text_minned),
    emotion_descr = get_emotion(description_minned)
  )

#### vendors & crm's ####
df_main <- df_main |>
  mutate(
    # tidy vendors
    vendor = tolower(vendor),
    vendor = case_when(
      str_detect(vendor, "c21") ~ 'century21',
      str_detect(vendor, "century21") ~ 'century21',
      str_detect(vendor, "coldwell") ~ 'coldwell',
      str_detect(vendor, "remax") ~ 'remax',
      TRUE ~ vendor
    )
  ) |>
  group_by(vendor) |>
  mutate(
    crm = case_when(
      n() >= 30 ~ vendor,
      str_detect(description_minned, 'nocnok') ~ 'nocnok',
      TRUE ~ 'other'
    )
  ) |>
  ungroup() |>
  mutate(
    crm = str_replace_all(crm, " ", "_")
  )
#### spatial analysis ####
# spatial join
df_main_st <- df_main |> 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) |> 
  st_transform(st_crs(df_ageb)) |> 
  st_join(df_ageb)

# get it as a dataframe
df_main <- df_main_st |> st_drop_geometry()

# coarsce values
df_main <- df_main |> 
  mutate(
    across(
      pobtot:vph_autom,
      as.double
    )
  )

#### time analysis ####
df_main <- df_main |> 
  mutate(
    since_days_value = case_when(
      since_period == 'months' ~ since_value * 30,
      since_period == 'years' ~ since_value * 365,
      TRUE ~ since_value
    )
  )
#### refactor ####
df_main <- df_main |> 
  mutate(
    across(
      c(amenities, bathrooms, cellars, num_floors),
      function(x) coalesce(x, 0)
    ),
    is_loft = ifelse(!is.na(department_type), 1, 0)
  )
#### ratios ####
# generate ratios
name_divider <- as.symbol('pobtot')
name_house_divider <- as.symbol('vivtot')

df_main <- df_main |> 
  mutate(
    across(
      p_18a24,
      function(x) x/!!name_divider,
      .names = "{.col}_ratio"
      ),
      across(
        pnacent:p18a24a,
        function(x) x/!!name_divider,
        .names = "{.col}_ratio"
      ),
      across(
        pea:vivtot,
        function(x) x/!!name_divider,
        .names = "{.col}_ratio"
      ),
      across(
        tvivhab:vivpar_hab,
        function(x) x/!!name_house_divider,
        .names = "{.col}_ratio"
      ),
      across(
        vph_sincint:vph_autom,
        function(x) x/!!name_house_divider,
        .names = "{.col}_ratio"
      )
    )

# graphs ------------------------------------------------------------------
dir.create("figures/eda/", recursive = TRUE)
#### nlp ####
dir.create("figures/eda/nlp/", recursive = TRUE)
# main text
set.seed(8)
par(mar=c(1,1,1,1))
wordcloud(
  words = main_text_docmatrix$word,
  freq = main_text_docmatrix$freq,
  max.words = 100,
  scale = c(1, 1),
  random.order = FALSE,
  random.color = FALSE,
  rot.per = 0.25,
  colors = brewer.pal(8, "Dark2")
)

# description text
set.seed(8)
par(mar=c(1,1,1,1))
wordcloud(
  words = description_docmatrix$word,
  freq = description_docmatrix$freq,
  max.words = 100,
  scale = c(1, 1),
  random.order = FALSE,
  random.color = FALSE,
  rot.per = 0.25,
  colors = brewer.pal(8, "Dark2")
)

# bar chars 4 freq
main_text_docmatrix |>
  slice_head(n = 30) |>
  ggplot(aes(reorder(word, freq, sum), freq)) +
  geom_col(fill = 'steelblue', position = 'dodge2') +
  xlab("Palabra") + ylab("Log Frecuencia") +
  theme_bw() +
  scale_y_log10() +
  ggtitle(
    "Frecuencias por palabras. Top30",
    "Título"
  ) +
  coord_flip() +
  theme(
    title = element_text(size = 23),
    axis.text.x = element_text(angle = 90),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 17),
    text = element_text(size = 17),
    legend.position = "none"
  )
ggsave(
  filename = "freq_words_main_text.png",
  path = "figures/eda/nlp/",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

description_docmatrix |>
  slice_head(n = 50) |>
  ggplot(aes(reorder(word, freq, sum), freq)) +
  geom_col(fill = 'steelblue', position = 'dodge2') +
  xlab("Palabra") + ylab("Log Frecuencia") +
  theme_bw() +
  scale_y_log10() +
  ggtitle(
    "Frecuencias por palabras. Top50",
    "Descripción"
  ) +
  coord_flip() +
  theme(
    title = element_text(size = 23),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 17),
    text = element_text(size = 17),
    legend.position = "none"
  )
ggsave(
  filename = "freq_words_descr.png",
  path = "figures/eda/nlp",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

# log frequencies
plt_logfreq(main_text_docmatrix, "Título")
ggsave(
  filename = "log_freq_main_text.png",
  path = "figures/eda/nlp",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)
plt_logfreq(description_docmatrix, "Descripción Texto")
ggsave(
  filename = "log_freq_descr.png",
  path = "figures/eda/nlp",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)
#### eda ####
dir.create("figures/eda/others/", recursive = TRUE)
# crm
df_main |>
  count(crm) |>
  mutate(prop = n/sum(n)) |> 
  ggplot(aes(reorder(crm, prop, sum), prop)) +
  geom_col(fill = 'steelblue', position = 'dodge2') +
  geom_label(aes(y = prop + 0.01, label = str_glue("{round(prop,2)*100}%"))) +
  xlab("CRM") + ylab("Porcentaje") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  coord_flip() +
  ggtitle(
    "CRMs"
  ) +
  theme(
    title = element_text(size = 23),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 17),
    text = element_text(size = 17),
    legend.position = "none"
  )
ggsave(
  filename = "crm_count.png",
  path = "figures/eda/others",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

# emotions
df_main |>
  ggplot(aes(emotion_descr, fill = emotion_descr)) +
  geom_bar(aes(y = ..count../sum(..count..)), position = 'dodge2') +
  scale_y_continuous(labels = scales::percent) +
  xlab("Emoción") + ylab("Proporción") +
  theme_bw() +
  coord_flip() +
  ggtitle(
    "Emoción de Descripciones",
  ) +
  theme(
    title = element_text(size = 23),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 17),
    text = element_text(size = 17),
    legend.position = "none"
  )
ggsave(
  filename = "feelings.png",
  path = "figures/eda/others",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

#### geospatial ####
dir.create("figures/eda/spatial/", recursive = TRUE)
# points #
# bcn
plt_map_points(
  sf_poly = df_ageb |> filter(CVE_ENT == '02'),
  sf_points = df_main_st |> filter(CVE_ENT == '02'),
  subtitle_name = 'Baja California'
)
ggsave(
  filename = "sfpoints_bc.png",
  path = "figures/eda/spatial",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

# cdmx
plt_map_points(
  sf_poly = df_ageb |> filter(CVE_ENT == '09'),
  sf_points = df_main_st |> filter(CVE_ENT == '09'),
  subtitle_name = 'Ciudad de México'
)
ggsave(
  filename = "sfpoints_cdmx.png",
  path = "figures/eda/spatial",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

# nuevo leon
plt_map_points(
  sf_poly = df_ageb |> filter(CVE_ENT == '19'),
  sf_points = df_main_st |> filter(CVE_ENT == '19'),
  subtitle_name = 'Nuevo León'
)
ggsave(
  filename = "sfpoints_nuevleon.png",
  path = "figures/eda/spatial",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

# prices #
# cdmx/cuauhtemoc
plt_map_prices(
  sf_poly = df_ageb |> filter(CVE_ENT == '09', CVE_MUN == '015'),
  sf_points = df_main_st |> filter(CVE_ENT == '09', CVE_MUN == '015'),
  subtitle_name = 'Ciudad de México / Delegación Cuauhtémoc'
)
ggsave(
  filename = "sfprices_cdmx_cuauhtemoc.png",
  path = "figures/eda/spatial",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

# nl/monterrey 
plt_map_prices(
  sf_poly = df_ageb |> filter(CVE_ENT == '19', CVE_MUN == '039'),
  sf_points = df_main_st |> filter(CVE_ENT == '19', CVE_MUN == '039'),
  subtitle_name = 'Nuevo León / Monterrey'
)
ggsave(
  filename = "sfprices_nl_mty.png",
  path = "figures/eda/spatial",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)
# subset-----------------------------------------------------
df_main_s <- df_main |> 
  select(
    # identifiers
    id, price_square_meter,
    # metadata
    timestamp, since_days_value, is_loft, m2, final_price, crm,
    # about home
    amenities, bathrooms, cellars, parking_lots, num_bedrooms,
    main_has_roof:desc_has_gym,
    # about descriptions
    sentiment_main_text, sentiment_description, emotion_main_text, emotion_descr,
    # about location
    CVEGEO, grado_de_rezago_social, contains("ratio"), pobtot, prom_hnv, graproes, prom_ocup,
    # extras
    main_text_minned, description_minned,
    CVE_ENT, CVE_MUN, CVE_LOC, CVE_AGEB,
    
    # identifiers
    main_name, link
  )
# eda ---------------------------------------------------------------
#### crm ####
table_crm <- df_main |> count(crm)
table_counts_has <- df_main |>
  summarise(
    across(contains('_has_'), list(sum = sum, mean = mean))
  ) |>
  pivot_longer(
    cols = main_has_preventa_sum:desc_has_gym_mean,
    names_pattern = "(.+)_(sum|mean)",
    names_to = c("var_name", "stat"),
    values_to = "value"
  ) |>
  mutate(
    type_text = ifelse(str_detect(var_name, "main"), "main_text", "description"),
    dummy_var = str_extract(var_name, "(?<=_)\\w+$")
  ) |>
  select(
    type_text, dummy_var, stat, value
  )

# vendors
table_vendors <- df_r |> count(vendor) |> arrange(vendor)

#### info per spatial entity ####
# distinct states
table_count_cveent <- df_main |> 
  count(CVE_ENT)

# distinct municipalities
table_count_cvemun <- df_main |> 
  count(CVE_ENT, CVE_MUN)

# distinct agebs
table_count_agebs <- df_main |> 
  count(CVE_ENT, CVE_MUN, CVE_LOC, CVE_AGEB) |> 
  arrange(desc(n))

# social lag
table_count_social_lag <- df_main |> 
  count(grado_de_rezago_social)

# data about agebs
table_ent_data <-  df_main |> 
  select(
    CVEGEO, CVE_ENT, 
    p_18a24_ratio:vph_autom_ratio, prom_hnv, graproes, prom_ocup
  ) |> 
  distinct(
    .keep_all = TRUE
  ) |> 
  group_by(CVE_ENT) |> 
  summarise(
    across(.fns = list(
      mean = function(x) mean(x, na.rm = TRUE)
    )
  )) 

# summary of all the data
table_ageb_data <- df_main |> 
  select(
    CVEGEO, CVE_ENT, 
    p_18a24_ratio:vph_autom_ratio, prom_hnv, graproes, prom_ocup
  ) |> 
  distinct(
    .keep_all = TRUE
  ) |> 
  summarise(
    across(.fns = list(
      mean = function(x) mean(x, na.rm = TRUE)
    )
  )) 


#### general ####
# summary
summary(df_main_s)

# timestamp
df_main_s |> 
  ggplot(aes(since_days_value, price_square_meter/1e+4)) +
  geom_point(shape = 1) +
  geom_smooth() +
  geom_hline(yintercept = mean(df_main$price_square_meter/1e+4), color = 'gray70', linetype = 2, linewidth = 0.5) +
  scale_y_continuous(labels = scales::dollar) +
  xlab("Días de Publicación") + ylab("Precio por metro2") +
  theme_minimal() +
  ggtitle(
    "Relación tiempo-precio",
  ) +
  theme(
    title = element_text(size = 23),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 17),
    text = element_text(size = 17),
    legend.position = "none"
  )
ggsave(
  filename = "time_price.png",
  path = "figures/eda/others",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

# amenities
df_main |>
  ggplot(aes(amenities)) +
  geom_bar(aes(y = ..count../sum(..count..)), position = 'dodge2', fill = 'steelblue') +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = 1:8) +
  xlab("Amenidades") + ylab("Proporción") +
  theme_minimal() +
  ggtitle(
    "Número de Amenidades",
  ) +
  theme(
    title = element_text(size = 23),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 17),
    text = element_text(size = 17),
    legend.position = "none"
  )
ggsave(
  filename = "amenities.png",
  path = "figures/eda/others",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

# bathrooms
df_main |>
  ggplot(aes(num_bedrooms)) +
  geom_bar(aes(y = ..count../sum(..count..)), position = 'dodge2', fill = 'steelblue') +
  scale_y_continuous(labels = scales::percent) +
  xlab("Recámaras") + ylab("Proporción") +
  theme_minimal() +
  ggtitle(
    "Número de Recámaras",
  ) +
  theme(
    title = element_text(size = 23),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 17),
    text = element_text(size = 17),
    legend.position = "none"
  )
ggsave(
  filename = "rec.png",
  path = "figures/eda/others",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

# bedrooms
df_main |>
  ggplot(aes(bathrooms)) +
  geom_bar(aes(y = ..count../sum(..count..)), position = 'dodge2', fill = 'steelblue') +
  scale_y_continuous(labels = scales::percent) +
  xlab("Baños") + ylab("Proporción") +
  theme_minimal() +
  ggtitle(
    "Número de Baños",
  ) +
  theme(
    title = element_text(size = 23),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 17),
    text = element_text(size = 17),
    legend.position = "none"
  )
ggsave(
  filename = "baths.png",
  path = "figures/eda/others",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

# parking lots
df_main |>
  ggplot(aes(parking_lots)) +
  geom_bar(aes(y = ..count../sum(..count..)), position = 'dodge2', fill = 'steelblue') +
  scale_y_continuous(labels = scales::percent) +
  xlab("Estacionamiento") + ylab("Proporción") +
  theme_minimal() +
  ggtitle(
    "Número de Estacionamientos",
  ) +
  theme(
    title = element_text(size = 23),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    legend.text = element_text(size = 12),
    legend.title = element_text(size = 17),
    text = element_text(size = 17),
    legend.position = "none"
  )
ggsave(
  filename = "parking_lots.png",
  path = "figures/eda/others",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)


# transform ---------------------------------------------------------------
df_main_s <- df_main_s |> 
  mutate(
    bathrooms = ifelse(bathrooms <= 5, bathrooms, find_mode(bathrooms))
  )


# write -------------------------------------------------------------------
#### nlp ####
dir.create("results/eda/nlp/", recursive = TRUE)
writeLines(paste(names(list_ass_main), list_ass_main, sep = ": "), con = "results/eda/nlp/main_text_ass.txt")
writeLines(paste(names(list_ass_descr), list_ass_main, sep = ": "), con = "results/eda/nlp/descr_ass.txt")

#### others ####
dir.create("results/eda/others", recursive = TRUE)
write_csv(table_crm, "results/eda/others/crm_counts.csv")
write_csv(table_counts_has, "results/eda/others/what_dpts_have.csv")
write_csv(table_vendors, "results/eda/others/vendors_counts.csv")

#### geospatial ####
dir.create("results/eda/spatial/", recursive = TRUE)
write_csv(table_count_cveent, "results/eda/spatial/ent_counts.csv")
write_csv(table_count_cvemun, "results/eda/spatial/mun_counts.csv")
write_csv(table_count_agebs, "results/eda/spatial/ageb_counts.csv")
write_csv(table_count_social_lag, "results/eda/spatial/social_lag_values.csv")
write_csv(table_ent_data, "results/eda/spatial/census_stats.csv")
write_csv(table_ageb_data, "results/eda/spatial/census_stats_by_ageb.csv")


#### main ####
dir.create("results/dbb/", recursive = TRUE)
write_csv(df_main_s |> st_drop_geometry(), "results/dbb/wrangled_database.csv")
# end ---------------------------------------------------------------------




