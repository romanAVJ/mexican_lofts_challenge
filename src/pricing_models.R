################################################################################
# @roman_avj                           09/02/23
# pricing script to solve dd360 challenge | models
# eda
################################################################################
# libraries
library(tidyverse)
library(forecast)
library(tree)
library(randomForest)
library(gbm)
library(glmnet)
library(ggrepel)
library(caret)
# functions ---------------------------------------------------------------
myscale <- function(x)
  return((x-mean(x))/sd(x))

mse <- function(y, yhat){
  mean((y - yhat)^2)
}

plt_errors <- function(fit, y, model_name, model_folder, is_log = FALSE){
  # create table
  if(is_log){
    yhat <- exp(predict(fit))
  }else{
    yhat <- predict(fit)
  }
  
  df_w <- tibble(
    ypred = yhat,
    yobs = y
  ) |> 
  mutate(
    res = yobs - ypred,
    res_stand = myscale(res),
    sqrt_res_stand = myscale(sqrt(res^2))
  )
  
  # some metrics
  mse_model <- mse(df_w$yobs, df_w$ypred)
  ymax <- max(c(df_w$yobs, df_w$ypred))
  ymin <- min(c(df_w$yobs, df_w$ypred))
  
  dir.create(model_folder, recursive = TRUE)
  # obs vs pred
  df_w |> 
    ggplot(aes(ypred, yobs)) +
    geom_point(shape = 1, color = 'steelblue') +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.75, linetype = 2, color = 'gray70') +
    scale_x_continuous(labels = scales::dollar, limits = c(ymin, ymax)) +
    scale_y_continuous(labels = scales::dollar, limits = c(ymin, ymax)) +
    labs(x='Predicción', y='Observado', 
         title=str_glue("Error del modelo {model_name}"),
         subtitle = str_glue('Error cuadrático medio: {round(mse_model, 2)}')
     ) +
    theme_minimal() +
    theme(
      title = element_text(size = 23),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      plot.caption = element_text(face = 'italic', size = 8),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 17),
      text = element_text(size = 17)
    )
  ggsave(
    filename = "obsvspred.png",
    path = model_folder,
    width = 14,
    height = 8,
    device = 'png',
    dpi  = 250
  )
  
  # pred vs err
  df_w |> 
    ggplot(aes(ypred, res)) +
    geom_point(shape = 1, color = 'steelblue') +
    geom_smooth(linewidth = 0.75, linetype = 1, color = 'red') +
    geom_abline(slope = 0, intercept = 0, linewidth = 0.75, linetype = 2, color = 'gray70') +
    scale_x_continuous(labels = scales::dollar, limits = c(ymin, ymax)) +
    scale_y_continuous(labels = scales::dollar, limits = c(ymin, ymax)) +
    labs(x='Predicción', y='Error', 
         title=str_glue("Residuales del modelo {model_name}")
    ) +
    theme_minimal() +
    theme(
      title = element_text(size = 23),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      plot.caption = element_text(face = 'italic', size = 8),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 17),
      text = element_text(size = 17)
    )
  ggsave(
    filename = "errors.png",
    path = model_folder,
    width = 14,
    height = 8,
    device = 'png',
    dpi  = 250
  )

  # qq-plot
  df_w |> 
    ggplot(aes(sample = res)) +
    stat_qq() + stat_qq_line(color = 'gray70', linewidth = 0.75, linetype = 2) +
    labs(x='QQ Teóricos', y='Residuos Estandar', 
         title=str_glue("QQ Plot del modelo {model_name}")
    ) +
    theme_minimal() +
    theme(
      title = element_text(size = 23),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      plot.caption = element_text(face = 'italic', size = 8),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 17),
      text = element_text(size = 17)
    )
  ggsave(
    filename = "qqplot.png",
    path = model_folder,
    width = 14,
    height = 8,
    device = 'png',
    dpi  = 250
  )
    
    # pred vs t-student
    df_w |> 
      ggplot(aes(ypred, sqrt_res_stand)) +
      geom_point(shape = 1, color = 'steelblue') +
      geom_smooth(linewidth = 0.75, linetype = 1, color = 'red') +
      geom_abline(slope = 0, intercept = 0, linewidth = 0.75, linetype = 2, color = 'gray70') +
      labs(x='Predicción', y='Residuales Studentizados', 
           title=str_glue("Residuales Studentizados del modelo {model_name}")
      ) +
      theme_minimal() +
      theme(
        title = element_text(size = 23),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        plot.caption = element_text(face = 'italic', size = 8),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 17),
        text = element_text(size = 17)
      )
    ggsave(
      filename = "resstudent.png",
      path = model_folder,
      width = 14,
      height = 8,
      device = 'png',
      dpi  = 250
    )
}

get_metrics_model <- function(fit, y, model_name, is_log = FALSE){
  # create table
  if(is_log){
    ypred <- exp(predict(fit))
  }else{
    ypred <- predict(fit)
  }
  df_w <- tibble(
    ypred = ypred,
    yobs = y
  ) |> 
  summarise(
    name = model_name,
    r2 = R2(ypred, yobs, formula = 'corr'),
    mse = mse(yobs, ypred),
    rmse = sqrt(mse),
    mae = MAE(ypred, yobs),
    maxerr = max(abs(ypred - yobs)),
    medianerr = median(yobs-ypred)
  )
}

plt_pred_mun <- function(fit, y, model_name, mun, model_folder, is_log = FALSE){
  # create table
  if(is_log){
    yhat <- exp(predict(fit))
  }else{
    yhat <- predict(fit)
  }
  
  df_w <- tibble(
    ypred = yhat,
    yobs = y,
    mun = mun
  ) |> 
    mutate(
      res = yobs - ypred,
      res_stand = myscale(res),
      sqrt_res_stand = myscale(sqrt(res^2)),
      mun = case_when(
        mun == '004' ~ 'Tijuana',
        mun == '010' ~ 'Álvaro Obregón',
        mun == '014' ~ 'Benito Juárez',
        mun == '015' ~ 'Cuauhtémoc',
        mun == '016' ~ 'Miguel Hidalgo',
        mun == '026' ~ 'Guadalupe',
        mun == '039' ~ 'Monterrey',
        TRUE ~ 'other'
      )
    )
  
  # some metrics
  mse_model <- mse(df_w$yobs, df_w$ypred)
  ymax <- max(c(df_w$yobs, df_w$ypred))
  ymin <- min(c(df_w$yobs, df_w$ypred))
  
  dir.create(model_folder, recursive = TRUE)
  # obs vs pred
  df_w |> 
    ggplot(aes(ypred, yobs, color = mun)) +
    geom_point(shape = 1) +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.75, linetype = 2, color = 'gray70') +
    scale_x_continuous(labels = scales::dollar, limits = c(ymin, ymax)) +
    scale_y_continuous(labels = scales::dollar, limits = c(ymin, ymax)) +
    labs(x='Predicción', y='Observado', 
         title=str_glue("Predicciones del modelo {model_name}"),
         subtitle = str_glue('Por municipio y delegación')
    ) +
    theme_minimal() +
    theme(
      title = element_text(size = 23),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      plot.caption = element_text(face = 'italic', size = 8),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 17),
      text = element_text(size = 17),
      legend.position = 'top'
    )
  ggsave(
    filename = "obsvspred_mun.png",
    path = model_folder,
    width = 14,
    height = 8,
    device = 'png',
    dpi  = 250
  )
  
}
# read & wrangle ----------------------------------------------------------
#### read ####
df_r <- read_csv("results/dbb/wrangled_database.csv") |> 
  filter(id != 546) # outlier
# objective var
yvar <- df_r$price_square_meter/1e+3

shapiro.test(scale(yvar))


#### wrangle ####
df_work <- df_r |> 
  mutate(
    price_square_meter = price_square_meter/1e+3,
    locality = str_c(CVE_ENT, CVE_MUN, CVE_LOC)
  ) |> 
  select(
    price_square_meter, m2,
    since_days_value, is_loft, crm,
    amenities, bathrooms, cellars, parking_lots, num_bedrooms,
    main_has_roof:desc_has_gym,
    sentiment_main_text, sentiment_description, emotion_main_text, emotion_descr,
    locality, grado_de_rezago_social, contains("ratio"), pobtot, prom_hnv, graproes, prom_ocup
  ) |> 
  mutate(
    across(
      c(emotion_main_text, emotion_descr, grado_de_rezago_social, locality, crm),
      as.factor
    )
  )

# boxcox transf
bxcx_lambda <- MASS::boxcox(
  lm(df_work$price_square_meter + 3 ~ 1), plotit = TRUE
)

bxcx_lambda_2 <- BoxCox.lambda(df_work$price_square_meter, method = 'loglik')



# null model --------------------------------------------------------------
#### null ####
fit_null <- lm(yvar ~ 1)

summary(fit_null)

mse_null <- mean((predict(fit_null) - yvar)^2)

# summaries
plt_errors(fit_null, yvar, 'Nulo', 'figures/models/null/')
null_metrics <- get_metrics_model(fit_null, yvar, 'modelo nulo')

# random forests ----------------------------------------------------------
#### trail 1 ####
set.seed(8)

# fit model (boosting 4 random forrest)
fit_rf <- gbm(
  formula = price_square_meter ~ ., 
  data = df_work, 
  distribution = 'gaussian',
  n.trees = 200,
  interaction.depth = 5
  )

# summaries
plt_errors(fit_rf, yvar, 'RF Boost', 'figures/models/rf/')
rf_metrics <- get_metrics_model(fit_rf, yvar, 'rf_boost')
table_imp_rf <- summary(fit_rf)

# variable dependencies
dir.create("figures/models/rf/imp", recursive = TRUE)
names_covars <- colnames(df_work)
names_covars <- names_covars[2:length(names_covars)]

# variance importance
i <- 1
for (covar in names_covars){
  png(paste0("figures/models/rf/imp/", i, covar, ".png"))
  print(plot(fit_rf, i = covar))
  dev.off()
  i <- i + 1
}
rm(i)

#### trail 2 ####
set.seed(8)
df_work <- df_r |> 
  mutate(
    deluxe_locality = ifelse(
      str_c(CVE_ENT, CVE_MUN, CVE_LOC) %in% c('090100001', '090150001', '090160001'),
      1,0 
    ),
    price_square_meter = price_square_meter/1e+3,
  ) |> 
  select(
    price_square_meter, m2, crm,
    is_loft, amenities, bathrooms, cellars, parking_lots, num_bedrooms,
    desc_has_roof, main_has_new, desc_has_luxury, deluxe_locality, 
    pobtot, vivpar_hab_ratio, p_18a24_ratio, p18a24a_ratio
  ) |> 
  mutate(
    across(
      c(crm),
      as.factor
    )
  )

# fit model (boosting 4 random forest)
fit_rf2 <- gbm(
  formula = price_square_meter ~ ., 
  data = df_work, 
  distribution = 'gaussian',
  n.trees = 200,
  interaction.depth = 5
)

# summaries
plt_errors(fit_rf2, yvar, 'RF Boost 2', 'figures/models/rf2/')
rf2_metrics <- get_metrics_model(fit_rf2, yvar, 'rf_boost2')
table_imp_rf2 <- summary(fit_rf2)

# variable dependencies
dir.create("figures/models/rf2/imp", recursive = TRUE)
names_covars <- colnames(df_work)
names_covars <- names_covars[2:length(names_covars)]

i <- 1
for (covar in names_covars){
  png(paste0("figures/models/rf2/imp/", i, covar, ".png"))
  print(plot(fit_rf2, i = covar))
  dev.off()
  i <- i + 1
}
rm(i)



# regression tree ---------------------------------------------------------
#### normal ####
set.seed(8)
fit_tree <- tree(
  formula = price_square_meter ~ ., 
  data = df_work 
)

# summaries
plt_errors(fit_tree, yvar, 'Árbol', 'figures/models/tree/')
tree_metrics <- get_metrics_model(fit_tree, yvar, 'tree')

# plot
dir.create("figures/models/tree/", recursive = TRUE)
png(paste0("figures/models/tree/", "tree_decision", ".png"))
plot(fit_tree)
text(fit_tree, pretty = 0)
dev.off()

#### pruned ####
# cv tree (to prune)
fit_cv_tree <- cv.tree(fit_tree)
png(paste0("figures/models/tree/", "cv_tree", ".png"))
plot(fit_cv_tree$size, fit_cv_tree$dev, type = 'b')
dev.off()
# prune 
fit_prune_tree <- prune.tree(fit_tree, best = 6)
png(paste0("figures/models/tree/", "pruned4", ".png"))
plot(fit_prune_tree)
text(fit_prune_tree, pretty = 0)
dev.off()

# summaries
plt_errors(fit_prune_tree, yvar, 'Árbol Recortado', 'figures/models/prune_tree/')
prune_tree_metrics <- get_metrics_model(fit_prune_tree, yvar, 'prune_tree')

# linear regression -------------------------------------------------------
#### get database ####
df_work <- df_r |> 
  mutate(
    deluxe_locality = ifelse(
      str_c(CVE_ENT, CVE_MUN, CVE_LOC) %in% c('090100001', '090150001', '090160001'),
      1,0 
    ),
    price_square_meter = price_square_meter/1e+3,
    pobtot = pobtot/1e+3,
    m2 = m2/1e+2
  ) |> 
  select(
    price_square_meter, m2, crm,
    is_loft, amenities, bathrooms, cellars, parking_lots, num_bedrooms,
    desc_has_roof, main_has_new, desc_has_luxury, deluxe_locality, 
    pobtot, vivpar_hab_ratio, p_18a24_ratio, p18a24a_ratio
  ) |> 
  mutate(
    across(
      c(crm),
      as.factor
    )
  )

#### elastic net #####
set.seed(8)

# get interactions
Xvars <- model.matrix(price_square_meter ~ ., df_work)[,-1]

# cross validation
fit_cvelastic <- cv.glmnet(
  x = Xvars, y = yvar, type.measure = 'mse', nfolds = 20, alpha = 0.5
)

lmbda_min <- fit_cvelastic$lambda.min
lmbda_1se <- fit_cvelastic$lambda.1se

# coefs in best lambda
coef(fit_cvelastic, s = "lambda.min")

# linear lsq
betasbroom <- broom::tidy(fit_cvelastic$glmnet.fit)
betas_elastic_model <- broom::tidy(fit_cvelastic$glmnet.fit) |> 
  filter(near(lambda, lmbda_min)) |> 
  select(term, estimate)

# plot betas
dir.create("figures/models/elasticnet/", recursive = TRUE)
betasbroom |> 
  filter(term != '(Intercept)') |> 
  ggplot(aes(lambda, estimate, color = term)) +
  geom_line() +
  geom_text_repel(
    data = betasbroom |> 
      filter(term != '(Intercept)') |> 
      group_by(term) |> 
      slice_max(order_by = step) |> 
      ungroup(),
    mapping = aes(
      x = min(betasbroom$lambda)*0.6, 
      y = estimate, 
      label = term, 
      color = term
    ),
    size = 4,
    force = 1.5,
    nudge_x = -1
  ) +
  geom_abline(slope = 0, intercept = 0, color = 'gray50', linewidth = 0.5, linetype = 2) +
  geom_vline(xintercept = lmbda_min, color = 'gray50', linewidth = 0.5, linetype = 2) +
  scale_x_continuous(trans = 'log2') +
  labs(
    x='Lambda', y='Beta', 
    title='Decaimiento Penalización Elastic Net',
    subtitle = 'Convexidad 0.5'
  ) +
  theme_bw() +
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
ggsave(
  filename = "betas_decay.png",
  path = "figures/models/elasticnet/",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

#### linear regression ####
# custom model
df_work <- df_r |> 
  mutate(
    deluxe_locality = ifelse(
      str_c(CVE_ENT, CVE_MUN, CVE_LOC) %in% c('090100001', '090150001', '090160001'),
      1,0 
    ),
    price_square_meter = price_square_meter/1e+3,
    pobtot = pobtot/1e+3,
    m2 = m2/1e+2,
    type_crm = case_when(
      crm %in% c('nocnok') ~ 'nocnok',
      crm %in% c('bienesraiceslomelin', 'buscatuhogarmexico', 'zona_ryg', 'rent_a_house_cdmx') ~ 'elite',
      TRUE ~ 'other'
    )
  ) |> 
  select(
    price_square_meter, m2, type_crm,
    is_loft, amenities, bathrooms, cellars, parking_lots, num_bedrooms,
    desc_has_roof, main_has_new, desc_has_luxury, deluxe_locality, 
    pobtot, vivpar_hab_ratio, p_18a24_ratio, p18a24a_ratio
  ) |> 
  mutate(
    across(
      c(type_crm),
      as.factor
    )
  )
# step model
stepaic_fit <- MASS::stepAIC(
  lm(data = df_work, formula = price_square_meter ~ .),
  direction = 'both', trace = FALSE
)
stepaic_fit <- MASS::stepAIC(
  lm(data = df_work, formula = price_square_meter ~ .),
  direction = 'both', trace = FALSE
)
stepaic_fit_multi <- MASS::stepAIC(
  lm(data = df_work, formula = log(price_square_meter) ~ .),
  direction = 'both', trace = FALSE
)
stepaic_fit_loft <- MASS::stepAIC(
  lm(data = df_work, formula = price_square_meter ~ is_loft + . + m2:is_loft + 
       bathrooms:is_loft + parking_lots:is_loft + num_bedrooms:is_loft +
       desc_has_roof:is_loft + main_has_new:is_loft + deluxe_locality:is_loft
     ),
  direction = 'both', trace = FALSE
)


# very linear
fit_lm1 <- lm(
  formula = price_square_meter ~  is_loft  + parking_lots + num_bedrooms  + type_crm +
    desc_has_roof + main_has_new + deluxe_locality + m2,
  data = df_work 
  )

# multi
fit_lm2 <- lm(
  formula = log(price_square_meter) ~  is_loft  + parking_lots + num_bedrooms  + type_crm +
    desc_has_roof + main_has_new + deluxe_locality + m2,
  data = df_work
  )

# log-log
fit_lm3 <- lm(
  formula = log(price_square_meter) ~  is_loft  + log(parking_lots) + log(num_bedrooms)  + type_crm +
    desc_has_roof +  deluxe_locality + log(m2) + main_has_new,
  data = df_work
)

#summary
plt_errors(fit_lm1, yvar, 'LM', 'figures/models/lm1/')
plt_errors(fit_lm2, yvar, 'LM log(y)', 'figures/models/lm_log/', is_log = TRUE)
plt_errors(fit_lm3, yvar, 'LM log-log', 'figures/models/lm_log_log/', is_log = TRUE)
plt_pred_mun(fit_lm3, yvar, 'LM log-log', mun = df_r$CVE_MUN, 'figures/models/lm_log_log/', is_log = TRUE)

lm1_metrics <-  get_metrics_model(fit_lm1, yvar, 'lm')
lm2_metrics <-  get_metrics_model(fit_lm2, yvar, 'lm_log', is_log = TRUE)
lm3_metrics <-  get_metrics_model(fit_lm3, yvar, 'lm_log_log', is_log = TRUE)

# betas
betaslm1 <- broom::tidy(fit_lm1)
betaslm2 <- broom::tidy(fit_lm2)
betaslm3 <- broom::tidy(fit_lm3)

# eda ---------------------------------------------------------------------
#### objective variable ####
df_work |> 
  ggplot(aes(price_square_meter)) +
  geom_histogram(fill = 'steelblue', color = 'black') +
  scale_x_continuous() +
  xlab("Precio") + ylab("Conteo") +
  scale_x_continuous(labels = scales::dollar) +
  theme_bw() +
  labs(
    title = "Precio por metro cuadrado", 
    caption = "Precio en miles de pesos"
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
  filename = "price_square_meter.png",
  path = "figures/eda/others",
  width = 14,
  height = 8,
  device = 'png',
  dpi  = 250
)

table_metrics <- null_metrics |> 
  add_row(rf_metrics) |> 
  add_row(rf2_metrics) |> 
  add_row(prune_tree_metrics) |> 
  add_row(lm1_metrics) |> 
  add_row(lm2_metrics) |> 
  add_row(lm3_metrics)
  
# write -------------------------------------------------------------------
dir.create("results/models/", recursive = TRUE)
write_csv(table_metrics, "results/models/table_metrics_models.csv")

dir.create("results/models/rf", recursive = TRUE)
write_csv(table_imp_rf, "results/models/rf/importance1.csv")
write_csv(table_imp_rf2, "results/models/rf/importance2.csv")

dir.create("results/models/lm", recursive = TRUE)
write_csv(betas_elastic_model, "results/models/lm/betas_elasticnet.csv")
write_csv(betaslm1, "results/models/lm/betas_lm.csv")
write_csv(betaslm2, "results/models/lm/betas_lm_log.csv")
write_csv(betaslm3, "results/models/lm/betas_lm_log_log.csv")








































