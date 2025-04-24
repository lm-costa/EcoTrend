# Função para carregar os dados e treinar o modelo
load_data_and_train_model <- function(cod_b3, forecasted_min) {
  # Carrega os arquivos de acordo com o código B3 escolhido
  files <- list.files('data', pattern = cod_b3, full.names = T)
  infos <- file.info(files)
  most_recent <- rownames(infos[order(infos$mtime, decreasing = TRUE), ])[1]
  df <- readxl::read_excel(most_recent)

  # Processa os dados
  df_sum <- df |>
    dplyr::group_by(date) |>
    dplyr::summarise(preco = mean(PrecoNegocio))

  df_prepared <- df_sum |>
    timetk::tk_augment_lags(preco, .lags = 1:10) |>
    drop_na()

  # Divide em treino e teste
  split <- rsample::initial_time_split(df_prepared, prop = .7)
  train_data <- rsample::training(split)
  test_data <- rsample::testing(split)

  # Cria o modelo Random Forest
  rec <- recipes::recipe(preco ~ ., data = train_data) |>
    recipes::update_role(date, new_role = "ID") |>
    recipes::step_normalize(recipes::all_predictors())

  rf_spec <- parsnip::rand_forest(trees = 500)  |>
    parsnip::set_engine("ranger")  |>
    parsnip::set_mode("regression")

  rf_workflow <- workflows::workflow() %>%
    workflows::add_recipe(rec) %>%
    workflows::add_model(rf_spec)


  rf_fit <- rf_workflow |>
    parsnip::fit(data = train_data)

  # Previsões
  rf_pred <- predict(rf_fit, new_data = test_data)  |>
    dplyr::bind_cols(test_data)

  future_preds <- forecast_rf_recursive(
    model_fit = rf_fit,
    df_original = df_sum,
    n_ahead = forecasted_min,
    step_seconds = 60,
    lags = 1:10
  )


  # Calcula as métricas R2 e RMSE
  metrics <- yardstick::metrics(rf_pred, truth = preco,
                                estimate = .pred)
  r2 <- metrics |>  filter(.metric == "rsq") |> pull(.estimate)
  rmse <- metrics |> filter(.metric == "rmse") |>  pull(.estimate)

  # Gráfico
  p <- ggplot() +
    geom_line(data = df_sum,
              aes(x = date, y = preco, color = "Real")) +
    geom_line(data = rf_pred,
              aes(x = date, y = preco, color = "Estimado"),
              linetype = "dashed") +
    geom_line(data = future_preds,
              aes(x = date, y = pred, color = "Previsto"),
              linetype = "dashed") +
    labs(title = "Previsão Random Forest",
         x = "Tempo", y = "Preço (R$)") +
    scale_color_manual(name="",
                       breaks = c("Real","Estimado","Previsto"),
                       values=c("Real"='blue',
                                "Estimado"='black',
                                "Previsto"='red'))+
    annotate("text", x = min(df_sum$date), y = min(df_sum$preco)*1.005,
             label = paste("R2: ", round(r2, 2), "\nRMSE: ", round(rmse, 2)),
             hjust = 0, color = "black", size = 5)


  return(p)
}
