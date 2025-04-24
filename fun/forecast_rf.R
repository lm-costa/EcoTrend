forecast_rf_recursive <- function(model_fit, df_original, n_ahead = 12, step_seconds = 5, lags = 1:10) {

  # Garantir que a variável de preços é chamada de "preco" no dataframe original
  last_known <- df_original %>% tail(max(lags))  # últimos valores conhecidos
  preds <- numeric(n_ahead)
  timestamps <- seq(
    from = max(df_original$date) + step_seconds,
    by = step_seconds,
    length.out = n_ahead
  )

  for (i in seq_len(n_ahead)) {
    # Criar vetor com os lags mais recentes (valores reais ou previsão anterior)
    lag_values <- rev(tail(last_known$preco, max(lags)))  # Usando 'preco' no lugar de 'valor'

    # Criar um novo dataframe com os lags, mas sem a variável 'preco' ainda
    new_data <- tibble(date = timestamps[i])

    # Criar dinamicamente as colunas de lags
    for (lag in lags) {
      new_data[[paste0("preco_lag", lag)]] <- lag_values[lag]
    }

    # Fazer a previsão
    pred <- predict(model_fit, new_data = new_data)$.pred

    # Guardar o valor da previsão
    preds[i] <- pred

    # Adicionar ao histórico para que seja usado como lag em passos futuros
    last_known <- bind_rows(last_known, tibble(date = timestamps[i], preco = pred))
  }

  # Retornar data.frame com os tempos e as previsões
  tibble(
    date = timestamps,
    pred = preds
  )
}
