my_basic <- function(cod_b3){
  files <- list.files('data', pattern = cod_b3, full.names = T)
  infos <- file.info(files)
  most_recent <- rownames(infos[order(infos$mtime, decreasing = TRUE), ])[1]
  df <- readxl::read_excel(most_recent)

  df_sum <- df |>
    dplyr::mutate(
      time1h= lubridate::ceiling_date(date,"60 minutes")
    ) |>
    group_by(time1h) |>
    dplyr::summarise(preco_medio=mean(PrecoNegocio),
                     preco_dp=sd(PrecoNegocio),
                     preco_cv=(preco_dp/preco_medio)*100,
                     )


  return(df_sum)
}

