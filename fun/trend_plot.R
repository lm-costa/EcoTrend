trend_plot <- function(cod_b3){
  files <- list.files('data', pattern = cod_b3, full.names = T)
  infos <- file.info(files)
  most_recent <- rownames(infos[order(infos$mtime, decreasing = TRUE), ])[1]
  df <- readxl::read_excel(most_recent)

  df_sum <- df |>
    dplyr::group_by(date) |>
    dplyr::summarise(preco = mean(PrecoNegocio))

  p <- df_sum |>
    ggplot(aes(x=date,y=preco)) +
    geom_line()+
    geom_smooth(method='lm')+
    ggpmisc::stat_poly_eq(formula = y ~ x,
                          ggplot2::aes(label = paste(..eq.label..,
                                                     ..rr.label..,
                                                     ..p.value.label..,
                                                     sep = "*`,`~")),
                          label.y = 0.01,
                          parse = TRUE
    )+
    labs(x = "Tempo", y = "Preço (R$)")


  return(p)
}
