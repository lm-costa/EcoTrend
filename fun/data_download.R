library(lubridate)
library(httr)
library(dplyr)
library(stringr)
library(tibble)
library(writexl)

my_invest_download_auto_minuto <- function(cod_b3, yyyymmdd,start_hhmm = "1550"){

  base_url <- "https://arquivos.b3.com.br/rapinegocios/tickercsv/"
  cod <- as.character(cod_b3)
  data_valida <- tryCatch(as_date(yyyymmdd), error = function(e) NA)

  if (is.na(data_valida)) {
    stop("Coloque um formato válido de data, ex: 2025-04-23")
  }

  # Define horário atual como limite final (end)
  end_hhmm <- format(Sys.time(), "%H%M")

  # Converte os horários para objetos POSIXct
  start_time <- ymd_hm(paste(yyyymmdd, substr(start_hhmm, 1, 2), substr(start_hhmm, 3, 4)))
  end_time <- ymd_hm(paste(yyyymmdd, substr(end_hhmm, 1, 2), substr(end_hhmm, 3, 4)))

  if (start_time > end_time) {
    stop("O horário inicial é maior que o horário atual do sistema.")
  }

  # Gera sequência de horários minuto a minuto
  horarios_possiveis <- format(seq(start_time, end_time, by = "1 min"), "%H%M")

  ultimo_valido <- NULL

  for (hhmm in horarios_possiveis) {
    url <- paste0(base_url, cod, "/", data_valida, "_NEGOCIOSAVISTA", cod, "_", hhmm, ".zip")
    resp <- HEAD(url)
    if (status_code(resp) == 200) {
      ultimo_valido <- hhmm
    }
  }

  if (is.null(ultimo_valido)) stop("Nenhum arquivo válido encontrado no intervalo especificado.")

  # Link final com o último horário válido
  url_final <- paste0(base_url, cod, "/", data_valida, "_NEGOCIOSAVISTA", cod, "_", ultimo_valido, ".zip")
  message("Último arquivo encontrado: ", url_final)

  dest_path <- paste0(data_valida,"_NEGOCIOSAVISTA",cod,"_",ultimo_valido,'.zip')
  dest_path_unzip <- paste0(data_valida,"_NEGOCIOSAVISTA",cod,"_",ultimo_valido)

  download.file(url_final,
                destfile = paste0("data-raw/", dest_path),
                method = "wget")

  unzip(paste0("data-raw/", dest_path),
        exdir = paste0("data-raw/", dest_path_unzip))

  files <- list.files(paste0("data-raw/", dest_path_unzip),
                      full.names = TRUE)
  fsimples <- list.files(paste0("data-raw/", dest_path_unzip),
                         full.names = F) |>
    str_remove('.txt')

  df <- read.table(files, header = TRUE, sep = ";", dec = ",")

  df <- df |>
    tibble() |>
    mutate(
      hora = str_sub(HoraFechamento, 1, 2),
      min = str_sub(HoraFechamento, 3, 4),
      seg = str_sub(HoraFechamento, 5, 6),
      date = make_datetime(
        year = year(DataReferencia),
        month = month(DataReferencia),
        day = day(DataReferencia),
        hour = as.numeric(hora),
        min = as.numeric(min),
        sec = as.numeric(seg)
      )
    )

  writexl::write_xlsx(df, path = paste0("data/", fsimples,'.xlsx'))
}


