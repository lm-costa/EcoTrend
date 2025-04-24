purrr::map(list.files('fun/',full.names = T),source)

stocks <- c("WINM25","PETR4")
Sys.time()

purrr::pmap(list(stocks,'2025-04-23','0740'),my_invest_download_auto_minuto)
