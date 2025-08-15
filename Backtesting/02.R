rm(list=ls())

i <- 1
list_all <- list()
all <- list.files(paste0(paste0(getwd(), "/Backtesting/results/")), full.names = T)
all <- all[2]

for(i in 1:length(all)){
  load(all[i])
  list_all[[i]] <- all_results
  rm(all_results)
}
all_results <- rbindlist(list_all, fill = TRUE)

setorder(all_results, -result_percent, max_drawdown)



