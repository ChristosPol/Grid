rm(list=ls())

i <- 1
list_all <- list()
all <- list.files(paste0(paste0(getwd(), "/Backtesting/results/")), full.names = T)
for(i in 1:length(all)){
  load(all[i])
  list_all[[i]] <- all_results
  rm(all_results)
}
all_results <- rbindlist(list_all, fill = TRUE)
length(unique(all_results$date))

mean_percent_param <- all_results[, sum(result_percent), by = params]
mean_percent_param_month <- all_results[, sum(result_percent), by = c("params", "date")]


setorder(mean_percent_param, -V1)
setorder(mean_percent_param_month, -V1)


ggplot(data=mean_percent_param_month, aes(x = date, y = V1))+
  geom_point()

