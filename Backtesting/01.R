rm(list=ls())
gc()

# Load libraries
library(data.table)
library(dplyr)
library(TTR)
library(foreach)
library(doSNOW)
library(parallel)
setDTthreads(1)
library(lubridate)
# PF_ETHUSD, PF_LINKUSD, PF_XBTUSD, PF_XRPUSD, PF_ADAUSD, PF_SOLUSD, PF_ARBUSD, PF_WIFUSD
prod_id <- "PF_ETHUSD"
pair <- "WIFUSD"
data_source <- "spot" # "futures"

if(data_source=="spot") {
  
  data_path <- "Data"
  pair_data_results <- paste(data_path, pair, sep ="/")
  file <- paste0(paste(pair_data_results, pair, sep = "/"), ".csv.gz")
  frame <- fread(file)
  colnames(frame) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit","misc",
                       "trade_id", "last_id", "Date_POSIXct", "Time", "Date", "Hour")
  # How many weeks should we backtest
  n_dates <- 1
  n_days <- 365
  vector_date <- unique(frame$Date)
  selected_dates <- sample(vector_date, size = n_dates, replace = F)
  # selected_dates <- "2024-01-01"
  # Pick subframe
  df <- frame[Date >= selected_dates & Date <= as.Date(selected_dates) + n_days]
  
} else {
  df <- setDT(read.csv("~/Repositories/Private/Grid/Data/kraken_futures_trades (3).csv"))
  df <- df[product_id == prod_id]
  colnames(df) <- c("time_ms", "Date_POSIXct", "price", "volume", "buy_sell","product_id",
                    "seq")
  df[, Date_POSIXct := ymd_hms(Date_POSIXct)]
  setorder(df, Date_POSIXct)
  df[, buy_sell := ifelse(buy_sell == "sell", "s", "b")]
  
}


# -------- Parameters --------
by <- data.table(by = c(0.002,0.004,0.006, 0.008,0.01, 0.02, 0.03), flag =1)
pos_start <- data.table(pos_start = c(0.002, 0.05,0.01, 0.02, 0.03), flag =1)
end <- data.table(end = c(0.05, 0.1, 0.15, 0.20), flag =1)
reset <- data.table(reset = c(4, 8, 12, 24), flag =1)
sl <- data.table(sl = c(0.005, 0.01,0.015,  0.02,  0.025, 0.03), flag =1)
ema <- data.table(ema = c(5, 50,  200), flag =1)
ema_ratio <- data.table(ema_ratio = c(1,  3), flag = 1)
type <- data.table(type = c("breakout"), flag = 1)
params <- left_join(by, pos_start, relationship =
                      "many-to-many") %>%
  left_join(end, relationship =
              "many-to-many") %>%
  left_join(reset, relationship =
              "many-to-many") %>%
  left_join(sl, relationship =
              "many-to-many") %>%
  left_join(ema_ratio, relationship =
              "many-to-many") %>%
  left_join(ema, relationship =
              "many-to-many") %>%
  left_join(type, relationship =
              "many-to-many")


# -------- Setup Parallel --------
cl <- parallel::makeCluster(5, type = "PSOCK")   # adjust number of cores here
on.exit(stopCluster(cl))
registerDoSNOW(cl)

# Source parallelization
source(file = paste0(getwd(), "/Backtesting/parallel_loop.R"))


# -------- Combine Results --------
all_results <- rbindlist(daily_results, fill = TRUE)
all_results[, params := paste(by, pos_start, end, reset,sl, ema_ratio, ema,type, sep = "_"), by = .I]

if(data_source=="spot") {
  save(all_results, file=paste0(paste0(getwd(), "/Backtesting/results/"),paste0("pair=", pair, "_n_dates=", n_dates,"_",selected_dates, "_n_days=", n_days, "_", "historical_spot_"), Sys.time(), ".Rdata"))
} else {
  save(all_results, file=paste0(paste0(getwd(), "/Backtesting/results/"),paste0("product_id=", prod_id, "_historical_futures_"), Sys.time(), ".Rdata"))
}

save(all_results, file=paste0(paste0(getwd(), "/Backtesting/results/"),paste0("pair=", pair, "_n_dates=", n_dates, "_n_days=", n_days, "_"), Sys.time(), ".Rdata"))

# mean_percent <- all_results[, list(result_percent, RoMaD), by = params]
# setorder(mean_percent, -result_percent)
# mean_percent
# plot(mean_percent$result_percent, mean_percent$RoMaD)




# model <- lm(RoMaD ~ by + pos_start + end + reset + sl + ema+ema_ratio, data = all_results)
# model <- lm(result_percent ~ by + pos_start + end + reset + sl + ema+ema_ratio, data = all_results)
# summary(model)
# library(broom)
# library(ggplot2)

# coef_df <- broom::tidy(model)
# 
# ggplot(coef_df, aes(x = reorder(term, estimate), y = estimate)) +
#   geom_col(fill = "steelblue") +
#   geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), width = 0.2) +
#   coord_flip() +
#   labs(title = "Effect of Parameters on Sharpe Ratio",
#        x = "Parameter", y = "Coefficient")


# ggplot(all_results, aes(x = sl, y = by, fill = RoMaD)) +
#   geom_tile() +
#   scale_fill_viridis_c() +
#   labs(title = "Interaction: SL × EMA",
#        x = "Stop Loss",
#        y = "EMA length",
#        fill = "Sharpe") +
#   theme_minimal()
# 
# 
# ggplot(all_results, aes(x = sl, y = RoMaD, color = factor(ema), group = ema)) +
#   geom_line() +
#   labs(title = "Sharpe vs SL for different EMA values",
#        x = "Stop Loss",
#        y = "Sharpe Ratio",
#        color = "EMA") +
#   theme_minimal()



# library(mgcv)
# 
# k_sl <- max(3, min(10, length(unique(all_results$sl)) - 1))
# k_by <- max(3, min(10, length(unique(all_results$by)) - 1))
# k_pos_start <- max(3, min(10, length(unique(all_results$pos_start)) - 1))
# k_end <- max(3, min(10, length(unique(all_results$end)) - 1))
# k_reset <- max(3, min(10, length(unique(all_results$reset)) - 1))
# k_ema <- max(3, min(10, length(unique(all_results$ema)) - 1))
# # k_ema_ratio <- max(3, min(10, length(unique(all_results$ema_ratio)) - 2))
# k_ema_ratio <- 1
# 
# m_gam <- gam(
#   RoMaD ~ s(sl, bs = "cr", k = k_sl) + s(by, bs = "cr", k = k_by)+
#     s(pos_start, bs = "cr", k = k_pos_start)+
#     s(end , bs = "cr", k = k_end)+
#     s(reset, bs = "cr", k = k_reset)+s(ema, bs = "cr", k = k_ema) ,
#   data   = all_results,
#   method = "REML",
#   select = TRUE   # drops extra wiggliness if not needed
# )
# summary(m_gam)
# plot(m_gam, pages = 1, shade = TRUE)
# 
# 
# plot(m_gam, pages = 1, shade = TRUE, rug = TRUE, seWithMean = TRUE)




