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
# -------- Load Data --------
# pair <- "XXBTZUSD"
pair <- "BONKUSD"
# pair <- "XXLMZUSD"
data_path <- "Data"
pair_data_results <- paste(data_path, pair, sep ="/")
file <- paste0(paste(pair_data_results, pair, sep = "/"), ".csv.gz")
frame <- fread(file)

colnames(frame) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit","misc",
                     "trade_id", "last_id", "Date_POSIXct", "Time", "Date", "Hour")

# -------- Parameters --------
by <- data.table(by = c(0.002,0.004,0.006, 0.008,0.01, 0.02), flag =1)
pos_start <- data.table(pos_start = c(0.01, 0.02, 0.03), flag =1)
end <- data.table(end = c(0.1, 0.15, 0.20), flag =1)
reset <- data.table(reset = c(4, 8, 12, 24), flag =1)
sl <- data.table(sl = c(0.01, 0.02), flag =1)
ema <- data.table(ema = c(5, 100, 200), flag =1)
ema_ratio <- data.table(ema_ratio = c(1, 2, 3), flag = 1)
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

# How many weeks should we backtest
n_dates <- 1

# Weekly batches
n_days <- 365
vector_date <- unique(frame$Date)
selected_dates <- sample(vector_date, size = n_dates, replace = F)
selected_dates <- "2024-01-01"
# Pick subframe
df <- frame[Date >= selected_dates & Date <= as.Date(selected_dates) + n_days]

# -------- Setup Parallel --------
cl <- parallel::makeCluster(5, type = "PSOCK")   # adjust number of cores here
on.exit(stopCluster(cl))
registerDoSNOW(cl)

# Source parallelization
source(file = paste0(getwd(), "/Backtesting/parallel_loop.R"))


# -------- Combine Results --------
all_results <- rbindlist(daily_results, fill = TRUE)
all_results[, params := paste(by, pos_start, end, reset,sl, ema_ratio, ema,type, sep = "_"), by = .I]


save(all_results, file=paste0(paste0(getwd(), "/Backtesting/results/"),paste0("pair=", pair, "_n_dates=", n_dates, "_n_days=", n_days, "_"), Sys.time(), ".Rdata"))



mean_percent <- all_results[, sum(result_percent), by = params]
setorder(mean_percent, -V1)
mean_percent







