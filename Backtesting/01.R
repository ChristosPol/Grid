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
data_path <- "Data"
pair_data_results <- paste(data_path, pair, sep ="/")
file <- paste0(paste(pair_data_results, pair, sep = "/"), ".csv.gz")
frame <- fread(file)

colnames(frame) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit","misc",
                     "trade_id", "last_id", "Date_POSIXct", "Time", "Date", "Hour")

# -------- Parameters --------
by <- data.table(by = seq(0.01, 0.03, 0.01), flag =1)
pos_start <- data.table(pos_start = c(0.01, 0.02, 0.03, 0.04), flag =1)
end <- data.table(end = c(0.1, 0.15, 0.20), flag =1)
reset <- data.table(reset = c(4, 8, 12, 24, 24*7), flag =1)
sl <- data.table(sl = c(0.02, 0.03, 0.04), flag =1)
ema <- data.table(ema = c(100, 200), flag =1)
ema_ratio <- data.table(ema_ratio = c(1, 2, 3), flag = 1)
type <- data.table(type = c("reversion"), flag = 1)

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
n_days <- 90
vector_date <- unique(frame$Date)
selected_dates <- sample(vector_date, size = n_dates, replace = F)

# Pick subframe
df <- frame[Date >= selected_dates & Date <= selected_dates + n_days]


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









hist(mean_percent$V1, breaks = 100)



i <- 1
list_all <- list()
all <- list.files(paste0(paste0(getwd(), "/strategies/Grid/results/")), full.names = T)
for(i in 1:length(all)){
  load(all[i])
  list_all[[i]] <- all_results
  rm(all_results)
}
all_results <- rbindlist(list_all, fill = TRUE)
length(unique(all_results$date))

mean_percent <- all_results[, sum(result_percent), by = params]
setorder(mean_percent, -V1)
View(mean_percent)

library(ggplot2)
ggplot(unique(all_results[, .(ema, result_percent)]), aes(x = ema, y = result_percent))+
  geom_point()

all_results


