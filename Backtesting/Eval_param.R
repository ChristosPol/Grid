rm(list=ls())
gc()
# 0.01_0.01_0.2_4_0.02_1_5_breakout
# Load libraries
library(data.table)
library(dplyr)
library(TTR)
library(ggpubr)
setDTthreads(1)
# -------- Load Data --------
# pair <- "XXBTZUSD"
pair <- "BONKUSD"
# pair <- "ALGOUSD"
# pair <- "AVAXUSD"
data_path <- "Data"
pair_data_results <- paste(data_path, pair, sep ="/")
file <- paste0(paste(pair_data_results, pair, sep = "/"), ".csv.gz")
frame <- fread(file)

colnames(frame) <- c("price", "volume", "epoch_time", "buy_sell", "market_limit","misc",
                     "trade_id", "last_id", "Date_POSIXct", "Time", "Date", "Hour")

# -------- Parameters --------
by <- data.table(by = c(0.01), flag =1)
pos_start <- data.table(pos_start = c(0.01), flag =1)
end <- data.table(end = c(0.2), flag =1)
reset <- data.table(reset = c(4), flag =1)
sl <- data.table(sl = c(0.02), flag =1)
ema <- data.table(ema = c(5), flag =1)
ema_ratio <- data.table(ema_ratio = c(1), flag = 1)
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
n_days <- 30
vector_date <- unique(frame$Date)
selected_dates <- sample(vector_date, size = n_dates, replace = F)
# selected_dates <- as.Date("2023-01-01")

# Pick subframe
df <- frame[Date >= selected_dates & Date <= selected_dates + n_days]



# Source parallelization
source(file = paste0(getwd(), "/Backtesting/single_loop.R"))


# -------- Combine Results --------
all_trades <- daily_results[[1]]
setDT(all_trades)
setorder(all_trades, interval_enter)

# View(all_trades)


all_results <- cbind(
  params[p, ],
  result_percent = sum(all_trades$res),
  n_trades = nrow(all_trades),
  avg_return = mean(all_trades$res),
  sd_return = sd(all_trades$res),
  win_rate = mean(all_trades$res > 0),
  max_drawdown = min(all_trades$res),
  sl_triggered = sum(all_trades$SL_act == TRUE),
  tp_triggered = sum(all_trades$SL_act == FALSE),
  date = selected_dates
)


# Compute cumulative returns
all_trades[, cum_return := cumsum(res)]
# Normalize price to 0% start
df[, bh_return := (price / first(price) - 1) * 100]
plot_dt <- merge(
  all_trades[, .(interval_exit, cum_return)],
  df[, .(Date_POSIXct, bh_return)],
  by.x = "interval_exit",
  by.y = "Date_POSIXct",
  all.x = TRUE
)
plot_dt[, bh_return := zoo::na.locf(bh_return, na.rm = FALSE)]
ggplot(plot_dt, aes(x = interval_exit)) +
  geom_step(aes(y = cum_return, color = "Strategy")) +
  geom_line(aes(y = bh_return, color = "Buy & Hold")) +
  labs(title = "Cumulative Returns vs Buy & Hold",
       x = "Time",
       y = "Return (%)",
       color = "Legend") +
  theme_minimal()


# # Vis
# p1 <- ggplot(data = df, aes(x = Date_POSIXct, y = price)) +
#   geom_line() +
#   theme_pubclean() +
#   geom_point(data = all_trades[position == "long"], aes(x = interval_enter, y = grid), fill = "darkgreen", color = "darkgreen", size = 1.5, shape = 24, alpha = 1) +
#   geom_point(data = all_trades[position == "short"], aes(x = interval_enter, y = grid), fill = "darkred", color = "darkred", size = 1.5, shape = 25, alpha = 1) +
#   
#   geom_point(data = all_trades[SL_act == FALSE], aes(x = interval_exit, y = actual_price_exit), color = "orange", size = 1, shape = 8, alpha = 1) +
#   geom_point(data = all_trades[SL_act == TRUE], aes(x = interval_exit, y = actual_price_exit), color = "blue", size = 1, shape = 8,  alpha = 1)
# 
# 
# p1
# 
# all_results
