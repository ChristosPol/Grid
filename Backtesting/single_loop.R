# -------- Single Loop --------
library(data.table)
library(TTR)

daily_results <- list()

for (p in 1:nrow(params)) {
  
  # ---- Precompute per-tick volumes/EMAs ----
  df[, cost := price * volume]
  df[buy_sell == "b", buy_vol := volume]
  df[is.na(buy_vol), buy_vol := 0]
  df[buy_sell == "s", sell_vol := volume]
  df[is.na(sell_vol), sell_vol := 0]
  
  df[, ema_buy  := EMA(buy_vol,  n = params$ema[p])]
  df[, ema_sell := EMA(sell_vol, n = params$ema[p])]
  
  pos_step <- seq(params$pos_start[p], params$end[p], params$by[p])
  neg_step <- -1 * seq(params$pos_start[p], params$end[p], params$by[p])
  
  hours_reset  <- params$reset[p]
  number_grids <- 24 * 7 / hours_reset  # (kept if you use it later)
  
  # ---- Block partitioning ----
  df[, elapsed_hours := as.numeric(difftime(Date_POSIXct, Date_POSIXct[1], units = "hours"))]
  df[, block_flag    := floor(elapsed_hours / hours_reset)]
  df[, flag_change   := c(TRUE, diff(block_flag) > 0)]
  df[, block_flag    := block_flag + 1L]
  df[, global_index  := .I]
  
  init <- data.table(
    indeces_init = which(df$flag_change),
    prices_init  = df$price[which(df$flag_change)]
  )
  init[, indeces_end := shift(indeces_init, type = "lead") - 1L]
  init[is.na(indeces_end), indeces_end := nrow(df)]
  
  # ---- Create grids ----
  grids <- list()
  all_chars <- c(LETTERS, 0:9)
  str_len   <- 20
  
  for (i in 1:nrow(init)) {
    spec_price <- init$prices_init[i]
    index_end  <- init$indeces_end[i]
    index_start<- init$indeces_init[i]
    
    grid_tab <- data.table(
      batch      = sample(x = 1:10E5, 1),
      grid       = c(spec_price + spec_price * pos_step,
                     spec_price + spec_price * neg_step),
      init_price = spec_price,
      status_enter = "open",
      status_exit  = "open",
      idx_start    = index_start,
      idx_end      = index_end,
      idx_enter    = NA_integer_,
      idx_exit     = NA_integer_,
      actual_price_exit = NA_real_
    )
    
    setorder(grid_tab, -grid)
    
    grid_tab[, `:=`(
      # Do NOT pre-create POSIXct columns (tz mismatch risk).
      # We'll create them later from df$Date_POSIXct.
      trade_id = replicate(.N, paste(sample(all_chars, str_len, replace = TRUE), collapse = "")),
      position = ifelse(grid > spec_price, "long", "short"),
      SL_act   = FALSE,
      bet      = 50
    )]
    
    # Take-profits per grid
    grid_tab[position == "short", exits := shift(grid, type = "lead")]
    grid_tab[position == "short" & is.na(exits), exits := grid - grid * params$by[p]]
    
    grid_tab[position == "long", exits := shift(grid, type = "lag")]
    grid_tab[position == "long" & is.na(exits), exits := grid + grid * params$by[p]]
    
    grid_tab[, tp := ((exits - grid) / grid) * 100]
    grid_tab[position == "short", tp := -tp]
    
    # Stop-loss levels
    grid_tab[position == "long",  SL_price := grid - grid * params$sl[p]]
    grid_tab[position == "short", SL_price := grid + grid * params$sl[p]]
    
    grids[[i]] <- grid_tab
  }
  
  # ---- Entry logic (Breakout / Reversion / Weak Breakout) ----
  for (n in seq_along(grids)) {
    block_df <- df[block_flag == n]
    grid_tab <- copy(grids[[n]])
    
    for (j in 1:nrow(grid_tab)) {
      price_level <- grid_tab$grid[j]
      pos_type    <- grid_tab$position[j]
      
      if (pos_type == "short") {
        entry_idx <- which(block_df$price <= price_level)[1]
        if (!is.na(entry_idx)) {
          ema_buy_val  <- block_df$ema_buy[entry_idx]
          ema_sell_val <- block_df$ema_sell[entry_idx]
          if (!is.na(ema_buy_val) && !is.na(ema_sell_val)) {
            if (params$type[p] == "breakout" && ema_sell_val > ema_buy_val) {
              grid_tab$idx_enter[j] <- block_df$global_index[entry_idx]
            } else if (params$type[p] == "reversion" && ema_buy_val > ema_sell_val) {
              grid_tab$position[j]  <- "long"
              grid_tab$idx_enter[j] <- block_df$global_index[entry_idx]
            } else if (params$type[p] == "weak_breakout" && ema_buy_val > ema_sell_val) {
              grid_tab$idx_enter[j] <- block_df$global_index[entry_idx]
            }
          }
        }
      }
      
      if (pos_type == "long") {
        entry_idx <- which(block_df$price >= price_level)[1]
        if (!is.na(entry_idx)) {
          ema_buy_val  <- block_df$ema_buy[entry_idx]
          ema_sell_val <- block_df$ema_sell[entry_idx]
          if (!is.na(ema_buy_val) && !is.na(ema_sell_val)) {
            if (params$type[p] == "breakout" && ema_buy_val > ema_sell_val) {
              grid_tab$idx_enter[j] <- block_df$global_index[entry_idx]
            } else if (params$type[p] == "reversion" && ema_sell_val > ema_buy_val) {
              grid_tab$position[j]  <- "short"
              grid_tab$idx_enter[j] <- block_df$global_index[entry_idx]
            } else if (params$type[p] == "weak_breakout" && ema_sell_val > ema_buy_val) {
              grid_tab$idx_enter[j] <- block_df$global_index[entry_idx]
            }
          }
        }
      }
    }
    
    grids[[n]] <- grid_tab
  }
  
  # ---- Collect trades ----
  all_trades <- rbindlist(grids)
  all_trades <- all_trades[!is.na(idx_enter)]
  
  if (nrow(all_trades) == 0) next
  
  # ---- Exit logic ----
  for (i in 1:nrow(all_trades)) {
    idx_ent  <- all_trades$idx_enter[i]
    idx_end  <- all_trades$idx_end[i]
    pos      <- all_trades$position[i]
    grid_exit<- all_trades$exits[i]
    sl_price <- all_trades$SL_price[i]
    
    price_path <- df[idx_ent:idx_end, price]
    index_path <- df[idx_ent:idx_end, global_index]
    
    idx_tp <- NA_integer_
    idx_sl <- NA_integer_
    
    if (pos == "short") {
      tmp <- which(price_path <= grid_exit)[1]
      if (!is.na(tmp)) idx_tp <- index_path[tmp]
      tmp <- which(price_path >= sl_price)[1]
      if (!is.na(tmp)) idx_sl <- index_path[tmp]
    } else { # long
      tmp <- which(price_path >= grid_exit)[1]
      if (!is.na(tmp)) idx_tp <- index_path[tmp]
      tmp <- which(price_path <= sl_price)[1]
      if (!is.na(tmp)) idx_sl <- index_path[tmp]
    }
    
    if (!is.na(idx_tp) & !is.na(idx_sl)) {
      if (idx_sl < idx_tp) { all_trades$idx_exit[i] <- idx_sl; all_trades$SL_act[i] <- TRUE }
      else                 { all_trades$idx_exit[i] <- idx_tp }
    } else if (!is.na(idx_sl)) {
      all_trades$idx_exit[i] <- idx_sl; all_trades$SL_act[i] <- TRUE
    } else if (!is.na(idx_tp)) {
      all_trades$idx_exit[i] <- idx_tp
    } else {
      all_trades$idx_exit[i] <- idx_end
    }
  }
  
  # ---- Finalize results (timestamps now inherit tz from df$Date_POSIXct) ----
  all_trades[, `:=`(
    interval_enter     = df$Date_POSIXct[idx_enter],
    interval_exit      = df$Date_POSIXct[idx_exit],
    actual_price_exit  = df$price[idx_exit]
  )]
  
  all_trades[, res := ((actual_price_exit - grid) / grid) * 100]
  all_trades[position == "short", res := -res]
  all_trades[, res := res - 0.10]  # fee/slippage
  
  daily_results[[p]] <- all_trades
}
# -------- End Single Loop --------
