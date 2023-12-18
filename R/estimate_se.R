estimate_se <- function(tbl, eb, ns) {
  n_df <- tbl |> 
    distinct(cond) |> 
    bind_cols(n = ns)
  
  dat <- tbl |> 
    left_join(n_df, by = join_by(cond))
  
  if (eb == "se") {
    dat <- dat |> 
      mutate(se = (upper - lower) / 2, 
             sd = se * sqrt(n))
  }
  
  if (eb == "sd") {
    dat <- dat |> 
      mutate(sd = (upper - lower) / 2, 
             se = sd / sqrt(n))
  }

  if (eb == "ci") {
    dat <- dat |> 
      mutate(se = (upper - lower) / (2 * qt(.975, df = n - 1)), 
             sd = se * sqrt(n))
  }
  
  if (eb == "iqr") {
    dat <- dat |> 
      mutate(sd = (upper - lower) / (qnorm(.75) - qnorm(.25)),
             se = sd / sqrt(n))
  }
  
  dat |> 
    select(-lower, -upper) |> 
    mutate(lower = m - se,
           upper = m + se)
}

