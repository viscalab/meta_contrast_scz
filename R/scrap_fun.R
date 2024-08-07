scrap_fun <- function(.file, log = FALSE, log_x_plot = TRUE, log_y_plot = TRUE, ...) {
  
  dots <- list2(...) 
  
  l_x <- dots[1]
  l_cond <- dots[-1]
  
  dat_raw <- read_csv(paste0(.file),
                      show_col_types = FALSE)
  
  n_cond <- nrow(dat_raw) / length(l_cond)
  
  cond <- rep(names(l_cond), each = n_cond)
  
  
  s <- map(l_cond, \(x) rep(x, n_cond / 2)) |> unlist()
  
  s |> as_tibble()
  
  dat <- dat_raw |>
    select(-x) |> 
    mutate(!!sym(names(l_x)) := rep(rep(l_x[[1]], each = 2), length(l_cond))) |>
    mutate(cond = cond, s = s) |>
    group_by(cond) |>
    pivot_wider(names_from = s, values_from = y)
  
  if (!("upper" %in% colnames(dat))) {
    dat <- dat |>
      mutate(upper = NA)
  }
  
  if (!("lower" %in% colnames(dat))) {
    dat <- dat |>
      mutate(lower = NA)
  }
  
  if (!("m" %in% colnames(dat))) {
    dat <- dat |>
      mutate(m = (upper + lower) / 2)
  }
  
  
  
  
  if (log == FALSE) {
    dat <- dat |>
      mutate(upper = if_else(is.na(upper), m + (m - lower), upper),
             lower = if_else(is.na(lower),  m - (upper - m), lower))
  }
  else {
    
    dat <- dat |>
      mutate(lower_log = log10(lower), 
             upper_log = log10(upper), 
             m_log = log10(m)) |> 
      mutate(upper = if_else(is.na(upper), 10^(m_log + (m_log - lower_log)), upper),
             lower = if_else(is.na(lower),  10^(m_log - (upper_log - m_log)), lower)) |> 
      select(-lower_log, -upper_log, -m_log)
  }
    
    
  
  p <- dat |>
    ggplot(aes(x = !!sym(names(l_x)), y = m, ymin = lower, ymax = upper, color = cond)) +
    geom_pointrange() +
    geom_line()
  
  if (log_x_plot) {
    p <- p +
      scale_x_log10()
  }
  
  if (log_y_plot) {
    p <- p +
      scale_y_log10()
  }
  
  plot(p)
  
  dat |> 
    mutate(study = str_extract(.file, "(?<=/)[^/]+(?=\\.csv$)")) |> 
    relocate(study) |> 
    relocate(m, lower, upper, .after = cond)
}

