pull_patients <- function(tbl, .x, log_x = TRUE, log_y = FALSE) {
  
  dat_control <- tbl |>
    filter(cond == "control") 
  
  dat_patient <- tbl |>
    filter(cond != "control")  |>
    select(-se, -lower, -upper) |> 
    group_by(pick({{.x}})) |>
    pivot_wider(names_from = cond, values_from = c(m, sd, n))  |>
    mutate(pool.groups(n1 = n_patient1,  n2 = n_patient2,
                       m1 = m_patient1, m2 = m_patient2,
                       sd1 = sd_patient1, sd2 = sd_patient2)) |>
    rename(m = Mpooled, sd = SDpooled, n = Npooled) |>
    select(-m_patient1, -m_patient2, -sd_patient1, -sd_patient2, 
           -n_patient1, -n_patient2) |>
    mutate(se = sd / sqrt(n),
           lower = m - se,
           upper = m + se) |>
    mutate(cond = "patient")
  
  dat <- dat_control |>
    bind_rows(dat_patient)
  
  p <- dat |>
    ggplot(aes(x = {{.x}}, y = m, ymin = lower, ymax = upper, color = cond)) +
    geom_pointrange() +
    geom_line()
  
  if (log_x) {
    p <- p +
      scale_x_log10()
  }
  
  if (log_y) {
    p <- p +
      scale_y_log10()
  }
  
  plot(p)
  
  dat
}