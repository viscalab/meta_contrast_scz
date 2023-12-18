calculate_es <- function(tbl, x, reverse = FALSE) {
  
  study_name <- tbl |> 
    first() |> 
    pluck("study")
  
  tbl <- tbl |>
    select(-study, -lower, -upper) |> 
    group_by({{x}}) |>
    pivot_wider(names_from = cond, values_from = c(m, se, sd, n)) |>
    mutate(g = list(esc_mean_sd(m_control, sd_control, n_control,
                                m_patient, sd_patient, n_patient,
                                study = study_name,
                                es.type = "g") |> 
                      as_tibble())) |>
    unnest(g) |> 
    relocate(study)
  
  if (reverse) {
    tbl <- tbl |> 
      mutate(es = -es, 
             ci.lo.temp = ci.lo,
             ci.lo = -ci.hi, 
             ci.hi = -ci.lo.temp) |> 
      select(-ci.lo.temp)
  }
  
  tbl |> 
    relocate(es, se, ci.lo, ci.hi, .before = m_control)
  
}