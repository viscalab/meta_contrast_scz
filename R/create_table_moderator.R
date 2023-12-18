create_table_moderator <- function(.dat, .name) {
  .dat |> 
    tidy() |> 
    mutate(term = if_else(term == "intercept", "intercept", "slope")) |> 
    select(-type, -std.error) |> 
    bind_cols(tibble(ci_lower = .dat[["ci.lb"]])) |> 
    bind_cols(tibble(ci_upper = .dat[["ci.ub"]])) |> 
    rowwise() |> 
    mutate(ci = write_ci_table(ci_lower, ci_upper)) |> 
    select(-ci_lower, -ci_upper) |> 
    mutate(p.value = write_p_table(p.value)) |> 
    mutate(across(c(estimate, statistic), 
                  write_number)) |> 
    relocate(ci, .after = estimate) |> 
    pivot_wider(names_from = term, values_from = -term) |>
    mutate(moderator = .name) |> 
    relocate(moderator) |> 
    mutate(n_samples = .dat$k) |> 
    relocate(n_samples, .after = moderator)
}