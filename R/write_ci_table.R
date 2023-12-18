write_ci_table <- function(.lower, .upper, .digits = 2, .percen = FALSE) {
  
  if (.percen) {
    .lower <- .lower * 100
    .upper <- .upper * 100
  }
  
  lower <- sign_digits(.lower, d = .digits)
  upper <- sign_digits(.upper, d = .digits)
  
  if (.percen) {
    lower <- paste0(lower, "%")
    upper <- paste0(upper, "%")
  }
  
  paste0("(", lower, ", ", upper, ")")
}