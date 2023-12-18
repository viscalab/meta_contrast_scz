write_number <- function(.number, .digits = 2, .percen = FALSE) {
  if (.percen) {
    .number <- .number * 100
  }
  
  number <- sign_digits(.number, d = .digits)
  
  if (.percen) {
    number <- paste0(number, "%")
  }
  
  number
}