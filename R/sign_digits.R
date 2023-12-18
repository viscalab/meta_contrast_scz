sign_digits <- function(x, d = 2){
  s <- format(x,digits=d)
  if(grepl("\\.", s) && ! grepl("e", s)) {
    n_sign_digits <- nchar(s) - 
      max( grepl("\\.", s), attr(regexpr("(^[-0.]*)", s), "match.length") )
    n_zeros <- max(0, d - n_sign_digits)
    s <- paste(s, paste(rep("0", n_zeros), collapse=""), sep="")
  }
  s
}