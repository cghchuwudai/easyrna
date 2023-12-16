

#' @title Change Count into FPKM
#'
#' @param counts Gene Count
#' @param effLen Gene Length
#'
#' @return FPKM value
#' @export
#'
#' @examples df %>%mutate(FPKM=countToFpkm(.$count,.$est_len))
countToFpkm <- function(counts, effLen)
{
  N <- sum(counts)
  exp( log(counts) + log(1e9) - log(effLen) - log(N) )
}





