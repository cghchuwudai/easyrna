
#' @title Change Gene Count into TPM
#'
#' @param counts Gene Count
#' @param effLen Gene Length
#'
#' @return TPM value
#' @export
#'
#' @examples df %>% mutate(TPM=countToTpm(.$count, .$est_len))
countToTpm <- function(counts, effLen)
{
  rate <- log(counts) - log(effLen)
  denom <- log(sum(exp(rate)))
  exp(rate - denom + log(1e6))
}





