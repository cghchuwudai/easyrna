


#' @title Change Count into CPM
#'
#' @param counts Gene Count
#'
#' @return CPM value
#' @export
#'
#' @examples df %>%mutate(CPM=countToCPM(.$count))
countToCPM <- function( counts)
{
  N <- sum(counts)
  exp( log(counts) + log(1e6) - log(N) )
}


