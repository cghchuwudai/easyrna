


#' @title Change fpkm into tpm
#'
#' @param fpkm input the fpkm value
#'
#' @return TPM value
#' @export
#'
#' @examples df %>% mutate(TPM=fpkmToTpm(.$fpkm))
fpkmToTpm <- function(fpkm)
{
  exp(log(fpkm) - log(sum(fpkm)) + log(1e6))
}















