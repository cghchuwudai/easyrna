

#' @title Find Another Type of Name
#' @description Find another name of same gene.
#'
#' @param query An object which want to query
#' @param dataset A dataset constructed from GTF
#' @param type Type of input gene name ensembl or symbol
#'
#' @return A global object which transformed name.
#' @export
#'
#' @examples result <- findname("ENSGALG00000017145",dataset, "ENSEMBL")
#'
#'
findname <- function(query,dataset,type) {
  library(lookup)
  library(tidyverse)
  if (!is.character(query)) {
    stop("Invalid input. Must provide a character input.")
  }
  if(type=="ENSEMBL") {
    for (type in c("ENSEMBL")) {
     result <- lookup(x = query,key = dataset$ENSEMBL,value = dataset$SYMBOL)
   }
  }  else if (type=="SYMBOL") {
    for (type in c("SYMBOL")) {
      result <- lookup(x = query,key = dataset$SYMBOL, value = dataset$ENSEMBL)
    }
  } else {
    stop("Invalid gene type. Choose either 'ENSEMBL' or 'SYMBOL'. ")
  }
return(result)
}











