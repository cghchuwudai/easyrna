


#' @title A GeneLength Used for Counting
#' @description Before transform the count into other, a gene length should be made.
#'
#'
#' @param gtf Input gtf for your species
#' @param df A combined dataframe with two column:gene_id and count. One of the column must be 'gene_id'
#'
#' @return A dataframe
#' @export
#' @import tidyverse
#' @examples length <- geneLength(gtf)
geneLength <- function(df,gtf){
  library(tidyverse)
  gtf <- read_tsv(gtf,comment = "#",
                  col_names = c("chr","source","type","start","end",
                                "score","strand","phase","attributes"))%>%
 filter(type=="exon")%>%mutate(len=end-start+1)%>% dplyr::select(start,end,attributes,len)
gtf$attributes %>% str_extract(., "gene_id \"[\\w|\\.]+") %>% str_remove(., "gene_id \"") -> gtf$gene_id
gtf %>% dplyr::select(start, end, gene_id, len) %>%
  distinct(start,end,gene_id, .keep_all = T) %>% dplyr::select(gene_id,len) %>%
  group_by(gene_id) %>% summarise(est_len=sum(len)) -> gtf

expmat <- df %>% inner_join(gtf, by="gene_id") %>% drop_na()
}







