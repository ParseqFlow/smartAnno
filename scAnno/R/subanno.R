#' Extract cell type information from annotation results
#'
#' @param annotations The annotation result data box processed by the cell_annotation function must contain the 'cluster' and 'content' columns
#'
#' @return Returns a data box with two columns, cluster and celltype:
#' \itemize{
#'   \item cluster
#'   \item celltype - Cell type after analysis (Format: Type [subtype])
#' }
#'
#' @export
#' @importFrom stringr str_match str_remove str_replace str_squish str_trim
#' @importFrom dplyr mutate case_when select

subanno <- function(annotations) {

  pattern <- ">\\s*(.*?)\\s*<"

  annotations %>%
    mutate(
      # Extract core content
      extracted = str_match(content, pattern)[,2],

      # Clean results
      celltype = case_when(
        is.na(extracted) ~ "Uncertain",
        # Handle trailing punctuation
        str_detect(extracted, "[:;,.]+$") ~ str_replace(extracted, "[:;,.]+$", ""),
        TRUE ~ extracted
      ) %>% str_squish()
    ) %>%
    select(cluster, celltype)
}
