#' Get record link html element
#'
#' @description Creates record url based on source and barcode and returns and html element with the link and occurrence ID as the text
#'
#' @export
get_link_source_record <- function(occurrenceID,
                                   bibliographicCitation,
                                   scientificNameReference)
{

    x <- data.frame(link=rep('',NROW(bibliographicCitation)), stringsAsFactors = FALSE)
    url <- sapply(1:nrow(x), function(i) get_link_source_record_url(occurrenceID[i], bibliographicCitation[i], scientificNameReference[i]))
    x$link <- paste0("<a href='", url, "'target='_blank'>", occurrenceID,"</a>")
}