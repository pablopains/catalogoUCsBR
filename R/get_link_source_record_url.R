#' Get record url
#'
#' @description creates record url based on source and barcode
#' @export
get_link_source_record_url <- function(occurrenceID,
                                       bibliographicCitation,
                                       scientificNameReference)
{

    url <- ''

    bibliographicCitation <- bibliographicCitation %>% tolower()

    if (bibliographicCitation == "reflora")
    {
        barcode <- gsub(paste0(tolower(bibliographicCitation),"=" ), '',occurrenceID )

        base_url <- "http://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/BemVindoConsultaPublicaHVConsultar.do?modoConsulta=LISTAGEM&quantidadeResultado=20&codigoBarra=%s"
        url <- sprintf(base_url, barcode)
    }


    # jabotRB
    if (bibliographicCitation == 'jabotrb')
    {

        # occurrenceID = 'jabotRB=RB01031072'

        barcode <- gsub(paste0(bibliographicCitation,"=" ), '',occurrenceID, ignore.case = TRUE)
        barcode <- gsub('RB', '',barcode, ignore.case = TRUE)

        base_url <- 'http://rb.jbrj.gov.br/v2/regua/visualizador.php?r=true&colbot=rb&codtestemunho='
        url <- paste0(base_url,barcode)

    }


    # jabot
    if (bibliographicCitation == "jabot")
    {
        occurrenceID_tmp <- gsub(paste0(tolower(bibliographicCitation),"=" ), '',occurrenceID, ignore.case = TRUE )

        occurrenceID_tmp2 <- str_split(occurrenceID_tmp,':')

        col <- occurrenceID_tmp2[[1]][1]

        cat <- occurrenceID_tmp2[[1]][2]
        cat <- gsub(col, '',cat )

        base_url <- "http://rb.jbrj.gov.br/v2/regua/visualizador.php?r=true&colbot=%s&codtestemunho=%s"
        url <- sprintf(base_url, col, cat)
    }



    if (bibliographicCitation == 'splink')
    {

        # com barcode

        if (!is.na(str_locate(occurrenceID,':')[[1]]))
        {

            occurrenceID_tmp <- gsub(paste0(tolower(bibliographicCitation),"=" ), '',occurrenceID )
            occurrenceID_tmp <- str_split(occurrenceID_tmp,':')
            col <- occurrenceID_tmp[[1]][1]
            cat <- occurrenceID_tmp[[1]][2]
            # url <- 'https://specieslink.net/search/'
            url <- paste0('https://specieslink.net/search/records/catalognumber/',cat,'/collectionCode/',col)

        }else
        {
            barcode <- gsub(paste0(tolower(bibliographicCitation),"=" ), '',occurrenceID )

            # https://specieslink.net/search/records/barcode/MO0101458866
            base_url <- "https://specieslink.net/search/records/barcode/%s"
            url <- sprintf(base_url, barcode)
        }
    }


    if(bibliographicCitation=='gbif')
    {

        occurrenceID_tmp <- gsub(paste0(tolower(bibliographicCitation),"=" ), '',occurrenceID )

        base_url <- "https://www.gbif.org/occurrence/search?occurrence_id=%s&advanced=1&occurrence_status=present"

        # url <- sprintf(base_url,occurrenceID_tmp)

        url <- paste0("https://www.gbif.org/occurrence/search?occurrence_id=",occurrenceID_tmp,"&advanced=1&occurrence_status=present")

    }

    return(url)
}