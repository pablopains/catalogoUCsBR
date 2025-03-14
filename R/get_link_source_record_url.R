#' Get record url
#'
#' @description creates record url based on source and barcode
#' @export
get_link_source_record_url <- function(occurrenceID,
                                                   bibliographicCitation,
                                                   scientificNameReference)
            {

              x <- data.frame(link=rep('',NROW(bibliographicCitation)), stringsAsFactors = FALSE)
              url <- ''

              bibliographicCitation <- bibliographicCitation %>% tolower()

              for(i in 1:NROW(bibliographicCitation))
              {

                if (bibliographicCitation[i] == "reflora")
                {
                  barcode <- gsub(paste0(tolower(bibliographicCitation[i]),"=" ), '',occurrenceID[i] )

                  base_url <- "http://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/BemVindoConsultaPublicaHVConsultar.do?modoConsulta=LISTAGEM&quantidadeResultado=20&codigoBarra=%s"
                  url <- sprintf(base_url, barcode)
                  url

                  x$link[i] <- paste0("<a href='", url, "'target='_blank'>", occurrenceID[i],"</a>")
                }


                # jabotRB
                if (bibliographicCitation[i] == 'jabotrb')
                {

                  # occurrenceID = 'jabotRB=RB01031072'

                  barcode <- gsub(paste0(bibliographicCitation[i],"=" ), '',occurrenceID[i], ignore.case = TRUE)
                  barcode <- gsub('RB', '',barcode, ignore.case = TRUE)

                  base_url <- 'http://rb.jbrj.gov.br/v2/regua/visualizador.php?r=true&colbot=rb&codtestemunho='
                  url <- paste0(base_url,barcode)

                  x$link[i] <- paste0("<a href='", url, "'target='_blank'> ", occurrenceID[i],"</a>")
                }


                # jabot
                if (bibliographicCitation[i] == "jabot")
                {
                  occurrenceID_tmp <- gsub(paste0(tolower(bibliographicCitation[i]),"=" ), '',occurrenceID[i], ignore.case = TRUE )

                  occurrenceID_tmp2 <- str_split(occurrenceID_tmp,':')

                  col <- occurrenceID_tmp2[[1]][1]

                  cat <- occurrenceID_tmp2[[1]][2]
                  cat <- gsub(col, '',cat )

                  base_url <- "http://rb.jbrj.gov.br/v2/regua/visualizador.php?r=true&colbot=%s&codtestemunho=%s"
                  url <- sprintf(base_url, col, cat)

                  # link <- paste0("<a href='", url, "> ", occurrenceID,"</a>")
                  x$link[i] <- paste0("<a href='", url, "'target='_blank'> ", occurrenceID[i],"</a>")

                }



                if (bibliographicCitation[i] == 'splink')
                {

                  # com barcode

                  if (!is.na(str_locate(occurrenceID[i],':')[[1]]))
                  {

                    occurrenceID_tmp <- gsub(paste0(tolower(bibliographicCitation[i]),"=" ), '',occurrenceID[i] )
                    occurrenceID_tmp <- str_split(occurrenceID_tmp,':')
                    col <- occurrenceID_tmp[[1]][1]
                    cat <- occurrenceID_tmp[[1]][2]
                    # url <- 'https://specieslink.net/search/'
                    url <- paste0('https://specieslink.net/search/records/catalognumber/',cat,'/collectionCode/',col)
                    x$link[i] <- paste0("<a href='", url, "'target='_blank'>", occurrenceID[i],"</a>")

                  }else
                  {
                    barcode <- gsub(paste0(tolower(bibliographicCitation[i]),"=" ), '',occurrenceID[i] )

                    # https://specieslink.net/search/records/barcode/MO0101458866
                    base_url <- "https://specieslink.net/search/records/barcode/%s"
                    url <- sprintf(base_url, barcode)

                    x$link[i] <- paste0("<a href='", url, "'target='_blank'>", occurrenceID[i],"</a>")
                  }



                }


                if(bibliographicCitation[i]=='gbif')
                {

                  occurrenceID_tmp <- gsub(paste0(tolower(bibliographicCitation),"=" ), '',occurrenceID )

                  base_url <- "https://www.gbif.org/occurrence/search?occurrence_id=%s&advanced=1&occurrence_status=present"

                  # url <- sprintf(base_url,occurrenceID_tmp)

                  url <- paste0("https://www.gbif.org/occurrence/search?occurrence_id=",occurrenceID_tmp,"&advanced=1&occurrence_status=present")

                  x$link <- paste0("<a href='", url, "'target='_blank'>", occurrenceID,"</a>")

                }
              }

              return(url)

            }