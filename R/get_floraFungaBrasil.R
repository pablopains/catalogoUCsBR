#' Get Flora&FungaBrasil
#'
get_floraFungaBrasil <- function(url_source = "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil",
                                path_results = NA,
                                full=FALSE) # if NULL
{

    destfile <- paste0(path_results,"/IPT_FloraFungaBrasil_",Sys.Date(),'.zip')
    downloader::download(url = url_source, destfile = destfile, mode = "wb")
    utils::unzip(destfile, exdir = path_results) # descompactar e salvar dentro subpasta "ipt" na pasta principal


    #  taxon
    fb2020_taxon  <- readr::read_delim(paste0(path_results,'/taxon.txt'), delim = "\t", quote = "") %>%
    dplyr::select(-id)

    index = fb2020_taxon$taxonRank %in% c("ESPECIE",
                                        "SUB_ESPECIE",
                                        "VARIEDADE",
                                        "FORMA")

    fb2020_taxon  <- fb2020_taxon[index==TRUE,]

    index = fb2020_taxon$taxonRank %in% c("ESPECIE",
                                        "SUB_ESPECIE",
                                        "VARIEDADE",
                                        "FORMA")

    scientificName_tmp <- fb2020_taxon$scientificName %>% stringr::str_split(.,pattern = ' ', simplify = TRUE)

    # carregando especie sem autor
    scientificName <- rep('',nrow(fb2020_taxon))

    scientificName[index==TRUE] <- scientificName_tmp[index==TRUE,1] %>% trimws(.,'right')

    index = fb2020_taxon$taxonRank %in% c("ESPECIE")

    scientificName[index==TRUE] <-  paste0(scientificName_tmp[index==TRUE,1], ' ', scientificName_tmp[index==TRUE,2]) #%>% trimws(.,'right')

    index = fb2020_taxon$taxonRank %in% c("VARIEDADE")
    scientificName[index==TRUE] <-  paste0(fb2020_taxon$genus[index==TRUE], ' ', fb2020_taxon$specificEpithet[index==TRUE], ' var. ', fb2020_taxon$infraspecificEpithet[index==TRUE])# %>% trimws(.,'right')

    index = fb2020_taxon$taxonRank %in% c("SUB_ESPECIE")
    scientificName[index==TRUE] <-  paste0(fb2020_taxon$genus[index==TRUE], ' ', fb2020_taxon$specificEpithet[index==TRUE], ' subsp. ', fb2020_taxon$infraspecificEpithet[index==TRUE])# %>% trimws(.,'right')

    index = fb2020_taxon$taxonRank %in% c("FORMA")
    scientificName[index==TRUE] <-  paste0(fb2020_taxon$genus[index==TRUE], ' ', fb2020_taxon$specificEpithet[index==TRUE], ' form. ', fb2020_taxon$infraspecificEpithet[index==TRUE])# %>% trimws(.,'right')

    fb2020_taxon$scientificNamewithoutAuthorship = scientificName
    fb2020_taxon$scientificNamewithoutAuthorship_U = toupper(scientificName)


    return(fb2020_taxon)

}