#' @title Plant Catalog of Units of Brazilian Conservation App
#' @name app_review
#' @description An R app for preparing species listings for the Plant Catalog of Units of Brazilian Conservation.
#' @return CSV files
#' @author Pablo Hendrigo Alves de Melo,
#'        Thuane Bochorny &
#'        Rafaela Forzza
#'        
#' @seealso \code{\link[utils]{download.file}}, \code{\link[utils]{aspell}}
#' 
#' @import dplyr
#' @import tidyr
#' @import readr
#' @import stringr
#' @import lubridate
#' @import jsonlite
#' @import sqldf
#' @import rvest
#' @import shiny
#' @import shinydashboard
#' @import rhandsontable
#' @import DT
#' @import rhandsontable
#' @import shinyWidgets
#' @import measurements
#' @import downloader
#' 
#' @examples
#' \donttest{
#' load_app_data_review()
#' }
#' @export
app_review <- function()
{
  # require(dplyr)
  # require(tidyr)
  # require(readr)
  # require(stringr)
  # require(lubridate)
  # require(jsonlite)
  # require(sqldf)
  # require(rvest)
  # require(shiny)
  # require(shinydashboard)
  # require(rhandsontable)
  # require(DT)
  # require(rhandsontable)
  # require(shinyWidgets)
  # require(measurements)
  # require(downloader)
  
  #  0 - Preparar ambiente R
  {
    #  carregar pacotes básicos
    {
      # 
      # # install.packages('plyr', dependencies = TRUE)
      # library(plyr) 
      # 
      # # install.packages('readxl', dependencies = TRUE)
      # library(readxl) 
      
      # install.packages('dplyr', dependencies = TRUE)
      library(dplyr)
      
      # install.packages('tidyr', dependencies = TRUE)
      library(tidyr)
      
      # # install.packages('biogeo', dependencies = TRUE)
      # library(biogeo)
      
      # install.packages('readr', dependencies = TRUE)
      library(readr)
      
      # install.packages('stringr', dependencies = TRUE)
      library(stringr)
      
      # # install.packages('devtools', dependencies = TRUE)
      # library(devtools)
      # 
      # # devtools::install_github("ropensci/CoordinateCleaner")
      # library(CoordinateCleaner)
      # 
      # # install.packages('dplyr', dependencies = TRUE)
      # library(dplyr)
      # 
      # # install.packages('textclean', dependencies = TRUE)
      # library(textclean)
      # 
      # # install.packages('googledrive', dependencies = TRUE)
      # library(googledrive)
      # 
      # # install.packages('rvest', dependencies = TRUE)
      # library(rvest)
      # 
      # # # install.packages('flora', dependencies = TRUE)
      # # library(flora)
      # 
      # # install.packages('raster', dependencies = TRUE)
      # library(raster)
      # 
      # # install.packages('sp', dependencies = TRUE)
      # library(sp)
      
      # install.packages('lubridate', dependencies = TRUE)
      library(lubridate)
      
      # # install.packages('rnaturalearthdata', dependencies = TRUE)
      # library(rnaturalearthdata)
      # 
      # # # install.packages('geobr', dependencies = TRUE)
      # # library(geobr) 
      # 
      # # # install.packages('monographaR', dependencies = TRUE)
      # # library(monographaR) 
      
      # install.packages('jsonlite', dependencies = TRUE)
      library(jsonlite)
      
      # install.packages('sqldf', dependencies = TRUE)
      library(sqldf) 
      
      # install.packages('rvest', dependencies = TRUE)
      # install.packages("rvest")
      library(rvest)
      
      # install.packages('shiny', dependencies = TRUE)
      library(shiny) 
      
      # # install.packages('shiny', dependencies = TRUE)
      # library(shinydashboardPlus)
      
      # install.packages('shinydashboard', dependencies = TRUE)
      library(shinydashboard)
      
      # install.packages('rhandsontable', dependencies = TRUE)
      library(rhandsontable)
      
      
      # # install.packages('mapview', dependencies = TRUE)
      # library(mapview)
      
      # install.packages('DT', dependencies = TRUE)
      library(DT)
      
      # install.packages('rhandsontable', dependencies = TRUE)
      library(rhandsontable) # tabela editavel
      
      # install.packages('shinyWidgets', dependencies = TRUE)
      library(shinyWidgets) # botoes
      
      # install.packages('measurements', dependencies = TRUE)
      library(measurements)
      
      # install.packages('downloader', dependencies = TRUE)
      library(downloader)
      
      
      
      options(shiny.maxRequestSize=10000*1024^2) 
      
    }
    
    #  cerregar funções desenvolvidas pelo CNCFLora
    {

      #  carregar pacotes R e funções
      {
        
        check_identificationQualifier <- function(txt_search='',
                                                  keyword = c(' aff.', ' cf.'))
        {
          df <- data.frame(txt_search=txt_search, stringsAsFactors = FALSE)
          df$identificationQualifier <- '' 
          
          for(kw in keyword)
          {
            index <- grepl(kw, txt_search, 
                           ignore.case = TRUE,
                           fixed = TRUE)
            
            if (any(index)==TRUE) 
              # { df$identificationQualifier[index==TRUE] <- rep(gsub('\\\\<|\\\\>','',kw),count(index==TRUE)[2,2]) }
            { df$identificationQualifier[index==TRUE] <- rep(gsub('\\\\<|\\\\>','',kw),sum(index==TRUE)) }
            
            df$txt_search[index==TRUE]
          }
          return(df$identificationQualifier)
        }  
        
        # source("get_floraFungaBrasil_online.R", encoding = "UTF-8")
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
        
        get_link_source_record <- function(occurrenceID,
                                           bibliographicCitation,
                                           scientificNameReference)
        {
          
          x <- data.frame(link=rep('',NROW(bibliographicCitation)), stringsAsFactors = FALSE)
          
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
                
                # occurrenceID_tmp <- gsub(paste0(tolower(bibliographicCitation),"=" ), '',occurrenceID )
                # occurrenceID_tmp <- str_split(occurrenceID_tmp,':')
                # 
                # col <- occurrenceID_tmp[[1]][1]
                # cat <- occurrenceID_tmp[[1]][2]
                
                # base_url <- "https://specieslink.net/search/records/col/%s"
                # url <- sprintf(base_url,col %>% tolower(), cat)
                url <- 'https://specieslink.net/search/'
                
                x$link[i] <- paste0("<a href='", url, "'target='_blank'>", occurrenceID[i],"</a>")
                
              }else  
              {
                barcode <- gsub(paste0(tolower(bibliographicCitation[i]),"=" ), '',occurrenceID[i] )
                
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
          
          return(x)
          
        }
        
        
      }
      
    }
  }
  
  #  Selecionar UC
  {
    
    # colunas_ctrl_dwc
    {
      colunas_wcvp_sel <<- c("wcvp_kew_id",
                             "wcvp_family",
                             "wcvp_genus",
                             "wcvp_species",
                             "wcvp_infraspecies",
                             "wcvp_taxon_name",
                             "wcvp_authors",
                             "wcvp_rank",
                             "wcvp_taxonomic_status",
                             "wcvp_accepted_kew_id" ,
                             "wcvp_accepted_name",
                             "wcvp_accepted_authors",
                             "wcvp_parent_kew_id",
                             "wcvp_parent_name",
                             "wcvp_parent_authors",  
                             "wcvp_reviewed",         
                             "wcvp_publication",
                             "wcvp_original_name_id",
                             "wcvp_TAXON_NAME_U",
                             "wcvp_searchNotes",
                             "wcvp_searchedName")
      
      colunas_fb2020_sel <<- c("fb2020_taxonID",
                               "fb2020_acceptedNameUsageID",
                               "fb2020_parentNameUsageID",
                               "fb2020_originalNameUsageID",
                               "fb2020_scientificName",
                               # "fb2020_acceptedNameUsage",
                               # "fb2020_parentNameUsage",
                               "fb2020_namePublishedIn",                  
                               "fb2020_namePublishedInYear",
                               "fb2020_higherClassification",             
                               # "fb2020_kingdom",
                               # "fb2020_phylum",                           
                               # "fb2020_class",
                               # "fb2020_order",                            
                               "fb2020_family",
                               # "fb2020_genus",                            
                               "fb2020_specificEpithet",
                               "fb2020_infraspecificEpithet",             
                               "fb2020_taxonRank",
                               "fb2020_scientificNameAuthorship",
                               "fb2020_taxonomicStatus",
                               "fb2020_nomenclaturalStatus",              
                               "fb2020_modified",
                               "fb2020_bibliographicCitation",
                               "fb2020_references",
                               "fb2020_scientificNamewithoutAuthorship",  
                               "fb2020_scientificNamewithoutAuthorship_U",
                               "fb2020_searchNotes",
                               "fb2020_searchedName")
      
      colunas_ctrl_dwc <<- c('Ctrl_occurrenceID',
                             # Ctrl_lastCrawled,
                             # Ctrl_lastParsed,
                             # Ctrl_lastInterpreted,
                             'Ctrl_bibliographicCitation',
                             'Ctrl_downloadAsSynonym',
                             'Ctrl_scientificNameSearched',
                             'Ctrl_scientificNameReference',
                             'Ctrl_acceptedNameUsage',
                             'Ctrl_scientificNameAuthorship',
                             'Ctrl_scientificName', 
                             'Ctrl_scientificNameOriginalSource', # aqui
                             'Ctrl_family',
                             'Ctrl_genus',
                             'Ctrl_specificEpithet',
                             'Ctrl_infraspecificEpithet',
                             'Ctrl_modified',
                             'Ctrl_institutionCode',
                             'Ctrl_collectionCode',
                             'Ctrl_catalogNumber',
                             # 'Ctrl_barCode', #aqui
                             'Ctrl_identificationQualifier',
                             'Ctrl_identifiedBy',
                             'Ctrl_dateIdentified',
                             'Ctrl_typeStatus',
                             'Ctrl_recordNumber',
                             'Ctrl_recordedBy',
                             'Ctrl_fieldNumber',
                             'Ctrl_country',
                             'Ctrl_stateProvince',
                             'Ctrl_municipality',
                             'Ctrl_locality',
                             
                             # 18-10-21
                             'Ctrl_year',
                             'Ctrl_month',
                             'Ctrl_day',
                             
                             'Ctrl_decimalLatitude',
                             'Ctrl_decimalLongitude',
                             'Ctrl_occurrenceRemarks',
                             'Ctrl_occurrenceID',
                             'Ctrl_comments',
                             'Ctrl_taxonRank')
      
      colunas_verbatin_check <<- c('verbatimNotes',
                                   'temAnoColeta',
                                   'temCodigoInstituicao',
                                   'temNumeroCatalogo',
                                   'temColetor',
                                   'temNumeroColeta',
                                   'temPais',
                                   'temUF',
                                   'temMunicipio',
                                   'temLocalidade',
                                   'temIdentificador',
                                   'temDataIdentificacao')
      
      
      colunas_main_collectors_dictionary <<- c('Ctrl_key_family_recordedBy_recordNumber',
                                               'Ctrl_nameRecordedBy_Standard',
                                               'Ctrl_recordNumber_Standard',
                                               'Ctrl_key_year_recordedBy_recordNumber')
      
      colunas_collection_code_dictionary <<- c('Ctrl_key_collectionCode_catalogNumber',
                                               'Ctrl_collectionCode_Standard',
                                               'Ctrl_catalogNumber_Standard')
      
      
    }
    
    {
      fb2020_names <- data.frame(stringsAsFactors = FALSE,
                                 
                                 fb2020_taxonID = NA,
                                 fb2020_acceptedNameUsageID = NA,
                                 fb2020_parentNameUsageID = NA,
                                 fb2020_originalNameUsageID = NA,
                                 fb2020_scientificName = NA,
                                 # fb2020_acceptedNameUsage = NA,
                                 # fb2020_parentNameUsage = NA,
                                 fb2020_namePublishedIn = NA,                  
                                 fb2020_namePublishedInYear = NA,
                                 fb2020_higherClassification = NA,             
                                 # fb2020_kingdom = NA,
                                 # fb2020_phylum = NA,                           
                                 # fb2020_class = NA,
                                 # fb2020_order = NA,                            
                                 fb2020_family = NA,
                                 # fb2020_genus = NA,                            
                                 fb2020_specificEpithet = NA,
                                 fb2020_infraspecificEpithet = NA,             
                                 fb2020_taxonRank = NA,
                                 fb2020_scientificNameAuthorship = NA,
                                 fb2020_taxonomicStatus = NA,
                                 fb2020_nomenclaturalStatus = NA,              
                                 fb2020_modified = NA,
                                 fb2020_bibliographicCitation = NA,
                                 fb2020_references = NA,
                                 fb2020_scientificNamewithoutAuthorship = NA,  
                                 fb2020_scientificNamewithoutAuthorship_U = NA,
                                 fb2020_searchNotes = NA,
                                 fb2020_searchedName = NA)
      
      wcvp_names <- data.frame(stringsAsFactors = FALSE,
                               
                               wcvp_kew_id = NA,
                               wcvp_family = NA,
                               wcvp_genus = NA,
                               wcvp_species = NA,
                               wcvp_infraspecies = NA,
                               wcvp_taxon_name = NA,
                               wcvp_authors = NA,
                               wcvp_rank = NA,
                               wcvp_taxonomic_status = NA,
                               wcvp_accepted_kew_id  = NA,
                               wcvp_accepted_name = NA,
                               wcvp_accepted_authors = NA,
                               wcvp_parent_kew_id = NA,
                               wcvp_parent_name = NA,
                               wcvp_parent_authors = NA,  
                               wcvp_reviewed = NA,         
                               wcvp_publication = NA,
                               wcvp_original_name_id = NA,
                               wcvp_TAXON_NAME_U = NA,
                               wcvp_searchNotes = NA,
                               wcvp_searchedName = NA)
      
      join_dwc <- data.frame(stringsAsFactors = FALSE,
                             
                             Ctrl_occurrenceID = NA,
                             # Ctrl_lastCrawled = NA,
                             # Ctrl_lastParsed = NA,
                             # Ctrl_lastInterpreted = NA,
                             Ctrl_bibliographicCitation = NA,
                             Ctrl_downloadAsSynonym = NA,
                             Ctrl_scientificNameSearched = NA,
                             Ctrl_scientificNameReference = NA,
                             Ctrl_acceptedNameUsage = NA,
                             Ctrl_scientificNameAuthorship = NA,
                             Ctrl_scientificName = NA, 
                             Ctrl_scientificNameOriginalSource = NA, # aqui
                             Ctrl_family = NA,
                             Ctrl_genus = NA,
                             Ctrl_specificEpithet = NA,
                             Ctrl_infraspecificEpithet = NA,
                             Ctrl_modified = NA,
                             Ctrl_institutionCode = NA,
                             Ctrl_collectionCode = NA,
                             Ctrl_catalogNumber = NA,
                             # Ctrl_barCode = NA, #aqui
                             Ctrl_identificationQualifier = NA,
                             Ctrl_identifiedBy = NA,
                             Ctrl_dateIdentified = NA,
                             Ctrl_typeStatus = NA,
                             Ctrl_recordNumber = NA,
                             Ctrl_recordedBy = NA,
                             Ctrl_fieldNumber = NA,
                             Ctrl_country = NA,
                             Ctrl_stateProvince = NA,
                             Ctrl_municipality = NA,
                             Ctrl_locality = NA,
                             
                             # 18-10-21
                             Ctrl_year = NA,
                             Ctrl_month = NA,
                             Ctrl_day = NA,
                             
                             Ctrl_decimalLatitude = NA,
                             Ctrl_decimalLongitude = NA,
                             Ctrl_occurrenceRemarks = NA,
                             Ctrl_occurrenceID = NA,
                             Ctrl_comments = NA,
                             Ctrl_taxonRank = NA)
      
      verbatin_check <- data.frame(stringsAsFactors = FALSE,
                                   verbatimNotes = NA,
                                   temAnoColeta = NA,
                                   temCodigoInstituicao = NA,
                                   temNumeroCatalogo = NA,
                                   temColetor = NA,
                                   temNumeroColeta = NA,
                                   temPais = NA,
                                   temUF = NA,
                                   temMunicipio = NA,
                                   temLocalidade = NA,
                                   temIdentificador = NA,
                                   temDataIdentificacao = NA)
      
      main_collectors_dictionary <- data.frame(stringsAsFactors = FALSE,
                                               Ctrl_key_family_recordedBy_recordNumber = NA,
                                               Ctrl_nameRecordedBy_Standard = NA,
                                               Ctrl_recordNumber_Standard = NA,
                                               Ctrl_key_year_recordedBy_recordNumber = NA)
      
      collection_code_dictionary <- data.frame(stringsAsFactors = FALSE,
                                               Ctrl_key_collectionCode_catalogNumber = NA,
                                               Ctrl_collectionCode_Standard = NA,
                                               Ctrl_catalogNumber_Standard = NA)
      
      
    }
    
    
    occ_result <<- list(
      fb2020_names = fb2020_names,
      wcvp_names = wcvp_names,
      join_dwc = join_dwc)
    
    
  }
  
  # app
  {
    # global
    {
      Ctrl_dateIdentified <<- Sys.Date()
      Ctrl_identifiedBy <<- ''
      Ctrl_emailVerificador <<- ''
      Ctrl_familyList <<- ''
      Ctrl_scientificNameList <<- ''
      
      # Ctrl_typeCheckIdentification <- ''
      
      
      
      # Ctrl_voucherAmostra = 'Não validado',
      # Ctrl_amostraVerificada = FALSE,
      Ctrl_naoPossivelVerificar <<- FALSE
      Ctrl_observacaoNaoPossivelVerificar <<- ''
      
      
      Ctrl_dataVerificacao <<- Sys.Date()
      # Ctrl_verificadoPor <<- ''
      
      # Ctrl_scientificName_verified <<- ''
      
      Ctrl_family_new_family <<- ''
      
      Ctrl_scientificName_new_family  <<- ''
      
      # atualizar_tabela_identificacao <<- FALSE
      atualizar_tabela_identificacao <<- TRUE
      
    }
    
    #   Preparaçao
    {
      
      Ctrl_observacaoNaoPossivelVerificar_list <- c('Não se aplica',
                                                    'Espécime danificado e/ou em condições não adequadas para verificação (Specimen damaged and/or in conditions not suitable for verification)',
                                                    'Espécime não possui imagem digitalizada (Specimen does not have a scanned image)',
                                                    'Espécime não descrito (Specimen not described)',
                                                    'Material estéril sem possibilidade de identificação (sterile material no possibility of identification)',
                                                    'Outros motivos (Other reasons)')
      
      
      occ_full_tmp <- {}
      occ_full <<- {}
      key_list <<- {}
      
      occ <<- list(reflora=data.frame(),
                   janot=data.frame(),
                   splink=data.frame(),
                   gbif=data.frame(),
                   all=data.frame(),
                   
                   all_results = data.frame(),
                   
                   all_collectionCode=data.frame(),
                   all_mainCollectorLastName=data.frame(),
                   
                   occ_vc=data.frame(),
                   occ_vc_summ=data.frame(),
                   
                   collectionCode=data.frame(),
                   collectionCodeNew=data.frame(),
                   collectionCodeSummary=data.frame(),
                   
                   mainCollectorLastName=data.frame(),
                   mainCollectorLastNameNew=data.frame(),
                   mainCollectorLastNameSummary=data.frame(),
                   # all_updated_collection_collector=data.frame(),
                   
                   all_geo = data.frame(),
                   
                   wcvp = data.frame(),
                   fb2020 = data.frame(),
                   
                   taxonomicAlignment = data.frame(),
                   
                   fb2020_colSearch  = data.frame(),
                   
                   
                   centroids = data.frame(),
                   
                   all_cc = data.frame(),
                   
                   scientificName_list <- data.frame())
      
      # colunas de busca na taxon
      {
        # fb2020
        {
          columns_FB2020 <<- c('taxonRank',
                               'taxonomicStatus')
          # ,
          #                      'nomenclaturalStatus')
          # 
          
          colSearch_fb2020 <<- {}
          
          # fb2020_taxon$taxonRank %>% unique() %>% sort()
          
          taxonRank_fb2020 <- c(
            "ESPECIE",
            "SUB_ESPECIE",
            "VARIEDADE",
            "FORMA")
          colSearch_fb2020[['taxonRank']] <- as.list(taxonRank_fb2020)
          
          # fb2020_taxon$taxonomicStatus %>% unique() %>% sort()
          taxonomicStatus_fb2020 <- c("NOME_ACEITO",
                                      "SINONIMO")
          colSearch_fb2020[['taxonomicStatus']] <- as.list(taxonomicStatus_fb2020)
          
          # nomenclaturalStatus_fb2020 <<-c("NOME_APLICACAO_INCERTA",
          #                                 "NOME_CORRETO",
          #                                 "NOME_CORRETO_VIA_CONSERVACAO",
          #                                 "NOME_ILEGITIMO",
          #                                 "NOME_LEGITIMO_MAS_INCORRETO",
          #                                 "NOME_MAL_APLICADO",
          #                                 "NOME_NAO_EFETIVAMENTE_PUBLICADO",
          #                                 "NOME_NAO_VALIDAMENTE_PUBLICADO" ,
          #                                 "NOME_REJEITADO",
          #                                 "VARIANTE_ORTOGRAFICA")
          # colSearch_fb2020[['nomenclaturalStatus']] <- as.list(nomenclaturalStatus_fb2020)
        }
        
        # wcvp
        {
          columns_wcvp <<- c('rank',
                             'taxonomic_status')
          
          
          colSearch_wcvp <<- {}
          
          # wcvp$rank %>% unique() %>% sort()
          
          taxonRank_wcvp <- c( "Form",
                               "InfraspecificName",
                               "SPECIES",
                               "Subform",
                               "SUBSPECIES",
                               "Subvariety",
                               "VARIETY"  )
          colSearch_wcvp[['rank']] <- as.list(taxonRank_wcvp)
          
          # wcvp$taxonomic_status %>% unique() %>% sort()
          taxonomicStatus_wcvp <- c("Accepted",
                                    "Artificial Hybrid",
                                    "Homotypic_Synonym",
                                    "Synonym",
                                    "Unplaced")
          colSearch_wcvp[['taxonomic_status']] <- as.list(taxonomicStatus_wcvp)
        }
        
      }
      
    }
    
    #  Tela APP--
    ui <- 
      {
        shinydashboard::dashboardPage(
          shinydashboard::dashboardHeader(title = "Flora UCs BR"),
          shinydashboard::dashboardSidebar(width = 350,
                                           collapsed = TRUE,
                                           
                                           box(status = "primary", width = 12,
                                               title = '', background = 'navy', # red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
                                               
                                               # selectInput("sp", label = 'Selecione uma espécie:',choices = spp$FB2020_AcceptedNameUsage),
                                               
                                               # actionButton("selectBtn", "Selecionar espécie", icon = icon("play")),
                                           )
          ),
          
          shinydashboard::dashboardBody(
            
            navbarPage("Verificador de dados de flora do Catálogo de Plantas das Unidades de Conservação do Brasil (Flora data verifier of the Catalog of Plants of Conservation Units in Brazil)",
                       
                       #  Tela para verificação da identificação de amostras e duplicatas 
                       tabPanel(icon("check"), 
                                # navbarPage("Validação de identificação taxonômica e seleção de material testemunho (Validation of taxonomic identification and voucher selection)",
                                #            tabPanel(icon("sitemap 
                                
                                box(title = '1. Informações do pesquisador (Researcher information)',
                                    status = "primary",
                                    width = 12,
                                    
                                    wellPanel(
                                      fluidRow(
                                        column(
                                          width = 12,
                                          
                                          textInput("Ctrl_verificadoPor_Input", "Verificado por (Verified by):", Ctrl_identifiedBy),
                                          
                                          textInput("Ctrl_emailVerificador_Input", "Email do verificador (Verifier email):", Ctrl_emailVerificador),
                                          
                                          dateInput("Ctrl_dataVerificacao_Input", "Data de verificação (Verification date):", value = Ctrl_dateIdentified, ) #language = "ru"
                                        )
                                      )
                                    )
                                ),
                                
                                box(title = '2. Carregar arquivo CSV e informações associadas (Upload CSV file and associated information)',
                                    status = "primary",
                                    width = 12,
                                    
                                    wellPanel(
                                      fluidRow(
                                        column(width = 12,
                                               
                                               helpText('2.1. Escolher o arquivo CSV padronizado com registros de ocorrência (Choose standardized CSV file with occurrence records)'),
                                               
                                               fileInput(inputId = "occResultsFile",
                                                         label = '',#"CSV file",
                                                         multiple = FALSE)
                                               
                                        )),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               
                                               helpText("2.2. Carregar lista de ocorrências para validação (Load data for validation)"),
                                               
                                               actionButton("getUnvalidatedNamesBtn", "Carregar dados (Load data)", 
                                                            icon = icon("play")),
                                               br(),
                                               br(),
                                        )),
                                      
                                      fluidRow(
                                        column(width = 12,
                                               
                                               shiny::tags$a('Flora e Funga do Brasil', href = 'ipt.jbrj.gov.br/jbrj/resource?r=lista_especies_flora_brasil'),
                                               helpText(paste0('2.3. Carregar dados da Flora e Funga do Brasil no formato Darwin Core (Download Flora and Funga do Brazil in Darwin Core format)')),
                                               actionButton("getfb2020Btn", "Carregar Flora e Funga do Brasil", onclick = 'Shiny.onInputChange(\"checkSpeciesNames_FB2020_Btn\",  Math.random())',
                                                            icon = icon("download"))),
                                      )),
                                    
                                    wellPanel(
                                      fluidRow(
                                        # column(width = 12,
                                        #        actionButton("getResults", "2.3. Carregar Revisões Prévias", onclick = 'Shiny.onInputChange(\"save_verified_namesBtn\",  Math.random())',
                                        #                     icon = icon("play"))),
                                        
                                        column(width = 12,
                                               actionButton("getResultsBtn", "Carregar dados (Load data)", 
                                                            icon = icon("play"))),
                                        
                                        column(
                                          width = 12,
                                          
                                          helpText('Amostras/espécies Verificadas (Verified samples/species)'),
                                          rHandsontableOutput('hot_verified_samples'),
                                        )
                                      )
                                      
                                      
                                    ),
                                    
                                    
                                )
                       ),
                       
                       box(title = '3. Selecionar família (Select family)',
                           status = "primary",
                           width = 12,
                           
                           wellPanel(
                             fluidRow(
                               column(
                                 width = 12,
                                 
                                 selectInput("Ctrl_familyList_Input", "Escolha uma família (Choose family) :",
                                             Ctrl_familyList),
                                 
                               )
                             )
                           )
                       ),
                       
                       
                       fluidRow(
                         
                         column(width = 6,
                                
                                
                                # helpText('Painel de Seleção'),
                                box(title = "4. Selecionar espécime/amostra por espécie (Select specimen/sample by species)",
                                    status = "primary",
                                    width = 12,
                                    
                                    wellPanel(
                                      
                                      
                                      fluidRow(
                                        column(
                                          width = 12,
                                          helpText('Lista de espécies (Species list)'),
                                          column(width = 12, rHandsontableOutput("hot_specie_key"))
                                        ))),
                                    
                                    wellPanel(
                                      fluidRow(
                                        column(
                                          width = 12,
                                          helpText('Amostras/espécies (Samples/species)'),
                                          column(width = 12, rHandsontableOutput('hot_summary_key')),
                                          
                                          # Amostra selecionada (Sample selected)
                                          # column(
                                          #   width = 12,
                                          #   helpText('Amostra selecionada (Sample selected)'),
                                          #   
                                          #   verbatimTextOutput("text_verified_names"),
                                          #   
                                          #   verbatimTextOutput("text_verified_samples")
                                          # ),
                                          
                                        ))),
                                    
                                    wellPanel(
                                      fluidRow(
                                        column(
                                          width = 12,
                                          helpText('Duplicatas/amostra (Duplicates/sample)'),
                                          
                                          helpText('Escolha o voucher (choose a voucher)'),
                                          
                                          htmlOutput("link_key_text"),
                                          
                                          rHandsontableOutput('hot_details_key')
                                        ))),
                                    
                                )),
                         
                         column(width = 6,
                                
                                
                                box(title = '5. Confirmar, ou atribuir nova, identificação taxonômica (Confirm, or assign new, taxonomic identification)',
                                    status = "primary",
                                    width = 12,
                                    
                                    wellPanel(
                                      fluidRow(
                                        column(
                                          width = 12,
                                          
                                          selectInput("Ctrl_scientificName_select_Input", "Selecionar nome científico (Choose scientific name):",
                                                      Ctrl_scientificNameList)
                                          
                                        ),
                                        
                                        column(
                                          width = 12,
                                          helpText('Amostras já selecionadas para esta espécie (Samples already selected for this species)'),
                                          
                                          # rHandsontableOutput('tbl_verified_samples')
                                          verbatimTextOutput("text_verified_samples_sel")
                                          
                                        ),
                                        
                                      )),
                                    
                                    wellPanel(
                                      fluidRow(
                                        column(
                                          width = 12,
                                          
                                          textInput("Ctrl_family_verified_Input", "Atribuir outra família (Assign another Family) :", Ctrl_family_new_family),
                                          
                                        )
                                      )
                                    ),
                                    
                                    wellPanel(
                                      fluidRow(
                                        column(
                                          width = 12,
                                          
                                          selectInput("Ctrl_observacaoNaoPossivelVerificar_list_Input", 
                                                      "Informe a razão caso o espécime não possa ser verificado (State reason in case the specimen cannot be verified) :",
                                                      Ctrl_observacaoNaoPossivelVerificar_list,
                                                      selected = 'Não se aplica')
                                          
                                        )
                                      ))
                                    
                                ),
                                
                                
                                wellPanel(
                                  fluidRow(
                                    column(
                                      width = 12,
                                      
                                      actionButton("save_verified_namesBtn", "Salvar (Save)", icon = icon("save")),
                                      
                                      verbatimTextOutput("text_getResults"),
                                      
                                    )
                                  )),
                                
                                
                                
                         )
                         
                       ),
                       
                       fluidRow(
                         column(width = 12,
                                box(title = '6. Baixar resultados (Download results)',
                                    status = "primary",
                                    width = 12,
                                    
                                    wellPanel(
                                      fluidRow(
                                        
                                        column(
                                          width = 12,
                                          downloadButton("downloadVerificacaoAmostra", "Baixar resultados (Download results)"),
                                          # br()
                                        )
                                      )
                                    )
                                    
                                ))
                       ),
                       
                       
                       fluidRow(
                         column(width = 12,
                                box(title = 'Copyright ©',
                                    status = "primary",
                                    width = 12,
                                    
                                    wellPanel(
                                      fluidRow(
                                        
                                        column(
                                          width = 12,
                                          helpText("Administrado pelo Instituto de Pesquisas Jardim Botânico do Rio de Janeiro"),
                                          
                                          helpText("Desenvolvido por: Melo, Pablo Hendrigo Alves de, Bochorny, Thuane & Forzza, Rafaela Campostrini"),
                                          
                                          helpText("Versão 1.2 de janeiro/2024"),
                                          
                                        ))
                                      
                                    )))),
            )
          )
          # }
        )}
    
    
    #  Server
    server <- function(input, output, session)
    {
      Ctrl_observacaoNaoPossivelVerificar <<- ''
      
      Ctrl_family_new_family <<- ''
      
      Ctrl_scientificName_new_family  <<- ''
      
      dados <- reactive({
        if(NROW(occ_full)<=1){
          return({})
        }else{
          return(occ_full)
        }
      })
      
      #  selecao especies por uc
      {
        
        output$link_key_text <- renderPrint({ 
          req(NROW(hot_to_r(input$hot_specie_key))>0)
          
          key_tmp <- ID_key(input)
          
          index <- occ_full$Ctrl_key_family_recordedBy_recordNumber %in% key_tmp
          
          dt <- occ_full[index==TRUE,]
          
          x <- get_link_source_record(dt$Ctrl_occurrenceID,
                                      dt$Ctrl_bibliographicCitation,
                                      dt$Ctrl_scientificName) #%>% data.frame(stringsAsFactors = FALSE)
          
          x
        })
        
        
        output$downloadVerificacaoAmostra <- downloadHandler(
          filename = function() {
            paste("Planilha_Verificador_Catalogo_de_Plantas_UCs_Brasil - ",input$Ctrl_verificadoPor_Input, ' - ', Sys.Date(), ".csv", sep="")
          },
          content = function(file) {
            write.csv(occ_full %>% data.frame(stringsAsFactors = FALSE), file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
          })
        
        
        output$text_verified_samples_sel <- renderText({
          index_res <- occ_full$Ctrl_scientificName_verified %in% input$Ctrl_scientificName_select_Input
          x_res <- occ_full[index_res==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>% unique() %>% na.omit() %>% as.character() 
          paste(x_res ,sep = ',')
        })
        
        
        output$text_verified_samples <- renderText({
          
          # key_tmp <- ID_specie(input)
          # index <- occ_full$Ctrl_scientificName %in% key_tmp
          # 
          # key_tmp <- unique(occ_full[index==TRUE,]$Ctrl_key_family_recordedBy_recordNumber)
          
          # rr <- hot_to_r(input$hot_summary_key)
          # key_tmp <- rr[,1]
          shiny::validate(
            need(NROW(hot_to_r(input$hot_specie_key))>0,  "..."))
          
          rr <- get_current_slice_specie() 
          
          index <- occ_full$Ctrl_key_family_recordedBy_recordNumber %in% rr[,1] & ! occ_full$Ctrl_scientificName_verified == ''
          
          x_res <- occ_full[index==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>% unique() %>% na.omit() %>% as.character()
          
          paste(x_res ,sep = ',')
          
          # rr <- hot_to_r(input$hot_details_key)
          # scientificName_verified_tmp <- unique(rr$scientificName_verified)
          # scientificName_verified_tmp <- ifelse(is.na(scientificName_verified_tmp),'x', scientificName_verified_tmp)
          # index_res <- occ_full$Ctrl_scientificName_verified %in% scientificName_verified_tmp
          # x_res <- occ_full[index_res==TRUE,]$Ctrl_key_family_recordedBy_recordNumber %>% unique() %>% as.character()
          # 
          # paste(x_res ,sep = ',')
        })
  

        output$text_getResults <- renderText({
          req(input$save_verified_namesBtn)
          x_tmp <- save_verified_names()
          print(paste0('Última alteração salva com sucesso em: ', Sys.Date()) )
        })
        
        
        output$hot_verified_samples <- renderRHandsontable(
          {
            # 02 02 24
            shiny::validate(
              need(NROW(hot_to_r(input$hot_specie_key))>0,  "..."))

              # dt <- save_verified_names()
            
              # dt <- dados()
              
              dt <- occ_full

              rr <- hot_to_r(input$hot_details_key)

              scientificName_verified_tmp <- unique(rr$scientificName_verified) %>% na.omit()

            index_res <- (dt$Ctrl_voucherAmostra == TRUE | dt$Ctrl_naoPossivelVerificar == TRUE) &
              (toupper(dt$Ctrl_emailVerificador) %in% toupper(input$Ctrl_emailVerificador_Input))

            dt <- dt[index_res==TRUE,] %>%
              dplyr::rename(source = Ctrl_bibliographicCitation,
                            collectionCode_catalogNumber = tombo,
                            family = Ctrl_family,
                            scientificName = Ctrl_scientificName,
                            identifiedBy = Ctrl_identifiedBy,
                            dateIdentified = Ctrl_dateIdentified,
                            recordedBy = Ctrl_recordedBy,
                            recordNumber = Ctrl_recordNumber,
                            country = Ctrl_country,
                            stateProvince = Ctrl_stateProvince,
                            municipality = Ctrl_municipality,
                            locality = Ctrl_locality,
                            Longitude = Ctrl_decimalLongitude,
                            Latitude = Ctrl_decimalLatitude,
                            year = Ctrl_year,
                            month = Ctrl_month,
                            day = Ctrl_day,
                            key = Ctrl_key_family_recordedBy_recordNumber,
                            family_verified = Ctrl_family_verified,
                            scientificName_verified = Ctrl_scientificName_verified,) %>%
              dplyr::select(Ctrl_voucherAmostra,
                            family_verified,
                            scientificName_verified,
                            fb2020_scientificName, fb2020_searchNotes, source, family, scientificName, identifiedBy, dateIdentified, recordedBy, recordNumber, country, stateProvince, municipality, locality, collectionCode_catalogNumber, Longitude, Latitude, year, month, day,
                            Ctrl_Record_ID_Review,
                            key)


            rhandsontable::rhandsontable(dt,
                                         # width = 600, height = 250,
                                         width = '100%', height = 150,

                                         digits = 0,

                                         selectionMode = 'single',
                                         selectCallback = TRUE) %>%
              hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE)

          })
        
        
        save_verified_names <- eventReactive(input$save_verified_namesBtn,
                                             {
                                               if (atualizar_tabela_identificacao == TRUE)
                                               {
                                                 
                                                 msg_tmp <- TRUE
                                                 
                                                 df <- hot_to_r(input$hot_details_key)
                                                 
                                                 # df_sample <- hot_to_r(input$hot_summary_key)
                                                 
                                                 if(input$Ctrl_emailVerificador_Input=='' | input$Ctrl_verificadoPor_Input=='')
                                                 { 
                                                   showModal(modalDialog( title = "Por gentileza, informe nome e email do veficador.",
                                                                          '', easyClose = TRUE, footer = NULL ))
                                                   msg_tmp <- FALSE
                                                 }
                                                 
                                                 
                                                 if(input$Ctrl_observacaoNaoPossivelVerificar_list_Input!='Não se aplica')
                                                 {
                                                   
                                                   for(i_upd in 1:NROW(df))
                                                   {
                                                     index_upd <- occ_full$Ctrl_Record_ID_Review %in% df$Ctrl_Record_ID_Review[i_upd] # %>% round(.,0) %>% as.character()
                                                     
                                                     occ_full[index_upd==TRUE,]$Ctrl_voucherAmostra <<- FALSE
                                                     
                                                     occ_full[index_upd==TRUE,]$Ctrl_amostraVerificada <<- FALSE
                                                     occ_full[index_upd==TRUE,]$Ctrl_naoPossivelVerificar <<- TRUE
                                                     
                                                     occ_full[index_upd==TRUE,]$Ctrl_observacaoNaoPossivelVerificar <<- input$Ctrl_observacaoNaoPossivelVerificar_list_Input
                                                     
                                                     occ_full[index_upd==TRUE,]$Ctrl_scientificName_verified <<- ''
                                                     
                                                     occ_full[index_upd==TRUE,]$Ctrl_family_verified <<- ''
                                                     
                                                     occ_full[index_upd==TRUE,]$Ctrl_dataVerificacao <<- input$Ctrl_dataVerificacao_Input %>% as.character()
                                                     occ_full[index_upd==TRUE,]$Ctrl_verificadoPor <<- input$Ctrl_verificadoPor_Input
                                                     occ_full[index_upd==TRUE,]$Ctrl_emailVerificador <<- input$Ctrl_emailVerificador_Input
                                                   }
                                                   
                                                 }else
                                                 {
                                                   if(sum(df$Ctrl_voucherAmostra==TRUE)>1)
                                                   {
                                                     showModal(modalDialog( title = "Selecione somente uma amostra.",
                                                                            '', easyClose = TRUE, footer = NULL ))
                                                     msg_tmp <- FALSE
                                                   }
                                                   
                                                   if(input$Ctrl_scientificName_select_Input=='' &
                                                      (sum(df$Ctrl_voucherAmostra)>0))
                                                   { 
                                                     showModal(modalDialog( title = "Selecione um nome científico para a amostra.",
                                                                            '', easyClose = TRUE, footer = NULL ))
                                                     msg_tmp <- FALSE
                                                   }
                                                   
                                                   spp_sel_tmp <- ""
                                                   
                                                   spp_sel_tmp <- input$Ctrl_scientificName_select_Input
                                                   
                                                   if(msg_tmp == TRUE)
                                                   {
                                                     for(i_upd in 1:NROW(df))
                                                     {
                                                       
                                                       if (sum(df$Ctrl_voucherAmostra)>0)
                                                       {  
                                                         #
                                                         index_upd <- occ_full$Ctrl_Record_ID_Review %in% df$Ctrl_Record_ID_Review[i_upd] # %>% round(.,0) %>% as.character()
                                                         
                                                         occ_full[index_upd==TRUE,]$Ctrl_voucherAmostra <<- df$Ctrl_voucherAmostra[i_upd]
                                                         
                                                         occ_full[index_upd==TRUE,]$Ctrl_amostraVerificada <<- TRUE
                                                         occ_full[index_upd==TRUE,]$Ctrl_naoPossivelVerificar <<- FALSE
                                                         
                                                         occ_full[index_upd==TRUE,]$Ctrl_observacaoNaoPossivelVerificar <<- ''
                                                         
                                                         # if(input$Ctrl_scientificName_Input!='')
                                                         # {
                                                         #   occ_full[index_upd==TRUE,]$Ctrl_scientificName_verified <<- input$Ctrl_scientificName_Input
                                                         # }else
                                                         # {
                                                         occ_full[index_upd==TRUE,]$Ctrl_scientificName_verified <<- input$Ctrl_scientificName_select_Input
                                                         # }
                                                         
                                                         # Ctrl_familyList_Input
                                                         if(input$Ctrl_family_verified_Input!='')
                                                         {
                                                           occ_full[index_upd==TRUE,]$Ctrl_family_verified <<- input$Ctrl_family_verified_Input
                                                         }else
                                                         {
                                                           occ_full[index_upd==TRUE,]$Ctrl_family_verified <<- input$Ctrl_familyList_Input
                                                         }
                                                         
                                                         occ_full[index_upd==TRUE,]$Ctrl_dataVerificacao <<- input$Ctrl_dataVerificacao_Input %>% as.character()
                                                         occ_full[index_upd==TRUE,]$Ctrl_verificadoPor <<- input$Ctrl_verificadoPor_Input
                                                         occ_full[index_upd==TRUE,]$Ctrl_emailVerificador <<- input$Ctrl_emailVerificador_Input
                                                       }else
                                                       {  
                                                         #
                                                         index_upd <- occ_full$Ctrl_Record_ID_Review %in% df$Ctrl_Record_ID_Review[i_upd] # %>% round(.,0) %>% as.character()
                                                         
                                                         occ_full[index_upd==TRUE,]$Ctrl_voucherAmostra <<- FALSE
                                                         
                                                         occ_full[index_upd==TRUE,]$Ctrl_amostraVerificada <<- FALSE
                                                         occ_full[index_upd==TRUE,]$Ctrl_naoPossivelVerificar <<- FALSE
                                                         
                                                         occ_full[index_upd==TRUE,]$Ctrl_observacaoNaoPossivelVerificar <<- ''
                                                         
                                                         occ_full[index_upd==TRUE,]$Ctrl_scientificName_verified <<- ''
                                                         
                                                         occ_full[index_upd==TRUE,]$Ctrl_family_verified <<- ''
                                                         
                                                         occ_full[index_upd==TRUE,]$Ctrl_dataVerificacao <<- ''
                                                         occ_full[index_upd==TRUE,]$Ctrl_verificadoPor <<- ''
                                                         occ_full[index_upd==TRUE,]$Ctrl_emailVerificador <<- ''
                                                       }    
                                                       
                                                       # showModal(modalDialog( title = "Alteração realizada com sucesso!",
                                                       #                        '', easyClose = TRUE, footer = NULL ))
                                                       
                                                     }
                                                     
                                                   }
                                                   
                                                 }
                                                 
                                               }
                                               
                                               return(dados())
                                               
                                             })
        
        
        getResults <- eventReactive(input$getResultsBtn,
                                    {
                                      atualizar_tabela_identificacao <<- FALSE
                                      
                                      save_verified_names()
                                      
                                      atualizar_tabela_identificacao <<- TRUE
                                      
                                      
                                    })
        
        
        loadoccResults <- reactive(
          {
            req(input$occResultsFile)
            tryCatch(
              {
                # files_tmp <- 'C:\\Users\\Pablo Hendrigo\\Downloads\\Standardize_Updated_Occurrences_CollectionCode_MainCollector - 2022-11-22 (2).csv'
                
                if(is.null(occ_full))
                {
                  
                  
                  files_tmp <- input$occResultsFile$datapath
                  occ_tmp <- readr::read_csv(files_tmp,
                                             locale = readr::locale(encoding = "UTF-8"),
                                             show_col_types = FALSE) %>%
                    data.frame() 
                  
                  
                  occ_tmp$Ctrl_family <- occ_tmp$Ctrl_family %>% toupper()
                  
                  # limpar occ_full$Ctrl_catalogNumber
                  occ_tmp$tombo <- paste0(occ_tmp$Ctrl_collectionCode,'_', occ_tmp$Ctrl_catalogNumber)
                  
                  occ_tmp$Ctrl_recordedBy_U <- occ_tmp$Ctrl_recordedBy %>% toupper()
                  
                  return(occ_tmp)
                }else
                {
                  return(occ_full)
                }
                
                
              },
              error = function(e) {
                stop(safeError(e))
              }
            )
          })
        
        
        getUnvalidatedNames <- eventReactive(input$getUnvalidatedNamesBtn,
                                             {
                                               req(input$occResultsFile)
                                               
                                               withProgress(message = 'Processing...', style = 'notification', value = 0.5, {
                                                 
                                                 # occ_full vem do loadoccResults
                                                 
                                                 occ_full <<- loadoccResults()
                                                 
                                                 
                                                 key_list <<- sqldf::sqldf("SELECT DISTINCT Ctrl_key_family_recordedBy_recordNumber as Chave_Para_Agrupar_Duplicatas
                                               FROM occ_full
                                               GROUP BY Chave_Para_Agrupar_Duplicatas
                                               ORDER BY Chave_Para_Agrupar_Duplicatas")
                                                 
                                                 incProgress(100, detail = '100')
                                                 
                                                 # source_data <- 'all_results'
                                                 
                                                 # occ[[source_data]] <<- occ_full
                                                 
                                                 
                                                 Ctrl_familyList <<- occ_full$fb2020_family %>% unique() %>% toupper() %>% sort()
                                                 
                                                 # Ctrl_familyList <<- occ[[source_data]]$fb2020_family %>% unique() %>% toupper() %>% sort()
                                                 
                                                 # updateTextAreaInput(session, "Ctrl_familyList_Input", value = Ctrl_familyList)
                                                 
                                                 updateSelectInput(session, "Ctrl_familyList_Input",
                                                                   # label = paste("Select input label", length(x)),
                                                                   choices = Ctrl_familyList)#,
                                                 # selected = Ctrl_familyList[1])
                                                 
                                                 return(occ_full)
                                                 
                                               })
                                               
                                             })
        
        
        get_current_slice_specie <- reactive(
          {
            req(input$getfb2020Btn)
            
            key_tmp <- ID_specie(input)
            index <- occ_full$Ctrl_scientificName %in% key_tmp
            occ_full_tmp <- occ_full[index==TRUE,]
            
            occ_full_tmp$typeStatus <- ''
            
            occ_full_tmp$typeStatus <- ifelse(is.na(occ_full_tmp$Ctrl_typeStatus), '', occ_full_tmp$Ctrl_typeStatus)
            occ_full_tmp$typeStatus <- ifelse(occ_full_tmp$Ctrl_typeStatus=='não', '', occ_full_tmp$Ctrl_typeStatus)
            
            
            sqldf::sqldf("SELECT DISTINCT Ctrl_key_family_recordedBy_recordNumber as Chave_Para_Agrupar_Duplicatas, typeStatus
                                               FROM occ_full_tmp
                                               GROUP BY Chave_Para_Agrupar_Duplicatas
                                               ORDER BY typeStatus DESC")
          })
        
        
        get_current_slice_key <- reactive(
          {
            # aqui
            req(input$getfb2020Btn)
            
            key_tmp <- ID_key(input)
            # key_tmp <- "ARALIACEAE_LOPES_528"
            
            # index <- occ[['all_results']]$Ctrl_key_family_recordedBy_recordNumber %in% key_tmp
            index <- occ_full$Ctrl_key_family_recordedBy_recordNumber %in% key_tmp
            
            {
              
              # selected_dup <- occ[['all_results']][index==TRUE,]$fb2020_scientificName[1]
              selected_dup <- occ_full[index==TRUE,]$fb2020_scientificName[1]
              
              index_tmp <- Ctrl_scientificNameList$scientificName %in% selected_dup
              
              
              if(sum(index_tmp)>0)
              {
                # aqui 
                updateSelectInput(session, "Ctrl_scientificName_select_Input",
                                  # label = paste("Select input label", length(x)),
                                  choices = Ctrl_scientificNameList$scientificName,
                                  selected = selected_dup)#,
              }else{
                updateSelectInput(session, "Ctrl_scientificName_select_Input",
                                  # label = paste("Select input label", length(x)),
                                  choices = Ctrl_scientificNameList$scientificName,
                                  selected = '')
              }   
            }
            
            
            # occ[['all_results']][index==TRUE,] %>%
            occ_full[index==TRUE,] %>%
              dplyr::rename(source = Ctrl_bibliographicCitation,
                            collectionCode_catalogNumber = tombo,
                            family = Ctrl_family,
                            scientificName = Ctrl_scientificName,
                            identifiedBy = Ctrl_identifiedBy,
                            dateIdentified = Ctrl_dateIdentified,
                            recordedBy = Ctrl_recordedBy,
                            recordNumber = Ctrl_recordNumber,
                            country = Ctrl_country,
                            stateProvince = Ctrl_stateProvince,
                            municipality = Ctrl_municipality,
                            locality = Ctrl_locality,
                            Longitude = Ctrl_decimalLongitude,
                            Latitude = Ctrl_decimalLatitude,
                            year = Ctrl_year,                                        
                            month = Ctrl_month,
                            day = Ctrl_day,
                            key = Ctrl_key_family_recordedBy_recordNumber,
                            family_verified = Ctrl_family_verified,
                            scientificName_verified = Ctrl_scientificName_verified,) %>% 
              dplyr::select(Ctrl_voucherAmostra, 
                            family_verified,
                            scientificName_verified,
                            fb2020_scientificName, fb2020_searchNotes, source, family, scientificName, identifiedBy, dateIdentified, recordedBy, recordNumber, country, stateProvince, municipality, locality, collectionCode_catalogNumber, Longitude, Latitude, year, month, day,
                            Ctrl_Record_ID_Review,
                            key)
          })
        
        
        output$hot_specie_key <- renderRHandsontable(
          {
            # shiny::validate(
            #    need(NROW(occ[['all_results']])>0,  "..."))
            
            
            req(input$getfb2020Btn)
            
            dt <- getUnvalidatedNames()
            
            dt <- dt %>% dplyr::filter(Ctrl_family %in% input$Ctrl_familyList_Input )
            
            # Ctrl_scientificName_select
            # Ctrl_scientificName_edit
            # Ctrl_scientificName
            # Ctrl_scientificName
            # fb2020_scientificName
            
            rhandsontable(dt  %>%
                            dplyr::filter(Ctrl_taxonRank %in% c('SPECIES', 'VARIETY', 'SUBSPECIES', 'FORM')) %>%
                            dplyr::select(Ctrl_family, Ctrl_scientificName, fb2020_searchNotes) %>%
                            dplyr::distinct_at(., c('Ctrl_family', 'Ctrl_scientificName', 'fb2020_searchNotes')) %>%
                            dplyr::rename(family=Ctrl_family,
                                          scientificName=Ctrl_scientificName) %>%
                            arrange_at(.,c('family','scientificName','fb2020_searchNotes') ), 
                          row_highlight = 1,
                          
                          width = '100%', height = 200,
                          
                          digits = 0,
                          selectionMode = 'single',
                          selectCallback = TRUE) %>%
              hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE) 
          })
        
        
        output$hot_summary_key <- renderRHandsontable(
          {
            shiny::validate(
              need(NROW(hot_to_r(input$hot_specie_key))>0,  "..."))
            
            dt <- get_current_slice_specie() 
            
            rhandsontable::rhandsontable(dt,
                                         # width = 600, height = 250,
                                         # width = '100%', height = 150,
                                         width = '100%', height = 100,
                                         
                                         digits = 0,
                                         
                                         selectionMode = 'single',
                                         selectCallback = TRUE) %>%
              hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE) 
            
          })
        
        
        output$hot_details_key <- renderRHandsontable(
          {
            shiny::validate(
              need(NROW(hot_to_r(input$hot_specie_key))>0,  "..."))
            
            {
              dt <- checkSpeciesNames_FB2020()
              dt <- dt %>% dplyr::filter(toupper(family) %in% input$Ctrl_familyList_Input )
              dt <- dt %>%
                dplyr::select(scientificName,
                              taxonomicStatus,
                              nomenclaturalStatus)
              
              Ctrl_scientificNameList <<- dt %>%
                dplyr::arrange(scientificName) %>%
                dplyr::select(scientificName)
              
            }
            
            dt <- get_current_slice_key()
            
            rhandsontable::rhandsontable(dt,
                                         # width = 1300, height = 150,
                                         width = '100%', height = 100,
                                         
                                         digits = 0,
                                         
                                         selectionMode = 'single',
                                         selectCallback = TRUE) %>%
              hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE) %>%
              hot_col(col = 'Ctrl_voucherAmostra', readOnly = FALSE, type='checkbox')
            
            
            
          })
        
        
        # funcoes internas
        {
          ID_specie <- function(input)
          {
            linha <- input$hot_specie_key_select$select$r
            rr <- hot_to_r(input$hot_specie_key)
            
            if ( is.null(linha))
            {
              return(rr[1,2])
            }
            
            if ( linha>NROW(rr))
            {
              return(rr[1,2])
            }else
            {
              return(rr[linha,2])
            }
          }
          
          ID_key <- function(input)
          {
            linha <- input$hot_summary_key_select$select$r
            rr <- hot_to_r(input$hot_summary_key)
            
            if ( is.null(linha))
            {
              return(rr[1,1])
            }
            
            if ( linha>NROW(rr))
            {
              return(rr[1,1])
            }else
            {
              return(rr[linha,1])
            }
          }
          
          ID_dup_name <- function(input)
          {
            linha <- input$hot_details_key_select$select$r
            rr <- hot_to_r(input$hot_details_key)
            
            if ( is.null(linha))
            {
              return(rr[1,1])
            }
            
            if ( linha>NROW(rr))
            {
              return(rr[1,1])
            }else
            {
              return(rr[linha,1])
            }
          }
          
          ID_list_name <- function(input)
          {
            linha <- input$hot_scientificName_identification_select$select$r
            rr <- hot_to_r(input$hot_scientificName_identification)
            
            if ( is.null(linha))
            {
              return(rr[1,1])
            }
            
            if ( linha>NROW(rr))
            {
              return(rr[1,1])
            }else
            {
              return(rr[linha,1])
            }
          }
          
          
        }
        
      }
      
      
      # applyTaxonomicAlignment_Contents
      # backbone
      {
        
        # save taxonomic
        {
          
          # downloadData_applyCollectionCodesPattern_NewCollectionCodesDictionary
          output$downloadData_applyTaxonomicAlignment <- downloadHandler(
            filename = function() {
              paste("TaxonomicAlignment - ", Sys.Date(), ".csv", sep="")
            },
            content = function(file) {
              
              colunas_occ <- colnames(occ[['all']])
              
              occ[['all_results']] <<- occ[['all']]
              
              if(NROW(occ[['taxonomicAlignment']])==NROW(occ[['all_results']]))
              {
                occ[['all_results']] <<- cbind(occ[['all_results']],
                                               occ[['taxonomicAlignment']] %>%
                                                 dplyr::select(-colunas_occ))
              }
              
              
              if(NROW(occ[['all_mainCollectorLastName']])==NROW(occ[['all_results']]))
              {
                occ[['all_results']] <<- cbind(occ[['all_results']],
                                               occ[['all_mainCollectorLastName']] %>%
                                                 dplyr::select(-colunas_occ))
              }
              
              if(NROW(occ[['all_collectionCode']])==NROW(occ[['all_results']]))
              {
                occ[['all_results']] <<- cbind(occ[['all_results']],
                                               occ[['all_collectionCode']] %>%
                                                 dplyr::select(-colunas_occ))
              }
              
              write.csv(occ[['all_results']] %>% data.frame(stringsAsFactors = FALSE), file, row.names = FALSE, fileEncoding = "UTF-8", na = "")
            })
        }
        
        
        # applyTaxonomicAlignment_Contents 
        {
          applyTaxonomicAlignment <- 
            eventReactive(input$applyTaxonomicAlignment_Btn,
                          {
                            # data set base  
                            {
                              index <- occ[['all']]$Ctrl_taxonRank %>% toupper() %in%
                                toupper(c('SPECIES',
                                          'VARIETY',
                                          'SUBSPECIES',
                                          'FORM'))
                              
                              occ_all <- occ[['all']] %>%
                                dplyr::mutate(.submittedToWCVP =  ifelse(index==TRUE, TRUE, FALSE))
                              
                              
                              name_search_wcvp <- occ_all[index==TRUE,]$Ctrl_scientificName %>% unique() %>% as.character()
                              # NROW(name_search_wcvp)
                              
                              occ_all <- occ_all %>%
                                dplyr::mutate(wcvp_kew_id = NA,
                                              wcvp_family = NA,
                                              wcvp_genus = NA,
                                              wcvp_species = NA,
                                              wcvp_infraspecies = NA,
                                              wcvp_taxon_name = NA,
                                              wcvp_authors = NA,
                                              wcvp_rank = NA,
                                              wcvp_taxonomic_status = NA,
                                              wcvp_accepted_kew_id = NA,
                                              wcvp_accepted_name = NA,
                                              wcvp_accepted_authors = NA,
                                              wcvp_parent_kew_id = NA,
                                              wcvp_parent_name = NA,
                                              wcvp_parent_authors = NA,
                                              wcvp_reviewed = NA,
                                              wcvp_publication = NA,
                                              wcvp_original_name_id = NA,
                                              wcvp_TAXON_NAME_U = NA,
                                              wcvp_searchNotes = NA,
                                              wcvp_searchedName = NA) %>%
                                dplyr::mutate(fb2020_taxonID = NA,
                                              fb2020_acceptedNameUsageID = NA,
                                              fb2020_parentNameUsageID = NA,
                                              fb2020_originalNameUsageID = NA,
                                              fb2020_scientificName = NA,
                                              # fb2020_acceptedNameUsage = NA,
                                              # fb2020_parentNameUsage = NA,
                                              fb2020_namePublishedIn = NA,
                                              fb2020_namePublishedInYear = NA,
                                              fb2020_higherClassification = NA,             
                                              # fb2020_kingdom = NA,
                                              # fb2020_phylum = NA,                     
                                              # fb2020_class = NA,
                                              # fb2020_order = NA,                       
                                              fb2020_family = NA,
                                              # fb2020_genus = NA,                      
                                              fb2020_specificEpithet = NA,
                                              fb2020_infraspecificEpithet = NA,             
                                              fb2020_taxonRank = NA,
                                              fb2020_scientificNameAuthorship = NA,
                                              fb2020_taxonomicStatus = NA,
                                              fb2020_nomenclaturalStatus = NA,             
                                              fb2020_modified = NA,
                                              fb2020_bibliographicCitation = NA,
                                              fb2020_references = NA,
                                              fb2020_scientificNamewithoutAuthorship = NA,
                                              fb2020_scientificNamewithoutAuthorship_U = NA,
                                              fb2020_searchNotes = NA,
                                              fb2020_searchedName = NA)
                              
                              wcvp_na <- data.frame(wcvp_kew_id = NA,
                                                    wcvp_family = NA,
                                                    wcvp_genus = NA,
                                                    wcvp_species = NA,
                                                    wcvp_infraspecies = NA,
                                                    wcvp_taxon_name = NA,
                                                    wcvp_authors = NA,
                                                    wcvp_rank = NA,
                                                    wcvp_taxonomic_status = NA,
                                                    wcvp_accepted_kew_id = NA,
                                                    wcvp_accepted_name = NA,
                                                    wcvp_accepted_authors = NA,
                                                    wcvp_parent_kew_id = NA,
                                                    wcvp_parent_name = NA,
                                                    wcvp_parent_authors = NA,
                                                    wcvp_reviewed = NA,
                                                    wcvp_publication = NA,
                                                    wcvp_original_name_id = NA,
                                                    wcvp_TAXON_NAME_U = NA,
                                                    wcvp_searchNotes = NA,
                                                    wcvp_searchedName = NA)
                              
                              fb2020_na <- data.frame(fb2020_taxonID = NA,
                                                      fb2020_acceptedNameUsageID = NA,
                                                      fb2020_parentNameUsageID = NA,
                                                      fb2020_originalNameUsageID = NA,
                                                      fb2020_scientificName = NA,
                                                      # fb2020_acceptedNameUsage = NA,
                                                      # fb2020_parentNameUsage = NA,
                                                      fb2020_namePublishedIn = NA,
                                                      fb2020_namePublishedInYear = NA,
                                                      fb2020_higherClassification = NA,             
                                                      # fb2020_kingdom = NA,
                                                      # fb2020_phylum = NA,                     
                                                      # fb2020_class = NA,
                                                      # fb2020_order = NA,                       
                                                      fb2020_family = NA,
                                                      # fb2020_genus = NA,                      
                                                      fb2020_specificEpithet = NA,
                                                      fb2020_infraspecificEpithet = NA,             
                                                      fb2020_taxonRank = NA,
                                                      fb2020_scientificNameAuthorship = NA,
                                                      fb2020_taxonomicStatus = NA,
                                                      fb2020_nomenclaturalStatus = NA,             
                                                      fb2020_modified = NA,
                                                      fb2020_bibliographicCitation = NA,
                                                      fb2020_references = NA,
                                                      fb2020_scientificNamewithoutAuthorship = NA,
                                                      fb2020_scientificNamewithoutAuthorship_U = NA,
                                                      fb2020_searchNotes = NA,
                                                      fb2020_searchedName = NA)
                              
                              # fb2020_na <- fb2020[1,]
                              # fb2020_na[1,] <- NA
                            }
                            
                            withProgress(message = 'Processing...', style = 'notification', value = 0.5, 
                                         {
                                           x <- {}
                                           i <- 1
                                           # i <- 938
                                           
                                           ok <- FALSE
                                           japrocessado <<- rep(FALSE,NROW(name_search_wcvp))
                                           
                                           while (i<=NROW(name_search_wcvp) & ok != TRUE)
                                           {
                                             try(
                                               {
                                                 
                                                 for(i in 1:NROW(name_search_wcvp))
                                                 {
                                                   if(japrocessado[i]==TRUE){next} # 03-05-2022 para evitar tentar baixar a mesma espécies 2 vezes caso ocorra erro
                                                   
                                                   sp_tmp <- name_search_wcvp[i]
                                                   # 'FloraFungaBrasil','WCVP'
                                                   if(any(input$taxonomicBackbone %in% c('WCVP'))==TRUE)
                                                   {
                                                     x_tmp <- checkName_WCVP(searchedName = sp_tmp,
                                                                             wcvp_name = occ[['wcvp']])
                                                   }else
                                                   {
                                                     x_tmp <- wcvp_na
                                                   }
                                                   
                                                   if(any(input$taxonomicBackbone %in% c('FloraFungaBrasil'))==TRUE)
                                                   {
                                                     x_tmp_fb2020 <- checkName_FloraFungaBrasil(searchedName = sp_tmp,
                                                                                                fb2020 = occ[['fb2020']])
                                                     # colnames(x_tmp) <- paste0('wcvp_',colnames(x_tmp))
                                                   }else
                                                   {
                                                     x_tmp_fb2020 <- fb2020_na
                                                   }
                                                   
                                                   
                                                   x <- rbind(x,
                                                              cbind(x_tmp[,
                                                                          colunas_wcvp_sel],
                                                                    x_tmp_fb2020[,
                                                                                 colunas_fb2020_sel]))
                                                   
                                                   index <- occ_all$Ctrl_scientificName %in% name_search_wcvp[i]
                                                   
                                                   n_reg <- NROW(occ_all[index==TRUE,])
                                                   
                                                   if(any(input$taxonomicBackbone %in% c('WCVP'))==TRUE)
                                                   {
                                                     print( str_c( i, ' - WCVP: ', name_search_wcvp[i], ' : ',n_reg, ' registros - ', ifelse(is.na(x_tmp$wcvp_taxon_name),'',x_tmp$wcvp_taxon_name), ' : ', x_tmp$wcvp_searchNotes))
                                                   }
                                                   
                                                   if(any(input$taxonomicBackbone %in% c('FloraFungaBrasil'))==TRUE)
                                                   {
                                                     print( str_c( i, ' - FB2020: ', name_search_wcvp[i], ' : ',n_reg, ' registros - ', ifelse(is.na(x_tmp_fb2020$fb2020_scientificName),'',x_tmp_fb2020$fb2020_scientificName), ' : ', x_tmp_fb2020$fb2020_searchNotes))
                                                   }
                                                   
                                                   # occ_all[index==TRUE,]$scientificNamewithoutAuthorship
                                                   
                                                   occ_all[index==TRUE,
                                                           c(colunas_wcvp_sel, colunas_fb2020_sel)] <- cbind(x_tmp[,
                                                                                                                   colunas_wcvp_sel],
                                                                                                             x_tmp_fb2020[,
                                                                                                                          colunas_fb2020_sel])
                                                   
                                                   
                                                   # occ_all[index==TRUE,
                                                   #         colunas_fb2020_sel] <- x_tmp_fb2020[,colunas_fb2020_sel]
                                                   
                                                   
                                                   japrocessado[i] <- TRUE
                                                 }
                                                 
                                                 ok <- TRUE
                                               })
                                             
                                             print('reconectar em 2 segundos...')
                                             Sys.sleep(2)
                                           }
                                           
                                           incProgress(100, detail = '100')
                                         })
                            
                            source_data <- 'taxonomicAlignment'
                            occ[[source_data]] <<- occ_all
                            NROW(occ[[source_data]]) 
                            
                            # return(x)
                            return(occ_all)
                          })
          
          output$applyTaxonomicAlignment_Contents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                         {
                                                                           applyTaxonomicAlignment()
                                                                           # occ[['taxonomicAlignment']] <<- applyTaxonomicAlignment()
                                                                           # occ[['taxonomicAlignment']]
                                                                           # print(NROW(occ[['taxonomicAlignment']]))
                                                                         })
        }
        
        
        # checkSpeciesNames_FB2020
        {
          checkSpeciesNames_FB2020 <- eventReactive(input$checkSpeciesNames_FB2020_Btn,
                                                    {
                                                      withProgress(message = 'Processing...', style = 'notification', value = 0.5, {
                                                        
                                                        # occ[['fb2020']] <<- get_floraFungaBrasil(path_results = "C:/Dados/APP_GBOT/data")#path_data) 
                                                        
                                                        #online 
                                                        occ[['fb2020']] <<- get_floraFungaBrasil(path_results = tempdir())#path_data) 
                                                        
                                                        
                                                        # colnames(occ[['fb2020']]) <<- paste0('fb2020_',colnames(occ[['fb2020']]))
                                                        
                                                        # occ_result[['fb2020_names']] <<- occ[['fb2020']] %>% 
                                                        #    dplyr::select(colunas_fb2020_sel)
                                                        
                                                        # occ_result[['fb2020_names']] <<- occ[['fb2020']] %>% 
                                                        #    dplyr::select(colunas_fb2020_sel)
                                                        
                                                        
                                                        # r_tmp <- get_floraFungaBrasil()
                                                        # 
                                                        # occ[['fb2020']] <<- r_tmp[['taxon_floraFungaBrasil']]
                                                        # 
                                                        # occ[['fb2020_colSearch']] <<- r_tmp[['colSearch']]
                                                        
                                                        incProgress(100, detail = '100')
                                                      })
                                                      
                                                      return(occ[['fb2020']])
                                                    })
          
          # aqui 21-11-2022
          
          output$checkSpeciesNames_FB2020_Contents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                          {
                                                                            data_sel_fb2020()
                                                                            # occ[['fb2020']] <<- checkSpeciesNames_FB2020()
                                                                          })
          
          
          output$forMenu    <- renderUI({createSelectRadio(1)})    
          
          createSelectRadio <- function(id){
            
            family_ID <- paste0('family_ID',id)
            
            if(!NROW(checkSpeciesNames_FB2020())>0)
            {res <- list(
              selectInput(inputId = family_ID,
                          label = 'Family:',
                          choices = as.list(c('Tudo', 'get data...'))))
            }else{   
              res <- list(
                selectInput(inputId = family_ID,
                            label = 'Family:',
                            choices = as.list(c('Tudo',
                                                checkSpeciesNames_FB2020()$family %>%
                                                  unique() %>% sort()))))
            }
            return(res)
          }
          
          data_sel_fb2020 <- reactive({
            
            occ[['fb2020']] <<- checkSpeciesNames_FB2020()
            
            if (input$family_ID1 == 'Tudo'){
              return(occ[['fb2020']] %>% dplyr::filter(
                # nomenclaturalStatus %in% input$nomenclaturalStatus &
                taxonomicStatus %in% input$taxonomicStatus &
                  taxonRank %in% input$taxonRank))
            }else{
              return(occ[['fb2020']] %>% dplyr::filter(family %in% input$family_ID1 & 
                                                         # nomenclaturalStatus %in% input$nomenclaturalStatus &
                                                         taxonomicStatus %in% input$taxonomicStatus &
                                                         taxonRank %in% input$taxonRank))
              
            }
          })
          
        }
        
        # checkSpeciesNames_WCVPN
        {
          checkSpeciesNames_WCVPN <- eventReactive(input$checkSpeciesNames_WCVPN_Btn,
                                                   {
                                                     withProgress(message = 'Processing...', style = 'notification', value = 0.5, {
                                                       
                                                       occ[['wcvp']] <<- get_wcvp(url_source = 'http://sftp.kew.org/pub/data-repositories/WCVP/Archive/wcvp_webapp_oct_2019_to_jun_2022/',
                                                                                  path_results = path_data,
                                                                                  update = update_wcvp)$wcvp_names
                                                       
                                                       incProgress(100, detail = '100')
                                                     })
                                                     
                                                     return(occ[['wcvp']])
                                                   })
          
          output$checkSpeciesNames_WCVPN_Contents <- DT::renderDataTable(options = list(scrollX = TRUE),
                                                                         {
                                                                           # checkSpeciesNames_WCVPN()
                                                                           
                                                                           data_sel_wcvp()
                                                                         })
          
          output$forMenu_wcvp    <- renderUI({createSelectRadio_wcvp(1)})    
          
          createSelectRadio_wcvp <- function(id){
            
            family_ID <- paste0('family_wcvp_ID',id)
            
            if(!NROW(checkSpeciesNames_WCVPN())>0)
            {res <- list(
              selectInput(inputId = family_ID,
                          label = 'Family:',
                          choices = as.list(c('Tudo', 'get data...'))))
            }else{   
              res <- list(
                selectInput(inputId = family_ID,
                            label = 'Family:',
                            choices = as.list(c('Tudo',
                                                checkSpeciesNames_WCVPN()$family %>%
                                                  unique() %>% sort()))))
            }
            return(res)
          }
          
          data_sel_wcvp <- reactive({
            
            occ[['wcvp']] <<- checkSpeciesNames_WCVPN()
            
            if (input$family_wcvp_ID1 == 'Tudo'){
              return(occ[['wcvp']] %>% dplyr::filter(
                # nomenclaturalStatus %in% input$nomenclaturalStatus &
                taxonomic_status %in% input$taxonomic_status &
                  rank %in% input$rank))
            }else{
              return(occ[['wcvp']] %>% dplyr::filter(family %in% input$family_wcvp_ID1 & 
                                                       # nomenclaturalStatus %in% input$nomenclaturalStatus &
                                                       taxonomic_status %in% input$taxonomic_status &
                                                       rank %in% input$rank))
              
            }
          })
        }
        
      }

    }
    
    #  Run the application 
    shinyApp(ui = ui, server = server)
    
  }

}

# load_app_data_review()
