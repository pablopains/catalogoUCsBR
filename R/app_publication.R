#' @title Plant Catalog of Units of Brazilian Conservation App
#' @name app_publication
#' @description An R app for preparing species listings for the Plant Catalog of Units of Brazilian Conservation.
#' @return CSV files
#' @author Pablo Hendrigo Alves de Melo,
#'        Thuane Bochorny &
#'        Rafaela Forzza
#'        
#' @seealso \code{\link[utils]{download.file}}, \code{\link[utils]{aspell}}
#' 
#' @importFrom dplyr %>% select filter arrange_at left_join mutate group_by summarise n distinct
#' @importFrom tidyr separate
#' @importFrom readr read_csv read_delim locale
#' @importFrom stringr str_split str_sub str_locate str_detect
#' @importFrom lubridate year month day
#' @importFrom jsonlite fromJSON
#' @importFrom sqldf sqldf
#' @importFrom shiny shinyApp reactive req validate need withProgress incProgress
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar dashboardBody sidebarMenu menuItem
#' @importFrom DT datatable
#' @importFrom rhandsontable rhandsontable hot_table hot_col
#' @importFrom shinyWidgets pickerInput
#' @importFrom measurements conv_unit
#' @importFrom downloader download
#' @importFrom writexl write_xlsx
#' @importFrom tidyverse everything
#' @importFrom purrr map_df
#' @importFrom utils download.file unzip
#' @importFrom stats na.omit
#'  
#' @examples
#' \donttest{
#' app_publication()
#' }
#' 
#' @export
app_publication <- function()
{
  {
    options(shiny.maxRequestSize=10000*1024^2) 
    
    require(dplyr)
    require(tidyr)
    require(readr)
    require(stringr)
    require(lubridate)
    require(jsonlite)
    require(sqldf)
    require(rvest)
    require(shiny)
    require(shinydashboard)
    require(rhandsontable)
    require(DT)
    require(rhandsontable)
    require(shinyWidgets)
    require(measurements)
    require(downloader)
    require(app_publication)
    require(writexl)
    require(tidyverse)
    require(purrr)
    
    # source('functions.R')
    {
      
      
      get_floraAmeacadaBR <- function(url_source = "https://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_oficial_ameacadas_portaria_443",
                                      path_results = NA)
        
      {  
        
        destfile <- paste0(path_results,"/IPT_floraAmeacadaBR_",Sys.Date(),'.zip')
        downloader::download(url = url_source, destfile = destfile, mode = "wb") 
        utils::unzip(destfile, exdir = path_results) # descompactar e salvar dentro subpasta "ipt" na pasta principal
        
        
        #  taxon
        taxon  <- readr::read_delim(paste0(path_results,'/taxon.txt'), delim = "\t", quote = "") 
        
        
        taxon$scientificName_U = toupper(taxon$scientificName)
        
        distribution.file <- paste0(path_results,"/distribution.txt")
        distribution <- read_delim(distribution.file, delim = "\t", quote = "") 
        
        taXON <- dplyr::left_join(taxon, distribution %>% dplyr::select(id,threatStatus),
                                  by = c('id'))
        
        return(taXON)
        
      }     
      
      categoria_ameaca_flora_Portaria_443 <- function(scientificName_search=c('Justicia ramulosa','Aphelandra margaritae'), 
                                                      floraAmeacadaBR=NA)
      {
        x <- data.frame(categoria_ameaca_flora_Portaria_443=rep('',NROW(scientificName_search)))
        
        for(i in 1:NROW(scientificName_search))
        {
          index <- floraAmeacadaBR$scientificName %in% scientificName_search[i]
          
          if(sum(index)>0){
            x$categoria_ameaca_flora_Portaria_443[i] <- floraAmeacadaBR$threatStatus[index==TRUE]
          }
        }
        
        return(x$categoria_ameaca_flora_Portaria_443)
        
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
        
        return(x)
        
      }
      
      get_floraFungaBrasil_v3 <- function(url_source = "http://ipt.jbrj.gov.br/jbrj/archive.do?r=lista_especies_flora_brasil",
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
        
        # fb2020_taxon$group <- str_split(fb2020_taxon$higherClassification,';',simplify = TRUE)[,2]
        
        distribution.file <- paste0(path_results,"/distribution.txt")
        distribution <- read_delim(distribution.file, delim = "\t", quote = "") 
        
        distribution <- distribution %>%
          dplyr::select(id, establishmentMeans)
        
        return(list(fb2020_taxon=fb2020_taxon,
                    fb2020_distribution=distribution))
        
      }
      
      
      FB2020_get_group <- function(x)
      {
        if (is.na(x))
        {return('')}
        # higherClassification <- floraFB$higherClassification[999] %>% as_tibble()
        x <- x %>% as_tibble()
        group <- x$value %>% str_split(.,pattern = ';', simplify = TRUE)
        group <- group[2]
        return(group)
      }
      
      
      pega_autor <- function(x)
      {
        autor <- rep('',length(x))
        for(i in 1:length(x))
        {
          x1 <- x[i]
          x2 <- paste0(word(x1,1) ,' ',word(x1,2),' ')
          
          x3 <- sub(x2, '', x1)
          autor[i] <- x3
        }
        return(autor)
      }
      
      
    }   
    
    # variaveis
    {
      UC_Input <<- ""
      fb2020 <<-{}
      distribution <<- {}
      floraAmeacadaBR <<- {}
      
      # UC_Input <<- ""
      # fb2020 <<-{}
      # occ_full <<- {}
      # distribution <<- {}
      # floraAmeacadaBR <<- {}
      
      Ctrl_observacaoNaoPossivelVerificar_list <- c('Não se aplica',
                                                    'Espécimes estão danificados e/ou em condições não adequadas para verificação',
                                                    'Espécimes não possuem imagens digitalizadas',
                                                    'Espécime não descrito',
                                                    'Material estéril sem possibilidade de identificação',
                                                    'Outra Família',
                                                    'Outros motivos')
      
      Ctrl_dateIdentified <<- Sys.Date()
      Ctrl_identifiedBy <<- ''
      Ctrl_emailVerificador <<- ''
      Ctrl_familyList <<- ''
      Ctrl_scientificNameList <<- ''
      Ctrl_naoPossivelVerificar <<- FALSE
      Ctrl_observacaoNaoPossivelVerificar <<- ''
      Ctrl_dataVerificacao <<- Sys.Date()
      Ctrl_scientificName_new_family  <<- ''
      atualizar_tabela_identificacao <<- TRUE
      spp_sel <<- ''
      Ctrl_FB2020_familyList <<- ''
      Ctrl_new_specieList <<- ''
      
      
    }
  }
  #  Tela APP--
  
  ui <- 
    {
      shinydashboard::dashboardPage(
        dashboardHeader(title = "Publicação Catálogo UCs BR"),
        
        dashboardSidebar(width = 230,
                         collapsed = TRUE,
                         sidebarMenu(
                           menuItem("Manual de instruções aplicativo", 
                                    icon = icon("book"), 
                                    href = "http://app-catalogo-ucs-brasil.jbrj.gov.br/Manual_aplicativo_catalogoUCsBR_portugues.pdf", 
                                    newtab = TRUE),
                           menuItem("Application Instruction Manual", 
                                    icon = icon("book"), 
                                    href = "http://app-catalogo-ucs-brasil.jbrj.gov.br/Manual_aplicativo_catalogoUCsBR_ingles.pdf", 
                                    newtab = TRUE),
                           menuItem("Manual para obtenção de dados", 
                                    icon = icon("book"), 
                                    href = "http://app-catalogo-ucs-brasil.jbrj.gov.br/Manual_de_Download_Dados_catalogoUCsBR_portugues.pdf",
                                    newtab = TRUE),
                           menuItem("Manual for obtaining data", 
                                    icon = icon("book"), 
                                    href = "http://app-catalogo-ucs-brasil.jbrj.gov.br/User_Guide_Data_Download_catalogoUCsBR_english.pdf",
                                    newtab = TRUE),
                           menuItem("REFLORA", 
                                    icon = icon("link"), 
                                    href = "http://reflora.jbrj.gov.br/reflora/herbarioVirtual/ConsultaPublicoHVUC/ResultadoDaConsultaNovaConsulta.do",
                                    newtab = TRUE),
                           menuItem("Jabot", 
                                    icon = icon("link"), 
                                    href = "http://jabot.jbrj.gov.br/v3/consulta.php",
                                    newtab = TRUE),
                           menuItem("SpeciesLink", 
                                    icon = icon("link"), 
                                    href = "https://specieslink.net/search/",
                                    newtab = TRUE),
                           menuItem("GBIF", 
                                    icon = icon("link"), 
                                    href = "https://www.gbif.org/occurrence/search?occurrence_status=present&q=",
                                    newtab = TRUE),
                           menuItem("Flora e Funga do Brasil", 
                                    icon = icon("link"), 
                                    href = "https://floradobrasil.jbrj.gov.br/consulta/#CondicaoTaxonCP",
                                    newtab = TRUE)
                         )),
        
        shinydashboard::dashboardBody(
          
          navbarPage("Aplicativo de publicação de dados para as listas do Catálogo de Plantas das Unidades de Conservação do Brasil",
                     
                     #  Tela para verificação da identificação de amostras e duplicatas 
                     tabPanel(icon("check"), 
                              # navbarPage("Validação de identificação taxonômica e seleção de material testemunho (Validation of taxonomic identification and voucher selection)",
                              #            tabPanel(icon("sitemap 
                              
                              box(title = '1. Informações sobre a unidade de conservação',
                                  status = "primary",
                                  width = 12,
                                  
                                  wellPanel(
                                    fluidRow(
                                      column(
                                        width = 12,
                                        shiny::tags$div(
                                          title = "Informe nome da unidade de conservação.",
                                          textInput("UC_Input", "1.1. Nome da Unidade de conservação", UC_Input))
                                      )),
                                    
                                    fluidRow(
                                      column(
                                        width = 12,
                                        helpText('Arquivos revisados por especialistas'),
                                        
                                        shiny::tags$div(
                                          title = "Carregar arquivos CSV revisados por especialistas.",
                                          
                                          fileInput(inputId = "occResultsFile",
                                                    label = '1.2. Carregar Arquivos CSVs',#"CSV file",
                                                    multiple = TRUE)) 
                                      ))
                                  )
                              ),
                              
                              
                              
                     ),
                     
                     
                     fluidRow(
                       column(width = 12,
                              box(title = '2. Seleção de vouchers para publicação no Catálogo de Plantas das Unidades de Conservação do Brasil',
                                  status = "primary",
                                  width = 12,
                                  
                                  wellPanel(
                                    fluidRow(
                                      column(
                                        width = 12,
                                        helpText('Escolher um voucher para cada espécie entre os não marcados.'),
                                        rHandsontableOutput('hot_verified_samples'),
                                        # ),
                                        # 
                                        # column(
                                        #    width = 6,
                                        br(),
                                        helpText('Planilha modelo'),
                                        rHandsontableOutput('hot_modelo')
                                      )))
                              ),
                              
                              
                              box(title = '3. Especies sem indicação de voucher',
                                  status = "primary",
                                  width = 12,
                                  
                                  wellPanel(
                                    
                                    fluidRow(
                                      column(
                                        width = 12,
                                        rHandsontableOutput('hot_sem_voucher')
                                      )))),
                              
                              box(title = '4. Amostras com problemas na identificação',
                                  status = "primary",
                                  width = 12,
                                  
                                  wellPanel(
                                    
                                    fluidRow(
                                      column(
                                        width = 12,
                                        rHandsontableOutput('hot_problemas')
                                      )))),
                              
                              
                              
                              
                              
                              box(title = '5. Salvar resuldados',
                                  status = "primary",
                                  width = 12,
                                  
                                  wellPanel(
                                    fluidRow(
                                      column(
                                        width = 12,
                                        downloadButton("download_ModeloCatalogo", "Baixar planilha Modelo Catálogo de Plantas das Unidades de Conservação do Brasil",
                                                       title = "Baixar planilha Modelo Catálogo de Plantas das Unidades de Conservação do Brasil."),
                                      )),
                                    
                                    br(),
                                    
                                    fluidRow(
                                      column(
                                        width = 12,
                                        downloadButton("download_problemas", "Baixar planilha com amostras com algum problema na identificação",
                                                       title = "Baixar planilha com amostras com algum problema na identificação"),
                                      )),
                                    
                                    br(),
                                    
                                    fluidRow(
                                      column(
                                        width = 12,
                                        downloadButton("download_sem_volcher", "Baixar planilha com amostras sem verificação",
                                                       title = "Baixar planilha com amostras sem verificação."),
                                      ))
                                  )),
                              
                       )
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
                                        helpText("Administrado pelo Instituto de Pesquisas Jardim Botânico do Rio de Janeiro "),
                                        
                                        helpText("Desenvolvido por: Melo, Pablo Hendrigo Alves de, Bochorny, Thuane & Forzza, Rafaela Campostrini"),
                                        
                                        helpText("Versão 1.0.0 de janeiro/2025"),
                                        
                                      ))
                                    
                                  )))),
          )
        )
        # }
      )}
  
  
  #  Server
  server <- function(input, output, session)
  {
    
    # var global
    {
      occ_full <<- {}

      key_list <<- {}
      
      occ_full_tmp <- {}
      
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
    }
    
    loadoccResults <- reactive(
      {
        req(input$occResultsFile)
        
        withProgress(message = 'Processing...', style = 'notification', value = 0.25, {
          tryCatch(
            {
              if(is.null(occ_full))
              {
                
                incProgress(0.2, detail = 'load...')
                # files_tmp <- 'C:\\Users\\Pablo Hendrigo\\Downloads\\Standardize_Updated_Occurrences_CollectionCode_MainCollector - 2022-11-22 (2).csv'
                files_tmp <- input$occResultsFile$datapath
                nf <- length(files_tmp)
                occ_tmp <- data.frame({})
                for(i in 1:nf)
                {
                  occ_tmp_1 <- readr::read_csv(files_tmp[i], 
                                               locale = readr::locale(encoding = "UTF-8"),
                                               show_col_types = FALSE)
                  
                  occ_tmp <- rbind.data.frame(occ_tmp, occ_tmp_1)   
                }
                
                
                occ_tmp$Ctrl_Record_ID_Review2 <- 1:NROW(occ_tmp)
                
                incProgress(0.2, detail = 'Flora & Funga...')
                fb2020_x <- get_floraFungaBrasil_v3(path_results = tempdir())
                
                fb2020 <- fb2020_x$fb2020_taxon
                distribution <- fb2020_x$fb2020_distribution
                
                index <- fb2020$scientificName %in% occ_tmp$Ctrl_scientificName_verified
                
                fb2020 <- fb2020[index==TRUE,]
                
                incProgress(0.1, detail = 'CNCFlora - Lista Oficial de Espécies Ameaçadas - Portaria 443...')
                
                floraAmeacadaBR <<- get_floraAmeacadaBR(path_results = tempdir())
                
                incProgress(0.1, detail = 'groups...')
                group1 <- lapply(fb2020$higherClassification, FB2020_get_group)
                group <- plyr::ldply(group1, data.frame) 
                colnames(group) <- 'group'
                fb2020$group <- group$group %>% as.character()
                
                occ_tmp$group <- rep('',NROW(occ_tmp))
                occ_tmp$establishmentMeans <- rep('',NROW(occ_tmp))
                occ_tmp$fb2020_id <- rep(0,NROW(occ_tmp))
                
                for(i in 1:NROW(occ_tmp))
                {
                  index <- fb2020$scientificName %in% occ_tmp$Ctrl_scientificName_verified[i]
                  
                  occ_tmp$group[i] <- fb2020$group[index==TRUE][1]
                  occ_tmp$fb2020_id[i] <- fb2020$taxonID[index==TRUE][1] 
                }
                
                incProgress(0.1, detail = 'distribution...')
                
                index <- distribution$id %in% occ_tmp$fb2020_id
                distribution <- distribution[index==TRUE,]
                
                for(i in 1:NROW(occ_tmp))
                {
                  index2 <- distribution$id %in% occ_tmp$fb2020_id[i]
                  
                  occ_tmp$establishmentMeans[i] <- distribution$establishmentMeans[index2==TRUE][1]
                  
                }
                
                
                occ_full <<- occ_tmp
                incProgress(0.1, detail = 'OK!')
              }
              
            },
            error = function(e) {
              stop(safeError(e))
            })
          
        })   
        
        return(occ_full)
      })      
    
    
    output$hot_verified_samples <- renderRHandsontable(
      {
        shiny::validate(
          need(NROW(sum(loadoccResults()$Ctrl_voucherAmostra == TRUE)) >0 ,  "..."))
        
        dt <- loadoccResults()
        
        index_res <- (dt$Ctrl_voucherAmostra == TRUE)
        
        if(sum(index_res)>0)
        {
          
          dt <- dt[index_res==TRUE,] 
          
          bancodados <- stringr::str_sub(dt$Ctrl_occurrenceID, 
                                         1, 
                                         stringr::str_locate(dt$Ctrl_occurrenceID, '=')[,1]-1)
          
          
          bancodados <- paste0(toupper(str_sub(bancodados,1,1)),str_sub(bancodados, 2,nchar(bancodados)))
          
          barcode <- stringr::str_sub(dt$Ctrl_occurrenceID,
                                      stringr::str_locate(dt$Ctrl_occurrenceID, '=')[,1]+1,
                                      nchar(dt$Ctrl_occurrenceID))
          
          x <- dt$Ctrl_scientificName_verified
          sp_tmp <- paste0(word(x,1) ,' ',word(x,2), ' ')
          autor <- rep('',NROW(x))
          
          dt$Incluir_Amostra <- TRUE
          for(i in 1:NROW(dt))
          {
            index <- dt$Ctrl_Record_ID_Review %in% dt$Ctrl_Record_ID_Review[i]
            
            index2 <- dt$Ctrl_scientificName_verified %in% dt$Ctrl_scientificName_verified[i]
            
            dt$Incluir_Amostra[i] <- !(sum(index)>1 | sum(index2)>1)
          }
          
          data_imp <- dt %>% 
            dplyr::arrange_at(.,c('Ctrl_family_verified','Ctrl_scientificName_verified','Ctrl_Record_ID_Review','Ctrl_emailVerificador')) %>%
            dplyr::select(Incluir_Amostra,
                          Ctrl_family_verified,Ctrl_scientificName_verified,Ctrl_occurrenceID,
                          Ctrl_emailVerificador,Ctrl_verificadoPor,
                          Ctrl_collectionCode, Ctrl_catalogNumber,Ctrl_key_family_recordedBy_recordNumber, 
                          
                          Ctrl_recordNumber,	Ctrl_recordedBy,
                          Ctrl_typeStatus,
                          Ctrl_locality,
                          Ctrl_year, Ctrl_month, Ctrl_day,
                          
                          fb2020_taxonID,group,establishmentMeans,
                          
                          Ctrl_Record_ID_Review2)
        }else{
          data_imp <- data.frame(vazio='vazio')
        }
        
        rhandsontable::rhandsontable(data_imp,#dt,
                                     width = '100%', height = 250,
                                     
                                     digits = 0,
                                     
                                     selectionMode = 'single',
                                     selectCallback = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE)%>%
          hot_col(col = c('Incluir_Amostra'), readOnly = FALSE, type='checkbox') 
        
      })
    
    
    output$hot_modelo <- renderRHandsontable(
      {
        shiny::validate(
          need(sum(hot_to_r(input$hot_verified_samples)$Incluir_Amostra == TRUE) >0 ,  "..."))
        
        dt <- hot_to_r(input$hot_verified_samples) 
        
        index_res <- (dt$Incluir_Amostra == TRUE) 
        
        dt <- dt[index_res==TRUE,]
        
        autor <- pega_autor(dt$Ctrl_scientificName_verified)
        
        dt$bancodados <- rep('',NROW(dt))
        dt$barcode <- rep('',NROW(dt)) 
        dt$sp <- rep('',NROW(dt)) 
        dt$categoria_Ameaca_Portaria_443 <- rep('',NROW(dt)) 
        
        for(ii in 1:NROW(dt))
        {
          bancodados <- stringr::str_sub(dt$Ctrl_occurrenceID[ii], 
                                         1, 
                                         stringr::str_locate(dt$Ctrl_occurrenceID[ii], '=')[,1]-1)
          
          
          bancodados <- paste0(toupper(stringr::str_sub(bancodados,1,1)),stringr::str_sub(bancodados, 2,nchar(bancodados)))
          
          barcode <- stringr::str_sub(dt$Ctrl_occurrenceID[ii],
                                      stringr::str_locate(dt$Ctrl_occurrenceID[ii], '=')[,1]+1,
                                      nchar(dt$Ctrl_occurrenceID[ii]))
          
          dt$bancodados[ii] <- bancodados
          dt$barcode[ii] <- barcode
          
          
          
          # sub(paste0('\\','(Nees) Hook.'), "", 'Aphelandra blanchetiana (Nees) Hook.')
          
          dt$sp[ii] <- sub(paste0('\\',autor[ii]), "", dt$Ctrl_scientificName_verified[ii], ignore.case = TRUE)
          
          dt$sp[ii] <-str_sub(dt$sp[ii],1, nchar(dt$sp[ii])-1)
          
          print(dt$sp[ii])
        }
        
        x <- categoria_ameaca_flora_Portaria_443(dt$sp,
                                                 floraAmeacadaBR=floraAmeacadaBR)
        dt$categoria_Ameaca_Portaria_443 <- x
        
        
        
        data_imp <- data.frame( sp = dt$sp,
                                `categoria Ameaça Portaria 443` = dt$categoria_Ameaca_Portaria_443,
                                UC = rep(input$UC_Input,NROW(dt)),
                                Grupos = dt$group,
                                `Família`= dt$Ctrl_family_verified,
                                `Gênero` =  word(dt$Ctrl_scientificName_verified,1),
                                `Espécie` = word(dt$Ctrl_scientificName_verified,2),
                                Autor = autor,
                                `Táxon completo (segundo Flora & Funga do Brasil)` = paste0(dt$Ctrl_family_verified, ' ', dt$Ctrl_scientificName_verified),
                                `Barcode`	=  dt$barcode,
                                `Banco de dados de origem` = dt$bancodados,
                                `Sigla Herbário` = dt$Ctrl_collectionCode,
                                `Coletor`	= dt$Ctrl_recordedBy,
                                `Número da Coleta`	= dt$Ctrl_recordNumber,
                                `Origem (segundo Flora e Funga do Brasil)` = dt$establishmentMeans)
        
        rhandsontable::rhandsontable(data_imp,
                                     width = '100%', height = 250,
                                     digits = 0,
                                     selectionMode = 'single',
                                     selectCallback = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE)
        
      })
    
    output$hot_problemas <- renderRHandsontable(
      {  
        shiny::validate(
          need(sum(loadoccResults()$Ctrl_naoPossivelVerificar==TRUE) >0 ,  "..."))
        
        rhandsontable::rhandsontable(occ_full %>% 
                                       dplyr::filter(Ctrl_naoPossivelVerificar==TRUE) %>%
                                       dplyr::select(Ctrl_observacaoNaoPossivelVerificar,
                                                     Ctrl_family,Ctrl_scientificName,
                                                     Ctrl_collectionCode, Ctrl_catalogNumber,Ctrl_key_family_recordedBy_recordNumber, 
                                                     Ctrl_emailVerificador,Ctrl_verificadoPor,
                                                     Ctrl_recordNumber,	Ctrl_recordedBy,
                                                     Ctrl_typeStatus,
                                                     Ctrl_locality,
                                                     Ctrl_year, Ctrl_month, Ctrl_day) %>% 
                                       dplyr::arrange_at(.,c('Ctrl_observacaoNaoPossivelVerificar','Ctrl_key_family_recordedBy_recordNumber','Ctrl_family','Ctrl_scientificName')),
                                     width = '100%', height = 250,
                                     digits = 0,
                                     selectionMode = 'single',
                                     selectCallback = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE)
        
        
      })
    
    output$hot_sem_voucher <- renderRHandsontable(
      {
        shiny::validate(
          need(NROW(loadoccResults()) >0 ,  "..."))
        
        spp <- occ_full$Ctrl_scientificName_verified %>% unique()
        
        key <- occ_full$Ctrl_key_family_recordedBy_recordNumber[!is.na(occ_full$Ctrl_scientificName_verified)] %>% unique()
        
        
        dt <- occ_full %>% 
          dplyr::filter((is.na(Ctrl_scientificName_verified)) & 
                          (Ctrl_naoPossivelVerificar==FALSE) &
                          (Ctrl_amostraVerificada==FALSE) &
                          ((! fb2020_scientificName %in% spp) & (! Ctrl_key_family_recordedBy_recordNumber %in% key)))
        
        dt <- dt %>%
          dplyr::select(Ctrl_family,Ctrl_scientificName,
                        Ctrl_collectionCode, Ctrl_catalogNumber,Ctrl_key_family_recordedBy_recordNumber, 
                        Ctrl_emailVerificador,Ctrl_verificadoPor,
                        Ctrl_recordNumber,	Ctrl_recordedBy,
                        Ctrl_typeStatus,
                        Ctrl_locality,
                        Ctrl_year, Ctrl_month, Ctrl_day)%>% 
          dplyr::arrange_at(.,c('Ctrl_family','Ctrl_scientificName','Ctrl_key_family_recordedBy_recordNumber'))
        
        rhandsontable::rhandsontable(dt,
                                     width = '100%', height = 250,
                                     digits = 0,
                                     selectionMode = 'single',
                                     selectCallback = TRUE) %>%
          hot_table(highlightCol = TRUE, highlightRow = TRUE, readOnly = TRUE)
      })
    
    output$download_ModeloCatalogo <- downloadHandler(
      filename = function() {
        paste("Catalogo_Plantas_UCs_Brasil - Planilha Modelo - Publicacao.xls", sep="")
      },
      content = function(file) {
        
        dt <- hot_to_r(input$hot_modelo) %>% data.frame(stringsAsFactors = FALSE,
                                                        check.names = FALSE)
        
        # setting the threshold for the maximum number of characters to be preserved
        n_char_to_truncate_threshold <- 32767
        
        # tidyverse
        # adjusted data.frame where the character columns are truncated so that they do not exceed the threshold of 32767 characters
        df2 <- purrr::map_df(dt, ~ifelse(is.character(.x) & nchar(.x) > n_char_to_truncate_threshold,  str_sub(.x, 1, n_char_to_truncate_threshold), .x))
        
        # checking the result to make sure it is truncated
        # you can also use it beforehand to see which columns are the ones causing problems
        purrr::map_df(df2, ~ifelse(is.character(.x), nchar(.x), NA) )
        
        writexl::write_xlsx(df2, 
                            file)
      })
    
    output$download_problemas <- downloadHandler(
      filename = function() {
        paste("Catalogo_Plantas_UCs_Brasil - Amostras com Problemas.xls", sep="")
      },
      content = function(file) {
        
        dt <- hot_to_r(input$hot_problemas)
        
        # setting the threshold for the maximum number of characters to be preserved
        n_char_to_truncate_threshold <- 32767
        
        # tidyverse
        # adjusted data.frame where the character columns are truncated so that they do not exceed the threshold of 32767 characters
        df2 <- purrr::map_df(dt, ~ifelse(is.character(.x) & nchar(.x) > n_char_to_truncate_threshold,  str_sub(.x, 1, n_char_to_truncate_threshold), .x))
        
        # checking the result to make sure it is truncated
        # you can also use it beforehand to see which columns are the ones causing problems
        purrr::map_df(df2, ~ifelse(is.character(.x), nchar(.x), NA) )
        
        writexl::write_xlsx(df2, 
                            file)
      })
    
    
    output$download_sem_volcher <- downloadHandler(
      filename = function() {
        paste("Catalogo_Plantas_UCs_Brasil - Amostras Sem Verificacao.xls", sep="")
      },
      content = function(file) {
        
        dt <- hot_to_r(input$hot_sem_voucher)
        
        # setting the threshold for the maximum number of characters to be preserved
        n_char_to_truncate_threshold <- 32767
        
        # tidyverse
        # adjusted data.frame where the character columns are truncated so that they do not exceed the threshold of 32767 characters
        df2 <- purrr::map_df(dt, ~ifelse(is.character(.x) & nchar(.x) > n_char_to_truncate_threshold,  str_sub(.x, 1, n_char_to_truncate_threshold), .x))
        
        # checking the result to make sure it is truncated
        # you can also use it beforehand to see which columns are the ones causing problems
        purrr::map_df(df2, ~ifelse(is.character(.x), nchar(.x), NA) )
        
        writexl::write_xlsx(df2, 
                            file)
      })
    
  }
  
  #  Run the application 
  # shinyApp(ui = ui, server = server)
  
  shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
  
  # shinyApp(ui = ui, server = server, options = list(launch.browser = TRUE))
  
}
