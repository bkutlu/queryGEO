library(rentrez)
entrez_dbs()

# search the database withh the term
geoSearch2 <- entrez_search(db = "gds", term = "Vasculitis", retmax = 10000)

geoSearch2$ids
# to retrieve the pubmed ids for multiple gds
# turn on by_id TRUE to get the provenance
# accessor
geoSearcLinked <- entrez_link(dbfrom='gds', id=geoSearch2$ids[1:400],
                              db='pubmed', by_id = T)
lapply(geoSearcLinked, function(x) x$links$gds_pubmed) %>% unlist()

# geoSearcLinked$links$gds_pubmed
# length(geoSearcLinked$links$gds_geoprofiles) #35557


# summary 
gds_summary <- entrez_summary(db = "gds", id = "4755")
# dealing with multiple ids - summary
# extract_from_esummary


### acil durum ####
## May 5 2017
## retrieve MeSH terms and look up the pubmed ids of gds entries 

library(readxl)
myMeshInput <- read_excel('data/20170203_MESH_disease_of_interest.xlsx', sheet = "Sheet1", col_names = F)


myMeshInput %>% collect() %>% .[[1]] -> myMesh

geoSearch <- entrez_search(db = "gds", term = myMesh[42], use_history = T, retmax = 10000)

geoSearcLinked <- entrez_link(dbfrom='gds',  
                db='pubmed', id =  geoSearch$ids[1:400],
                by_id = T)

myPmids <- lapply(geoSearcLinked, function(x) x$links$gds_pubmed) %>% unlist()

library(GEOmetadb)
  
con <- dbConnect(SQLite(),'GEOmetadb.sqlite')

dbFetch(dbSendQuery(con, paste0('select * from gse where pubmed_id IN(15150433,26616563,25333715)')))

geoContent <- dbFetch(dbSendQuery(con, paste0('select * from gse where pubmed_id IN(', paste(myPmids, collapse = ','),')')))



 # myRes <- list()
for (m in 23:length(myMesh)){
  Sys.sleep(time = 60)
  myQ <- mesh2Pmids(myMesh[m])
  geoContent <- fetchGEOdat(conn = 'con', pmids = myQ[['pmids']])
  myRes[[m]] <- list(mesh = myMesh[m], myQ, geoContent)
}

 createGseReport(myRes[-c(1:23)])



#### functions ###

createGseReport <- function(resList = myRes) {
  lapply(resList, function (x){
    write.xlsx2(x[[3]][,c('title','gse','last_update_date','pubmed_id','summary','type','overall_design')] %>% as_data_frame() %>% arrange(desc(last_update_date)), file = 'mesh_term_search_geo_second.xlsx', sheetName = x[[1]], append = T)
  })
}

mesh2Pmids <- function(myMesh) {
  geoSearch <- entrez_search(db = "gds", term = myMesh, use_history = T, retmax = 10000)
  geoSearcLinked <- entrez_link(dbfrom='gds',  
    db='pubmed', id =  geoSearch$ids[1:500],
    by_id = T)
  myPmids <- lapply(geoSearcLinked, function(x) x$links$gds_pubmed) %>% unlist()
  list(pmids = myPmids, search = geoSearch)
}


fetchGEOdat <- function(conn = 'con', pmids = pmids){
  conn <- eval(parse(text=conn))
  geoContent <- dbFetch(dbSendQuery(conn, paste0('select * from gse where pubmed_id IN(', paste(pmids, collapse = ','),')')))
  geoContent
}#fetchGEOdat




