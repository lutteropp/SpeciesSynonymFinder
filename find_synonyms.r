library(jsonlite)

findSynonyms <- function(species_name) {
  synonyms = list()
  species_name_no_spaces = gsub(" ","+",species_name, fixed=TRUE)
  query = paste("http://webservice.catalogueoflife.org/col/webservice?name=", species_name_no_spaces, "&format=json&response=full", sep = "")
  dbentry = fromJSON(query, flatten=TRUE)
  if (length(dbentry$results) == 0) {
    print(paste("Unfortunately, no results were found for ", species_name, ". Please check your spelling."), sep="")
  } else {
    for (i in 1:length(dbentry$results)) {
      name = paste(dbentry$results[i,]$name)
      if (name == species_name) {
        status = paste(dbentry$results[i,]$name_status)
        if (status == "synonym") {
          #print(paste("Adding", dbentry$results[i,]$accepted_name.name))
          synonyms = c(synonyms,c(paste(dbentry$results[i,]$accepted_name.name)))
        } else { #accepted name
          for (j in 1:length(dbentry$results[i,]$synonyms[[1]]$name)) {
            #print(paste("Adding", dbentry$results[i,]$synonyms[[1]]$name[j]))
            synonyms = c(synonyms,c(paste(dbentry$results[i,]$synonyms[[1]]$name[j])))
          }
        }
      }
    }
  }
  return(synonyms)
}
