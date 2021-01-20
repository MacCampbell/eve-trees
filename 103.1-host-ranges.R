#! /usr/local/bin/RScript

# This script reads in species names then queries GBIF, return continental distributions for the species
library("plyr")
library("tidyverse")
library("rgbif")
library("countrycode")


#Antartica is returned as country, but not as a continent
#Can also for more resolution set "destination = "region""
sppToContinents <- function(sp) {
  #out<-occ_search(scientificName=sp, limit=20)
  out<-occ_search(scientificName=sp, limit=10000)
  countries<-out$data$country
  countries<-countries[!is.na(countries)]
  countries<-countries[!countries=="unknown or invalid"]
  
if(length(countries)>0)  {
# Throws errors, but probably fine
# countries<-countries[!countries %in% c("South Georgia and the South Sandwich Islands","French Southern Territories")]
# May want to filter occurrences that are rare, since sometimes you get the wrong biogeographic province.
  temp<- as_tibble(plyr::count(countries)) %>%
    filter(freq>.05*sum(freq))
  countries<-temp$x
  
#Has country values including Antartica    
      if (length(countries[!is.na(countries)]) >= 1 && length(countries[countries=="Antarctica"])>=1) {
      continents<-unique(countrycode(sourcevar = countries, origin="country.name",destination="continent"))
      continents<-continents[!is.na(continents)]
      continents<-c(continents, "Antarctica")
          return(cbind(sp, continents)) 
      } 

#Has country values, but not including Antartica
      if (length(countries[!is.na(countries)]) >= 1 && length(countries[countries=="Antarctica"])==0) {
      continents<-unique(countrycode(sourcevar = countries, origin="country.name",destination="continent"))
      continents<-continents[!is.na(continents)]
        return(cbind(sp, continents)) 
       } 
  #Only has Anatartica
    if (unique(countries)=="Antarctica") {
    continents<-c("Antarctica")
    return(cbind(sp, continents))
 #No returns  
      } else {
    continents<-c("None")
    return(cbind(sp, continents))
    }
   
} else {
  continents<-c("None")
  return(cbind(sp, continents))
}
}
  
#The species
species<-read_tsv("data/species-list-102.tsv", col_names = "Species")
#species<-read_tsv("outputs/103/proto-critters.tsv", col_names = "Species")

#spp<-c(species[1:5,]$Species,"Arctocephalus gazella")
spp<-species$Species

#Do the search
clist<-lapply(spp, sppToContinents)

#Save the search
df<-as_tibble(ldply(clist, data.frame))
save(df,file="outputs/103/df.rda")
