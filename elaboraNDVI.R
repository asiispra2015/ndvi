#Elaborazione dei file tif dell'NDVI
#1) I valori  da 251 a 255 corrispondono a dei flag per situazioni speciali (water, cloud etc etc) e quindi
#vanno esclusi dalla'elaborazione
#2) il range di valori è -0.08 a 0.92
#3) Il valore x nel file va trasformato  secondo la seuente formula: x*0.004-0.08
#4) I file sono proiettati sono la proiezione epsg:4326. La proiezione 32662 (Plate Caree) è deprecata e non va usata.
#E' necessario sovrascrivere il CRS associando il CRS epsg:4326: in questo modo è possibili ottenere poi i risultati  in epsg: 32632
rm(list=objects())
library("raster")
library("stringr")
options(error=recover,warn = 2)

ANNO<-2015
list.files(pattern="^g2.+tiff$")->listaFile

stopifnot(length(listaFile)!=0)

tryCatch({
  raster("griglia.tif")
},error=function(e){
  stop("Non trovo il file griglia.tif")
})->griglia


#dai nomi file acquisiamo le date dei file NDVI
str_replace(str_replace(str_extract(listaFile,paste0("_",ANNO,"[0-9]{2}[0-9]{2}0000_")),"0000_$",""),"_","")->dateFile

#aggiungiamo il 31-12
dateFile[length(dateFile)+1]<-paste0(ANNO,"1231")

#i file NDVI vanno di 10 giorni in 10 giorni. Vogliamo associare a ciascun giorno dell'anno un dato NDVI
as.Date(dateFile,format="%Y%m%d")->dateNDVI

purrr::map(1:(length(dateNDVI)-1),.f=function(ii){

  seq.Date(from=dateNDVI[ii],to=dateNDVI[ii+1],by="day")->giorni
  
  #se ii+1 non corrisponde al 31 dicembre, allora devo togliere in giorni
  #l'ultimo giorno della sequenza, in quanto rappresemta il primo elemento della sequenza successiva
  #Per il 31-12 questo non vale, con la fine dell'anno termina il ciclo
  if((ii+1)!=length(dateNDVI)){
    giorni[1:(length(giorni)-1)]-> giorni
  }

  raster(listaFile[ii])->ndvi
  crs(ndvi)<-CRS("+init=epsg:4326")
  #da 251 a 255 flag speciali, questi valori vanno eliminati altrimenti si ottengono valori senza senso facendo la riproiezione
  ndvi[ndvi>=251]<-NA
  
  #riporto i valori al range -0.08 a 0.92
  calc(ndvi,fun = function(x){x*0.004-0.08})->ndvi
  #epsg 32632
  projectRaster(from=ndvi,to=griglia,method="bilinear")->ndvi
  mask(ndvi,griglia)->ndvi

  #vogliamo gli stessi dati NDVI per tutti i giorni elencati in "giorni". Infatti l'NDVI è il risultato
  #di osservazioni su 10 giorni (la stima migliore su 10 giorni). Per ottenere un valore di NDVI giornaliero
  #ripetiamo l'NDVI per tutti i giorni giorni
  purrr::map(1:length(giorni),.f=~(ndvi))->lista  
  
  names(lista)<-giorni
  
  lista
  
}) %>% unlist %>% brick->mybrick #fine map

writeRaster(mybrick,filename ="ndvi_daily_utm.tif",format="GTiff",overwrite=TRUE)

