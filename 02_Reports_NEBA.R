### GENERAR TODOS LOS REPORTS NECESARIOS A PARTIR DE LOS DATOS DE NEBA

## Library load

library(stringr)
library(data.table)
library(zoo)
library(xlsx)

## Environment cleanning

rm(list = ls())

cols.tabla.comillas <- function(tabla.datos){
  cat(paste("c(\"", paste(colnames(tabla.datos), sep=" ", collapse = "\", \"")), "\")", sep="",collapse="")
}

cols.tabla <- function(tabla.datos){
  cat(paste("c(", paste(colnames(tabla.datos), sep="", collapse = ", ")), ")", sep="",collapse="")
}

# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

# Paste data into R

paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}


## Path and static variables definition

## Definicion de rutas y variables estáticas


FTTH.TESA.SI.file<- './out_data/datos_neba_categorizados.txt'
coberturaMMB.file <- '../000_DWH_txt_files/00_Coberturas/01_out_data/01_total_direcciones_FTTH.txt'


FTTH.TESA.SI <- data.table(read.csv(file = FTTH.TESA.SI.file,
                                     header = T,
                                     sep = ";",
                                     dec = ",",
                                     colClasses = 'character',
                                     strip.white = T,
                                     comment.char = ""))


CoberturaMMB <- data.table(read.csv(file = coberturaMMB.file,
                                    header = T,
                                    sep = ";",
                                    dec = ",",
                                    colClasses = 'character',
                                    encoding = 'UTF-8',
                                    strip.white = T,
                                    comment.char = ""))

CoberturaMMB <- CoberturaMMB[ranking ==1 & ordenada.tipo.huella != '10_NEBA',]
CoberturaMMB$UUII <- as.integer(CoberturaMMB$UUII)

CoberturaMMB <- CoberturaMMB[, .(UUII=sum(UUII)), by = c('G18', 'ordenada.tipo.huella')]
CoberturaMMB <- CoberturaMMB[order(-UUII, G18)]
CoberturaMMB <- CoberturaMMB[!duplicated(G18),]
