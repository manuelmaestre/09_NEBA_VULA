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
datos.munis.NEBA.file <- './input_data/ine_municipios_NEBA.xlsx'
por.cabecera.file <- './out_data/analisis_cabecera.xlsx'


FTTH.TESA.SI <- data.table(read.csv(file = FTTH.TESA.SI.file,
                                     header = T,
                                     sep = ";",
                                     dec = ",",
                                     colClasses = 'character',
                                     strip.white = T,
                                     comment.char = "",
                                     encoding = 'UTF-8'))

rm(FTTH.TESA.SI.file)


CoberturaMMB <- data.table(read.csv(file = coberturaMMB.file,
                                    header = T,
                                    sep = ";",
                                    dec = ",",
                                    colClasses = 'character',
                                    encoding = 'UTF-8',
                                    strip.white = T,
                                    comment.char = ""))

rm(coberturaMMB.file)

CoberturaMMB <- CoberturaMMB[ranking ==1 & ordenada.tipo.huella != '10_NEBA',]
CoberturaMMB$UUII <- as.integer(CoberturaMMB$UUII)

CoberturaMMB <- CoberturaMMB[, .(UUII=sum(UUII)), by = c('G18', 'ordenada.tipo.huella')]
CoberturaMMB <- CoberturaMMB[order(-UUII, G18)]
CoberturaMMB <- CoberturaMMB[!duplicated(G18),]

provincias.NEBA <- as.data.table(data.table(read_excel(path = datos.munis.NEBA.file, sheet = 'PROVINCIAS', col_names = T)))

FTTH.TESA.SI <- merge(FTTH.TESA.SI, provincias.NEBA[, c('CODIGO_PROVINCIA', 'PROVINCIA','PROVINCIA_ABIERTA')], all.x = T, by.x = 'CP', by.y = 'CODIGO_PROVINCIA')
FTTH.TESA.SI <- merge(FTTH.TESA.SI, CoberturaMMB[, c('G18', 'ordenada.tipo.huella')], all.x = T, by.x = 'Gescal', by.y = 'G18')
FTTH.TESA.SI$solapada <- 'no'
FTTH.TESA.SI[!is.na(ordenada.tipo.huella), solapada := 'si']
FTTH.TESA.SI$UUII <- as.integer(FTTH.TESA.SI$UUII)

analisis.cabecera.PAI <- FTTH.TESA.SI[, .(UUII = sum(UUII), edificios = length(Gescal)), by = c('CP', 'Localidad', 'MIGA.Central', 'PAI.L', 'ZET', 'ZEP', 'PROVINCIA','PROVINCIA_ABIERTA', 'solapada')]

ineTESA.NEBA <- as.data.table(data.table(read_excel(path = datos.munis.NEBA.file, sheet = 'ZET', col_names = T)))
ineTESA.NEBA <- ineTESA.NEBA[, 1:3]
analisis.cabecera.PAI <- merge(analisis.cabecera.PAI, ineTESA.NEBA, all.x = T, by.x = c('CP', 'Localidad'), by.y = c('CP', 'Localidad'))


write.xlsx(analisis.cabecera.PAI, por.cabecera.file, sheetName = 'datos', row.names = F)

