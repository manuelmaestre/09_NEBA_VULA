## 1.- Identificar las direcciones VULA con UUII validas desde las
## fuentes disponibles (Bitstream, despliegues en curso, etc)
## 2.- Listar por municipio % con UUII, % sin UUI, UUII ine
## 3.- Generar listados de direcciones a localizar UUII

## Library load

library(readxl)
library(stringr)
library(data.table)
library(zoo)

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

VULA.file <- 'C:/00_datos_usuario/01_projects/000_DWH_txt_files/00_Coberturas/02_NEBA_VULA/FICH2IRO.csv'
coberturaMMB.file <- 'C:/00_datos_usuario/01_projects/000_DWH_txt_files/00_Coberturas/01_out_data/01_total_direcciones_FTTH.txt'
preMMB.file <- './input_data/datos_preMMB.csv'
datos.NAE.file <- './input_data/20180402_MUN190_OUTPUT_INMUEBLES_PROCESADO.csv'
datos.GESCON.file <- './input_data/NEBA_SI.txt'
datos.munis.NEBA.file <- './input_data/ine_municipios_NEBA.xlsx'
munis.control.file <- '../00_Analisis_municipios/data/clean/00_Analisis_municipios.xlsx'
migaCU.file <- 'C:/00_datos_usuario/01_projects/01_remedy/data/clean/irolista/G17_miga.txt'



CoberturaVULA <- data.table(read.csv(file = VULA.file,
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

### Poblamos los datos de cobertura en la tabla de VULA

CoberturaVULA <- CoberturaVULA[!duplicated(Gescal),]

CoberturaVULA <- merge(CoberturaVULA, CoberturaMMB, all.x = TRUE, by.x = 'Gescal', by.y = 'G18')
setnames(CoberturaVULA, 'ordenada.tipo.huella', 'origenUUII')

### Separamos con UUII sin UUII
coberturaVULA.con.UUII <- CoberturaVULA[is.na(origenUUII) == FALSE,]
coberturaVULA.sin.UUII <- CoberturaVULA[is.na(origenUUII) == TRUE,]
coberturaVULA.sin.UUII[, c('UUII', 'origenUUII') := NULL]

rm(CoberturaVULA)

### Poblar cobertura pendiente VULA con datos pre MMB

G17.preMMB <- data.table(read.csv(file = preMMB.file,
                                    header = T,
                                    sep = ";",
                                    dec = ",",
                                    colClasses = 'character',
                                    encoding = 'UTF-8',
                                    comment.char = ""))

G17.preMMB$UUII <- as.integer(G17.preMMB$UUII)

G17.preMMB <- G17.preMMB[, c('GESCAL17', 'UUII')]
G17.preMMB$origenUUII <- 'preMMB'

G17.preMMB <- G17.preMMB[!duplicated(GESCAL17),]

coberturaVULA.sin.UUII <- merge(coberturaVULA.sin.UUII, G17.preMMB, all.x = T, by.x = 'Gescal', by.y = 'GESCAL17')
coberturaVULA.con.UUII <- rbind(coberturaVULA.con.UUII, coberturaVULA.sin.UUII[is.na(UUII) == FALSE,])
coberturaVULA.sin.UUII <- coberturaVULA.sin.UUII[is.na(UUII) == TRUE,]
coberturaVULA.sin.UUII[, c('UUII', 'origenUUII') := NULL]

rm(G17.preMMB)

### Poblar cobertura pendiente VULA con datos NAE

NAE.datos <- data.table(read.csv(file = datos.NAE.file,
                                  header = T,
                                  sep = ";",
                                  dec = ",",
                                  colClasses = 'character',
                                  encoding = 'UTF-8',
                                  comment.char = ""))

NAE.datos <- NAE.datos[, .N, by = 'gescal17']
setnames(NAE.datos, 'N', 'UUII')
NAE.datos$origenUUII <- 'NAE'
NAE.datos <- NAE.datos[!duplicated(gescal17),]
coberturaVULA.sin.UUII <- merge(coberturaVULA.sin.UUII, NAE.datos, all.x = T, by.x = 'Gescal', by.y = 'gescal17')
coberturaVULA.con.UUII <- rbind(coberturaVULA.con.UUII, coberturaVULA.sin.UUII[is.na(UUII) == FALSE,])
coberturaVULA.sin.UUII <- coberturaVULA.sin.UUII[is.na(UUII) == TRUE,]
coberturaVULA.sin.UUII[, c('UUII', 'origenUUII') := NULL]

rm(NAE.datos)


### Poblar resto de información con datos de GESCON

GESCON.datos <- data.table(read.csv(file = datos.GESCON.file,
                                 header = T,
                                 sep = ";",
                                 dec = ",",
                                 colClasses = 'character',
                                 encoding = 'Latin-1',
                                 comment.char = ""))

GESCON.datos$TOTAL <- as.integer(GESCON.datos$TOTAL)
GESCON.datos <- GESCON.datos[is.na(TOTAL)==FALSE,]
GESCON.datos <- GESCON.datos[order(G17, TERRITORY_OWNER)]
GESCON.datos.duplicados <- GESCON.datos[duplicated(G17),]
GESCON.datos <- GESCON.datos[!duplicated(G17),]
GESCON.datos <- GESCON.datos[, c('G17', 'TOTAL')]
GESCON.datos <- GESCON.datos[, .(UUII=sum(TOTAL)), by = 'G17']
GESCON.datos$origenUUII <- 'GESCON'

coberturaVULA.sin.UUII <- merge(coberturaVULA.sin.UUII, GESCON.datos, all.x = T, by.x = 'Gescal', by.y = 'G17')
coberturaVULA.con.UUII <- rbind(coberturaVULA.con.UUII, coberturaVULA.sin.UUII[is.na(UUII) == FALSE,])
coberturaVULA.sin.UUII <- coberturaVULA.sin.UUII[is.na(UUII) == TRUE,]
coberturaVULA.sin.UUII[, c('UUII', 'origenUUII') := NULL]

#rm(GESCON.datos)


### Poblar restante con datos Miga Cobre

MIGA.cu.G17 <- data.table(read.csv(file = migaCU.file,
                                    header = T,
                                    sep = ",",
                                    colClasses = 'character',
                                    encoding = 'UTF-8',
                                    skipNul = T)
                          )

MIGA.cu.G17 <- MIGA.cu.G17[order(G18, -UUII)]
MIGA.cu.G17 <- MIGA.cu.G17[!duplicated(G18),]
MIGA.cu.G17$origenUUII <- 'Migacobre'
coberturaVULA.sin.UUII <- merge(coberturaVULA.sin.UUII, MIGA.cu.G17, all.x = T, by.x = 'Gescal', by.y = 'G18')
coberturaVULA.sin.UUII$MIGA_Cu <- NULL
setcolorder(coberturaVULA.sin.UUII, colnames(coberturaVULA.con.UUII))
coberturaVULA.con.UUII <- rbind(coberturaVULA.con.UUII, coberturaVULA.sin.UUII[is.na(UUII) == FALSE,])
coberturaVULA.sin.UUII <- coberturaVULA.sin.UUII[is.na(UUII) == TRUE,]

### Las que no tienen UUII asociamos las UUII promedio del municipio
coberturaVULA.con.UUII$UUII <- as.integer(coberturaVULA.con.UUII$UUII)
UUII.promedio.municipio <- coberturaVULA.con.UUII[, .(UUII = as.integer(mean(UUII))), by = 'Localidad']

orden.columnas <- colnames(coberturaVULA.sin.UUII)
coberturaVULA.sin.UUII$UUII <- NULL
coberturaVULA.sin.UUII <- merge(coberturaVULA.sin.UUII, UUII.promedio.municipio, all.x = T, by.x = 'Localidad', by.y = 'Localidad')
setcolorder(coberturaVULA.sin.UUII, orden.columnas)
coberturaVULA.sin.UUII[!is.na(UUII), origenUUII := 'Promedio']
coberturaVULA.con.UUII <- rbind(coberturaVULA.con.UUII, coberturaVULA.sin.UUII[is.na(UUII) == FALSE,])
coberturaVULA.sin.UUII <- coberturaVULA.sin.UUII[is.na(UUII) == TRUE,]


### Dejamos un unico fichero de coberturaVULA, si no se ha podido asignar UUII estarán a 0
coberturaVULA.sin.UUII$UUII <- '0'
coberturaVULA.sin.UUII$origenUUII <- 'Sin_dato_UUII'
coberturaVULA.con.UUII <- rbind(coberturaVULA.con.UUII, coberturaVULA.sin.UUII[is.na(UUII) == FALSE,])
coberturaVULA.sin.UUII <- coberturaVULA.sin.UUII[is.na(UUII) == TRUE,]

coberturaVULA.con.UUII$UUII <- as.integer(coberturaVULA.con.UUII$UUII)

## 1 MIGA de cobre desde ficheros de area atendida con cruce directo

coberturaVULA.con.UUII <- merge(coberturaVULA.con.UUII, MIGA.cu.G17[, c('G18', 'MIGA_Cu')], all.x = T, by.x = 'Gescal', by.y = 'G18')

## 2 MIGA de cobre arrastrando ascendente y descendente por calle desde el primer valor no NA

coberturaVULA.con.UUII$ID_TECNICO_DE_LA_VIA <- str_sub(coberturaVULA.con.UUII$Gescal, 1, 12)
coberturaVULA.con.UUII <- coberturaVULA.con.UUII[order(ID_TECNICO_DE_LA_VIA,Gescal)]
coberturaVULA.con.UUII$Par.impar <- 'Par'
coberturaVULA.con.UUII[as.integer(str_sub(Gescal, 13,17)) %% 2 != 0, Par.impar:= 'Impar']

#test <- coberturaVULA.con.UUII[ID_TECNICO_DE_LA_VIA == '010000200010',]
#test <- rbind(coberturaVULA.con.UUII[ID_TECNICO_DE_LA_VIA == '010000200004',], test)
#test[, MIGA_Cu:= na.locf(MIGA_Cu, na.rm = F), by = ID_TECNICO_DE_LA_VIA]
#test[, MIGA_Cu:= na.locf(MIGA_Cu, na.rm = F, fromLast=T), by = ID_TECNICO_DE_LA_VIA]

### Arrastramos agrupando por via, par, impar

coberturaVULA.con.UUII[, MIGA_Cu:= na.locf(MIGA_Cu, na.rm = F), by = .(ID_TECNICO_DE_LA_VIA, Par.impar)]
coberturaVULA.con.UUII[, MIGA_Cu:= na.locf(MIGA_Cu, na.rm = F, fromLast = T), by = .(ID_TECNICO_DE_LA_VIA, Par.impar)]

### Arrastramos para el resto

coberturaVULA.con.UUII[, MIGA_Cu:= na.locf(MIGA_Cu, na.rm = F), by = ID_TECNICO_DE_LA_VIA]
coberturaVULA.con.UUII[, MIGA_Cu:= na.locf(MIGA_Cu, na.rm = F, fromLast = T), by = ID_TECNICO_DE_LA_VIA]

## 4 MIGA de cobre desde MIGA más frecuente por municipio

coberturaVULA.con.UUII[, G7:=str_sub(Gescal, 1, 7)]
MigaCU.municipio <- coberturaVULA.con.UUII[is.na(MIGA_Cu) == F, .N, by = .(G7, MIGA_Cu)]
MigaCU.municipio <- MigaCU.municipio[order(G7, -N)]
MigaCU.municipio <- MigaCU.municipio[!duplicated(G7),]
MigaCU.municipio$N  <- NULL

coberturaVULA.sin.MigaCU <- coberturaVULA.con.UUII[is.na(MIGA_Cu),]
coberturaVULA.con.MigaCU <- coberturaVULA.con.UUII[is.na(MIGA_Cu)==F,]

coberturaVULA.sin.MigaCU$MIGA_Cu <- NULL
coberturaVULA.sin.MigaCU <- merge(coberturaVULA.sin.MigaCU, MigaCU.municipio, all.x = T, by.x = 'G7', by.y = 'G7')
setcolorder(coberturaVULA.sin.MigaCU, colnames(coberturaVULA.con.MigaCU))

coberturaVULA.con.UUII <- rbind(coberturaVULA.con.MigaCU, coberturaVULA.sin.MigaCU)

## Los que queden sin MIGA de cobre asignado, le asigna el mismo MIGA de la colectora


coberturaVULA.con.UUII[is.na(MIGA_Cu), MIGA_Cu := MIGA.Central]


## Cargar datos de ine y Marcar ZET

NEBA.munis <- data.table(read_excel(datos.munis.NEBA.file, sheet = 'ZET', col_names = T))
NEBA.munis <- NEBA.munis[is.na(ine_txt) == FALSE,]
NEBA.munis$Muni_ine <- NULL
NEBA.munis <- NEBA.munis[!duplicated('CP', 'Localidad'),]
coberturaVULA.con.UUII <- merge(coberturaVULA.con.UUII, NEBA.munis, all.x = T, by.x = c('Provincia', 'Localidad'), by.y = c('CP', 'Localidad'))

#### Los que no han cruzado los marcamos fuera de ZET

coberturaVULA.con.UUII[is.na(ZET), ZET:=0]

## Marcar provincias abiertas

provincias.abiertas <- data.table(read_excel(datos.munis.NEBA.file, sheet = 'PROVINCIAS', col_names = T))
provincias.abiertas$PROVINCIA <- NULL

coberturaVULA.con.UUII <- merge(coberturaVULA.con.UUII, provincias.abiertas, all.x = T, by.x = 'Provincia', by.y = 'CODIGO_PROVINCIA')

## Marcar exclusiones por MIGA de cobre en ZEP

ZEP.MIGA <- data.table(read_excel(datos.munis.NEBA.file, sheet = 'ZEP', col_names = T))
ZEP.MIGA$INE_muni <- NULL

coberturaVULA.con.UUII <- merge(coberturaVULA.con.UUII, ZEP.MIGA, all.x = T, by.x = 'MIGA_Cu', by.y = 'MIGA')

#### Los que no han cruzado los marcamos fuera de ZEP

coberturaVULA.con.UUII[is.na(ZEP), ZEP:=0]

## Cargar datos de municipios control

munis.control <- data.table(read_excel(munis.control.file, sheet = 'Revision', col_names = T, skip = 7))
munis.control <- munis.control[, 2:9]
munis.control[, c('UUII_55_INE') :=NULL]

### Resumen avance

agregado.UUII.VULA <- coberturaVULA.con.UUII[, sum(UUII), by = c('ine_txt')]
agregado.UUII.VULA <- merge(agregado.UUII.VULA, munis.control, all.x = T, by.x = 'ine_txt', by.y = 'ine txt')


copy.table(coberturaVULA.con.UUII[, sum(UUII), by = 'origenUUII'])
sum(coberturaVULA.con.UUII$UUII)
coberturaVULA.sin.UUII[, .N, by = 'Provincia'][order(-N)]


###### CHECKPOINT 1



### Preparar datos para exportar huella
setnames(coberturaVULA.con.UUII, c('Provincia', 'Direccion'), c('CP', 'Nombre.Via'))
setcolorder(coberturaVULA.con.UUII, 
            c("CP",
              "Localidad",
              "Nombre.Via",
              "Gescal",
              "MIGA.Central",
              "PAI.L",
              "MIGA_Cu",
              "UUII",
              "ZET",
              "ZEP",
              "PROVINCIA_ABIERTA",
              "origenUUII", "ID_TECNICO_DE_LA_VIA", "Par.impar", "G7", "ine_txt")
            )
coberturaVULA.con.UUII$Tipo.Via <- str_trim(str_replace(coberturaVULA.con.UUII$Tipo.Via, str_sub(coberturaVULA.con.UUII$Gescal, 13, 17), ''))


### Marcamos NEBA disponible ("NEB01" residencial ZET != 1 provincia_abierta = 1, resto "NEB02", siempre para provincias abiertas)

coberturaVULA.con.UUII$NEBA_disponible <- "0"
#coberturaVULA.con.UUII[PROVINCIA_ABIERTA == 1 & ZET == 0 & ZEP == 0, NEBA_disponible:='NEB01',]
coberturaVULA.con.UUII[PROVINCIA_ABIERTA == 1 & ZET != 1, NEBA_disponible:='NEB01',]
coberturaVULA.con.UUII[PROVINCIA_ABIERTA == 1 & NEBA_disponible!='NEB01', NEBA_disponible:='NEB02',]

rm('coberturaVULA.con.MigaCU', 'coberturaVULA.sin.MigaCU', 'GESCON.datos', 'MIGA.cu.G17')


provincias <- data.table(read_excel('../000_DWH_txt_files/_Tablas_Referencia_generales/INE.xlsx', sheet = 'Provincias', col_names = T))
provincias <- provincias[, 1:2]

coberturaVULA.con.UUII <- merge(coberturaVULA.con.UUII, provincias, all.x = T, by.x = 'CP', by.y = 'CP')
coberturaVULA.con.UUII$Tipo.Via <- ''
coberturaVULA.con.UUII$BIS <- ''
coberturaVULA.con.UUII$OLT <- ''
coberturaVULA.con.UUII$TIPO_INSTALACION <- ''
coberturaVULA.con.UUII$accesos <- ''
coberturaVULA.con.UUII$CodigoPostal <- ''
coberturaVULA.con.UUII$tipo_huella <- 'NEBA'
coberturaVULA.con.UUII$Numero <- substr(coberturaVULA.con.UUII$Gescal, 13, 17)
str_fecha_dato <- str_replace_all(Sys.Date(), "-", "")
coberturaVULA.con.UUII$fecha_dato <- str_fecha_dato
setcolorder(coberturaVULA.con.UUII, c("ine_txt", "Gescal", "ID_TECNICO_DE_LA_VIA", "Provincia", 
                                      "Localidad", "CodigoPostal", "Tipo.Via", "Nombre.Via", "Numero", "BIS", "PAI.L",
                                      "OLT", "TIPO_INSTALACION", "UUII","accesos", "tipo_huella","fecha_dato",
                                      "origenUUII", "CP", "MIGA.Central", "MIGA_Cu", "ZET", "ZEP", "NEBA_disponible", "PROVINCIA_ABIERTA", "Par.impar", "G7" ))

########################################################### REPORTING ################################################################################

# gescon01 <- data.table(read.csv(file = './input_data/Gescales No Solapados FIBNEB01.txt',
#                                 header = T,
#                                 sep = ";",
#                                 dec = ",",
#                                 colClasses = 'character',
#                                 encoding = 'UTF-8',
#                                 comment.char = ""))
# 
# 
# gescon01 <- gescon01[, .N, by = .(str_sub(GESCAL, 1, 17))]
# setnames(gescon01, c('str_sub', 'N'), c('G17', 'registros'))
# 
# gescon01 <- merge(gescon01, coberturaVULA.con.UUII[, c('Gescal', 'UUII', 'NEBA_disponible')], all.x = T, by.x = 'G17', by.y = 'Gescal')
# error.neba.actual <- gescon01[NEBA_disponible != 'NEB01', ]
# 
# deficit.neba <- merge(coberturaVULA.con.UUII[NEBA_disponible == 'NEB01', c('Gescal', 'UUII')], gescon01, all.x = T, by.x = 'Gescal', by.y = 'G17')
# deficit.neba <- deficit.neba[is.na(NEBA_disponible),]


# #### Analisis Baleares
# 
# CoberturaMMB <- data.table(read.csv(file = coberturaMMB.file,
#                                     header = T,
#                                     sep = ";",
#                                     dec = ",",
#                                     colClasses = 'character',
#                                     encoding = 'UTF-8',
#                                     strip.white = T,
#                                     comment.char = ""))
# 
# CoberturaMMB$UUII <- as.integer(CoberturaMMB$UUII)
# 
# baleares.bitstream <- CoberturaMMB[Provincia == 'SANTA CRUZ DE TENERIFE' | Provincia == 'LAS PALMAS', .(UUII = sum(UUII)), by =.(G18, Provincia, Poblacion, PAI)]
# baleares.bitstream <- baleares.bitstream[!duplicated(baleares.bitstream$G18),]
# 
# baleares.NEBA <- coberturaVULA.con.UUII[CP == '35' | CP == '38',]
# baleares.NEBA <- merge(baleares.NEBA, baleares.bitstream[, c('G18', 'PAI')], all.x = T, by.x = 'Gescal', by.y = 'G18')
# baleares.NEBA$clasificacion <- 'Libre'
# baleares.NEBA[!is.na(PAI), clasificacion:='Solapado BTS']
# baleares.NEBA[clasificacion == 'Libre' & ZET == 1, clasificacion:='ZET']
# baleares.NEBA[clasificacion == 'Libre' & ZEP == 1, clasificacion:='ZEP']
# 
# copy.table(baleares.NEBA[, .(UUII = sum(UUII), edficios = length(Gescal)), by = .(clasificacion, Provincia, Localidad, MIGA.Central)])

########################################################### EXPORTS ################################################################################


## Exportamos filtrando a la disponibilidad de PAI y municipios no exclusion

# write.table(coberturaVULA.con.UUII[NEBA_disponible == 'NEB01', 1:17],
#             './out_data/huella_NEBA.txt',
#             row.names = F,
#             quote = F,
#             sep = ';',
#             fileEncoding = 'UTF-8')

write.table(coberturaVULA.con.UUII[, 1:22],
            './out_data/huella_total_NEBA.txt',
            row.names = F,
            quote = F,
            sep = ';',
            fileEncoding = 'UTF-8')


## Export para sistemas

write.table(coberturaVULA.con.UUII[, .(CP, Localidad, Nombre.Via, Gescal, MIGA.Central, PAI.L,MIGA_Cu, ZET, ZEP, UUII, origenUUII)],
            './out_data/datos_neba_categorizados.txt',
            row.names = F,
            quote = F,
            sep = ';',
            fileEncoding = 'UTF-8')



#### Resumen por provincia y huella

coberturaVULA.con.UUII$tipo_huella <- NULL
CoberturaMMB$UUII <- NULL

coberturaVULA.con.UUII <- merge(coberturaVULA.con.UUII, CoberturaMMB, all.x = T, by.x ='Gescal', by.y = 'G18')
coberturaVULA.con.UUII[is.na(ordenada.tipo.huella), ordenada.tipo.huella:= '10_NEBA']

copy.table(coberturaVULA.con.UUII[, .(UUII = sum(UUII), edificios = length(Gescal)), by = c('CP', 'Provincia', 'ZET', 'ZEP', 'ordenada.tipo.huella', 'PROVINCIA_ABIERTA')])


