rm(list=ls())

Sys.setlocale(category = "LC_ALL", locale='es_ES.UTF-8')

library(pacman) ; p_load("data.table", "tidyverse")

options(digits = 10)

# Se define el directorio donde se encuentran almacenadas las bases de datos
setwd('C:/intercensal_2015/')

# Inicia la estimación de las variables a nivel persona

pers <- lapply(1:32, function(ent){
  
 print(paste0('Inicia entidad:', str_pad(ent, 2, "left", pad = "0")))
  
df <- fread(paste0('TR_PERSONA',str_pad(ent, 2, "left", pad = "0"),'.csv'))%>% rename_all(tolower)

df<- df[cobertura!=3]  

df[,`:=`(
 entidad=str_pad(ent, 2, "left", pad = "0"),
 mun=paste0(str_pad(ent, 2, "left", pad = "0"), str_pad(mun, 3, "left", pad = "0")) ,

#Población total
 pob_tot=factor,

# Se recodifican valores no especificados
 edad =ifelse(edad==999, NA, edad),
 alfabet =ifelse(alfabet==9, NA, alfabet),
 asisten =ifelse(asisten==9, NA, asisten),
 nivacad =ifelse(nivacad==99, NA, nivacad),
 escolari =ifelse(escolari==99, 0, escolari),
 dhsersal1 =ifelse(dhsersal1==99, NA, dhsersal1)
       )]

df[,`:=`(
# Población de 15 años o más
 p_15ymas =ifelse(edad>=15 & edad<=110,factor,NA),

# Población de 6 a 14 años
 p_6a14 =ifelse(edad>=6 & edad<=14,factor,NA),

# Población de 6 a 14 años que no asiste a la escuela
 p_6a14_noa =ifelse(asisten==7 & (edad>=6 & edad<=14),factor,NA),

# Población de 15 años y más analfabeta
 p15ym_an =ifelse(alfabet==7 & (edad>=15 & edad<=110),factor,NA),

# Población de 15 años o más con educación básica incompleta
 p_15ymas_edbasinc=dplyr::case_when(
   nivacad<=2 & (edad>=15 & edad<=110) ~ factor,
   nivacad==3 & (escolari<3)  & (edad>=15 & edad<=110) ~ factor,
   nivacad==6 & (escolari<3)  & (edad>=15 & edad<=110) ~ factor),

# Población sin derechohabiencia a servicios de salud 
 psinder =ifelse(dhsersal1== 8,factor,NA)
        )]

# Se recodifican las observaciones de municipios con muestra insuficiente
vars <- c('p_15ymas', 'p_6a14', 'p15ym_an', 'p_6a14_noa', 'p_15ymas_edbasinc', 'psinder')

df[,(vars):= lapply(.SD,function(x) ifelse(cobertura==3,NA,x)),.SDcols=vars]

# Se obtienen los resultados a nivel estatal y municipal
vars <- c('pob_tot','p_15ymas','p_6a14','p15ym_an','p_6a14_noa','p_15ymas_edbasinc','psinder')

mun <- df[, lapply(.SD, sum , na.rm=T), by=list(entidad, mun), .SDcols = vars ]
ent <- df[, lapply(.SD, sum , na.rm=T), by=list(entidad), .SDcols = vars ]

df <- bind_rows(ent, mun)
df<- df[, .(entidad, mun, pob_tot, p_15ymas, p_6a14, p15ym_an, p_6a14_noa, p_15ymas_edbasinc, psinder)]

}) %>% rbindlist()


# Inicia la estimación de las variables a nivel vivienda

viv <- lapply(1:32, function(ent){

 print(paste0('Inicia entidad:', str_pad(ent, 2, "left", pad = "0")))
  
df <- fread(paste0('TR_VIVIENDA',str_pad(ent, 2, "left", pad = "0"),'.csv'))%>% rename_all(tolower)

df<- df[cobertura!=3]

df[,`:=`(
  entidad=str_pad(ent, 2, "left", pad = "0"),
  mun=paste0(str_pad(ent, 2, "left", pad = "0"), str_pad(mun, 3, "left", pad = "0")) ,
# Viviendas particulares habitadas
 vivpar_hab=factor,

# Se recodifican valores no especificados
 sersan=ifelse(sersan==9, NA, sersan)

)]


df[,`:=`(
# Viviendas particulares habitadas con piso de tierra
  vph_pisoti =ifelse(pisos==1, factor, NA), 
  
# Viviendas particulares habitadas que no disponen de excusado o sanitario
 vph_excsa =ifelse(sersan < 3, factor, NA),

# Viviendas particulares habitadas que no disponen de agua entubada de la red pública
 vph_aguafv =ifelse(agua_entubada==3, factor, NA),

# Viviendas particulares habitadas que no disponen de drenaje
 vph_nodren =ifelse(drenaje==5, factor, NA),

# Viviendas particulares habitadas que no disponen de energía eléctrica
 vph_s_elec =ifelse(electricidad==7, factor, NA),

# Viviendas particulares habitadas que no disponen de lavadora
 vph_lavad =ifelse(lavadora==3, factor, NA),

# Viviendas particulares habitadas que no disponen de refrigerador
 vph_refri =ifelse(refrigerador==1, factor, NA),

# Viviendas particulares habitadas que no disponen de celular
 vph_cel =ifelse(celular==3, factor, NA),

# Viviendas particulares habitadas que no disponen de computadora
 vph_pc =ifelse(computadora==7, factor, NA),

# Viviendas particulares habitadas que no disponen de internet
 vph_inter =ifelse(internet==5, factor, NA),

# Viviendas particulares habitadas que no disponen de televisión
 vph_tv =ifelse(televisor==3 | televisor_pp==5, factor, NA)
)]


# Se obtienen los resultados a nivel estatal y municipal
vars <- c('vivpar_hab', 'vph_pisoti', 'vph_excsa', 'vph_aguafv', 'vph_nodren',
          'vph_s_elec', 'vph_lavad', 'vph_refri', 'vph_cel', 'vph_pc', 'vph_inter', 'vph_tv')

mun <- df[, lapply(.SD, sum , na.rm=T), by=list(entidad, mun), .SDcols = vars ]
ent <- df[, lapply(.SD, sum , na.rm=T), by=list(entidad), .SDcols = vars ]

df <- bind_rows(ent, mun)

df <- df[, .(entidad, mun, vivpar_hab, vph_pisoti, vph_excsa, vph_aguafv, vph_nodren,
             vph_s_elec, vph_lavad, vph_refri, vph_cel, vph_pc, vph_inter, vph_tv)]

}) %>% rbindlist()

iter15 <- left_join(pers, viv, by = c("entidad", "mun"))

fwrite(iter15, 'C:/bases/ITER_15_CONEVAL/ITER_NALCSV15.csv')
