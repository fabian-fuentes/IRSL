rm(list=ls())
options(digits = 10)
Sys.setlocale(category = "LC_ALL", locale='es_ES.UTF-8')

# Este programa debe ser utilizado con la versión 4 o superior 

################################################################################
#   PROGRAMA DE CÁLCULO DEL ÍNDICE DE REZAGO SOCIAL LONGITUDINAL, 2000-2020    # 
################################################################################

# Se cargan los paquetes necesarios
if (!require(pacman)) install.packages("pacman") 
library(pacman) ; p_load("data.table", "foreign", "tidyverse", "stratification", "fpc")

# Modificar el directorio en caso de no abrir el proyecto
#setwd("C:/IRSL") 

col <- c("i_analf","i_asistesc","i_edbasinc","i_sdsalud","i_ptierra","i_nosan",
         "i_noagua","i_nodren","i_noelec","i_nolav","i_noref",'i_notv','i_nopc',
         'i_nocel','i_nointer')

# Variables utilizadas para la construcción del Índice de Rezago Social Longitudinal

col_c <- c("i_analf","i_asistesc","i_edbasinc","i_sdsalud","i_ptierra","i_nosan",
           "i_noagua","i_nodren","i_noelec","tics")

# Variables de bienes y tecnologías de la información y la comunicación (TIC)

col_b <- c('i_nolav','i_noref','i_notv','i_nopc','i_nocel','i_nointer')

# Variables de bienes y tecnologías de la información y la comunicación (TIC) por año

col_2000 <- c('i_nolav', 'i_noref', 'i_notv')
col_2005 <- c('i_nolav', 'i_noref', 'i_notv', 'i_nopc')
col_2010 <- c('i_nolav', 'i_noref', 'i_notv', 'i_nopc', 'i_nocel', 'i_nointer')
col_2015 <- c('i_nolav', 'i_noref', 'i_notv', 'i_nopc', 'i_nocel', 'i_nointer')
col_2020 <- c('i_nolav', 'i_noref', 'i_notv', 'i_nopc', 'i_nocel', 'i_nointer')

# Se enlistan las bases de los Principales resultados por localidad (ITER) del INEGI
# para los años: 2000, 2005, 2010 y 2020, que se encuentran en la carpeta "bases"

# En el 2015 la información se construye a partir de la "Encuesta intercensal 2015"
# para conocer la construcción de esta base consulte el programa de cálculo "ITER_15"

files <-list.files('bases',full.names = TRUE,recursive = T)

# Se ejecuta la rutina de limpieza de datos para cada una de las bases enlistadas

iter <- lapply(files, function(f){
  
  print(paste0('Inicia la base',f))
  
  # Se carga la base dependiendo del formato en que se encuentra
  
  if(grepl('.csv',f)){
    df <- fread(f)
  }else{
    df <- read.dbf(f, as.is = T)
    df <- setDT(df)
  }
  
  # Se homologa el nombre de las variables utilizadas
  
  colnames(df) <- tolower(colnames(df))
  colnames(df) <- gsub('x\\.u\\.feff\\.',             '',
                  gsub('p_total|pobtot',              'pob_tot',
                  gsub('p_6a11_an',                   'p_6a11',
                  gsub('pob6_14|p_6a14_an',           'p_6a14',
                  gsub('p_12a14_an',                  'p_12a14',
                  gsub('p_6a11_noa|p6a11_noa',        'p_6a11_noa',
                  gsub('p6_14naesc|p6a14noa',         'p_6a14_noa',
                  gsub('p12a14_noa|p12a14noa',        'p_12a14_noa',
                  gsub('p_15maan|p15_analf',          'p15ym_an',
                  gsub('p_sinder|psderss',            'psinder',
                  gsub('vivparha|vivparhab',          'vivpar_hab',
                  gsub('vph_con_pt',                  'vph_pisoti',
                  gsub('vph_noag',                    'vph_aguafv',
                  gsub('vph_enel|vp_electr',          'vph_c_elec',
                  gsub('vph_refri|vp_refri|vph_refr', 'vph_refri',
                  gsub('vph_lavad|vp_lavad|vph_lava', 'vph_lavad',
                  gsub('vp_tv',                       'vph_tv',
                  gsub('pob15_',                      'p_15ymas',
                  gsub('vp_sersan',                   'vph_excsa',
                  gsub('p15ymase|p15_sinstr',         'p15ym_se',
                  gsub('p15_sprima',                  'p15pri_in',
                  gsub('p15_cprima',                  'p15pri_co',
                  gsub('p15_ssecu',                   'p15sec_in',
                       colnames(df))))))))))))))))))))))))
  
  # Se ajusta el formato de las variables numéricas
  
  noms <- colnames(df)[!grepl('nom_',colnames(df))]
  df[,(noms):=lapply(.SD,as.numeric),.SDcols=noms]
  
  # Se ajusta el formato de las claves de entidad, municipio y localidad
  if(grepl('15',f)){
  df[,cve_ent:=str_pad(entidad, 2, "left", pad = "0")]
  df[,cve_mun:=str_pad(mun, 5, "left", pad = "0")] 
  } else {
  df[,cve_ent:=str_pad(entidad, 2, "left", pad = "0")]
  df[,cve_mun:= paste0(cve_ent, str_pad(mun, 3, "left", pad = "0"))] 
  df[,cve_loc:= paste0(cve_mun, str_pad(loc, 4, "left", pad = "0"))]
  }
  
  # Se homologan las variables que serán utilizadas para el cálculo en función de 
  # las especificidades de cada periodo
  
  if(grepl('20',f)){
    df[,`:=`(vivpar_hab = vivparh_cv,
             vph_excsa  = vph_excsa+vph_letr,
             p_6a14     = p_6a11+p_12a14,
             p_6a14_noa = p_6a11_noa+p_12a14_noa,
             p_15ymas_ref = p_15ymas,
             p_15ymas_edbasinc = p15pri_in+p15ym_se+p15pri_co+p15sec_in)]
  }
  
  if(grepl('15',f)){
    df[,`:=`(p_15ymas_ref = p_15ymas)]
  }
  
  if(grepl('10',f)){
    df[,`:=`(p_6a14       = p_6a11+p_12a14,
             p_6a14_noa   = p_6a11_noa+p_12a14_noa,
             p_15ymas_ref = p_15ymas,
             p_15ymas_edbasinc = p15pri_in+p15ym_se+p15pri_co+p15sec_in)]
  }
  
  if(grepl('05',f)){
    df[,`:=`(vph_s_elec =vivpar_hab-vph_c_elec,
             p_15ymas_ref =p15ym_se + p15ym_ebin + p15ym_ebc + p15ymapb,
             p_15ymas_edbasinc =p15ym_se+p15ym_ebin,
             vph_inter =NA,
             vph_cel =NA)]
  }
  
  if(grepl('00',f)){
    df[,`:=`(vph_pisoti =vivpar_hab-vp_pisdes,
             vph_aguafv =vivpar_hab-vp_aguent,
             vph_nodren =vivpar_hab-vp_drenaj,
             vph_s_elec =vivpar_hab-vph_c_elec,
             p_15ymas_ref =p_15ymas,
             p_15ymas_edbasinc =p15pri_in+p15ym_se+p15pri_co+p15sec_in,
             vph_pc =NA,
             vph_inter =NA,
             vph_cel =NA)]
  }
  
  # Se filtran las observaciones que no cuentan con viviendas ni con población de 15 años o más  
  
  df <- df[!is.na(vivpar_hab) & !is.na(pob_tot) & !is.na(p_15ymas) & !is.na(entidad)]
  df <- df[vivpar_hab > 0 & p_15ymas > 0]
  
  # Se calculan las variables que serán empleadas para la construcción del Índice de Rezago Social Longitudinal
  
  df[,`:=`(i_analf    =p15ym_an/p_15ymas,
           i_asistesc =ifelse(p_6a14==0, 0, p_6a14_noa/p_6a14),
           i_edbasinc =ifelse(p_15ymas_ref==0, 0,p_15ymas_edbasinc/p_15ymas_ref),
           i_sdsalud  =psinder/pob_tot,
           i_ptierra  =vph_pisoti/vivpar_hab,
           i_noelec   =vph_s_elec/vivpar_hab,
           i_nosan    =1-vph_excsa/vivpar_hab,
           i_noagua   =vph_aguafv/vivpar_hab,
           i_nodren   =vph_nodren/vivpar_hab,
           i_noref    =1-vph_refri/vivpar_hab,
           i_nolav    =1-vph_lavad/vivpar_hab,
           i_notv     =1-vph_tv/vivpar_hab,
           i_nopc     =1-vph_pc/vivpar_hab,
           i_nointer  =1-vph_inter/vivpar_hab,
           i_nocel    =1-vph_cel/vivpar_hab)]
  
  # Se ajusta el formato de las variables calculadas para que se expresen en términos de porcentajes
  
  df[,(col):=lapply(.SD, function(x) x*100),.SDcols=col]
  
  # Se extrae el año a partir del nombre del archivo
  
  df[,anio:=as.numeric(paste0('20',gsub("[^0-9.-]", "", gsub(".*/","",f))))]
  
  # Se define el nivel de agregación del registro a partir de las claves de localidad, municipio y entidad
  
  if(grepl('15',f)){
  df[,nivel:=ifelse( is.na(mun),'entidad',
             ifelse(!is.na(mun),'municipal', NA))]
   } else{
  df[,nivel:=ifelse(!loc %in% c(9999,9998,0),'localidad',
             ifelse(loc == 0 & substr(cve_mun,3,5)!='000','municipal',
             ifelse(loc == 0 & substr(cve_mun,3,5)=='000' & cve_ent!='00','entidad',NA)))]
  }
  # Se eliminan las claves de localidad y municipio de los registros con un nivel de agregación superior
  
  if(grepl('15',f)){
  df[,cve_loc:=NA]
  df[,cve_mun:=ifelse(nivel == 'entidad', NA, cve_mun)]
  }else{
  df[,cve_loc:=ifelse(nivel == 'entidad', NA, cve_loc)]
  df[,cve_loc:=ifelse(nivel == 'municipal', NA, cve_loc)]
  df[,cve_mun:=ifelse(nivel == 'entidad', NA, cve_mun)] 
  }
  
  # Se filtran los registros que corresponden a los niveles de agregación considerados para el análisis
  
  df <- df[!is.na(nivel)]
  
  print(paste0('Termina la base',f))
  
  # Se filtran las variables relevantes para el análisis
    df[,c('cve_ent','cve_mun','cve_loc',col,'anio','nivel'),with=FALSE]
})

# Se conjuntan las bases y se ordenan de acuerdo con el año de origen de la información

bases <- rbindlist(iter, fill=T)
bases <- bases[order(anio,decreasing = T)]

# Se realiza la estimación para las desagregaciones correspondientes unique(bases$nivel)

tbs <- lapply(unique(bases$nivel), function(nv){
  
  print(paste0('Inicia la estimación a nivel:',nv))
  
  dt <- bases[nivel==nv]
  claves <- c(grep('cve_',colnames(dt),value=T))
  dt[,(claves):= lapply(.SD,as.numeric),.SDcols=claves]
  
  ##############################################################################
  # 1. Índice de bienes y tecnologías de la información y la comunicación (TIC)
  ##############################################################################
  
  # Se extraen y ordenan las columnas relevantes para los cálculos de índice
  
  dt[,c(grep('cve_',colnames(dt),value=T),col,'anio'),with=FALSE]
  dt <- dt[cve_ent!=0]
  
  obs <- dt
  
  df1 <- obs[,c('anio',col_b),with=FALSE]
  
  # Se calcula la desviación estándar promedio a partir de la desviación observada por año, 
  # para escalar los datos con unidades comparables, de esta forma el cálculo del ACP 
  # no estará sesgado hacia las variables con mayor varianza
  
  # Desviación estándar por año
  
  pcaj_sds_impu <- lapply(1:length(unique(obs$anio)), function(n_an){
    
    an<-unique(df1$anio)[n_an]
    
    cs_df_cs<-scale(df1[df1$anio==an,c(col_b),with=FALSE], center = T, scale = T)
    cs_s<-attributes(cs_df_cs)$`scaled:scale`
    cs_s[cs_s == 0] <- NA
    cs_s
  })
  
  # Desviacion estándar promedio 
  
  pcaj_sds_mean <- colMeans(do.call(rbind,pcaj_sds_impu),na.rm=T)
  
  
  # Se calcula la media promedio a partir de la media observada por año, para centrar 
  # los datos con unidades comparables, de esta forma el cálculo del índice (posterior al ACP) 
  # partirá del mismo punto para cada observación
  
  # Media por año
  
  pcaj_mean_impu <- lapply(1:length(unique(obs$anio)), function(n_an){
    
    an<-unique(df1$anio)[n_an]
    
    cs_df_cs<-scale(df1[df1$anio==an,c(col_b),with=FALSE], center = T, scale = T)
    cs_c<-attributes(cs_df_cs)$`scaled:center`
    cs_c[cs_c == 0] <- NA
    cs_c[is.nan(cs_c)]<-NA
    cs_c
    
  })
  
  # Media promedio
  
  pcaj_mean_mean <- colMeans(do.call(rbind,pcaj_mean_impu),na.rm=T)
  
  pcaj_impu<-t(t(sweep(do.call(rbind,pcaj_mean_impu), 2, pcaj_mean_mean))/pcaj_sds_mean)
  
  
  # Base intermedia con años y variables
  
  df2 <- obs[,c('anio',col_b),with=FALSE]
  
  
  # Cálculo del ACP por año, se ponderan los pesos para sumar 1
  
  pc_sianre <- lapply(unique(obs$anio), function(an){
    
    ps <-prcomp(obs[obs$anio==an,get(paste0('col_',an)),with=FALSE],center=T,
                scale=pcaj_sds_mean[names(pcaj_sds_mean) %in% get(paste0('col_',an))])[["rotation"]][,1]
    data.frame(t(ps))/sum(ps)
    
  }) %>% rbindlist(fill=T)
  
  # Estimación del Índice de bienes y tecnologías de la información y la comunicación (TIC) 
  # a partir de los pesos calculados en el ACP del paso anterior
  
  pc_sianre_val <- lapply(1:length(unique(obs$anio)), function(n_an){
    
    an <- unique(obs$anio)[n_an]
    
    ps <-as.matrix(scale(obs[obs$anio==an,get(paste0('col_',an)),with=FALSE],
                         center=pcaj_mean_mean[names(pcaj_mean_mean) %in% get(paste0('col_',an))],
                         scale=pcaj_sds_mean[names(pcaj_sds_mean) %in% get(paste0('col_',an))])) %*% as.numeric(pc_sianre[n_an,get(paste0('col_',an)),with=FALSE])
    
    df_ps<-obs[obs$anio==an,c(grep('cve_',colnames(obs),value=T),'anio'),with=FALSE]
    df_ps$tics <- ps
    
    df_ps
    
  }) %>% rbindlist()
  
  
  obs <- left_join(dt,pc_sianre_val,by=c(grep('cve_',colnames(dt),value=T),'anio'))
  
  ##############################################################################
  # 2. Construcción del Índice de Rezago Social Longitudinal (IRSL)
  ##############################################################################
  
  # Variables para el IRSL (incluyendo la de bienes y tecnologías de la información y la comunicación (TIC))
  
  df1 <- obs[,c('anio',col_c),with=FALSE]
  
  # Se calcula la desviación estándar promedio a partir de la desviación observada por año, 
  # para escalar los datos con unidades comparables, de esta forma el cálculo del ACP 
  # no estará sesgado hacia las variables con mayor varianza
  
  # Desviación estándar por año
  
  pcaj_sds_impu <- lapply(1:length(unique(obs$anio)), function(n_an){
    
    an<-unique(df1$anio)[n_an]
    
    cs_df_cs<-scale(df1[df1$anio==an,c(col_c),with=FALSE], center = T, scale = T)
    cs_s<-attributes(cs_df_cs)$`scaled:scale`
    cs_s[cs_s == 0] <- NA
    cs_s
  })
  
  # Desviación estándar promedio 
  
  pcaj_sds_mean <- colMeans(do.call(rbind,pcaj_sds_impu),na.rm=T)
  
  
  # Se calcula la media promedio a partir de la media observada por año, para centrar 
  # los datos con unidades comparables, de esta forma el cálculo del IRSL (posterior al ACP) 
  # partirá del mismo punto para cada observación
  
  # Media por año
  
  pcaj_mean_impu <- lapply(1:length(unique(obs$anio)), function(n_an){
    
    an<-unique(df1$anio)[n_an]
    
    cs_df_cs<-scale(df1[df1$anio==an,c(col_c),with=FALSE], center = T, scale = T)
    cs_c<-attributes(cs_df_cs)$`scaled:center`
    cs_c[cs_c == 0] <- NA
    cs_c[is.nan(cs_c)]<-NA
    cs_c
    
  })
  
  # Media promedio
  
  pcaj_mean_mean <- colMeans(do.call(rbind,pcaj_mean_impu),na.rm=T)
  
  pcaj_impu<-t(t(sweep(do.call(rbind,pcaj_mean_impu), 2, pcaj_mean_mean))/pcaj_sds_mean)
  
  
  df2 <- obs[,c('anio',col_c),with=FALSE]
  
  # Calculo del ACP por año, se ponderan los pesos para que la norma del vector sea 1
  
  pc_sianre <- lapply(unique(obs$anio), function(an){
    
    ps <-prcomp(obs[obs$anio==an,col_c,with=FALSE],center=T,scale=pcaj_sds_mean[names(pcaj_sds_mean) %in% col_c])[["rotation"]][,1]
    data.frame(t(ps))/sum(ps)*abs(sum(ps))
    
  }) %>% rbindlist(fill=T)
  
  
  pc_sianre <- colMeans(pc_sianre)
  pc_sianre <- pc_sianre/sum(pc_sianre)
  
  # Estimación del IRSL a partir de los pesos promedio calculados en el ACP del paso anterior
  
  pc_sianre_val <- lapply(1:length(unique(obs$anio)), function(n_an){
    
    an <- unique(obs$anio)[n_an]
    
    ps <-as.matrix(scale(obs[obs$anio==an,col_c,with=FALSE],
                         center=pcaj_mean_mean[names(pcaj_mean_mean) %in% col_c],
                         scale=pcaj_sds_mean[names(pcaj_sds_mean) %in% col_c])) %*% as.numeric(pc_sianre)
    
    df_ps<-obs[obs$anio==an,c(grep('cve_',colnames(obs),value=T),'anio'),with=FALSE]
    df_ps$irsl <- ps
    
    df_ps
    
  }) %>% rbindlist()
  
  pc_sianre_val$irsl <- pc_sianre_val$irsl - min(pc_sianre_val$irsl,na.rm=TRUE)
  
  ##############################################################################
  # 3. Grado de Rezago Social Longitudinal
  ##############################################################################
  
  # Cálculo del Grado de Rezago Social Longitudinal a través del método de Dalenius y Hodges
  
  # Se realizan las iteraciones con la información nacional
  
  dh <- lapply(5:200, function(cl){
    
    print(paste0('Iteración:',cl))
    dh <- suppressWarnings(strata.cumrootf(as.numeric(pc_sianre_val$irsl), CV = 0.05, Ls = 5, nclass = cl)$stratumID) %>% data.table()
    dh <- cbind(dh,pc_sianre_val[,c(grep('cve_',colnames(pc_sianre_val),value=T),'irsl','anio'),with=FALSE])
    dh$cl <- cl
    colnames(dh)[1] <- 'strata' 
    dh
    
  })
  
  # Se calcula el Índice de Calinski-Harabasz para identificar el número de clases óptimo
  
  ch <- lapply(dh, function(ej){
    if(sum(is.na(ej$strata))==nrow(ej)){
      NA
    }else{
      calinhara(data.frame(ej$irsl),as.numeric(ej$strata))
    }
  })
  
  pc_sianre_val$strata <- dh[[which(unlist(ch)==max(unlist(ch),na.rm=T))[1]]]$strata
  pc_sianre_val$irsl <- round(pc_sianre_val$irsl, 10)
  
  pc_sianre_val[,`:=`(grsl=dplyr::case_when(
    strata==1 ~ 'Muy bajo',
    strata==2 ~ 'Bajo',
    strata==3 ~ 'Medio',
    strata==4 ~ 'Alto',
    strata==5 ~ 'Muy alto'
  ))]
  
  dt[,(col):= lapply(.SD,function(x) round(x,10)), .SDcols=col]
  
  print(paste0('Termina nivel:', nv))
  
  dt[,c(grep('cve_',colnames(dt),value=T),col,'anio'),with=FALSE] %>%
    left_join(pc_sianre_val,by = c(grep('cve_',colnames(dt),value=T),'anio')) %>%
    fwrite(paste0('bases_finales/IRSL_',nv,'.csv'))
  
})
