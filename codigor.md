# TP
Trabajo práctico análisis exploratorio


# Subo todas las bases 

ruta <- "C:/Users/xlisouski/Documents/Facu/Organizacion de Datos/events.rds"
readRDS(ruta)->events

ruta <- "C:/Users/xlisouski/Documents/Facu/Organizacion de Datos/auctions.rds"
readRDS(ruta)->auctions

ruta <- "C:/Users/xlisouski/Documents/Facu/Organizacion de Datos/clicks.csv.gzip"
read.csv(ruta)->clicks

ruta <- "C:/Users/xlisouski/Documents/Facu/Organizacion de Datos/installs.csv.gzip"
read.csv(ruta)->installs

# Abro librerias

library(StatMeasures)
library(lubridate)
library(plotly)
library(reshape2)
library(formattable)

options(scippen=999)

#### CONTEOS INICIALES #### 

CONTEOS_INICIALES <- data.frame(Data_set=c("Auctions","Clicks","Events","Installs"),
                                Cantidad_Columnas = c(ncol(auctions),
                                                      ncol(clicks),
                                                      ncol(events),
                                                      ncol(installs)),
                                Cantidad_Registros = c(nrow(auctions),
                                                     nrow(clicks),
                                                     nrow(events),
                                                     nrow(installs)),
                                Cantidad_Dispositivos = c(length(unique(auctions$device_id)),
                                                   length(unique(clicks$ref_hash)),
                                                   length(unique(events$ref_hash)),
                                                   length(unique(installs$ref_hash))))

CONTEOS_INICIALES$Prom_Repeticiones_Disp <- round(CONTEOS_INICIALES$Cantidad_Registros/CONTEOS_INICIALES$Cantidad_Dispositivos,1)

formattable(CONTEOS_INICIALES,row.names=FALSE,
            list(Cantidad_Columnas = color_bar("#FA614B"),
                 Cantidad_Registros = color_bar("#FFD700"),
                 Cantidad_Dispositivos = color_bar("#66CDAA"),
                 Prom_Repeticiones_Disp = color_bar("#87CEEB"))) 

# CONTENIDO 

resumen_auctions <- contents(auctions)
resumen_clicks <- contents(clicks)
resumen_installs <- contents(installs)
resumen_events <- contents(events)

# AUCTIONS EN CLICKS 

conteos <- as.data.frame(table(unique(auctions$device_id) %in% clicks$ref_hash))
colnames(conteos)[1]<- "device_id de Auctions en ref_hash de Clicks"
conteos$Freq_Rel <- round(conteos$Freq/sum(conteos$Freq),2)

formattable(conteos,
            list(Freq_Rel = color_bar("#FFC0CB"))) 

# AUCTIONS EN EVENTS 

conteos <- as.data.frame(table(unique(auctions$device_id) %in% events$ref_hash))
colnames(conteos)[1]<- "device_id de Auctions en ref_hash de Events"
conteos$Freq_Rel <- round(conteos$Freq/sum(conteos$Freq),2)

formattable(conteos,
            list(Freq_Rel = color_bar("#FFC0CB"))) 

# AUCTIONS EN INSTALLS 

conteos <- as.data.frame(table(unique(auctions$device_id) %in% installs$ref_hash))
colnames(conteos)[1]<- "device_id de Auctions en ref_hash de Installs"
conteos$Freq_Rel <- round(conteos$Freq/sum(conteos$Freq),4)

formattable(conteos,
           list(Freq_Rel = color_bar("#FFC0CB"))) 


# Cruce de todo: 

device_id_auctions <- unique(auctions$device_id)
ref_hash_clicks <- unique(clicks$ref_hash)
ref_hash_events <- unique(events$ref_hash)
ref_hash_installs <- unique(installs$ref_hash)

dispositivos <- data.frame(Data_set=c("Auctions","Clicks","Events","Installs"),
                           Cantidad_Dispositivos = c(length(device_id_auctions),
                                                     length(ref_hash_clicks),
                                                     length(ref_hash_events),
                                                     length(ref_hash_installs)))

dispositivos$Cant_En_Auctions <- c(length(device_id_auctions[device_id_auctions%in%device_id_auctions]),
                                   length(ref_hash_clicks[ref_hash_clicks%in%device_id_auctions]),
                                   length(ref_hash_events[ref_hash_events%in%device_id_auctions]),
                                   length(ref_hash_installs[ref_hash_installs%in%device_id_auctions]))

dispositivos$Cant_En_Clicks <- c(length(device_id_auctions[device_id_auctions%in%ref_hash_clicks]),
                                   length(ref_hash_clicks[ref_hash_clicks%in%ref_hash_clicks]),
                                   length(ref_hash_events[ref_hash_events%in%ref_hash_clicks]),
                                   length(ref_hash_installs[ref_hash_installs%in%ref_hash_clicks]))

dispositivos$Cant_En_Events <- c(length(device_id_auctions[device_id_auctions%in%ref_hash_events]),
                                 length(ref_hash_clicks[ref_hash_clicks%in%ref_hash_events]),
                                 length(ref_hash_events[ref_hash_events%in%ref_hash_events]),
                                 length(ref_hash_installs[ref_hash_installs%in%ref_hash_events]))

dispositivos$Cant_En_Installs <- c(length(device_id_auctions[device_id_auctions%in%ref_hash_installs]),
                                 length(ref_hash_clicks[ref_hash_clicks%in%ref_hash_installs]),
                                 length(ref_hash_events[ref_hash_events%in%ref_hash_installs]),
                                 length(ref_hash_installs[ref_hash_installs%in%ref_hash_installs]))

dispositivos

formattable(dispositivos,
            list(area(col = colnames(dispositivos)[2:ncol(dispositivos)]) ~ color_tile("transparent", "lightblue")))

dispositivos_rel <- dispositivos[,3:ncol(dispositivos)]/dispositivos[,2]
dispositivos_rel <- data.frame(Data_set=c("Auctions","Clicks","Events","Installs"),
                               dispositivos_rel)

formattable(dispositivos_rel,
            list(area(col = colnames(dispositivos_rel)[2:ncol(dispositivos_rel)]) ~ color_tile("transparent", "pink")))


#### DISTRIBUCION SEGUN DIA Y HORA:

#### Auctions #### 

# Todos los registros son de 2019:
anio <- substr(auctions$date,1,4)
table(anio)

# Todos los registros son de marzo:
meses <- substr(auctions$date,6,7)
table(meses)

# Ocurren entre el 05 y 13 de marzo:
auctions$dia <- substr(auctions$date,9,10)
table(auctions$dia)

# Hora: 
auctions$hora <- hour(auctions$date)
table(auctions$hora)


#### Events #### 

# Todos los registros son de 2019:
anio <- substr(events$date,1,4)
table(anio)

# Todos los registros son de marzo:
meses <- substr(events$date,6,7)
table(meses)

# Ocurren entre el 05 y 13 de marzo:
events$dia <- substr(events$date,9,10)
table(events$dia)

# Hora: 
events$hora <- hour(events$date)
table(events$hora)


#### Installs #### 

# Todos los registros son de 2019:
anio <- substr(installs$created,1,4)
table(anio)

# Todos los registros son de marzo:
meses <- substr(installs$created,6,7)
table(meses)

# Ocurren entre el 05 y 13 de marzo:
installs$dia <- substr(installs$created,9,10)
table(installs$dia)

# Hora: 
installs$hora <- hour(installs$created)
table(installs$hora)


#### Clicks #### 

# Todos los registros son de 2019:
anio <- substr(clicks$created,1,4)
table(anio)

# Todos los registros son de marzo:
meses <- substr(clicks$created,6,7)
table(meses)

# Ocurren entre el 05 y 13 de marzo:
clicks$dia <- substr(clicks$created,9,10)
table(clicks$dia)

# Hora: 
clicks$hora <- substr(clicks$created,12,13)
table(clicks$hora)

# Tabla de relaciones para dia de la semana: 

relaciones_semana <- data.frame(NUMERO=5:13,
                                DIA=c("Martes",
                                      "Miercoles",
                                      "Jueves",
                                      "Viernes",
                                      "Sabado",
                                      "Domingo",
                                      "Lunes",
                                      "Martes",
                                      "Miercoles"))

# Armo la tabla de frecuencias segun dia: 
frecuencias <- as.data.frame(table(auctions$dia))
colnames(frecuencias)[2]<-"Freq_auctions"

frecuencias_alt <- as.data.frame(table(events$dia))
frecuencias$Freq_events <- frecuencias_alt$Freq[match(frecuencias$Var1,frecuencias_alt$Var1)]

frecuencias_alt <- as.data.frame(table(installs$dia))
frecuencias$Freq_installs <- frecuencias_alt$Freq[match(frecuencias$Var1,frecuencias_alt$Var1)]

frecuencias_alt <- as.data.frame(table(clicks$dia))
frecuencias$Freq_clicks <- frecuencias_alt$Freq[match(frecuencias$Var1,frecuencias_alt$Var1)]

colnames(frecuencias)[1]<-"Nro_Dia"

frecuencias$Nro_Dia <- paste(frecuencias$Nro_Dia,
                             relaciones_semana$DIA[match(as.numeric(as.character(frecuencias$Nro_Dia)),
                                                         relaciones_semana$NUMERO)])

frecuencias

formattable(frecuencias,row.names=FALSE,
            list(Freq_auctions = color_bar("#87CEEB"),
                 Freq_events = color_bar("#FFC0CB"),
                 Freq_installs = color_bar("#FFFACD"),
                 Freq_clicks = color_bar("#90EE90"))) 

# Calculo las frecuencias relativas: 

frecuencias$Freq_auctions_Rel <- frecuencias$Freq_auctions/sum(frecuencias$Freq_auctions)
frecuencias$Freq_events_Rel <- frecuencias$Freq_events/sum(frecuencias$Freq_events)
frecuencias$Freq_installs_Rel <- frecuencias$Freq_installs/sum(frecuencias$Freq_installs)
frecuencias$Freq_clicks_Rel <- frecuencias$Freq_clicks/sum(frecuencias$Freq_clicks)

frecuencias

# Armo el grÃ¡fico

p <- frecuencias %>% 
  plot_ly() %>%
  
  add_trace(x = ~Nro_Dia, y = ~Freq_auctions_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_auctions_Rel*100,1),"%"), textposition = 'auto',
            marker = list(color = '#87CEEB',
                          line = list(color = 'rgb(0,0,0)', width = 1.5)),
            name = "Auctions") %>%
  
  add_trace(x = ~Nro_Dia, y = ~Freq_events_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_events_Rel*100,1),"%"), textposition = 'auto',
            marker = list(color = '#FFC0CB',
                          line = list(color = 'rgb(0,0,0)', width = 1.5)),
            name = "Events") %>%
  
  add_trace(x = ~Nro_Dia, y = ~Freq_installs_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_installs_Rel*100,1),"%"), textposition = 'auto',
            marker = list(color = '#FFFACD',
                          line = list(color = 'rgb(0,0,0)', width = 1.5)),
            name = "Installs") %>%
  
  add_trace(x = ~Nro_Dia, y = ~Freq_clicks_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_clicks_Rel*100,1),"%"), textposition = 'auto',
            marker = list(color = '#90EE90',
                          line = list(color = 'rgb(0,0,0)', width = 1.5)),
            name = "Clicks") %>%
  
  layout(title = "DistribuciÃ³n de Registros segÃºn dÃ­a de ocurrencia",
         barmode = 'group',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = list(title = "Dia"))


p


# Armo la tabla de frecuencias segun hora: 
frecuencias <- as.data.frame(table(auctions$hora))
colnames(frecuencias)[2]<-"Freq_auctions"

frecuencias_alt <- as.data.frame(table(events$hora))
frecuencias$Freq_events <- frecuencias_alt$Freq[match(frecuencias$Var1,frecuencias_alt$Var1)]

frecuencias_alt <- as.data.frame(table(installs$hora))
frecuencias$Freq_installs <- frecuencias_alt$Freq[match(frecuencias$Var1,frecuencias_alt$Var1)]

frecuencias_alt <- as.data.frame(table(clicks$hora))
frecuencias$Freq_clicks <- frecuencias_alt$Freq[match(as.numeric(frecuencias$Var1),as.numeric(frecuencias_alt$Var1))]

colnames(frecuencias)[1]<-"Hora"

frecuencias$Hora <- paste(frecuencias$Hora,"Hs")

frecuencias

formattable(frecuencias,row.names=FALSE,
            list(Freq_auctions = color_bar("#87CEEB"),
                 Freq_events = color_bar("#FFC0CB"),
                 Freq_installs = color_bar("#FFFACD"),
                 Freq_clicks = color_bar("#90EE90"))) 

# Calculo las frecuencias relativas: 

frecuencias$Freq_auctions_Rel <- frecuencias$Freq_auctions/sum(frecuencias$Freq_auctions)
frecuencias$Freq_events_Rel <- frecuencias$Freq_events/sum(frecuencias$Freq_events)
frecuencias$Freq_installs_Rel <- frecuencias$Freq_installs/sum(frecuencias$Freq_installs)
frecuencias$Freq_clicks_Rel <- frecuencias$Freq_clicks/sum(frecuencias$Freq_clicks)

frecuencias

# Defino la forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = frecuencias$Hora)

# Armo el grÃ¡fico

p <- frecuencias %>% 
  plot_ly() %>%
  
  add_trace(x = ~Hora, y = ~Freq_auctions_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_auctions_Rel*100,1),"%"), textposition = 'auto',
            marker = list(color = '#87CEEB',
                          line = list(color = 'rgb(0,0,0)', width = 1.5)),
            name = "Auctions") %>%
  
  add_trace(x = ~Hora, y = ~Freq_events_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_events_Rel*100,1),"%"), textposition = 'auto',
            marker = list(color = '#FFC0CB',
                          line = list(color = 'rgb(0,0,0)', width = 1.5)),
            name = "Events") %>%
  
  add_trace(x = ~Hora, y = ~Freq_installs_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_installs_Rel*100,1),"%"), textposition = 'auto',
            marker = list(color = '#FFFACD',
                          line = list(color = 'rgb(0,0,0)', width = 1.5)),
            name = "Installs") %>%
  
  add_trace(x = ~Hora, y = ~Freq_clicks_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_clicks_Rel*100,1),"%"), textposition = 'auto',
            marker = list(color = '#90EE90',
                          line = list(color = 'rgb(0,0,0)', width = 1.5)),
            name = "Clicks") %>%
  
  layout(title = "DistribuciÃ³n de Registros segÃºn hora de ocurrencia",
         barmode = 'group',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = xform)


p


# Armo la tabla de frecuencias segun fecha y hora: 

# Auctions: 

dia_hs <- as.data.frame.matrix(table(auctions$hora,auctions$dia))
dia_hs <- dia_hs[order(as.numeric(rownames(dia_hs)),decreasing = TRUE),]

m <- as.matrix(dia_hs)
p <- plot_ly(
  x = paste(colnames(dia_hs),relaciones_semana$DIA[match(as.numeric(as.character(colnames(dia_hs))),
                                                         relaciones_semana$NUMERO)]), 
  y = paste(rownames(dia_hs),"hs"),
  z = m, type = "heatmap",
  colors = colorRamp(c("white", "red"))) %>%
  layout(title = "Auctions: Cantidad de Registros segun Dia y Horario",
         xaxis = list(title = "Dia"), yaxis = list(title = "Horario"))

p


# Events: 

dia_hs <- as.data.frame.matrix(table(events$hora,events$dia))
dia_hs <- dia_hs[order(as.numeric(rownames(dia_hs)),decreasing = TRUE),]

m <- as.matrix(dia_hs)
p <- plot_ly(
  x = paste(colnames(dia_hs),relaciones_semana$DIA[match(as.numeric(as.character(colnames(dia_hs))),
                                                         relaciones_semana$NUMERO)]), 
  y = paste(rownames(dia_hs),"hs"),
  z = m, type = "heatmap",
  colors = colorRamp(c("white", "blue"))) %>%
  layout(title = "Events: Cantidad de Registros segun Dia y Horario",
         xaxis = list(title = "Dia"), yaxis = list(title = "Horario"))

p


# Installs: 

dia_hs <- as.data.frame.matrix(table(trunc(installs$hora/4)*4,installs$dia))
dia_hs <- dia_hs[order(as.numeric(rownames(dia_hs)),decreasing = TRUE),]

m <- as.matrix(dia_hs)
p <- plot_ly(
  x = paste(colnames(dia_hs),relaciones_semana$DIA[match(as.numeric(as.character(colnames(dia_hs))),
                                                         relaciones_semana$NUMERO)]), 
  y = paste(rownames(dia_hs),"hs"),
  z = m, type = "heatmap",
  colors = colorRamp(c("white", "orange"))) %>%
  layout(title = "Installs: Cantidad de Registros segun Dia y Horario",
         xaxis = list(title = "Dia"), yaxis = list(title = "Horario"))

p

# Clicks: 

dia_hs <- as.data.frame.matrix(table(trunc(as.numeric(as.character(clicks$hora))/4)*4,clicks$dia))
dia_hs <- dia_hs[order(as.numeric(rownames(dia_hs)),decreasing = TRUE),]

m <- as.matrix(dia_hs)
p <- plot_ly(
  x = paste(colnames(dia_hs),relaciones_semana$DIA[match(as.numeric(as.character(colnames(dia_hs))),
                                                         relaciones_semana$NUMERO)]), 
  y = paste(rownames(dia_hs),"hs"),
  z = m, type = "heatmap",
  colors = colorRamp(c("white", "pink"))) %>%
  layout(title = "Clicks: Cantidad de Registros segun Dia y Horario",
         xaxis = list(title = "Dia"), yaxis = list(title = "Horario"))

p

#### Explorando Auctions ####

#### Analisis de cantidad de dispositivos

length(unique(auctions$device_id))

TABLA_DISP <- as.data.frame(table(auctions$device_id))
summary(TABLA_DISP$Freq)

minimo <- min(TABLA_DISP$Freq)
maximo <- max(TABLA_DISP$Freq)

promedio <- mean(TABLA_DISP$Freq)

desvio <- sd(TABLA_DISP$Freq)
cv <- desvio / mean(TABLA_DISP$Freq)

Perc_25<-quantile(TABLA_DISP$Freq,0.25)
Perc_50<-quantile(TABLA_DISP$Freq,0.5)
Perc_75<-quantile(TABLA_DISP$Freq,0.75)

valores <- c(minimo,
             Perc_25,
             Perc_50,
             promedio,
             Perc_75,
             maximo,
             desvio,
             cv)


nombres <- c("Minimo",
             "Perc_25",
             "Perc_50",
             "Promedio",
             "Perc_75",
             "Maximo",
             "Desvio_Std",
             "Coef_Variac")

# Tabla de metricas:
p <- data.frame(Metrica=nombres,
                Valores=round(valores,2))

formattable(p)

# Histograma: 
p <- plot_ly(x = TABLA_DISP$Freq[TABLA_DISP$Freq<quantile(TABLA_DISP$Freq,0.9)], type = "histogram") %>%
  
  layout(title = "Distribucion de cantidad de veces que aparece un dispositivo",
         xaxis = list(title = "Cantidad de veces que aparece un dispositivo"),
         yaxis = list(title = "Frecuencia"))
p

# Tabla de conteos: 

TABLA_DISP$Cant_Disp_Bin <- ifelse(TABLA_DISP$Freq>500,500,
                                   ifelse(TABLA_DISP$Freq>250,250,
                                          ifelse(TABLA_DISP$Freq>100,100,
                                                 ifelse(TABLA_DISP$Freq>50,50,
                                                        ifelse(TABLA_DISP$Freq>20,20,
                                                               ifelse(TABLA_DISP$Freq>10,10,
                                                                      ifelse(TABLA_DISP$Freq>5,6,
                                                                             ifelse(TABLA_DISP$Freq>1,3,
                                                                                    TABLA_DISP$Freq))))))))

match_conteos <- data.frame(valor=sort(unique(TABLA_DISP$Cant_Disp_Bin),decreasing = TRUE),
                            texto=c("> 500",
                                    ">250 & <=500",
                                    ">100 & <=250",
                                    ">50 & <=100",
                                    ">20 & <=50",
                                    ">10 & <=20",
                                    ">5 & <=10",
                                    ">1 & <=5",
                                    1))


frecuencias <- as.data.frame(table(TABLA_DISP$Cant_Disp_Bin))
frecuencias

frecuencias_final <- data.frame(Cant_Dispositivos=match_conteos$texto[match(frecuencias$Var1,match_conteos$valor)],
                                Frec_Absoluta=round(frecuencias$Freq,0),
                                Frec_Relativa=paste(round(frecuencias$Freq/sum(frecuencias$Freq)*100,1),"%"))
frecuencias_final

formattable(frecuencias_final,
            list(Frec_Absoluta = color_bar("#FFB6C1")))


#### Analisis de columna Platform

# Primero analizo la distribucion: 

frecuencias <- as.data.frame(table(auctions$platform))
frecuencias

# Grafico: 
p <- plot_ly(
  x = frecuencias$Var1,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Registros segun Platform",
         xaxis = list(title = "Platform"),
         yaxis = list(title = "Cantidad de Registros"))
p

# Platform por dia y hora

HORA_AGR <- trunc(auctions$hora/6)*6

Eje_x <- paste(auctions$dia,HORA_AGR,"hs")

DIA_HORA_PLATFORM_EVOL <- as.data.frame.matrix(table(Eje_x,auctions$platform))

colsplit(rownames(DIA_HORA_PLATFORM_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_PLATFORM_EVOL <- DIA_HORA_PLATFORM_EVOL[order(as.numeric(a$HORA)),]

colsplit(rownames(DIA_HORA_PLATFORM_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_PLATFORM_EVOL <- DIA_HORA_PLATFORM_EVOL[order(as.numeric(a$DIA)),]

DIA_HORA_PLATFORM_EVOL$FREQ_REL_1 <- DIA_HORA_PLATFORM_EVOL$`1`/sum(DIA_HORA_PLATFORM_EVOL$`1`)
DIA_HORA_PLATFORM_EVOL$FREQ_REL_2 <- DIA_HORA_PLATFORM_EVOL$`2`/sum(DIA_HORA_PLATFORM_EVOL$`2`)

DIA_HORA_PLATFORM_EVOL$TASA_REL_1 <- DIA_HORA_PLATFORM_EVOL$`1`/rowSums(DIA_HORA_PLATFORM_EVOL[,1:2])
DIA_HORA_PLATFORM_EVOL$TASA_REL_2 <- DIA_HORA_PLATFORM_EVOL$`2`/rowSums(DIA_HORA_PLATFORM_EVOL[,1:2])

DIA_HORA_PLATFORM_EVOL$Eje_x <- rownames(DIA_HORA_PLATFORM_EVOL)

# Defino la forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = c(DIA_HORA_PLATFORM_EVOL$Eje_x))

# Armo grafico 1

p <- DIA_HORA_PLATFORM_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~`1`,  type = 'scatter', mode = 'lines', 
            text = DIA_HORA_PLATFORM_EVOL$`1`, textposition = 'auto',
            line = list(color = 'rgb(236,18,171)', width = 1.5),
            name = "Platform = 1") %>%
  
  add_trace(x = ~Eje_x, y = ~`2`, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_PLATFORM_EVOL$`2`, textposition = 'auto',
            line = list(color = 'rgb(30,199,242)', width = 1.5),
            name = "Platform = 2") %>%
  
  layout(title = "Evolucion de la cantidad de subastas segun platform",
         yaxis = list(title = "Cantidad de Subastas"),
         xaxis = xform)

p
# Acumulado

DIA_HORA_PLATFORM_EVOL$FREQ_1_AC <- cumsum(DIA_HORA_PLATFORM_EVOL$`1`)
DIA_HORA_PLATFORM_EVOL$FREQ_2_AC <- cumsum(DIA_HORA_PLATFORM_EVOL$`2`)


p <- DIA_HORA_PLATFORM_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~FREQ_1_AC,  type = 'scatter', mode = 'lines', 
            text = DIA_HORA_PLATFORM_EVOL$FREQ_1_AC, textposition = 'auto',
            line = list(color = 'rgb(236,18,171)', width = 1.5),
            name = "Platform = 1") %>%
  
  add_trace(x = ~Eje_x, y = ~FREQ_2_AC, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_PLATFORM_EVOL$FREQ_2_AC, textposition = 'auto',
            line = list(color = 'rgb(30,199,242)', width = 1.5),
            name = "Platform = 2") %>%
  
  layout(title = "Evolucion de la cantidad acumulada de subastas segun platform",
         yaxis = list(title = "Cantidad de Subastas"),
         xaxis = xform)

p

# Armo grafico 2

p <- DIA_HORA_PLATFORM_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_1, type = 'bar', 
            text = paste(round(DIA_HORA_PLATFORM_EVOL$TASA_REL_1*100,1),"%"), textposition = 'auto',
            marker = list(color = 'rgb(236,18,171)',
                          line = list(color = 'rgb(236,18,171)', width = 1.5)),
            name = "Platform = 1") %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_2, type = 'bar', 
            text = paste(round(DIA_HORA_PLATFORM_EVOL$TASA_REL_2*100,1),"%"), textposition = 'auto',
            marker = list(color = 'rgb(30,199,242)',
                          line = list(color = 'rgb(30,199,242)', width = 1.5)),
            name = "Platform = 2") %>%
  
  layout(title = "Evolucion de la distribucion de subastas segun platform",
         barmode = 'stack',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = xform)

p


#### Analisis de columna source_id

# Primero analizo la distribucion: 

frecuencias <- as.data.frame(table(auctions$source_id))
frecuencias

# Grafico: 
p <- plot_ly(
  x = frecuencias$Var1,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Registros segun source_id",
         xaxis = list(title = "source_id"),
         yaxis = list(title = "Cantidad de Registros"))
p

peso_cats_poco_frec <- sum(frecuencias$Freq[frecuencias$Var1 %in% c(2,6)])/sum(frecuencias$Freq)
peso_cats_poco_frec


# source_id por dia y hora

HORA_AGR <- trunc(auctions$hora/6)*6

Eje_x <- paste(auctions$dia,HORA_AGR,"hs")

DIA_HORA_SOURCE_EVOL <- as.data.frame.matrix(table(Eje_x,auctions$source_id))

colsplit(rownames(DIA_HORA_SOURCE_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_SOURCE_EVOL <- DIA_HORA_SOURCE_EVOL[order(as.numeric(a$HORA)),]

colsplit(rownames(DIA_HORA_SOURCE_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_SOURCE_EVOL <- DIA_HORA_SOURCE_EVOL[order(as.numeric(a$DIA)),]

DIA_HORA_SOURCE_EVOL$TASA_REL_0 <- DIA_HORA_SOURCE_EVOL$`0`/rowSums(DIA_HORA_SOURCE_EVOL[,1:5])
DIA_HORA_SOURCE_EVOL$TASA_REL_1 <- DIA_HORA_SOURCE_EVOL$`1`/rowSums(DIA_HORA_SOURCE_EVOL[,1:5])
DIA_HORA_SOURCE_EVOL$TASA_REL_2 <- DIA_HORA_SOURCE_EVOL$`2`/rowSums(DIA_HORA_SOURCE_EVOL[,1:5])
DIA_HORA_SOURCE_EVOL$TASA_REL_5 <- DIA_HORA_SOURCE_EVOL$`5`/rowSums(DIA_HORA_SOURCE_EVOL[,1:5])
DIA_HORA_SOURCE_EVOL$TASA_REL_6 <- DIA_HORA_SOURCE_EVOL$`6`/rowSums(DIA_HORA_SOURCE_EVOL[,1:5])


DIA_HORA_SOURCE_EVOL$Eje_x <- rownames(DIA_HORA_SOURCE_EVOL)

# Defino la forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = c(DIA_HORA_SOURCE_EVOL$Eje_x))

# Armo grafico 1

p <- DIA_HORA_SOURCE_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~`0`,  type = 'scatter', mode = 'lines', 
            text = DIA_HORA_SOURCE_EVOL$`0`, textposition = 'auto',
            line = list(color = 'rgb(236,18,171)', width = 1.5),
            name = "source_id = 0") %>%
  
  add_trace(x = ~Eje_x, y = ~`1`, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_SOURCE_EVOL$`1`, textposition = 'auto',
            line = list(color = 'rgb(30,199,242)', width = 1.5),
            name = "source_id = 1") %>%
  
  add_trace(x = ~Eje_x, y = ~`2`, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_SOURCE_EVOL$`2`, textposition = 'auto',
            line = list(color = 'rgb(15,209,99)', width = 1.5),
            name = "source_id = 2") %>%
  
  add_trace(x = ~Eje_x, y = ~`5`, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_SOURCE_EVOL$`2`, textposition = 'auto',
            line = list(color = 'rgb(243,229,36)', width = 1.5),
            name = "source_id = 5") %>%
  
  add_trace(x = ~Eje_x, y = ~`6`, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_SOURCE_EVOL$`2`, textposition = 'auto',
            line = list(color = 'rgb(149,77,212)', width = 1.5),
            name = "source_id = 6") %>%
  
  layout(title = "Evolucion de la cantidad de subastas segun source_id",
         yaxis = list(title = "Cantidad de Subastas"),
         xaxis = xform)

p


# Acumulado

DIA_HORA_SOURCE_EVOL$FREQ_0_AC <- cumsum(DIA_HORA_SOURCE_EVOL$`0`)
DIA_HORA_SOURCE_EVOL$FREQ_1_AC <- cumsum(DIA_HORA_SOURCE_EVOL$`1`)
DIA_HORA_SOURCE_EVOL$FREQ_2_AC <- cumsum(DIA_HORA_SOURCE_EVOL$`2`)
DIA_HORA_SOURCE_EVOL$FREQ_5_AC <- cumsum(DIA_HORA_SOURCE_EVOL$`5`)
DIA_HORA_SOURCE_EVOL$FREQ_6_AC <- cumsum(DIA_HORA_SOURCE_EVOL$`6`)

p <- DIA_HORA_SOURCE_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~FREQ_0_AC,  type = 'scatter', mode = 'lines', 
            text = DIA_HORA_SOURCE_EVOL$FREQ_0_AC, textposition = 'auto',
            line = list(color = 'rgb(236,18,171)', width = 1.5),
            name = "source_id = 0") %>%
  
  add_trace(x = ~Eje_x, y = ~FREQ_1_AC, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_SOURCE_EVOL$FREQ_0_AC, textposition = 'auto',
            line = list(color = 'rgb(30,199,242)', width = 1.5),
            name = "source_id = 1") %>%
  
  add_trace(x = ~Eje_x, y = ~FREQ_2_AC, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_SOURCE_EVOL$FREQ_2_AC, textposition = 'auto',
            line = list(color = 'rgb(15,209,99)', width = 1.5),
            name = "source_id = 2") %>%
  
  add_trace(x = ~Eje_x, y = ~FREQ_5_AC, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_SOURCE_EVOL$FREQ_5_AC, textposition = 'auto',
            line = list(color = 'rgb(243,229,36)', width = 1.5),
            name = "source_id = 5") %>%
  
  add_trace(x = ~Eje_x, y = ~FREQ_6_AC, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_SOURCE_EVOL$FREQ_6_AC, textposition = 'auto',
            line = list(color = 'rgb(149,77,212)', width = 1.5),
            name = "source_id = 6") %>%
  
  layout(title = "Evolucion de la cantidad acumulada de subastas segun source_id",
         yaxis = list(title = "Cantidad de Subastas"),
         xaxis = xform)

p

# Armo grafico 2

p <- DIA_HORA_SOURCE_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_0,  type = 'bar', 
            text = DIA_HORA_SOURCE_EVOL$TASA_REL_0, textposition = 'auto',
            marker = list(color = 'rgb(236,18,171)',
                          line = list(color = 'rgb(236,18,171)', width = 1.5)),
            name = "source_id = 0") %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_1, type = 'bar', 
            text = DIA_HORA_SOURCE_EVOL$TASA_REL_1, textposition = 'auto',
            marker = list(color = 'rgb(30,199,242)',
                          line = list(color = 'rgb(30,199,242)', width = 1.5)),
            name = "source_id = 1") %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_2, type = 'bar',  
            text = DIA_HORA_SOURCE_EVOL$TASA_REL_2, textposition = 'auto',
            marker = list(color = 'rgb(15,209,99)',
                          line = list(color = 'rgb(15,209,99)', width = 1.5)),
            name = "source_id = 2") %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_5, type = 'bar',  
            text = DIA_HORA_SOURCE_EVOL$TASA_REL_5, textposition = 'auto',
            marker = list(color = 'rgb(243,229,36)',
                          line = list(color = 'rgb(243,229,36)', width = 1.5)),
            name = "source_id = 5") %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_6, type = 'bar', 
            text = DIA_HORA_SOURCE_EVOL$TASA_REL_6, textposition = 'auto',
            marker = list(color = 'rgb(149,77,212)',
                          line = list(color = 'rgb(149,77,212)', width = 1.5)),
            name = "source_id = 6") %>%  
  
  layout(title = "Evolucion de la distribucion de subastas segun source_id",
         barmode = 'stack',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = xform)

p


#### Midiendo el tiempo entre subastas: ####

# Ordeno la base de menor a mayor fecha, y luego por dispositivo: 

auctions <- auctions[order(auctions$date),]
auctions <- auctions[order(auctions$device_id),]

head(auctions)

# Armo el vector de id dispositivo corrido, para ver si el siguiente es el mismo: 

a <- c(auctions$device_id[-1],0)

auctions$device_id_2 <- a

auctions$mismo_disp <- ifelse(auctions$device_id==auctions$device_id_2,1,0)
table(auctions$mismo_disp)

# Agrego la fecha corrida 

as.character(auctions$date) -> auctions$date

a <- auctions$date[-1]

a[nrow(auctions)]<-auctions$date[length(auctions$date)]

auctions$date_2 <- a

head(auctions)

# Elimino incalculables (aparecen una sola vez, son el ultimo dispositivo): 

frecuencias <- as.data.frame(table(auctions$device_id))

subset(auctions,auctions$device_id %in% frecuencias$Var1[frecuencias$Freq>1]) -> auctions_2
subset(auctions_2,auctions_2$mismo_disp==1)->auctions_2

# Tomo una muestra para analizar (por tiempos de procesamiento): 

# Verifico que la semilla anda bien: 
set.seed(5)
runif(nrow(auctions_2),0,1) -> aleatorios_1 

set.seed(5)
runif(nrow(auctions_2),0,1) -> aleatorios_2 

# Tomo la muestra aleatoria con semilla: 
set.seed(5)
muestra_aleatoria <- subset(auctions_2,runif(nrow(auctions_2),0,1)<=0.2)

muestra_aleatoria$dif_tiempo <- difftime(muestra_aleatoria$date_2,
                                         muestra_aleatoria$date,
                                         units = "secs")

muestra_aleatoria$dif_tiempo

muestra_aleatoria$dif_tiempo_2 <- as.numeric(muestra_aleatoria$dif_tiempo)

table(is.na(muestra_aleatoria$dif_tiempo))


# Armo algunas metricas:

minimo <- min(muestra_aleatoria$dif_tiempo_2)
maximo <- max(muestra_aleatoria$dif_tiempo_2)
promedio <- mean(muestra_aleatoria$dif_tiempo_2)
desvio <- sd(muestra_aleatoria$dif_tiempo_2)
cv <- desvio / mean(muestra_aleatoria$dif_tiempo_2)

Perc_25<-quantile(muestra_aleatoria$dif_tiempo_2,0.25)
Perc_50<-quantile(muestra_aleatoria$dif_tiempo_2,0.5)
Perc_75<-quantile(muestra_aleatoria$dif_tiempo_2,0.75)

valores <- c(minimo,
             Perc_25,
             Perc_50,
             promedio,
             Perc_75,
             maximo,
             desvio,
             cv)


nombres <- c("Minimo",
             "Perc_25",
             "Perc_50",
             "Promedio",
             "Perc_75",
             "Maximo",
             "Desvio_Std",
             "Coef_Variac")

# Tabla de metricas:
p <- data.frame(Metrica=nombres,
                Valores_DifTime_EnSegundos=round(valores,6))

formattable(p)

(maximo/minimo-1) * 100

quantile(muestra_aleatoria$dif_tiempo_2,0.90)


# Histograma: 
p <- plot_ly(x = muestra_aleatoria$dif_tiempo_2[muestra_aleatoria$dif_tiempo_2 <= quantile(muestra_aleatoria$dif_tiempo_2,0.75)], 
             type = "histogram") %>%
  
  layout(title = "Distribucion de tiempo entre subastas (hasta percentil 75)",
         xaxis = list(title = "Tiempo entre subastas"),
         yaxis = list(title = "Frecuencia"))
p


# Histograma 2: 

outliers <- muestra_aleatoria$dif_tiempo_2[muestra_aleatoria$dif_tiempo_2 > quantile(muestra_aleatoria$dif_tiempo_2,0.75)]

# Topeo los outliers en 2000 segundos 

outliers <- outliers[outliers<3600]

p <- plot_ly(x = outliers, 
             type = "histogram") %>%
  
  layout(title = "Distribucion de tiempo entre subastas (superior percentil 75 y menor a 3600 seg)",
         xaxis = list(title = "Tiempo entre subastas"),
         yaxis = list(title = "Frecuencia"))
p

table(muestra_aleatoria$dif_tiempo_2>3600)/nrow(muestra_aleatoria)

table(muestra_aleatoria$dif_tiempo_2>3600*24)/nrow(muestra_aleatoria)


# Compara medidas segun platform 

compara <- aggregate(muestra_aleatoria$dif_tiempo_2,
                     by=list(muestra_aleatoria$platform),
                     FUN = function(x) c(Cantidad=length(x),Minimo=min(x),Media=mean(x),
                                         Pct_25=quantile(x,0.25),
                                         Mediana=median(x),
                                         Pct_75=quantile(x,0.75),
                                         Maximo=max(x),Desvio=sd(x),
                                         Coef_Var=sd(x)/mean(x)))
compara <- data.frame(Platform=compara$Group.1,
                      compara$x)

compara

formattable(compara,row.names=FALSE,
            list(Mediana = color_bar("#FA614B"),
                 Pct_25.25. = color_bar("#FA614B"),
                 Pct_75.75. = color_bar("#FA614B"))) 


# DistribuciÃ³n de los tiempos entre subastas segun platform

table(muestra_aleatoria$platform)

tiempo_platform_1 <- muestra_aleatoria$dif_tiempo_2[muestra_aleatoria$platform==1]
tiempo_platform_1 <- ifelse(tiempo_platform_1<2,1,
                            ifelse(tiempo_platform_1<5,2,
                                   ifelse(tiempo_platform_1<10,3,
                                          ifelse(tiempo_platform_1<20,4,
                                                 ifelse(tiempo_platform_1<60,5,
                                                        ifelse(tiempo_platform_1<120,6,
                                                               ifelse(tiempo_platform_1<240,7,
                                                                      ifelse(tiempo_platform_1<480,8,
                                                                             ifelse(tiempo_platform_1<960,9,
                                                                                    ifelse(tiempo_platform_1<3600,10,11))))))))))

table(tiempo_platform_1)

tiempo_platform_2 <- muestra_aleatoria$dif_tiempo_2[muestra_aleatoria$platform==2]
tiempo_platform_2 <- ifelse(tiempo_platform_2<2,1,
                            ifelse(tiempo_platform_2<5,2,
                                   ifelse(tiempo_platform_2<10,3,
                                          ifelse(tiempo_platform_2<20,4,
                                                 ifelse(tiempo_platform_2<60,5,
                                                        ifelse(tiempo_platform_2<120,6,
                                                               ifelse(tiempo_platform_2<240,7,
                                                                      ifelse(tiempo_platform_2<480,8,
                                                                             ifelse(tiempo_platform_2<960,9,
                                                                                    ifelse(tiempo_platform_2<3600,10,11))))))))))

table(tiempo_platform_2)

frecuencias <- as.data.frame(table(tiempo_platform_1))
frecuencias_alt <- as.data.frame(table(tiempo_platform_2))
frecuencias_alt[is.na(frecuencias_alt)]<-0

frecuencias$Freq_Platform_2 <- frecuencias_alt$Freq[match(frecuencias$tiempo_platform_1,
                                                          frecuencias_alt$tiempo_platform_2)]

frecuencias$Freq_Rel_Platform_1 <- frecuencias$Freq/sum(frecuencias$Freq)
frecuencias$Freq_Rel_Platform_2 <- frecuencias$Freq_Platform_2/sum(frecuencias$Freq_Platform_2)

match_tiempo <- data.frame(cod=c(1:11),
                           valor=c("< 2 seg",
                                   "< 5 seg",
                                   "< 10 seg",
                                   "< 20 seg",
                                   "< 1 min",
                                   "< 2 min",
                                   "< 4 min",
                                   "< 8 min",
                                   "< 16 min",
                                   "< 1 hs",
                                   "> 1 hs"))

frecuencias <- data.frame(Tiempo_Entre_Subastas=match_tiempo$valor[match(frecuencias$tiempo_platform_1,
                                                                         match_tiempo$cod)],
                          Freq_Rel_Platform_1=frecuencias$Freq_Rel_Platform_1,
                          Freq_Rel_Platform_2=frecuencias$Freq_Rel_Platform_2)

# Armo el grafico: 

# Defino la forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = frecuencias$Tiempo_Entre_Subastas)

p <- frecuencias %>% 
  plot_ly() %>%
  
  add_trace(x = ~Tiempo_Entre_Subastas, y = ~Freq_Rel_Platform_1, type = 'bar', 
            text = paste(round(frecuencias$Freq_Rel_Platform_1*100,1),"%"), 
            marker = list(color = 'rgb(255,165,0)',
                          line = list(width = 1.5)),
            name = "Platform_1") %>%
  
  add_trace(x = ~Tiempo_Entre_Subastas, y = ~Freq_Rel_Platform_2, type = 'bar', 
            text = paste(round(frecuencias$Freq_Rel_Platform_2*100,1),"%"), 
            marker = list(color = 'rgb(30,144,255)',
                          line = list(width = 1.5)),
            name = "Platform_2") %>%
  
   layout(title = "Distribuciones de tiempo entre subastas segun platform",
         barmode = 'group',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = xform)

p


# Compara medidas segun source_id 

compara <- aggregate(muestra_aleatoria$dif_tiempo_2,
                     by=list(muestra_aleatoria$source_id),
                     FUN = function(x) c(Cantidad=length(x),Minimo=min(x),Media=mean(x),
                                         Pct_25=quantile(x,0.25),
                                         Mediana=median(x),
                                         Pct_75=quantile(x,0.75),
                                         Maximo=max(x),Desvio=sd(x),
                                         Coef_Var=sd(x)/mean(x)))
compara <- data.frame(Source_id=compara$Group.1,
                      compara$x)

compara

formattable(compara,row.names=FALSE,
            list(Mediana = color_bar("#FA614B"),
                 Pct_25.25. = color_bar("#FA614B"),
                 Pct_75.75. = color_bar("#FA614B"))) 


#### Explorando Installs ####

length(unique(installs$ref_hash))
nrow(installs)

TABLA_DISP <- as.data.frame(table(installs$ref_hash))
summary(TABLA_DISP$Freq)

minimo <- min(TABLA_DISP$Freq)
maximo <- max(TABLA_DISP$Freq)

promedio <- mean(TABLA_DISP$Freq)

desvio <- sd(TABLA_DISP$Freq)
cv <- desvio / mean(TABLA_DISP$Freq)

Perc_25<-quantile(TABLA_DISP$Freq,0.25)
Perc_50<-quantile(TABLA_DISP$Freq,0.5)
Perc_75<-quantile(TABLA_DISP$Freq,0.75)

valores <- c(minimo,
             Perc_25,
             Perc_50,
             promedio,
             Perc_75,
             maximo,
             desvio,
             cv)


nombres <- c("Minimo",
             "Perc_25",
             "Perc_50",
             "Promedio",
             "Perc_75",
             "Maximo",
             "Desvio_Std",
             "Coef_Variac")

# Tabla de metricas:
p <- data.frame(Metrica=nombres,
                Valores=round(valores,2))

formattable(p)

# Tabla de conteos: 

table(TABLA_DISP$Freq>1)
table(TABLA_DISP$Freq>3)

#### Distribucion de aplicatoin_id

frecuencias <- as.data.frame(table(installs$application_id))
frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

# Agrupo las categorias con menos de 50 registros
binning <- ifelse(installs$application_id %in% frecuencias$Var1[frecuencias$Freq<50],
                  "Otros",
                  installs$application_id)

frecuencias <- as.data.frame(table(binning))
frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

# Defino la forma de las x
xform <- list(categoryorder = "array",
              categoryarray = frecuencias$binning)

# Armo el grafico
p <- plot_ly(
  x = frecuencias$binning,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun Aplication_id",
         xaxis = xform,
         yaxis = list(title = "Cantidad de Registros"))
p

# TOP 10 de lo incluido en otros:

otros <- as.data.frame(table(installs$application_id[binning=="Otros"]))
otros <- otros[order(otros$Freq,decreasing = TRUE),]
colnames(otros)[1]<-"Aplication_id"

formattable(head(otros,10),row.names=FALSE,
            list(Freq = color_bar("#FA614B"))) 


# Calculo pesos: 

sum(frecuencias$Freq[frecuencias$binning==7])/sum(frecuencias$Freq)
sum(frecuencias$Freq[frecuencias$binning==9])/sum(frecuencias$Freq)
sum(frecuencias$Freq[frecuencias$binning==10])/sum(frecuencias$Freq)
sum(frecuencias$Freq[frecuencias$binning==16])/sum(frecuencias$Freq)
sum(frecuencias$Freq[frecuencias$binning==8])/sum(frecuencias$Freq)

sum(frecuencias$Freq[frecuencias$binning%in%c(7,9,10,16,8)])/sum(frecuencias$Freq)

# Grafico importancia de categorias mas frecuentes

bin_cat <- ifelse(installs$application_id %in% c(7,9,10,16,8),"Aplication_id in c(7,9,10,16,8)","Otros")
frecuencias <- as.data.frame(table(bin_cat))
frecuencias$Freq_Rel <-frecuencias$Freq/sum(frecuencias$Freq)

frecuencias

# Grafico
p <- plot_ly(
  x = frecuencias$bin_cat,
  y = frecuencias$Freq_Rel,
  type = "bar",
  marker = list(color = c('rgba(110,193,160,1)', 'rgba(240,142,142,0.8)'))) %>%
  layout(title = "Distribucion de Installs segun grupos de Aplication_id",
         xaxis = list(title = "Grupos de Aplicacion_id"),
         yaxis = list(title = "Frecuencia Relativa"))
p

#### Distribucion de ref_type

frecuencias <- as.data.frame(table(installs$ref_type))
frecuencias

p <- plot_ly(
  x = frecuencias$Var1,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun ref_type",
         xaxis = list(title = "ref_type"),
         yaxis = list(title = "Cantidad de Registros"))
p


#### Evolucion de ref_type

HORA_AGR <- trunc(installs$hora/12)*12

Eje_x <- paste(installs$dia,HORA_AGR,"hs")

DIA_HORA_EVOL <- as.data.frame.matrix(table(Eje_x,installs$ref_type))

colsplit(rownames(DIA_HORA_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_EVOL <- DIA_HORA_EVOL[order(as.numeric(a$HORA)),]

colsplit(rownames(DIA_HORA_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_EVOL <- DIA_HORA_EVOL[order(as.numeric(a$DIA)),]

DIA_HORA_EVOL$FREQ_REL_1 <- DIA_HORA_EVOL$`1494519392962156800`/sum(DIA_HORA_EVOL$`1494519392962156800`)
DIA_HORA_EVOL$FREQ_REL_2 <- DIA_HORA_EVOL$`1891515180541284352`/sum(DIA_HORA_EVOL$`1891515180541284352`)

DIA_HORA_EVOL$TASA_REL_1 <- DIA_HORA_EVOL$`1494519392962156800`/rowSums(DIA_HORA_EVOL[,1:2])
DIA_HORA_EVOL$TASA_REL_2 <- DIA_HORA_EVOL$`1891515180541284352`/rowSums(DIA_HORA_EVOL[,1:2])

DIA_HORA_EVOL$Eje_x <- rownames(DIA_HORA_EVOL)

# Defino la forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = c(DIA_HORA_EVOL$Eje_x))

# Armo grafico 1

p <- DIA_HORA_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~`1494519392962156800`,  type = 'scatter', mode = 'lines', 
            text = DIA_HORA_EVOL$`1494519392962156800`, textposition = 'auto',
            line = list(color = 'rgb(236,18,171)', width = 1.5),
            name = "1494519392962156800") %>%
  
  add_trace(x = ~Eje_x, y = ~`1891515180541284352`, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_EVOL$`1891515180541284352`, textposition = 'auto',
            line = list(color = 'rgb(30,199,242)', width = 1.5),
            name = "1891515180541284352") %>%
  
  layout(title = "Cantidades de installs por dia y horario (cada 12 hs) segun ref_type",
         yaxis = list(title = "Cantidad de Installs"),
         xaxis = xform)

p

# Armo grafico 2

p <- DIA_HORA_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_1, type = 'bar', 
            text = paste(round(DIA_HORA_EVOL$TASA_REL_1*100,1),"%"), textposition = 'auto',
            marker = list(color = 'rgb(236,18,171)',
                          line = list(color = 'rgb(236,18,171)', width = 1.5)),
            name = "1494519392962156800") %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_2, type = 'bar', 
            text = paste(round(DIA_HORA_EVOL$TASA_REL_2*100,1),"%"), textposition = 'auto',
            marker = list(color = 'rgb(30,199,242)',
                          line = list(color = 'rgb(30,199,242)', width = 1.5)),
            name = "1891515180541284352") %>%
  
  layout(title = "Evolucion de la distribucion de installs segun ref_type",
         barmode = 'stack',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = xform)

p


#### Distribucion de implicit

frecuencias <- as.data.frame(table(installs$implicit))
frecuencias
frecuencias$Freq/sum(frecuencias$Freq)

p <- plot_ly(
  x = frecuencias$Var1,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun implicit",
         xaxis = list(title = "implicit"),
         yaxis = list(title = "Cantidad de Registros"))
p


#### Evolucion de implicit

HORA_AGR <- trunc(installs$hora/12)*12

Eje_x <- paste(installs$dia,HORA_AGR,"hs")

DIA_HORA_EVOL <- as.data.frame.matrix(table(Eje_x,installs$implicit))

colsplit(rownames(DIA_HORA_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_EVOL <- DIA_HORA_EVOL[order(as.numeric(a$HORA)),]

colsplit(rownames(DIA_HORA_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_EVOL <- DIA_HORA_EVOL[order(as.numeric(a$DIA)),]

DIA_HORA_EVOL$FREQ_REL_1 <- DIA_HORA_EVOL$False/sum(DIA_HORA_EVOL$False)
DIA_HORA_EVOL$FREQ_REL_2 <- DIA_HORA_EVOL$True/sum(DIA_HORA_EVOL$True)

DIA_HORA_EVOL$TASA_REL_1 <- DIA_HORA_EVOL$False/rowSums(DIA_HORA_EVOL[,1:2])
DIA_HORA_EVOL$TASA_REL_2 <- DIA_HORA_EVOL$True/rowSums(DIA_HORA_EVOL[,1:2])

DIA_HORA_EVOL$Eje_x <- rownames(DIA_HORA_EVOL)

# Defino la forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = c(DIA_HORA_EVOL$Eje_x))

# Armo grafico 1

p <- DIA_HORA_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~False,  type = 'scatter', mode = 'lines', 
            text = DIA_HORA_EVOL$False, textposition = 'auto',
            line = list(color = 'rgb(236,18,171)', width = 1.5),
            name = "False") %>%
  
  add_trace(x = ~Eje_x, y = ~True, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_EVOL$True, textposition = 'auto',
            line = list(color = 'rgb(30,199,242)', width = 1.5),
            name = "True") %>%
  
  layout(title = "Cantidades de installs por dia y horario (cada 12 hs) segun implicit",
         yaxis = list(title = "Cantidad de Installs"),
         xaxis = xform)

p

# Armo grafico 2

p <- DIA_HORA_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_1, type = 'bar', 
            text = paste(round(DIA_HORA_EVOL$TASA_REL_1*100,1),"%"), textposition = 'auto',
            marker = list(color = 'rgb(236,18,171)',
                          line = list(color = 'rgb(236,18,171)', width = 1.5)),
            name = "Implcit = False") %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_2, type = 'bar', 
            text = paste(round(DIA_HORA_EVOL$TASA_REL_2*100,1),"%"), textposition = 'auto',
            marker = list(color = 'rgb(30,199,242)',
                          line = list(color = 'rgb(30,199,242)', width = 1.5)),
            name = "Implicit = True") %>%
  
  layout(title = "Evolucion de la distribucion de installs segun implicit",
         barmode = 'stack',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = xform)

p

#### Distribucion de device_countrycode

frecuencias <- as.data.frame(table(installs$device_countrycode))
frecuencias

p <- plot_ly(
  x = frecuencias$Var1,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun device_countrycode",
         xaxis = list(title = "device_countrycode"),
         yaxis = list(title = "Cantidad de Registros"))
p

#### Evolucion de device_countrycode

HORA_AGR <- trunc(installs$hora/12)*12

Eje_x <- paste(installs$dia,HORA_AGR,"hs")

DIA_HORA_EVOL <- as.data.frame.matrix(table(Eje_x,installs$device_countrycode))

colsplit(rownames(DIA_HORA_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_EVOL <- DIA_HORA_EVOL[order(as.numeric(a$HORA)),]

colsplit(rownames(DIA_HORA_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_EVOL <- DIA_HORA_EVOL[order(as.numeric(a$DIA)),]

DIA_HORA_EVOL$FREQ_REL_1 <- DIA_HORA_EVOL[,1]/sum(DIA_HORA_EVOL[,1])
DIA_HORA_EVOL$FREQ_REL_2 <- DIA_HORA_EVOL[,2]/sum(DIA_HORA_EVOL[,2])

DIA_HORA_EVOL$TASA_REL_1 <- DIA_HORA_EVOL[,1]/rowSums(DIA_HORA_EVOL[,1:2])
DIA_HORA_EVOL$TASA_REL_2 <- DIA_HORA_EVOL[,2]/rowSums(DIA_HORA_EVOL[,1:2])

DIA_HORA_EVOL$Eje_x <- rownames(DIA_HORA_EVOL)

# Defino la forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = c(DIA_HORA_EVOL$Eje_x))

# Armo grafico 1

p <- DIA_HORA_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~`6333597102633388032`,  type = 'scatter', mode = 'lines', 
            text = DIA_HORA_EVOL$`6333597102633388032`, textposition = 'auto',
            line = list(color = 'rgb(236,18,171)', width = 1.5),
            name = "`6333597102633388032`") %>%
  
  add_trace(x = ~Eje_x, y = ~`2970470518450881024`, type = 'scatter', mode = 'lines',  
            text = DIA_HORA_EVOL$`2970470518450881024`, textposition = 'auto',
            line = list(color = 'rgb(30,199,242)', width = 1.5),
            name = "`2970470518450881024`") %>%
  
  layout(title = "Cantidades de installs por dia y horario (cada 12 hs) segun device_countrycode",
         yaxis = list(title = "Cantidad de Installs"),
         xaxis = xform)

p

# Armo grafico 2

p <- DIA_HORA_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_1, type = 'bar', 
            text = paste(round(DIA_HORA_EVOL$TASA_REL_1*100,1),"%"), textposition = 'auto',
            marker = list(color = 'rgb(236,18,171)',
                          line = list(color = 'rgb(236,18,171)', width = 1.5)),
            name = colnames(DIA_HORA_EVOL)[1]) %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_2, type = 'bar', 
            text = paste(round(DIA_HORA_EVOL$TASA_REL_2*100,1),"%"), textposition = 'auto',
            marker = list(color = 'rgb(30,199,242)',
                          line = list(color = 'rgb(30,199,242)', width = 1.5)),
            name = colnames(DIA_HORA_EVOL)[2]) %>%
  
  layout(title = "Evolucion de la distribucion de installs segun device_countrycode",
         barmode = 'stack',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = xform)

p

#### Distribucion de device_brand

# Analisis cantidad de vacios 

frecuencias <- as.data.frame(table(is.na(installs$device_brand)))
colnames(frecuencias)[1] <- "is_NA"
frecuencias
frecuencias$Freq/sum(frecuencias$Freq)

p <- plot_ly(
  x = frecuencias$is_NA,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun is NA device_brand",
         xaxis = list(title = "is NA device_brand"),
         yaxis = list(title = "Cantidad de Registros"))
p


# Frecuencias de no vacios
frecuencias <- as.data.frame(table(installs$device_brand[is.na(installs$device_brand)==FALSE]))
frecuencias<-frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias
frecuencias$Freq/NROW(installs)

binning <- ifelse(installs$device_brand %in% frecuencias$Var1[frecuencias$Freq<50],
                  "Otros",
                  installs$device_brand)

frecuencias <- as.data.frame(table(binning))

frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]

# Defino forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = frecuencias$binning)

# Armo el grafico:
p <- plot_ly(
  x = frecuencias$binning,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun device_brand",
         xaxis = xform,
         yaxis = list(title = "Cantidad de Registros"))
p


# Detalle de otros: 

otros <- as.data.frame(table(installs$device_brand[binning=="Otros"]))
otros <- otros[order(otros$Freq,decreasing = TRUE),]
colnames(otros)[1]<-"device_brand"

formattable(head(otros,10),row.names=FALSE,
            list(Freq = color_bar("#FA614B")))



#### Distribucion de device_model

length(unique(installs$device_model))

TABLA_DISP <- as.data.frame(table(installs$device_model))
summary(TABLA_DISP$Freq)

minimo <- min(TABLA_DISP$Freq)
maximo <- max(TABLA_DISP$Freq)

promedio <- mean(TABLA_DISP$Freq)

desvio <- sd(TABLA_DISP$Freq)
cv <- desvio / mean(TABLA_DISP$Freq)

Perc_25<-quantile(TABLA_DISP$Freq,0.25)
Perc_50<-quantile(TABLA_DISP$Freq,0.5)
Perc_75<-quantile(TABLA_DISP$Freq,0.75)

valores <- c(minimo,
             Perc_25,
             Perc_50,
             promedio,
             Perc_75,
             maximo,
             desvio,
             cv)


nombres <- c("Minimo",
             "Perc_25",
             "Perc_50",
             "Promedio",
             "Perc_75",
             "Maximo",
             "Desvio_Std",
             "Coef_Variac")

# Tabla de metricas:
p <- data.frame(Metrica=nombres,
                Valores=round(valores,2))

formattable(p)

# Histograma: 
p <- plot_ly(x = TABLA_DISP$Freq[TABLA_DISP$Freq], type = "histogram") %>%
  
  layout(title = "Distribucion de cantidad de veces que aparece un device_model",
         xaxis = list(title = "Cantidad de veces que aparece un device_model"),
         yaxis = list(title = "Frecuencia"))
p

# Frecuencias 
frecuencias <- as.data.frame(table(installs$device_model))
frecuencias<-frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

binning <- ifelse(installs$device_model %in% frecuencias$Var1[frecuencias$Freq<100],
                  "Otros",
                  installs$device_model)

frecuencias <- as.data.frame(table(binning))

frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias
frecuencias$Freq/sum(frecuencias$Freq)

# Defino forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = frecuencias$binning)

# Armo el grafico:
p <- plot_ly(
  x = frecuencias$binning,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun device_model",
         xaxis = xform,
         yaxis = list(title = "Cantidad de Registros"))
p

#### Distribucion de session_user_agent

frecuencias <- as.data.frame(table(installs$session_user_agent))
frecuencias<-frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

binning <- ifelse(installs$session_user_agent %in% frecuencias$Var1[frecuencias$Freq<50],
                  "Otros",
                  installs$session_user_agent)

frecuencias <- as.data.frame(table(binning))

frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]

p <- plot_ly(
  x = frecuencias$binning,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun session_user_agent",
         xaxis = list(title = "session_user_agent"),
         yaxis = list(title = "Cantidad de Registros"))
p

# TOP 10 de lo incluido en otros:

otros <- as.data.frame(table(installs$session_user_agent[binning=="Otros"]))
otros <- otros[order(otros$Freq,decreasing = TRUE),]
colnames(otros)[1]<-"session_user_agent"

formattable(head(otros,10),row.names=FALSE,
            list(Freq = color_bar("#FA614B"))) 

#### Distribucion de user_agent

frecuencias <- as.data.frame(table(installs$user_agent))
frecuencias<-frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

binning <- ifelse(installs$user_agent %in% frecuencias$Var1[frecuencias$Freq<50],
                  "Otros",
                  installs$user_agent)

frecuencias <- as.data.frame(table(binning))

frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]

frecuencias$Freq/sum(frecuencias$Freq)

p <- plot_ly(
  x = frecuencias$binning,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun user_agent",
         xaxis = list(title = "user_agent"),
         yaxis = list(title = "Cantidad de Registros"))
p

#### Combinando informacion de user agents
frecuencias <- as.data.frame(table(installs$user_agent))
binning_user_agent <- ifelse(installs$user_agent %in% frecuencias$Var1[frecuencias$Freq<50],
                             "Otros",
                             installs$user_agent)

frecuencias <- as.data.frame(table(installs$session_user_agent))
binning_session_user_agent <- ifelse(installs$session_user_agent %in% frecuencias$Var1[frecuencias$Freq<50],
                                     "Otros",
                                     installs$session_user_agent)


frecuencias <- as.data.frame.matrix(table(binning_user_agent,binning_session_user_agent))

colnames(frecuencias)<-paste("session_user_agent",colnames(frecuencias))
frecuencias

rownames(frecuencias)<-paste("user_agent",rownames(frecuencias))
frecuencias

formattable(frecuencias,
            list(area(col = colnames(frecuencias)) ~ color_tile("transparent", "lightblue")))


#### Distribucion de event_uuid

frecuencias <- as.data.frame(table(installs$event_uuid))
frecuencias<-frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

binning <- ifelse(installs$event_uuid %in% frecuencias$Var1[frecuencias$Freq<2],
                  "Otros",
                  installs$event_uuid)

frecuencias <- as.data.frame(table(binning))
frecuencias

frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]

frecuencias$Freq/sum(frecuencias$Freq)

p <- plot_ly(
  x = frecuencias$binning,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun event_uuid",
         xaxis = list(title = "event_uuid"),
         yaxis = list(title = "Cantidad de Registros"))
p

# Distribucion Otros

otros <- as.data.frame(table(installs$event_uuid[binning=="Otros"]))
otros <- otros[order(otros$Freq,decreasing = TRUE),]

formattable(head(otros))

#### Distribucion de kind

frecuencias <- as.data.frame(table(installs$kind))
frecuencias<-frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

binning <- ifelse(as.character(installs$kind) %in% frecuencias$Var1[frecuencias$Freq<10],
                  "Otros",
                  as.character(installs$kind))

frecuencias <- as.data.frame(table(binning))
frecuencias

frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]

# Defino la forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = frecuencias$binning)

p <- plot_ly(
  x = frecuencias$binning,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun kind",
         xaxis = xform,
         yaxis = list(title = "Cantidad de Registros"))
p


# Armo tabla de frecuencias relativas

frecuencias$Freq_Rel <- round(frecuencias$Freq/sum(frecuencias$Freq),2)

formattable(frecuencias,row.names=FALSE,
            list(Freq_Rel = color_bar("#FA614B")))

#### Distribucion de wifi

frecuencias <- as.data.frame(table(as.character(installs$wifi)))
frecuencias<-frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

p <- plot_ly(
  x = frecuencias$Var1,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun wifi",
         xaxis = list(title = "wifi"),
         yaxis = list(title = "Cantidad de Registros"))
p


#### Distribucion de trans_id

frecuencias <- as.data.frame(table(as.character(installs$trans_id)))
frecuencias<-frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

frecuencias$Freq/sum(frecuencias$Freq)

p <- plot_ly(
  x = frecuencias$Var1,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun trans_id",
         xaxis = list(title = "trans_id"),
         yaxis = list(title = "Cantidad de Registros"))
p



#### Distribucion de device_language

frecuencias <- as.data.frame(table(installs$device_language))
frecuencias<-frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

binning <- ifelse(as.character(installs$device_language) %in% frecuencias$Var1[frecuencias$Freq<100],
                  "Otros",
                  as.character(installs$device_language))

frecuencias <- as.data.frame(table(binning))
frecuencias

frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias$Freq/sum(frecuencias$Freq)


# Forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = frecuencias$binning)

# Grafico
p <- plot_ly(
  x = frecuencias$binning,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Installs segun device_language",
         xaxis = xform,
         yaxis = list(title = "Cantidad de Registros"))
p

# Otros: 

otros <- as.data.frame(table(installs$device_language[binning=="Otros"]))
otros <- otros[order(otros$Freq,decreasing = TRUE),]
colnames(otros)[1]<-"device_language"

formattable(head(otros,10),row.names=FALSE,
            list(Freq = color_bar("#FA614B"))) 

#### Distribucion de ip_adress

length(unique(installs$ip_address))

TABLA_DISP <- as.data.frame(table(installs$ip_address))
summary(TABLA_DISP$Freq)

minimo <- min(TABLA_DISP$Freq)
maximo <- max(TABLA_DISP$Freq)

promedio <- mean(TABLA_DISP$Freq)

desvio <- sd(TABLA_DISP$Freq)
cv <- desvio / mean(TABLA_DISP$Freq)

Perc_25<-quantile(TABLA_DISP$Freq,0.25)
Perc_50<-quantile(TABLA_DISP$Freq,0.5)
Perc_75<-quantile(TABLA_DISP$Freq,0.75)

valores <- c(minimo,
             Perc_25,
             Perc_50,
             promedio,
             Perc_75,
             maximo,
             desvio,
             cv)


nombres <- c("Minimo",
             "Perc_25",
             "Perc_50",
             "Promedio",
             "Perc_75",
             "Maximo",
             "Desvio_Std",
             "Coef_Variac")

# Tabla de metricas:
p <- data.frame(Metrica=nombres,
                Valores=round(valores,2))

formattable(p)

# Histograma: 
p <- plot_ly(x = TABLA_DISP$Freq, type = "histogram") %>%
  
  layout(title = "Distribucion de cantidad de veces que aparece un ip_address",
         xaxis = list(title = "Cantidad de veces que aparece un ip_address"),
         yaxis = list(title = "Frecuencia"))
p

#### Explorando Events ####

# Hago la marca de eventos que terminaron en instalaciones.
# Primero armo una tabla con las fechas de instalacion por dispositivo 

resumen_installs <- aggregate(installs$created,
                              by=list(installs$ref_hash),
                              paste, collapse = ",")


colsplit(resumen_installs$x,pattern = ",",names = c("FEC_1","FEC_2","FEC_3","FEC_4"))->a

cbind(resumen_installs,a)->resumen_installs

# Marco si event esta en installs 

En_Installs <- ifelse(events$ref_hash %in% resumen_installs$Group.1,1,0)
table(En_Installs)

FEC_1_POST <- ifelse(resumen_installs$FEC_1[match(events$ref_hash,
                                                  resumen_installs$Group.1)]>as.character(events$date),1,0)

table(FEC_1_POST)

FEC_2_POST <- ifelse(resumen_installs$FEC_2[match(events$ref_hash,
                                                  resumen_installs$Group.1)]>as.character(events$date),1,0)

table(FEC_2_POST)

FEC_3_POST <- ifelse(resumen_installs$FEC_3[match(events$ref_hash,
                                                  resumen_installs$Group.1)]>as.character(events$date),1,0)

table(FEC_3_POST)

FEC_4_POST <- ifelse(resumen_installs$FEC_4[match(events$ref_hash,
                                                  resumen_installs$Group.1)]>as.character(events$date),1,0)

table(FEC_4_POST)

# Armo la marca final:

events$Install_Post <- ifelse(En_Installs==1,
                              ifelse(FEC_1_POST+FEC_2_POST+FEC_3_POST+FEC_4_POST>0,1,0),0)
table(events$Install_Post)


#### Conteos iniciales

conteo_events <- as.data.frame.matrix(table(events$attributed,events$Install_Post))
conteo_events

rownames(conteo_events)<- paste("Attributed",rownames(conteo_events))
colnames(conteo_events)<- paste("Install_Post",colnames(conteo_events))

conteo_events

formattable(conteo_events)

conteo_events_rel <- conteo_events/sum(conteo_events)
conteo_events_rel
formattable(conteo_events_rel)

# Tasas 

tasa_attributed <- sum(conteo_events[2,])/sum(conteo_events)
tasa_install_post <- sum(conteo_events[,2])/sum(conteo_events)

tasas <- data.frame(nombre=c("tasa_attributed","tasa_install_post"),
                    tasas=c(tasa_attributed,tasa_install_post))
tasas

#### Tasas

p <- plot_ly(
  x = tasas$nombre,
  y = tasas$tasas,
  type = "bar",
  marker = list(color = c('rgb(50,205,50)','rgb(255,215,0)'),
                line = list(color = '	(47,79,79)',
                            width = 1.5)))  %>%
  layout(title = "Tasas de attributed e installs",
         xaxis = list(title = "attributed"),
         yaxis = list(title = "Tasas"))
p



#### Evolucion de tasa attributed

HORA_AGR <- trunc(events$hora/4)*4

Eje_x <- paste(events$dia,HORA_AGR,"hs")

DIA_HORA_EVOL <- as.data.frame.matrix(table(Eje_x,events$attributed))

colsplit(rownames(DIA_HORA_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_EVOL <- DIA_HORA_EVOL[order(as.numeric(a$HORA)),]

colsplit(rownames(DIA_HORA_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_EVOL <- DIA_HORA_EVOL[order(as.numeric(a$DIA)),]

DIA_HORA_EVOL$FREQ_REL_1 <- DIA_HORA_EVOL$False/sum(DIA_HORA_EVOL$False)
DIA_HORA_EVOL$FREQ_REL_2 <- DIA_HORA_EVOL$True/sum(DIA_HORA_EVOL$True)

DIA_HORA_EVOL$TASA_REL_1 <- DIA_HORA_EVOL$False/rowSums(DIA_HORA_EVOL[,1:2])
DIA_HORA_EVOL$TASA_REL_2 <- DIA_HORA_EVOL$True/rowSums(DIA_HORA_EVOL[,1:2])

DIA_HORA_EVOL$Eje_x <- rownames(DIA_HORA_EVOL)

# Defino la forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = c(DIA_HORA_EVOL$Eje_x))

# Armo grafico 2

p <- DIA_HORA_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_2, type = 'bar', 
            text = paste(round(DIA_HORA_EVOL$TASA_REL_2*100,1),"%"), textposition = 'auto',
            marker = list(color = 'rgb(50,205,50)',
                          line = list(width = 1.5)),
            name = "attributed = True") %>%
  
  layout(title = "Evolucion de la tasa de attributed",
         barmode = 'stack',
         yaxis = list(title = "Tasa de attributed"),
         xaxis = xform)

p


#### Evolucion de tasa installs post

HORA_AGR <- trunc(events$hora/4)*4

Eje_x <- paste(events$dia,HORA_AGR,"hs")

DIA_HORA_EVOL <- as.data.frame.matrix(table(Eje_x,events$Install_Post))

colsplit(rownames(DIA_HORA_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_EVOL <- DIA_HORA_EVOL[order(as.numeric(a$HORA)),]

colsplit(rownames(DIA_HORA_EVOL),pattern = " ",names = c("DIA","HORA","HS"))->a

DIA_HORA_EVOL <- DIA_HORA_EVOL[order(as.numeric(a$DIA)),]

DIA_HORA_EVOL$FREQ_REL_1 <- DIA_HORA_EVOL$`0`/sum(DIA_HORA_EVOL$`0`)
DIA_HORA_EVOL$FREQ_REL_2 <- DIA_HORA_EVOL$`1`/sum(DIA_HORA_EVOL$`1`)

DIA_HORA_EVOL$TASA_REL_1 <- DIA_HORA_EVOL$`0`/rowSums(DIA_HORA_EVOL[,1:2])
DIA_HORA_EVOL$TASA_REL_2 <- DIA_HORA_EVOL$`1`/rowSums(DIA_HORA_EVOL[,1:2])

DIA_HORA_EVOL$Eje_x <- rownames(DIA_HORA_EVOL)

# Defino la forma del eje x:
xform <- list(categoryorder = "array",
              categoryarray = c(DIA_HORA_EVOL$Eje_x))

# Armo grafico 2

p <- DIA_HORA_EVOL %>% 
  plot_ly() %>%
  
  add_trace(x = ~Eje_x, y = ~TASA_REL_2, type = 'bar', 
            text = paste(round(DIA_HORA_EVOL$TASA_REL_2*100,1),"%"), textposition = 'auto',
            marker = list(color = 'rgb(255,215,0)',
                          line = list(width = 1.5)),
            name = "attributed = True") %>%
  
  layout(title = "Evolucion de la tasa de installs posteriores",
         barmode = 'stack',
         yaxis = list(title = "Tasa de installs"),
         xaxis = xform)

p


#### Distribucion de ref_type

frecuencias <- as.data.frame(table(events$ref_type))
frecuencias_alt <- as.data.frame.matrix(table(events$ref_type,events$attributed))
frecuencias$Freq_attributed <- frecuencias_alt$True
frecuencias_alt <- as.data.frame.matrix(table(events$ref_type,events$Install_Post))
frecuencias$Freq_Installs_Post <- frecuencias_alt$`1`

frecuencias$Freq_Rel <- frecuencias$Freq/sum(frecuencias$Freq)
frecuencias$Freq_attributed_Rel <- frecuencias$Freq_attributed/sum(frecuencias$Freq_attributed)
frecuencias$Freq_Installs_Post_Rel <- frecuencias$Freq_Installs_Post/sum(frecuencias$Freq_Installs_Post)

frecuencias

p <- frecuencias %>% 
  plot_ly() %>%
  
  add_trace(x = ~Var1, y = ~Freq_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_Rel*100,1),"%"), 
            marker = list(color = '(32,178,170)',
                          line = list(width = 1.5)),
            name = "Total Events") %>%
  
  add_trace(x = ~Var1, y = ~Freq_attributed_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_attributed_Rel*100,1),"%"), 
            marker = list(color = 'rgb(50,205,50)',
                          line = list(width = 1.5)),
            name = "Attributed Events") %>%
  
  add_trace(x = ~Var1, y = ~Freq_Installs_Post_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_Installs_Post_Rel*100,1),"%"), 
            marker = list(color = 'rgb(255,215,0)',
                          line = list(width = 1.5)),
            name = "Installs Post Events") %>%
  
  layout(title = "Distribuciones de events seg?n ref_type",
         barmode = 'group',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = list(title = "ref_type"))

p

# Tasas
frecuencias$tasa_att <- frecuencias$Freq_attributed/frecuencias$Freq
frecuencias$tasa_inst <- frecuencias$Freq_Installs_Post/frecuencias$Freq

p <- frecuencias %>% 
  plot_ly() %>%
  
  add_trace(x = ~Var1, y = ~tasa_att, type = 'bar', 
            text = paste(round(frecuencias$tasa_att*100,1),"%"), 
            marker = list(color = 'rgb(50,205,50)',
                          line = list(width = 1.5)),
            name = "Tasa Attributed Events") %>%
  
  add_trace(x = ~Var1, y = ~tasa_inst, type = 'bar', 
            text = paste(round(frecuencias$tasa_inst*100,1),"%"), 
            marker = list(color = 'rgb(255,215,0)',
                          line = list(width = 1.5)),
            name = "Tasa Installs Post Events") %>%
  
  layout(title = "Tasas de attributed e installs, seg?n ref_type",
         barmode = 'group',
         yaxis = list(title = "Tasas"),
         xaxis = list(title = "ref_type"))

p



#### Distribucion de device_os

frecuencias <- as.data.frame(table(events$device_os))
frecuencias_alt <- as.data.frame.matrix(table(events$device_os,events$attributed))
frecuencias$Freq_attributed <- frecuencias_alt$True
frecuencias_alt <- as.data.frame.matrix(table(events$device_os,events$Install_Post))
frecuencias$Freq_Installs_Post <- frecuencias_alt$`1`

frecuencias$Freq_Rel <- frecuencias$Freq/sum(frecuencias$Freq)
frecuencias$Freq_attributed_Rel <- frecuencias$Freq_attributed/sum(frecuencias$Freq_attributed)
frecuencias$Freq_Installs_Post_Rel <- frecuencias$Freq_Installs_Post/sum(frecuencias$Freq_Installs_Post)

frecuencias

p <- frecuencias %>% 
  plot_ly() %>%
  
  add_trace(x = ~Var1, y = ~Freq_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_Rel*100,1),"%"), 
            marker = list(color = '(32,178,170)',
                          line = list(width = 1.5)),
            name = "Total Events") %>%
  
  add_trace(x = ~Var1, y = ~Freq_attributed_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_attributed_Rel*100,1),"%"), 
            marker = list(color = 'rgb(50,205,50)',
                          line = list(width = 1.5)),
            name = "Attributed Events") %>%
  
  add_trace(x = ~Var1, y = ~Freq_Installs_Post_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_Installs_Post_Rel*100,1),"%"), 
            marker = list(color = 'rgb(255,215,0)',
                          line = list(width = 1.5)),
            name = "Installs Post Events") %>%
  
  layout(title = "Distribuciones de events seg?n device_os",
         barmode = 'group',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = list(title = "device_os"))

p

# Tasas
frecuencias$tasa_att <- frecuencias$Freq_attributed/frecuencias$Freq
frecuencias$tasa_inst <- frecuencias$Freq_Installs_Post/frecuencias$Freq

frecuencias <- frecuencias[3:4,]

p <- frecuencias %>% 
  plot_ly() %>%
  
  add_trace(x = ~Var1, y = ~tasa_att, type = 'bar', 
            text = paste(round(frecuencias$tasa_att*100,1),"%"), 
            marker = list(color = 'rgb(50,205,50)',
                          line = list(width = 1.5)),
            name = "Tasa Attributed Events") %>%
  
  add_trace(x = ~Var1, y = ~tasa_inst, type = 'bar', 
            text = paste(round(frecuencias$tasa_inst*100,1),"%"), 
            marker = list(color = 'rgb(255,215,0)',
                          line = list(width = 1.5)),
            name = "Tasa Installs Post Events") %>%
  
  layout(title = "Tasas de attributed e installs, seg?n device_os",
         barmode = 'group',
         yaxis = list(title = "Tasas"),
         xaxis = list(title = "device_os"))

p


#### WIFI + CONNECTION_TYPE

conteos <- as.data.frame.matrix(table(events$wifi,events$connection_type))
colnames(conteos) <- paste("Connect_Type:",colnames(conteos))
rownames(conteos) <- paste("wifi:",rownames(conteos))

conteos

formattable(conteos,
            list(area(col = colnames(conteos)[1:ncol(conteos)]) ~ color_tile("transparent", "lightblue")))

# Creo nueva variable: 

events$WIFI_CONN_TYPE <- ifelse(as.character(events$wifi)=="",
                                as.character(events$connection_type),
                                paste("Wifi",as.character(events$wifi)))
table(events$WIFI_CONN_TYPE)

# Hago el analisis 

frecuencias <- as.data.frame(table(events$WIFI_CONN_TYPE))
frecuencias_alt <- as.data.frame.matrix(table(events$WIFI_CONN_TYPE,events$attributed))
frecuencias$Freq_attributed <- frecuencias_alt$True
frecuencias_alt <- as.data.frame.matrix(table(events$WIFI_CONN_TYPE,events$Install_Post))
frecuencias$Freq_Installs_Post <- frecuencias_alt$`1`

frecuencias$Freq_Rel <- frecuencias$Freq/sum(frecuencias$Freq)
frecuencias$Freq_attributed_Rel <- frecuencias$Freq_attributed/sum(frecuencias$Freq_attributed)

frecuencias$Freq_Installs_Post_Rel <- frecuencias$Freq_Installs_Post/sum(frecuencias$Freq_Installs_Post)

frecuencias

p <- frecuencias %>% 
  plot_ly() %>%
  
  add_trace(x = ~Var1, y = ~Freq_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_Rel*100,1),"%"), 
            marker = list(color = '(32,178,170)',
                          line = list(width = 1.5)),
            name = "Total Events") %>%
  
  add_trace(x = ~Var1, y = ~Freq_attributed_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_attributed_Rel*100,1),"%"), 
            marker = list(color = 'rgb(50,205,50)',
                          line = list(width = 1.5)),
            name = "Attributed Events") %>%
  
  add_trace(x = ~Var1, y = ~Freq_Installs_Post_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_Installs_Post_Rel*100,1),"%"), 
            marker = list(color = 'rgb(255,215,0)',
                          line = list(width = 1.5)),
            name = "Installs Post Events") %>%
  
  layout(title = "Distribuciones de events seg?n WIFI_CONN_TYPE",
         barmode = 'group',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = list(title = "WIFI_CONN_TYPE"))

p

# Tasas
frecuencias$tasa_att <- frecuencias$Freq_attributed/frecuencias$Freq
frecuencias$tasa_inst <- frecuencias$Freq_Installs_Post/frecuencias$Freq

frecuencias <- frecuencias[c(1:3,5:6),]

p <- frecuencias %>% 
  plot_ly() %>%
  
  add_trace(x = ~Var1, y = ~tasa_att, type = 'bar', 
            text = paste(round(frecuencias$tasa_att*100,1),"%"), 
            marker = list(color = 'rgb(50,205,50)',
                          line = list(width = 1.5)),
            name = "Tasa Attributed Events") %>%
  
  add_trace(x = ~Var1, y = ~tasa_inst, type = 'bar', 
            text = paste(round(frecuencias$tasa_inst*100,1),"%"), 
            marker = list(color = 'rgb(255,215,0)',
                          line = list(width = 1.5)),
            name = "Tasa Installs Post Events") %>%
  
  layout(title = "Tasas de attributed e installs, seg?n WIFI_CONN_TYPE",
         barmode = 'group',
         yaxis = list(title = "Tasas"),
         xaxis = list(title = "WIFI_CONN_TYPE"))

p


# Event_id

# Me quedo con los 12 events_id mas frecuentes
frecuencias <- as.data.frame(table(events$event_id))

binning <- ifelse(!events$event_id %in% frecuencias$Var1[1:12],
                  "Otros",
                  events$event_id)

frecuencias <- as.data.frame(table(binning))
frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

frecuencias_alt <- as.data.frame.matrix(table(binning,events$attributed))
frecuencias$Freq_attributed <- frecuencias_alt$True
frecuencias_alt <- as.data.frame.matrix(table(binning,events$Install_Post))
frecuencias$Freq_Installs_Post <- frecuencias_alt$`1`

frecuencias$Freq_Rel <- frecuencias$Freq/sum(frecuencias$Freq)
frecuencias$Freq_attributed_Rel <- frecuencias$Freq_attributed/sum(frecuencias$Freq_attributed)
frecuencias$Freq_Installs_Post_Rel <- frecuencias$Freq_Installs_Post/sum(frecuencias$Freq_Installs_Post)

frecuencias$tasa_att <- frecuencias$Freq_attributed/frecuencias$Freq
frecuencias$tasa_inst <- frecuencias$Freq_Installs_Post/frecuencias$Freq

colnames(frecuencias)[1]<-"event_id"

sum(frecuencias$Freq[frecuencias$event_id != "Otros"])/sum(frecuencias$Freq)

formattable(frecuencias[,1:4],row.names=FALSE,
            list( Freq = color_bar('rgb(32,178,170)'),
                  Freq_attributed = color_bar('rgb(50,205,50)'),
                  Freq_Installs_Post = color_bar("#FFD700"))) 

# Distribuciones 

p <- frecuencias %>% 
  plot_ly() %>%
  
  add_trace(x = ~event_id, y = ~Freq_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_Rel*100,1),"%"), 
            marker = list(color = 'rgb(32,178,170)',
                          line = list(width = 1.5)),
            name = "Total Events") %>%
  
  add_trace(x = ~event_id, y = ~Freq_attributed_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_attributed_Rel*100,1),"%"), 
            marker = list(color = 'rgb(50,205,50)',
                          line = list(width = 1.5)),
            name = "Attributed Events") %>%
  
  add_trace(x = ~event_id, y = ~Freq_Installs_Post_Rel, type = 'bar', 
            text = paste(round(frecuencias$Freq_Installs_Post_Rel*100,1),"%"), 
            marker = list(color = 'rgb(255,215,0)',
                          line = list(width = 1.5)),
            name = "Installs Post Events") %>%
  
  layout(title = "Distribuciones de events seg?n event_id",
         barmode = 'group',
         yaxis = list(title = "Frecuencia Relativa"),
         xaxis = list(title = "event_id"))

p


# Comparando aplication_id en events e installs 

frecuencias <- as.data.frame(table(events$application_id))
frecuencias$Freq_Rel <- frecuencias$Freq/sum(frecuencias$Freq)

frecuencias_alt <- as.data.frame(table(installs$application_id))

frecuencias$Freq_Installs <- frecuencias_alt$Freq[match(frecuencias$Var1,frecuencias_alt$Var1)]

head(frecuencias)

# Filtro por las que matchean 

frecuencias[is.na(frecuencias$Freq_Installs)==FALSE,]->frecuencias

frecuencias$Freq_Installs_Rel <- frecuencias$Freq_Installs/nrow(installs)

colnames(frecuencias)[1]<-"application_id"
frecuencias$application_id <- as.character(frecuencias$application_id)

otros <- c("Otros",
           nrow(events)-sum(frecuencias$Freq),
           1-sum(frecuencias$Freq_Rel),
           nrow(installs)-sum(frecuencias$Freq_Installs),
           1-sum(frecuencias$Freq_Installs_Rel))

frecuencias<-rbind(frecuencias,otros)

frecuencias[order(as.numeric(frecuencias$Freq),decreasing = TRUE),]->frecuencias

formattable(frecuencias,row.names=FALSE,
            list( Freq = color_bar('rgb(50,205,50)'),
                  Freq_Rel = color_bar('rgb(50,205,50)'),
                  Freq_Installs = color_bar("#FFD700"),
                  Freq_Installs_Rel = color_bar("#FFD700"))) 


#### Explorando Clicks ####

#### Distribucion de advertiser_id

frecuencias <- as.data.frame(table(clicks$advertiser_id))
frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

# Defino la forma de las x
xform <- list(categoryorder = "array",
              categoryarray = frecuencias$Var1)

# Armo el grafico
p <- plot_ly(
  x = frecuencias$Var1,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de Clicks segun Advertiser_id",
         xaxis = xform,
         yaxis = list(title = "Cantidad de Registros"))
p

#### Distribucion de source_id

frecuencias <- as.data.frame(table(clicks$source_id))
frecuencias

p <- plot_ly(
  x = frecuencias$Var1,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribuci?n de clicks segun source_id",
         xaxis = list(title = "source_id"),
         yaxis = list(title = "Cantidad de Registros"))
p

sum(frecuencias$Freq[frecuencias$Var1%in% c(0,1,6,5)]) / sum(frecuencias$Freq)


# Latitud y longitud

p <- plot_ly(
  x = clicks$latitude,
  y = clicks$longitude,
  type = "scatter",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Posicion del Click",
         xaxis = list(title = "Latitud"),
         yaxis = list(title = "Longitud"))
p

table(clicks$longitude<=1.075 & clicks$latitude<=1.21)/nrow(clicks)


# Distribucion os_minor 

frecuencias <- as.data.frame(table(clicks$os_minor))
frecuencias<-frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias

binning <- ifelse(clicks$os_minor %in% frecuencias$Var1[frecuencias$Freq<100],
                  "Otros",
                  clicks$os_minor)

frecuencias <- as.data.frame(table(binning))

frecuencias <- frecuencias[order(frecuencias$Freq,decreasing = TRUE),]

p <- plot_ly(
  x = frecuencias$binning,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de clicks segun os_minor",
         xaxis = list(title = "os_minor"),
         yaxis = list(title = "Cantidad de Registros"))
p

#### Distribucion de os_major ####

frecuencias <- as.data.frame(table(clicks$os_major))
frecuencias<-frecuencias[order(frecuencias$Freq,decreasing = TRUE),]
frecuencias


p <- plot_ly(
  x = frecuencias$Var1,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de clicks segun os_major",
         xaxis = list(title = "os_major"),
         yaxis = list(title = "Cantidad de Registros"))
p


#### TOUCH X VS Y ####
p <- plot_ly(
  x = clicks$touchX,
  y = clicks$touchY,
  type = "scatter",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Posicion del Click",
         xaxis = list(title = "touchX"),
         yaxis = list(title = "TouchY"))
p

# Distribucion ref_type

frecuencias <- as.data.frame(table(clicks$ref_type))
frecuencias

p <- plot_ly(
  x = frecuencias$Var1,
  y = frecuencias$Freq,
  type = "bar",
  marker = list(color = 'rgb(158,202,225)',
                line = list(color = 'rgb(8,48,107)',
                            width = 1.5)))  %>%
  layout(title = "Distribucion de clicks segun ref_type",
         xaxis = list(title = "ref_type"),
         yaxis = list(title = "Cantidad de Registros"))
p
