# Librerías
library("tidyverse")
library("zoo")    
library("ggplot2")
library("plotly")

library("cffdrs") # El paquete para calcular fwi

# CARGAR DATOS .XLSX O .CSV -------------------------------------------------------
raw_data_ing <- read.csv("./Data_Input/Inibioma_Manso_Inferior_31-1-2025.csv", skip = 1)
raw_data_esp <- read.csv("./Data_Input/Inibioma_Manso_Inferior_1-2-2025.csv", skip = 1)

# Reemplazar nombres de columnas en raw_data_ing con los de raw_data_esp
names(raw_data_ing) <- names(raw_data_esp)
raw_data <- rbind(raw_data_ing, raw_data_esp)
rm(raw_data_ing, raw_data_esp)

# Seleccionar columnas de interés 
datos <- raw_data %>% select(Fecha.Tiempo..GMT.03.00, 
                             Temp...C..LGR.S.N..20210973..SEN.S.N..20215229., 
                             HR.....LGR.S.N..20210973..SEN.S.N..20215229., 
                             Velocidad.del.viento..m.s..LGR.S.N..20210973..SEN.S.N..20203022.,
                             Lluvia..mm..LGR.S.N..20210973..SEN.S.N..20219452.,
                             Velocidad.de.Ráfagas..m.s..LGR.S.N..20210973..SEN.S.N..20203022.,
                             Dirección.del.viento..ø..LGR.S.N..20210973..SEN.S.N..20203073.)


# PREPARAR COLUMNAS PARA FUNCIÓN FWI ---------------------------------------------
# Wind Speed de m/s a Km/h
datos$WindSpeed <- datos$Velocidad.del.viento..m.s..LGR.S.N..20210973..SEN.S.N..20203022. * 3.6
datos$GustSpeed <- datos$Velocidad.de.Ráfagas..m.s..LGR.S.N..20210973..SEN.S.N..20203022. * 3.6

# Columna Datetime
datos$Datetime <- as.POSIXct(datos$Fecha.Tiempo..GMT.03.00, 
                             format = "%m/%d/%y %I:%M:%S %p", tz = "UTC")
# Columna yr, mon y day
datos$yr <- as.numeric(format(datos$Datetime, "%Y"))  # Año
datos$mon <- as.numeric(format(datos$Datetime, "%m")) # Mes
datos$day <- as.numeric(format(datos$Datetime, "%d")) # Día

# Agrega columna LAT LONG
datos$lat <- "-41 36 16"
datos$long <- "-71 32 28.5"

# Calcular rain24hs a partir de rain
datos$rain24hs <- round(rollsumr(datos$Lluvia..mm..LGR.S.N..20210973..SEN.S.N..20219452., k = 23, fill = 0),3)

# Datos finales
datos_fwi <- datos %>% 
  select(lat, long, yr, mon, day,
                              Temp...C..LGR.S.N..20210973..SEN.S.N..20215229., 
                              HR.....LGR.S.N..20210973..SEN.S.N..20215229.,
         WindSpeed, rain24hs) %>%
  rename(temp = Temp...C..LGR.S.N..20210973..SEN.S.N..20215229., 
         rh = HR.....LGR.S.N..20210973..SEN.S.N..20215229., 
         prec = rain24hs,
         ws = WindSpeed)

rm(raw_data)

# Elimina el NA del primer dato de rain24hs y lo convierte a cero
datos_fwi <- datos_fwi %>%
  mutate_at(vars(prec), ~ ifelse(is.na(.), 0, .))

# Ponerle 0.1 a los valores 0 de WindSpeed para que no calcule NAN FWI
datos_fwi <- datos_fwi %>%
  mutate(ws = ifelse(ws == 0, 0.1, ws))

# CALCULO FWI CON cffdrs -------------------------------------------------
## (1) FWI System variables for a single weather station:
# Using the default initial values and batch argument,
# the function calculate FWI variables chronically:

fwi <- fwi(datos_fwi, init = c(ffmc = 85, dmc = 6, dc = 15, lat = -41), batch = TRUE)
fwi <- fwi %>% 
  mutate_if(is.numeric, round, digits=3)


## Guardar .CSV de FWI con fecha
# Adecuar el dataframe
fwi$Datetime <- datos$Datetime
fwi$DIA <- as.numeric(format(fwi$Datetime, "%d"))
fwi$MES <- as.numeric(format(fwi$Datetime, "%m"))
fwi$HORA <- as.numeric(format(fwi$Datetime, "%H"))

fwi <- fwi %>% select(LAT, LONG, Datetime, DIA, MES, HORA,
                      TEMP, RH, WS, PREC,
                      FFMC, DMC, DC, ISI, BUI, FWI, DSR)

timestamp <- format(Sys.time(), "%Y-%m-%d_%H")
timestamp <- paste0(timestamp, "hs")
file_name <- paste0("FWI_INIBIOMA_", timestamp, ".csv")

fwi$WindDirection <- datos$Dirección.del.viento..ø..LGR.S.N..20210973..SEN.S.N..20203073.
fwi$GustSpeed <- datos$GustSpeed

write_csv2(fwi, paste0("./Output/", file_name))

rm(file_name, timestamp, datos)


# ANALISIS ------------------------------------------------------------------
# Seleccionar un rango de fechas de interés
#desde <- as.POSIXct("2024-12-01 00:00:00")
#hasta <- as.POSIXct("2025-01-03 23:59:59")

# Toma las últimas 4 semanas
hasta <- now()
desde <- now() - ddays(28) 

fwi_filter <- fwi %>%
  filter(Datetime >= desde, Datetime <= hasta)


### GRAFICO TEMP y FWI vs Fecha -------------------------------------------------------
g1 <- ggplot(fwi_filter, aes(x = Datetime)) +
  geom_area(aes(y = TEMP, fill = "Temperatura"), stat = "identity") +
  geom_line(aes(y = FWI, color = "FWI"), size = 0.5) +
  
  scale_y_continuous(name = "FWI / °C") +
  scale_x_datetime(name = "", date_labels = "%d/%m/%y", date_breaks = "1 days") +
  
  scale_fill_manual(values = c("Temperatura" = "peachpuff"), name = "") +
  scale_color_manual(values = c("FWI" = "red"), name = "") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top" 
  ) +
  ggtitle("FWI / Temperatura (Cº)")

ggplotly(g1)


### GRAFICO  PRECIP y FWI vs Fecha --------------------------------------------------
g2 <- ggplot(fwi_filter, aes(x = Datetime)) +
  geom_area(aes(y = PREC, fill = "Precipitaciones últimas 24hs"), stat = "identity") +
  geom_line(aes(y = FWI, color = "FWI"), size = 0.5) +
  
  scale_y_continuous(name = "FWI / mm") +
  scale_x_datetime(name = "", date_labels = "%d/%m/%y", date_breaks = "1 days") +
  
  scale_fill_manual(values = c("Precipitaciones últimas 24hs" = "#7AC5CD"), name = "") +
  scale_color_manual(values = c("FWI" = "red"), name = "") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top" 
  ) +
  ggtitle("FWI / Precip 24hs")

ggplotly(g2)

### GRAFICO  WindSpeed y FWI vs Fecha ----------------------------------------------
g3 <- ggplot(fwi_filter, aes(x = Datetime)) +
  geom_area(aes(y = WS, fill = "Velocidad del viento (Km/h)"), stat = "identity") +
  geom_line(aes(y = FWI, color = "FWI"), size = 0.5) +
  
  scale_y_continuous(name = "FWI / Km/h") +
  scale_x_datetime(name = "", date_labels = "%d/%m/%y", date_breaks = "1 days") +
  
  scale_fill_manual(values = c("Velocidad del viento (Km/h)" = "grey"), name = "") +
  scale_color_manual(values = c("FWI" = "red"), name = "") +
  
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top" 
  ) +
  ggtitle("FWI / Velocidad del viento")

ggplotly(g3)



### GRÁFICO CATEGORÍAS DE FWI -------------------------------------------
# Calcular los percentiles (solo temporada de incendios)
fwi_desde <- as.POSIXct("2024-11-15 00:00:00")
fwi_hasta <- as.POSIXct("2025-03-01 23:59:59")

fwi_filter_percentiles <- fwi %>%
  filter(Datetime >= fwi_desde, Datetime <= fwi_hasta)  

percentiles <- quantile(fwi_filter_percentiles$FWI, probs = c(0, 0.25, 0.5, 0.75, 0.9, 1))

# Crear el histograma
hist(fwi_filter_percentiles$FWI, 
     main = "Percentiles para cortes de clase FWI\n (solo considerando temporada incendios)", 
     xlab = "FWI", ylab = "Frecuencia", 
     col = "skyblue", border = "black", 
     breaks = 30)

# Agregar líneas de percentiles
abline(v = percentiles[2], col = "red", lty = 2, lwd = 2)  # Percentil 25
abline(v = percentiles[3], col = "blue", lty = 2, lwd = 2) # Percentil 50
abline(v = percentiles[4], col = "green", lty = 2, lwd = 2) # Percentil 75
abline(v = percentiles[5], col = "brown", lty = 2, lwd = 2) # Percentil 90

# Agregar leyenda
legend("topright", legend = c("Percentil 25", "Percentil 50", "Percentil 75", "Decil 90"),
       col = c("red", "blue", "green", "brown"), lty = 2, lwd = 2, bty = "n")



# Crear las categorías basadas en los cortes
fwi_filter <- fwi_filter %>%
  mutate(FWI_class = case_when(
    FWI >= percentiles[1] & FWI < percentiles[2] ~ "Bajo",
    FWI >= percentiles[2] & FWI < percentiles[3] ~ "Moderado",
    FWI >= percentiles[3] & FWI < percentiles[4] ~ "Alto",
    FWI >= percentiles[4] & FWI < percentiles[5] ~ "Muy alto",
    FWI >= percentiles[5] & FWI <= percentiles[6] ~ "Extremo"))

fwi_filter$FWI_class <- factor(fwi_filter$FWI_class, 
                               levels = c("Bajo", "Moderado", "Alto", "Muy alto", "Extremo"))


rm(fwi_filter_percentiles, fwi_desde, fwi_hasta, percentiles)


# Categoría de FWI
g4 <- ggplot(fwi_filter, aes(x = Datetime, y = FWI, fill = FWI_class)) +
  geom_col() +  
  scale_fill_manual(
    values = c(
      "Bajo" = "#009342",  
      "Moderado" = "#3b5a89",       
      "Alto" = "#fed20d",  
      "Muy alto" = "#f07a30",      
      "Extremo" = "#da3845"), 
    limits = c("Bajo", "Moderado", "Alto", "Muy alto", "Extremo")) +  # Orden jerárquico leyenda
  scale_x_datetime(name = "", date_labels = "%d/%m/%y", date_breaks = "1 days") +
  theme_minimal() +  
  labs(title = "FWI categorizado por categoría de riesgo", x = "Tiempo", y = "FWI", fill = "Clase") +
  theme(plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "top")

ggplotly(g4)


### EXPORTAR GRÁFICOS -------------------------------------------
ggsave("./Output/grafico_1.jpeg", plot = g1, width = 8, height = 6, dpi = 300)
ggsave("./Output/grafico_2.jpeg", plot = g2, width = 8, height = 6, dpi = 300)
ggsave("./Output/grafico_3.jpeg", plot = g3, width = 8, height = 6, dpi = 300)
ggsave("./Output/grafico_4.jpeg", plot = g4, width = 8, height = 6, dpi = 300)
