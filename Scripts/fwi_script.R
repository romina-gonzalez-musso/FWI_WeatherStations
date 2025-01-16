# Librerías
library("tidyverse")
library("readxl") # Ya viene en tidyverse pero por las dudas
library("zoo")    
library("ggplot2")
library("plotly")
library("vroom")

library("cffdrs") # El paquete para calcular fwi

# CARGAR DAT ---------------------------------------------------
# dat <- read.delim("./Data_Input/WStn_03_WeathDat60min.dat", sep=",", skip = 1)[-c(1:2), ] # Elimina la primera fila y luego las dos siguientes

datos <- vroom("./Data_Input/WStn_03_WeathDat60min.dat",
               skip = 1, 
               col_types = c(TIMESTAMP = "T",
                             .default = "n")) %>% 
  drop_na(TIMESTAMP)%>% 
  transmute(
    stationName = "PasoElLeon",
    Datetime = TIMESTAMP,
    airTemperature = AirT_C_Avg,
    rh = RH,
    windSpeed = WS_ms_Avg,
    rain = Rain_mm_Tot
  ) 


# CARGAR DATOS .XLSX  -------------------------------------------------------
# xlsx <- read_excel("./Data_Input/weatherStation60minDataElLeon.xlsx", sheet = "weatherStation60minDataElManso")

# Seleccionar columnas de interés 
# datos <- xlsx %>% select(stationName, Fecha, Hora, airTemperature, rh, windSpeed, rain)


# PREPARAR COLUMNAS PARA FUNCIÓN FWI ---------------------------------------------
# Wind Speed a Km/h
datos$wskmh <- datos$windSpeed * 3.6

# Columna Datetime
#(already done on-the-fly by zoom)
#datos$Datetime <- as.POSIXct(
#  paste(datos$Fecha, sprintf("%02d:00:00", datos$Hora)), 
#  format = "%Y-%m-%d %H:%M:%S")

# Calcular rain24hs a partir de rain
datos$rain24hs <- round(rollsumr(datos$rain, k = 23, fill = 0),3)

# Columna yr, mon y day
datos$yr <- as.numeric(format(datos$Datetime, "%Y"))  # Año
datos$mon <- as.numeric(format(datos$Datetime, "%m")) # Mes
datos$day <- as.numeric(format(datos$Datetime, "%d")) # Día

# Agrega columna LAT LONG
datos$lat <- "-41"
datos$long <- "-71"

# Dataframe final
col_order <- c("stationName", "lat", "long", "yr", "mon", "day", "airTemperature", "rh", "wskmh", "rain24hs")
datos_fwi <- datos[, col_order]
colnames(datos_fwi) <- c("id",  "lat", "long", "yr", "mon", "day", "temp", "rh", "ws", "prec")

rm(col_order)


# CALCULO FWI CON cffdrs -------------------------------------------------
## (1) FWI System variables for a single weather station:
# Using the default initial values and batch argument,
# the function calculate FWI variables chronically:

fwi <- fwi(datos_fwi, init = c(ffmc = 85, dmc = 6, dc = 15, lat = -41), batch = TRUE)
fwi <- fwi %>% 
  mutate_if(is.numeric, round, digits=3)

rm(datos_fwi)


## Guardar .CSV de FWI con fecha
# Adecuar el dataframe
fwi$Datetime <- datos$Datetime
fwi$DIA <- as.numeric(format(fwi$Datetime, "%d"))
fwi$MES <- as.numeric(format(fwi$Datetime, "%m"))
fwi$HORA <- as.numeric(format(fwi$Datetime, "%H"))

fwi <- fwi %>% select(ID, LAT, LONG, Datetime, DIA, MES, HORA,
                      TEMP, RH, WS, PREC,
                      FFMC, DMC, DC, ISI, BUI, FWI, DSR)

timestamp <- format(Sys.time(), "%Y-%m-%d_%H")
timestamp <- paste0(timestamp, "hs")
file_name <- paste0("FWI_", timestamp, ".csv")

write_csv2(fwi, paste0("./Output/", file_name))

rm(file_name, timestamp, datos)

# ANALISIS ------------------------------------------------------------------
# Seleccionar un rango de fechas de interés
#desde <- as.POSIXct("2024-12-01 00:00:00")
#hasta <- as.POSIXct("2025-01-03 23:59:59")

hasta <- now()
desde <- now() - ddays(28) # four weeks

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



