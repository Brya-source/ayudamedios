required_packages <- c("DBI", "RMySQL", "dplyr", "ggplot2", "tidyr", 
                       "reshape2", "geodata", "sf", "scales")


library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# cargamos nuevamente dplyr (puede ser redundante, revisamos)
library(dplyr)

# nos aseguramos de que los campos 'mes_secuestro' y 'año_secuestro' sean numéricos
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

# contamos los secuestros por mes
secuestros_tiempo <- data %>%
  group_by(fecha_secuestro) %>%
  summarise(total_secuestros = n())

# visualizamos la tendencia de secuestros a lo largo del tiempo
library(ggplot2)
ggplot(secuestros_tiempo, aes(x = fecha_secuestro, y = total_secuestros)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Tendencia de Secuestros 2016 - nov 2024",
       x = "Fecha",
       y = "Cantidad de Secuestros") +
  theme_minimal()

#-----------------------------------------------------------------------------
# contamos los secuestros por estado
secuestros_estado <- data %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  arrange(desc(total_secuestros))

# visualizamos la distribución de secuestros por estado
ggplot(secuestros_estado, aes(x = reorder(estado, -total_secuestros), y = total_secuestros)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +
  labs(title = "Distribución de Secuestros por Estado",
       x = "Estado",
       y = "Cantidad de Secuestros") +
  theme_minimal()
#------------------------------------------------------------------------------

# contamos los secuestros por tipo de liberación
tipo_liberacion_data <- data %>%
  group_by(tipo_liberacion) %>%
  summarise(total_secuestros = n())

# visualizamos la distribución por tipo de liberación
ggplot(tipo_liberacion_data, aes(x = reorder(tipo_liberacion, -total_secuestros), y = total_secuestros, fill = tipo_liberacion)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribución por Tipo de Liberación",
       x = "Tipo de Liberación",
       y = "Cantidad de Secuestros") +
  theme_minimal()
#-------------------------------
# contamos los secuestros por tipo de captor
captor_data <- data %>%
  group_by(captor) %>%
  summarise(total_secuestros = n()) %>%
  arrange(desc(total_secuestros))

# visualizamos la distribución de características del captor
ggplot(captor_data, aes(x = reorder(captor, -total_secuestros), y = total_secuestros, fill = captor)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribución de Características del Captor",
       x = "Captor",
       y = "Cantidad de Secuestros") +
  theme_minimal()
#-----------------------------------------------
# contamos los secuestros por lugar
lugar_data <- data %>%
  group_by(lugar) %>%
  summarise(total_secuestros = n())

# visualizamos la distribución por lugar de secuestro
ggplot(lugar_data, aes(x = reorder(lugar, -total_secuestros), y = total_secuestros, fill = lugar)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribución por Lugar de Secuestro",
       x = "Lugar",
       y = "Cantidad de Secuestros") +
  theme_minimal()
#-----------------------------------------------------------------------
# contamos los secuestros por tipo de captura
captura_data <- data %>%
  group_by(captura) %>%
  summarise(total_secuestros = n()) %>%
  arrange(desc(total_secuestros))

# visualizamos la distribución por tipo de captura
ggplot(captura_data, aes(x = reorder(captura, -total_secuestros), y = total_secuestros, fill = captura)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribución por Tipo de Captura",
       x = "Tipo de Captura",
       y = "Cantidad de Secuestros") +
  theme_minimal()
#----------------------------------------------------------
# contamos los secuestros por tipo de secuestro
tipo_secuestro_data <- data %>%
  group_by(tipo_secuestro) %>%
  summarise(total_secuestros = n()) %>%
  arrange(desc(total_secuestros))

# visualizamos la distribución por tipo de secuestro
ggplot(tipo_secuestro_data, aes(x = reorder(tipo_secuestro, -total_secuestros), y = total_secuestros, fill = tipo_secuestro)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribución por Tipo de Secuestro",
       x = "Tipo de Secuestro",
       y = "Cantidad de Secuestros") +
  theme_minimal()
#---------------------------------------------------------
# contamos los secuestros por liberación (Sí o No)
liberacion_data <- data %>%
  group_by(liberacion) %>%
  summarise(total_secuestros = n())

# visualizamos la distribución de secuestros por liberación
ggplot(liberacion_data, aes(x = liberacion, y = total_secuestros, fill = liberacion)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución de Secuestros por Liberación",
       x = "¿Hubo Liberación?",
       y = "Cantidad de Secuestros") +
  theme_minimal()
#----------------------------------------------

#---------------------------------------------------------------
# visualizamos la relación entre lugar, captor y tipo de captura
ggplot(data, aes(x = captor, fill = captura)) +
  geom_bar(position = "fill") +
  facet_wrap(~lugar) +
  labs(
    title = "Relación entre Lugar, Captor y Tipo de Captura",
    x = "Captor",
    y = "Proporción de Secuestros",
    fill = "Tipo de Captura"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#--------------------------------------------------------------

#------------------------------------------------------------------------
# visualizamos la relación entre tipo de liberación, captura y año
ggplot(data, aes(x = año_secuestro, fill = captura)) +
  geom_bar(position = "fill") +
  facet_wrap(~tipo_liberacion) +
  labs(
    title = "Relación entre Tipo de Liberación, Captura y Año",
    x = "Año",
    y = "Proporción de Secuestros",
    fill = "Tipo de Captura"
  ) +
  theme_minimal()
#-----------------------------------------------------------
# visualizamos la relación entre estado, lugar y tipo de secuestro
ggplot(data, aes(x = estado, fill = tipo_secuestro)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~lugar) +
  labs(
    title = "Relación entre Estado, Lugar y Tipo de Secuestro",
    x = "Estado",
    y = "Cantidad de Secuestros",
    fill = "Tipo de Secuestro"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#---------------------------------------------------------------------------

#-----------------------------------------------------------------
# contamos los secuestros por lugar y año
trend_data <- data %>%
  group_by(lugar, año_secuestro) %>%
  summarise(total_secuestros = n())

# visualizamos la tendencia de secuestros por lugar y año
ggplot(trend_data, aes(x = año_secuestro, y = total_secuestros, color = lugar, group = lugar)) +
  geom_line(linewidth = 1) +
  labs(
    title = "Tendencia de Secuestros por Lugar y Año",
    x = "Año",
    y = "Cantidad de Secuestros",
    color = "Lugar"
  ) +
  theme_minimal()
#----------------------------------------------------------------------

#--------------------------------------------------------------------------
# listamos los estados con mayor incidencia
estados_seleccionados <- c("Ciudad e México", "Chiapas", "Estado de México", "Jalisco", "Oaxaca")

# filtramos los datos para los estados seleccionados
data_filtrada <- data %>%
  filter(estado %in% estados_seleccionados)

# configuramos los gráficos individuales para cada estado
library(ggplot2)

# creamos gráficos individuales para cada estado
for (estado in estados_seleccionados) {
  # filtramos los datos para el estado actual
  data_estado <- data_filtrada %>%
    filter(estado == !!estado)
  
  # creamos el gráfico
  p <- ggplot(data_estado, aes(x = año_secuestro, fill = tipo_secuestro)) +
    geom_bar(position = "dodge") +
    labs(
      title = paste("Tipos de Secuestro en", estado, "por Año"),
      x = "Año",
      y = "Cantidad de Secuestros",
      fill = "Tipo de Secuestro"
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # mostramos el gráfico
  print(p)
}

#------------------------------------------------------------------
# listamos los lugares principales
lugares <- c("casa", "transporte", "vía pública")

# filtramos los datos por los lugares seleccionados
data_filtrada_lugares <- data %>%
  filter(lugar %in% lugares)

# creamos gráficos individuales por lugar
library(ggplot2)

for (lugar_actual in lugares) {
  # filtramos los datos para el lugar actual
  data_lugar <- data_filtrada_lugares %>%
    filter(lugar == lugar_actual)
  
  # verificamos si hay datos para el lugar actual
  if (nrow(data_lugar) > 0) {
    # creamos el gráfico
    p <- ggplot(data_lugar, aes(x = estado, fill = tipo_secuestro)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(
        title = paste("Relación entre Estado y Tipo de Secuestro en", lugar_actual),
        x = "Estado",
        y = "Cantidad de Secuestros",
        fill = "Tipo de Secuestro"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 16, face = "bold")
      )
    
    # mostramos el gráfico
    print(p)
  } else {
    message(paste("No hay datos para el lugar:", lugar_actual))
  }
}

#-------------------------------------------------------
# contamos los secuestros por lugar y tipo de secuestro
data_lugar_secuestro <- data %>%
  group_by(lugar, tipo_secuestro) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# visualizamos la proporción de tipos de secuestro según lugar
ggplot(data_lugar_secuestro, aes(x = reorder(lugar, -total_secuestros), y = total_secuestros, fill = tipo_secuestro)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Proporción de Tipos de Secuestro según Lugar",
    x = "Lugar del Secuestro",
    y = "Proporción de Secuestros",
    fill = "Tipo de Secuestro"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#-------------------------------------------------------------------
# contamos los secuestros por lugar y tipo de secuestro
data_lugar_tipo <- data %>%
  group_by(lugar, tipo_secuestro) %>%
  summarise(total = n(), .groups = "drop")

# visualizamos la distribución de tipos de secuestro según el lugar
ggplot(data_lugar_tipo, aes(x = lugar, y = total, fill = tipo_secuestro)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Distribución de Tipos de Secuestro según el Lugar",
    x = "Lugar del Secuestro",
    y = "Cantidad de Secuestros",
    fill = "Tipo de Secuestro"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#-------------------------------------------------------------------
# contamos los secuestros por fecha y tipo de secuestro
data_tiempo_tipo <- data %>%
  group_by(fecha_secuestro, tipo_secuestro) %>%
  summarise(total = n(), .groups = "drop")

# visualizamos la tendencia de secuestros por tipo a lo largo del tiempo
ggplot(data_tiempo_tipo, aes(x = fecha_secuestro, y = total, color = tipo_secuestro)) +
  geom_line(size = 1) +
  labs(
    title = "Tendencia de Secuestros por Tipo a lo Largo del Tiempo",
    x = "Fecha",
    y = "Cantidad de Secuestros",
    color = "Tipo de Secuestro"
  ) +
  theme_minimal()
#-------------------------------------------------------------------
# contamos los secuestros por estado y captor
data_estado_captor <- data %>%
  group_by(estado, captor) %>%
  summarise(total = n(), .groups = "drop")

# visualizamos el mapa de calor de secuestros por estado y tipo de captor
ggplot(data_estado_captor, aes(x = captor, y = estado, fill = total)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Mapa de Calor de Secuestros por Estado y Tipo de Captor",
    x = "Tipo de Captor",
    y = "Estado",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()
#-------------------------------------------------------------------
# contamos los secuestros por tipo de liberación y tipo de secuestro
data_liberacion_secuestro <- data %>%
  group_by(tipo_liberacion, tipo_secuestro) %>%
  summarise(total = n(), .groups = "drop")

# visualizamos la relación entre tipo de liberación y tipo de secuestro
ggplot(data_liberacion_secuestro, aes(x = tipo_liberacion, y = total, fill = tipo_secuestro)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Relación entre Tipo de Liberación y Tipo de Secuestro",
    x = "Tipo de Liberación",
    y = "Proporción de Secuestros",
    fill = "Tipo de Secuestro"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#----------------------------------------------------------------
# contamos los secuestros por estado y lugar
data_estado_lugar <- data %>%
  group_by(estado, lugar) %>%
  summarise(total = n(), .groups = "drop")

# visualizamos los secuestros por estado y lugar del secuestro
ggplot(data_estado_lugar, aes(x = reorder(estado, -total), y = total, fill = lugar)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Secuestros por Estado y Lugar del Secuestro",
    x = "Estado",
    y = "Cantidad de Secuestros",
    fill = "Lugar del Secuestro"
  ) +
  theme_minimal()
#-----------------------------------------------------
# modificamos la agrupación para relacionar por año y no solo por mes
# contamos los secuestros por mes y captor
data_mes_captor <- data %>%
  group_by(mes_secuestro, captor) %>%
  summarise(total = n(), .groups = "drop")

# visualizamos los secuestros por mes y tipo de captor
ggplot(data_mes_captor, aes(x = mes_secuestro, y = total, color = captor)) +
  geom_point(size = 3) +
  geom_line(aes(group = captor), size = 1) +
  labs(
    title = "Secuestros por Mes y Tipo de Captor",
    x = "Mes",
    y = "Cantidad de Secuestros",
    color = "Tipo de Captor"
  ) +
  theme_minimal()
#--------------------------------------
# no nos gustó y no entendemos la gráfica, hacemos cambios o la descartamos
# contamos los secuestros por año, lugar y liberación
data_ano_lugar_liberacion <- data %>%
  group_by(año_secuestro, lugar, liberacion) %>%
  summarise(total = n(), .groups = "drop")

# visualizamos la proporción de liberaciones por año y lugar del secuestro
ggplot(data_ano_lugar_liberacion, aes(x = año_secuestro, y = total, fill = liberacion)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ lugar) +
  labs(
    title = "Proporción de Liberaciones por Año y Lugar del Secuestro",
    x = "Año",
    y = "Proporción de Secuestros",
    fill = "Liberación"
  ) +
  theme_minimal()
#-----------------------------------------------
# contamos los secuestros por captor y captura
data_captor_captura <- data %>%
  group_by(captor, captura) %>%
  summarise(total = n(), .groups = "drop")

# visualizamos la relación entre tipo de captor y captura
ggplot(data_captor_captura, aes(x = captor, y = total, fill = captura)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Relación entre Tipo de Captor y Captura",
    x = "Tipo de Captor",
    y = "Cantidad de Secuestros",
    fill = "Captura"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#--------------------------------------------------

# instalamos paquetes si aún no lo hemos hecho
install.packages("geodata")
install.packages("sf")
install.packages("ggplot2")
install.packages("dplyr")

# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# obtenemos los nombres de los estados en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa:")
print(unique_states_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes después de estandarizar:")
print(estados_no_coincidentes)

# creamos una tabla de equivalencias para corregir nombres
equivalencias <- data.frame(
  estado_datos = c(
    "ciudad de mexico",
    "estado de mexico"
  ),
  estado_mapa = c(
    "distrito federal",
    "mexico"
  )
)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "white", high = "red", na.value = "grey80") +
  labs(
    title = "Distribución Geográfica de Secuestros en México",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()
#--------------------------------------------------------------------

# contamos los secuestros por lugar y tipo de liberación
data_lugar_liberacion <- data %>%
  group_by(lugar, tipo_liberacion) %>%
  summarise(total = n(), .groups = "drop")

# visualizamos la relación entre lugar del secuestro y tipo de liberación
ggplot(data_lugar_liberacion, aes(x = lugar, y = total, fill = tipo_liberacion)) +
  geom_bar(stat = "identity", position = "fill") +
  labs(
    title = "Relación entre Lugar del Secuestro y Tipo de Liberación",
    x = "Lugar del Secuestro",
    y = "Proporción de Secuestros",
    fill = "Tipo de Liberación"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#---------------------------------------------------------------
# mostramos mejor cada 5 municipios, corregimos la gráfica
# nos aseguramos de que los campos necesarios estén presentes y correctos
library(dplyr)

# convertimos 'año_secuestro' a numérico si no lo está
data <- data %>%
  mutate(año_secuestro = as.numeric(año_secuestro))

# filtramos datos válidos
data <- data %>%
  filter(!is.na(municipio), !is.na(año_secuestro), !is.na(tipo_secuestro))

# contamos los secuestros por municipio
municipios_secuestros <- data %>%
  group_by(municipio) %>%
  summarise(total_secuestros = n()) %>%
  arrange(desc(total_secuestros)) %>%
  slice(1:15)

# vemos los 15 municipios seleccionados
print(municipios_secuestros)

# obtenemos la lista de los 15 municipios
top_municipios <- municipios_secuestros$municipio

# filtramos los datos para esos municipios
data_top_municipios <- data %>%
  filter(municipio %in% top_municipios)

library(ggplot2)

# agrupamos datos por municipio y año
data_municipio_año <- data_top_municipios %>%
  group_by(municipio, año_secuestro) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# visualizamos los secuestros por año en los 15 municipios con mayor incidencia
ggplot(data_municipio_año, aes(x = año_secuestro, y = total_secuestros, fill = as.factor(año_secuestro))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ municipio, scales = "free_y") +
  labs(
    title = "Secuestros por Año en los 15 Municipios con Mayor Incidencia",
    x = "Año",
    y = "Cantidad de Secuestros",
    fill = "Año"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
#-------------------------------------------------------------
# agrupamos datos por municipio y tipo de secuestro, renderizamos la gráfica
data_municipio_tipo <- data_top_municipios %>%
  group_by(municipio, tipo_secuestro) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# visualizamos la distribución de tipos de secuestro en los 15 municipios con mayor incidencia
ggplot(data_municipio_tipo, aes(x = reorder(municipio, total_secuestros), y = total_secuestros, fill = tipo_secuestro)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Distribución de Tipos de Secuestro en los 15 Municipios con Mayor Incidencia",
    x = "Municipio",
    y = "Cantidad de Secuestros",
    fill = "Tipo de Secuestro"
  ) +
  theme_minimal()
#-------------------------------------------------------------------

# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2022
data_2022 <- data %>%
  filter(año_secuestro == 2022)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2022 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México en 2022
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "cyan", high = "blue", na.value = "grey80") +
  labs(
    title = "Distribución Geográfica de Secuestros en México en 2022",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()
#-------------------------------------------------------------------

# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2023
data_2023 <- data %>%
  filter(año_secuestro == 2023)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2023 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México en 2023
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "white", high = "purple", na.value = "grey80") +
  labs(
    title = "Distribución Geográfica de Secuestros en México en 2023",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()
#-----------------------------------------------------------------
# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2024
data_2024 <- data %>%
  filter(año_secuestro == 2024)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2024 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México durante enero a octubre 2024
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "cyan", high = "blue", na.value = "grey80") +
  labs(
    title = "Distribución Geográfica de Secuestros en México durante enero a octubre 2024",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()
#------------------------------------------------------------------
# contamos los secuestros por municipio nuevamente
library(dplyr)
library(ggplot2)

# convertimos 'año_secuestro' a numérico si no lo está
data <- data %>%
  mutate(año_secuestro = as.numeric(año_secuestro))

# filtramos datos válidos
data <- data %>%
  filter(!is.na(municipio), !is.na(año_secuestro), !is.na(tipo_secuestro))

# contamos los secuestros por municipio
municipios_secuestros <- data %>%
  group_by(municipio) %>%
  summarise(total_secuestros = n()) %>%
  arrange(desc(total_secuestros)) %>%
  slice(1:15)

# vemos los 15 municipios seleccionados
print(municipios_secuestros)

# obtenemos la lista de los 15 municipios
top_municipios <- municipios_secuestros$municipio

# dividimos los municipios en grupos de 3
lista_municipios_grupos <- split(top_municipios, ceiling(seq_along(top_municipios)/3))

# iteramos sobre cada grupo de municipios para crear gráficos individuales
for (i in seq_along(lista_municipios_grupos)) {
  # obtenemos el grupo actual de municipios
  municipios_grupo <- lista_municipios_grupos[[i]]
  
  # filtramos los datos para los municipios del grupo actual
  data_grupo <- data %>%
    filter(municipio %in% municipios_grupo)
  
  # agrupamos datos por municipio y año
  data_municipio_año <- data_grupo %>%
    group_by(municipio, año_secuestro) %>%
    summarise(total_secuestros = n()) %>%
    ungroup()
  
  # creamos la gráfica para el grupo actual
  titulo_municipios <- paste(municipios_grupo, collapse = ", ")
  grafica <- ggplot(data_municipio_año, aes(x = año_secuestro, y = total_secuestros, fill = as.factor(año_secuestro))) +
    geom_bar(stat = "identity") +
    facet_wrap(~ municipio, scales = "free_y") +
    labs(
      title = paste("Secuestros por Año en:", titulo_municipios),
      x = "Año",
      y = "Cantidad de Secuestros",
      fill = "Año"
    ) +
    theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  # mostramos la gráfica
  print(grafica)
  
  # opcional: pausar brevemente entre gráficas para facilitar la visualización
  Sys.sleep(1) # pausa de 1 segundo entre gráficas
}
#---------------------------------------
# descargamos y cargamos nuevamente las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2022
data_2022 <- data %>%
  filter(año_secuestro == 2021)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2022 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México en 2019
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "yellow", high = "green", na.value = "white") +
  labs(
    title = "Distribución Geográfica de Secuestros en México en 2019",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()
#-----------------------------------------------------------
# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2022
data_2022 <- data %>%
  filter(año_secuestro == 2020)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2022 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México en 2020
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "yellow", high = "green", na.value = "white") +
  labs(
    title = "Distribución Geográfica de Secuestros en México en 2020",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()

# instalamos paquetes si es necesario
# install.packages(c("DBI", "RMySQL", "dplyr", "ggplot2", "tidyr", "reshape2"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# nos aseguramos de que los campos 'mes_secuestro' y 'año_secuestro' sean numéricos
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

# --------------------------------
# filtramos valores faltantes en 'liberacion'
data <- data %>%
  filter(!is.na(liberacion))

# --------------------------------
# paso 1: agrupamos y calculamos porcentajes
# agrupamos por año y liberación, contamos los secuestros
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

# --------------------------------
# paso 2: creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

# instalamos paquetes si es necesario
# install.packages(c("DBI", "RMySQL", "dplyr", "ggplot2", "tidyr"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

# --------------------------------
# paso 1: contamos los secuestros por mes
secuestros_tiempo <- data %>%
  group_by(fecha_secuestro) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# paso 2: calculamos el promedio anual de secuestros
promedio_anual <- data %>%
  group_by(año_secuestro) %>%
  summarise(total_secuestros_anual = n(),
            promedio_mensual = total_secuestros_anual / 12) %>%
  ungroup()

# creamos una columna de fecha representativa para cada año (ejemplo: 1 de julio de cada año)
promedio_anual <- promedio_anual %>%
  mutate(fecha_promedio = as.Date(paste(año_secuestro, "07", "01", sep = "-"))) # centro del año

# --------------------------------
# paso 3: visualización
ggplot() +
  # línea de tendencia mensual
  geom_line(data = secuestros_tiempo, aes(x = fecha_secuestro, y = total_secuestros), color = "blue") +
  
  # puntos de secuestros mensuales
  geom_point(data = secuestros_tiempo, aes(x = fecha_secuestro, y = total_secuestros), color = "lightblue", alpha = 0.6, size = 1) +
  
  # puntos de promedio anual
  geom_point(data = promedio_anual, aes(x = fecha_promedio, y = promedio_mensual), color = "red", size = 3) +
  
  # línea de promedio anual
  geom_line(data = promedio_anual, aes(x = fecha_promedio, y = promedio_mensual), color = "red", linetype = "dashed") +
  
  # etiquetas y títulos
  labs(title = "Tendencia de Secuestros 2016 - Nov 2024",
       subtitle = "Línea azul: Secuestros mensuales | Puntos rojos: Promedio mensual anual",
       x = "Fecha",
       y = "Cantidad de Secuestros") +
  
  # tema minimalista
  theme_minimal() +
  
  # rotamos etiquetas del eje X para mejor legibilidad
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # escala del eje Y
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
#----------------------

# cargamos las librerías necesarias
library(dplyr)
library(ggplot2)

# suponemos que nuestro dataset ya se llama 'data' y está correctamente procesado

# 1. agrupamos datos por año y tipo de captor
data_anio_captor <- data %>%
  group_by(año_secuestro, captor) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# 2. visualizamos los secuestros por año y tipo de captor
ggplot(data_anio_captor, aes(x = año_secuestro, y = total_secuestros, color = captor)) +
  geom_point(size = 3) +
  geom_line(aes(group = captor), size = 1) +
  labs(
    title = "Secuestros por Año y Tipo de Captor",
    x = "Año",
    y = "Cantidad de Secuestros",
    color = "Tipo de Captor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
#---------------------------------------------

# instalamos paquetes si es necesario
# install.packages(c("dplyr", "ggplot2", "DBI", "RMySQL", "tidyr"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# contamos secuestros por estado, modificamos la agrupación
secuestros_estado <- data %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  arrange(desc(total_secuestros))

# visualizamos la distribución de secuestros por estado (barras verticales y sin ordenar)
ggplot(secuestros_estado, aes(x = estado, y = total_secuestros)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribución de Secuestros por Estado",
       x = "Estado",
       y = "Cantidad de Secuestros") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-----------------------------------------------------------------------------
# contamos los secuestros por año
secuestros_anio <- data %>%
  group_by(año_secuestro) %>%
  summarise(total_secuestros = n(), .groups = "drop") %>%
  arrange(año_secuestro)

# visualizamos la tendencia de secuestros por año con un gráfico de líneas
ggplot(secuestros_anio, aes(x = año_secuestro, y = total_secuestros)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Tendencia de Secuestros por Año",
       x = "Año",
       y = "Cantidad de Secuestros") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-----------------------------------------------------------------------------
# contamos los secuestros por estado y año
secuestros_estado_anio <- data %>%
  group_by(estado, año_secuestro) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# visualizamos la tendencia de secuestros por año y estado con un gráfico de líneas
ggplot(secuestros_estado_anio, aes(x = año_secuestro, y = total_secuestros, color = estado)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Tendencia de Secuestros por Año y Estado",
       x = "Año",
       y = "Cantidad de Secuestros",
       color = "Estado") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-----------------------------------------------------------------------------
# visualizamos de manera combinada con facetas: secuestros por año y estado
ggplot(secuestros_estado_anio, aes(x = factor(año_secuestro), y = total_secuestros, fill = factor(año_secuestro))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ estado, ncol = 3, scales = "free_y") + # 3 columnas para distribuir las facetas
  labs(title = "Distribución de Secuestros por Año en Cada Estado",
       x = "Año",
       y = "Cantidad de Secuestros",
       fill = "Año") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# cerramos la conexión a la base de datos
dbDisconnect(con)
#--------------------------------------------
# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2022
data_2022 <- data %>%
  filter(año_secuestro == 2019)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2022 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México en 2021
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "pink", high = "red", na.value = "white") +
  labs(
    title = "Distribución Geográfica de Secuestros en México en 2021",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()
#-------------------------------------------------------------------

# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2022
data_2022 <- data %>%
  filter(año_secuestro == 2019)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2022 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México en 2020
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "yellow", high = "green", na.value = "white") +
  labs(
    title = "Distribución Geográfica de Secuestros en México en 2020",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()

# instalamos paquetes si es necesario
# install.packages(c("DBI", "RMySQL", "dplyr", "ggplot2", "tidyr"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# filtramos valores faltantes en 'liberacion'
data <- data %>%
  filter(!is.na(liberacion))

#-----------------------------------------------------------------------------
# paso 1: agrupamos y calculamos porcentajes
# agrupamos por año y liberación, contamos los secuestros
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

#-----------------------------------------------------------------------------
# paso 2: creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

# instalamos paquetes si es necesario
# install.packages(c("DBI", "RMySQL", "dplyr", "ggplot2", "tidyr"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# nos aseguramos de que los campos 'mes_secuestro' y 'año_secuestro' sean numéricos
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

# --------------------------------
# contamos los secuestros por mes
secuestros_tiempo <- data %>%
  group_by(fecha_secuestro) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# contamos el promedio anual de secuestros
promedio_anual <- data %>%
  group_by(año_secuestro) %>%
  summarise(total_secuestros_anual = n()) %>%
  mutate(promedio_mensual = total_secuestros_anual / 12) %>%
  ungroup()

# creamos una columna de fecha representativa para cada año (ejemplo: 1 de julio de cada año)
promedio_anual <- promedio_anual %>%
  mutate(fecha_promedio = as.Date(paste(año_secuestro, "07", "01", sep = "-"))) # centro del año

# --------------------------------
# visualizamos la tendencia de secuestros con líneas y puntos
ggplot() +
  # línea de tendencia mensual
  geom_line(data = secuestros_tiempo, aes(x = fecha_secuestro, y = total_secuestros), color = "blue") +
  
  # puntos de secuestros mensuales
  geom_point(data = secuestros_tiempo, aes(x = fecha_secuestro, y = total_secuestros), color = "lightblue", alpha = 0.6, size = 1) +
  
  # puntos de promedio anual
  geom_point(data = promedio_anual, aes(x = fecha_promedio, y = promedio_mensual), color = "red", size = 3) +
  
  # línea de promedio anual
  geom_line(data = promedio_anual, aes(x = fecha_promedio, y = promedio_mensual), color = "red", linetype = "dashed") +
  
  # etiquetas y títulos
  labs(title = "Tendencia de Secuestros 2016 - Nov 2024",
       subtitle = "Línea azul: Secuestros mensuales | Puntos rojos: Promedio mensual anual",
       x = "Fecha",
       y = "Cantidad de Secuestros") +
  
  # tema minimalista
  theme_minimal() +
  
  # rotamos etiquetas del eje X para mejor legibilidad
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # escala del eje Y
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))
#----------------------

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# contamos secuestros por estado, modificamos la agrupación
secuestros_estado <- data %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  arrange(desc(total_secuestros))

# visualizamos la distribución de secuestros por estado (barras verticales y sin ordenar)
ggplot(secuestros_estado, aes(x = estado, y = total_secuestros)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Distribución de Secuestros por Estado",
       x = "Estado",
       y = "Cantidad de Secuestros") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-----------------------------------------------------------------------------
# contamos los secuestros por año
secuestros_anio <- data %>%
  group_by(año_secuestro) %>%
  summarise(total_secuestros = n(), .groups = "drop") %>%
  arrange(año_secuestro)

# visualizamos la tendencia de secuestros por año con un gráfico de líneas
ggplot(secuestros_anio, aes(x = año_secuestro, y = total_secuestros)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Tendencia de Secuestros por Año",
       x = "Año",
       y = "Cantidad de Secuestros") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-----------------------------------------------------------------------------
# contamos los secuestros por estado y año
secuestros_estado_anio <- data %>%
  group_by(estado, año_secuestro) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# visualizamos la tendencia de secuestros por año y estado con un gráfico de líneas
ggplot(secuestros_estado_anio, aes(x = año_secuestro, y = total_secuestros, color = estado)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Tendencia de Secuestros por Año y Estado",
       x = "Año",
       y = "Cantidad de Secuestros",
       color = "Estado") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-----------------------------------------------------------------------------
# visualizamos de manera combinada con facetas: secuestros por año y estado
ggplot(secuestros_estado_anio, aes(x = factor(año_secuestro), y = total_secuestros, fill = factor(año_secuestro))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ estado, ncol = 3, scales = "free_y") + # 3 columnas para distribuir las facetas
  labs(title = "Distribución de Secuestros por Año en Cada Estado",
       x = "Año",
       y = "Cantidad de Secuestros",
       fill = "Año") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# cerramos la conexión a la base de datos
dbDisconnect(con)
#--------------------------------------------
# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2022
data_2022 <- data %>%
  filter(año_secuestro == 2021)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2022 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México en 2021
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "pink", high = "red", na.value = "white") +
  labs(
    title = "Distribución Geográfica de Secuestros en México en 2021",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()
#-------------------------------------------------------------------

# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2023
data_2023 <- data %>%
  filter(año_secuestro == 2023)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2023 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México en 2023
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "white", high = "purple", na.value = "grey80") +
  labs(
    title = "Distribución Geográfica de Secuestros en México en 2023",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()
#-----------------------------------------------------------------
# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# filtramos valores faltantes en 'liberacion'
data <- data %>%
  filter(!is.na(liberacion))

#-----------------------------------------------------------------------------
# paso 1: agrupamos y calculamos porcentajes
# agrupamos por año y liberación, contamos los secuestros
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

#-----------------------------------------------------------------------------
# paso 2: creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

# instalamos paquetes si es necesario
# install.packages(c("DBI", "RMySQL", "dplyr", "ggplot2", "tidyr"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

# --------------------------------
# contamos los secuestros por mes
secuestros_tiempo <- data %>%
  group_by(fecha_secuestro) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# contamos el promedio anual de secuestros
promedio_anual <- data %>%
  group_by(año_secuestro) %>%
  summarise(total_secuestros_anual = n()) %>%
  mutate(promedio_mensual = total_secuestros_anual / 12) %>%
  ungroup()

# si hay años con menos de 12 meses de datos, ajustamos el denominador
# por ejemplo, contando los meses únicos por año
meses_por_año <- data %>%
  group_by(año_secuestro) %>%
  summarise(meses_disponibles = n_distinct(mes_secuestro)) 

promedio_anual <- promedio_anual %>%
  left_join(meses_por_año, by = "año_secuestro") %>%
  mutate(promedio_mensual = total_secuestros_anual / meses_disponibles) %>%
  select(año_secuestro, promedio_mensual)

# --------------------------------
# visualizamos la tendencia de secuestros con líneas y puntos
ggplot() +
  # línea de tendencia mensual
  geom_line(data = secuestros_tiempo, aes(x = fecha_secuestro, y = total_secuestros), color = "blue") +
  
  # puntos de secuestros mensuales
  geom_point(data = secuestros_tiempo, aes(x = fecha_secuestro, y = total_secuestros), color = "lightblue", alpha = 0.6, size = 1) +
  
  # puntos de promedio anual
  geom_point(data = promedio_anual, aes(x = fecha_promedio, y = promedio_mensual), color = "red", size = 3) +
  
  # línea de promedio anual
  geom_line(data = promedio_anual, aes(x = fecha_promedio, y = promedio_mensual), color = "red", linetype = "dashed") +
  
  # etiquetas y títulos
  labs(title = "Tendencia de Secuestros 2016 - Nov 2024",
       subtitle = "Línea azul: Secuestros mensuales | Puntos rojos: Promedio mensual anual",
       x = "Fecha",
       y = "Cantidad de Secuestros") +
  
  # tema minimalista
  theme_minimal() +
  
  # rotamos etiquetas del eje X para mejor legibilidad
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # escala del eje Y
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#----------------------

# cargamos las librerías necesarias
library(dplyr)
library(ggplot2)

# suponemos que nuestro dataset ya se llama 'data' y está correctamente procesado

# 1. agrupamos datos por año y tipo de captor
data_anio_captor <- data %>%
  group_by(año_secuestro, captor) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# 2. visualizamos los secuestros por año y tipo de captor
ggplot(data_anio_captor, aes(x = año_secuestro, y = total_secuestros, color = captor)) +
  geom_point(size = 3) +
  geom_line(aes(group = captor), size = 1) +
  labs(
    title = "Secuestros por Año y Tipo de Captor",
    x = "Año",
    y = "Cantidad de Secuestros",
    color = "Tipo de Captor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
#---------------------------------------------

# instalamos paquetes si es necesario
# install.packages(c("dplyr", "ggplot2", "DBI", "RMySQL", "tidyr"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# filtramos valores faltantes en 'liberacion'
data <- data %>%
  filter(!is.na(liberacion))

#-----------------------------------------------------------------------------
# paso 1: agrupamos y contamos los secuestros por año y liberación
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

#-----------------------------------------------------------------------------
# paso 2: creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

# instalamos paquetes si es necesario
# install.packages(c("DBI", "RMySQL", "dplyr", "ggplot2", "tidyr"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# paso 1: contamos los secuestros por mes
secuestros_tiempo <- data %>%
  group_by(fecha_secuestro) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# paso 2: calculamos el promedio anual de secuestros
promedio_anual <- data %>%
  group_by(año_secuestro) %>%
  summarise(total_secuestros_anual = n()) %>%
  ungroup()

# calculamos el promedio mensual considerando los meses disponibles
meses_por_año <- data %>%
  group_by(año_secuestro) %>%
  summarise(meses_disponibles = n_distinct(mes_secuestro)) 

promedio_anual <- promedio_anual %>%
  left_join(meses_por_año, by = "año_secuestro") %>%
  mutate(promedio_mensual = total_secuestros_anual / meses_disponibles) %>%
  select(año_secuestro, promedio_mensual)

# --------------------------------
# visualizamos la tendencia de secuestros con líneas y puntos
ggplot() +
  # línea de tendencia mensual
  geom_line(data = secuestros_tiempo, aes(x = fecha_secuestro, y = total_secuestros), color = "blue") +
  
  # puntos de secuestros mensuales
  geom_point(data = secuestros_tiempo, aes(x = fecha_secuestro, y = total_secuestros), color = "lightblue", alpha = 0.6, size = 1) +
  
  # puntos de promedio anual
  geom_point(data = promedio_anual, aes(x = fecha_promedio, y = promedio_mensual), color = "red", size = 3) +
  
  # línea de promedio anual
  geom_line(data = promedio_anual, aes(x = fecha_promedio, y = promedio_mensual), color = "red", linetype = "dashed") +
  
  # etiquetas y títulos
  labs(title = "Tendencia de Secuestros 2016 - Nov 2024",
       subtitle = "Línea azul: Secuestros mensuales | Puntos rojos: Promedio mensual anual",
       x = "Fecha",
       y = "Cantidad de Secuestros") +
  
  # tema minimalista
  theme_minimal() +
  
  # rotamos etiquetas del eje X para mejor legibilidad
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  
  # escala del eje Y
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

#----------------------

# cargamos las librerías necesarias
library(dplyr)
library(ggplot2)

# suponemos que nuestro dataset ya se llama 'data' y está correctamente procesado

# 1. agrupamos datos por año y tipo de captor
data_anio_captor <- data %>%
  group_by(año_secuestro, captor) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# 2. visualizamos los secuestros por año y tipo de captor
ggplot(data_anio_captor, aes(x = año_secuestro, y = total_secuestros, color = captor)) +
  geom_point(size = 3) +
  geom_line(aes(group = captor), size = 1) +
  labs(
    title = "Secuestros por Año y Tipo de Captor",
    x = "Año",
    y = "Cantidad de Secuestros",
    color = "Tipo de Captor"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )
#---------------------------------------------

# instalamos paquetes si es necesario
# install.packages(c("dplyr", "ggplot2", "DBI", "RMySQL", "tidyr"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# contamos secuestros por año
secuestros_anio <- data %>%
  group_by(año_secuestro) %>%
  summarise(total_secuestros = n(), .groups = "drop") %>%
  arrange(año_secuestro)

# visualizamos la tendencia de secuestros por año con un gráfico de líneas
ggplot(secuestros_anio, aes(x = año_secuestro, y = total_secuestros)) +
  geom_line(color = "darkgreen", size = 1) +
  geom_point(color = "darkgreen", size = 2) +
  labs(title = "Tendencia de Secuestros por Año",
       x = "Año",
       y = "Cantidad de Secuestros") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-----------------------------------------------------------------------------
# contamos secuestros por estado y año
secuestros_estado_anio <- data %>%
  group_by(estado, año_secuestro) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# visualizamos la tendencia de secuestros por año y estado con un gráfico de líneas
ggplot(secuestros_estado_anio, aes(x = año_secuestro, y = total_secuestros, color = estado)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  labs(title = "Tendencia de Secuestros por Año y Estado",
       x = "Año",
       y = "Cantidad de Secuestros",
       color = "Estado") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

#-----------------------------------------------------------------------------
# visualizamos de manera combinada con facetas: secuestros por año y estado
ggplot(secuestros_estado_anio, aes(x = factor(año_secuestro), y = total_secuestros, fill = factor(año_secuestro))) +
  geom_bar(stat = "identity") +
  facet_wrap(~ estado, ncol = 3, scales = "free_y") + # 3 columnas para distribuir las facetas
  labs(title = "Distribución de Secuestros por Año en Cada Estado",
       x = "Año",
       y = "Cantidad de Secuestros",
       fill = "Año") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5)
  )

# cerramos la conexión a la base de datos
dbDisconnect(con)
#--------------------------------------------
# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2022
data_2022 <- data %>%
  filter(año_secuestro == 2019)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2022 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México en 2019
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "yellow", high = "brown", na.value = "white") +
  labs(
    title = "Distribución Geográfica de Secuestros en México en 2019",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()
#--------------------------------------------------------
# instalamos paquetes si es necesario
# install.packages(c("DBI", "RMySQL", "dplyr", "ggplot2", "tidyr"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# filtramos valores faltantes en 'liberacion'
data <- data %>%
  filter(!is.na(liberacion))

#-----------------------------------------------------------------------------
# paso 1: agrupamos y contamos los secuestros por año y liberación
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

#-----------------------------------------------------------------------------
# paso 2: creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

# instalamos paquetes si es necesario
# install.packages(c("DBI", "RMySQL", "dplyr", "ggplot2", "tidyr"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

# -------------------------------
# filtramos valores faltantes en 'liberacion'
data <- data %>%
  filter(!is.na(liberacion))

# -------------------------------
# paso 1: contamos los secuestros por año y liberación
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

# -------------------------------
# paso 2: creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

#----------------------

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# 1. contamos los secuestros por estado y seleccionamos los 6 con más secuestros
secuestros_estado <- data %>%
  group_by(estado) %>%
  summarise(total_secuestros = n(), .groups = "drop") %>%
  arrange(desc(total_secuestros)) %>%
  slice(1:6) # seleccionamos los 6 primeros

# vemos los 6 municipios seleccionados (opcional)
print(secuestros_estado)

# 2. obtenemos la lista de los 6 estados con más secuestros
top_estados <- secuestros_estado$estado

# 3. filtramos los datos para esos 6 estados
data_top_estados <- data %>%
  filter(estado %in% top_estados)

# 4. agrupamos datos por estado, año y tipo de secuestro
data_estado_anio_secuestro <- data_top_estados %>%
  group_by(estado, año_secuestro, tipo_secuestro) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# 5. generamos gráficas separadas para cada estado
# creamos una lista de los 6 estados
estados_list <- unique(data_estado_anio_secuestro$estado)

# iteramos sobre cada estado y creamos una gráfica individual
for (estado_actual in estados_list) {
  
  # filtramos los datos para el estado actual
  datos_estado <- data_estado_anio_secuestro %>%
    filter(estado == estado_actual)
  
  # creamos la gráfica para el estado actual
  grafica_estado <- ggplot(datos_estado, aes(x = factor(año_secuestro), y = total_secuestros, fill = tipo_secuestro)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = paste("Distribución de Tipos de Secuestro por Año en", estado_actual),
      x = "Año",
      y = "Cantidad de Secuestros",
      fill = "Tipo de Secuestro"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
  
  # mostramos la gráfica en el panel de Plots
  print(grafica_estado)
  
  # opcional: pausar brevemente entre gráficas para facilitar la visualización
  Sys.sleep(1) # pausa de 1 segundo entre gráficas
}

# Cerrar la conexión a la base de datos
dbDisconnect(con)
#---------------------------------------
# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2022
data_2022 <- data %>%
  filter(año_secuestro == 2019)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2022 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México en 2021
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "yellow", high = "brown", na.value = "white") +
  labs(
    title = "Distribución Geográfica de Secuestros en México en 2021",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()
#-------------------------------------------------------------------

# cargamos las librerías necesarias para mapas
library(geodata)
library(sf)
library(ggplot2)
library(dplyr)

# filtramos los datos para el año 2022
data_2022 <- data %>%
  filter(año_secuestro == 2020)

# agrupamos los datos por estado y contamos los secuestros
data_estado_total <- data_2022 %>%
  group_by(estado) %>%
  summarise(total_secuestros = n()) %>%
  ungroup()

# descargamos y cargamos el mapa de México a nivel de estados
mexico_map <- geodata::gadm(country = "MEX", level = 1, path = tempdir())

# convertimos a objeto sf si no lo es
mexico_map_sf <- st_as_sf(mexico_map)

# estandarizamos los nombres en nuestros datos
data_estado_total$estado <- tolower(iconv(data_estado_total$estado, to = "ASCII//TRANSLIT"))

# estandarizamos los nombres en el mapa
mexico_map_sf$NAME_1 <- tolower(iconv(mexico_map_sf$NAME_1, to = "ASCII//TRANSLIT"))

# obtenemos los nombres únicos en nuestros datos
unique_states_data <- unique(data_estado_total$estado)
print("Estados en tus datos (después de estandarizar):")
print(unique_states_data)

# obtenemos los nombres únicos en el mapa
unique_states_map <- unique(mexico_map_sf$NAME_1)
print("Estados en el mapa (después de estandarizar):")
print(unique_states_map)

# identificamos estados que no coinciden
estados_no_coincidentes <- setdiff(unique_states_data, unique_states_map)
print("Estados no coincidentes antes de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con la tabla de equivalencias
data_estado_total <- data_estado_total %>%
  left_join(equivalencias, by = c("estado" = "estado_datos")) %>%
  mutate(
    estado = ifelse(is.na(estado_mapa), estado, estado_mapa)
  ) %>%
  select(-estado_mapa)

# verificamos estados no coincidentes después de aplicar equivalencias
estados_no_coincidentes <- setdiff(unique(data_estado_total$estado), unique_states_map)
print("Estados no coincidentes después de aplicar equivalencias:")
print(estados_no_coincidentes)

# unimos nuestros datos con el mapa
map_data <- mexico_map_sf %>%
  left_join(data_estado_total, by = c("NAME_1" = "estado"))

# visualizamos el mapa de distribución geográfica de secuestros en México en 2020
ggplot(map_data) +
  geom_sf(aes(fill = total_secuestros)) +
  scale_fill_gradient(low = "yellow", high = "green", na.value = "white") +
  labs(
    title = "Distribución Geográfica de Secuestros en México en 2020",
    fill = "Cantidad de Secuestros"
  ) +
  theme_minimal()

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# filtramos valores faltantes en 'liberacion'
data <- data %>%
  filter(!is.na(liberacion))

#-----------------------------------------------------------------------------
# paso 1: agrupamos y contamos los secuestros por año y liberación
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

#-----------------------------------------------------------------------------
# paso 2: creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

# instalamos paquetes si es necesario
# install.packages(c("DBI", "RMySQL", "dplyr", "ggplot2", "tidyr"))

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# paso 1: contamos los secuestros por año y liberación
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

#-----------------------------------------------------------------------------
# paso 2: creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

#----------------------

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

# -------------------------------
# filtramos valores faltantes en 'liberacion'
data <- data %>%
  filter(!is.na(liberacion))

# -------------------------------
# paso 1: agrupamos y contamos los secuestros por año y liberación
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

# -------------------------------
# paso 2: creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

#----------------------

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# contamos los secuestros por año y liberación
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

#-----------------------------------------------------------------------------
# creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

#----------------------

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# contamos los secuestros por año y liberación
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

#-----------------------------------------------------------------------------
# creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

#----------------------

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# contamos los secuestros por año y liberación
liberacion_anual <- data %>%
  group_by(año_secuestro, liberacion) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# calculamos el porcentaje de secuestros con y sin liberación para cada año
liberacion_anual <- liberacion_anual %>%
  group_by(año_secuestro) %>%
  mutate(porcentaje = (total_secuestros / sum(total_secuestros)) * 100) %>%
  ungroup()

# verificamos los datos
head(liberacion_anual)

#-----------------------------------------------------------------------------
# creamos la gráfica de barras apiladas en porcentajes
ggplot(liberacion_anual, aes(x = factor(año_secuestro), y = porcentaje, fill = liberacion)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) +
  labs(title = "Porcentaje de Secuestros con y sin Liberación por Año",
       x = "Año",
       y = "Porcentaje de Secuestros (%)",
       fill = "¿Hubo Liberación?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("Sí" = "#1f78b4", "No" = "#e31a1c"))

#----------------------

# cargamos las librerías necesarias
library(DBI)
library(RMySQL)
library(dplyr)
library(ggplot2)
library(tidyr)

# establecemos conexión a la base de datos
con <- dbConnect(RMySQL::MySQL(),
                 dbname = "noticias",
                 host = "localhost",
                 port = 3306,
                 user = "root",
                 password = "Soccer.8a")

# cargamos los datos de la tabla 'extracciones_filtradas'
data <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# convertimos 'año_secuestro' y 'mes_secuestro' a numérico si no lo están
data <- data %>%
  mutate(
    mes_secuestro = as.numeric(mes_secuestro),
    año_secuestro = as.numeric(año_secuestro)
  )

# filtramos datos válidos (meses entre 1 y 12)
data <- data %>%
  filter(!is.na(mes_secuestro), !is.na(año_secuestro), mes_secuestro >= 1 & mes_secuestro <= 12)

# creamos una columna de fecha (usando el primer día del mes)
data <- data %>%
  mutate(fecha_secuestro = as.Date(paste(año_secuestro, mes_secuestro, "01", sep = "-")))

#-----------------------------------------------------------------------------
# 1. contamos los secuestros por estado y seleccionamos los 6 con más secuestros
secuestros_estado <- data %>%
  group_by(estado) %>%
  summarise(total_secuestros = n(), .groups = "drop") %>%
  arrange(desc(total_secuestros)) %>%
  slice(1:6) # seleccionamos los 6 primeros

# vemos los 6 municipios seleccionados (opcional)
print(secuestros_estado)

# 2. obtenemos la lista de los 6 estados con más secuestros
top_estados <- secuestros_estado$estado

# 3. filtramos los datos para esos 6 estados
data_top_estados <- data %>%
  filter(estado %in% top_estados)

# 4. agrupamos datos por estado, año y tipo de secuestro
data_estado_anio_secuestro <- data_top_estados %>%
  group_by(estado, año_secuestro, tipo_secuestro) %>%
  summarise(total_secuestros = n(), .groups = "drop")

# 5. generamos gráficas separadas para cada estado
# creamos una lista de los 6 estados
estados_list <- unique(data_estado_anio_secuestro$estado)

# iteramos sobre cada estado y creamos una gráfica individual
for (estado_actual in estados_list) {
  
  # filtramos los datos para el estado actual
  datos_estado <- data_estado_anio_secuestro %>%
    filter(estado == estado_actual)
  
  # creamos la gráfica para el estado actual
  grafica_estado <- ggplot(datos_estado, aes(x = factor(año_secuestro), y = total_secuestros, fill = tipo_secuestro)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(
      title = paste("Distribución de Tipos de Secuestro por Año en", estado_actual),
      x = "Año",
      y = "Cantidad de Secuestros",
      fill = "Tipo de Secuestro"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(hjust = 0.5)
    )
  
  # mostramos la gráfica en el panel de Plots
  print(grafica_estado)
  
  # opcional: pausar brevemente entre gráficas para facilitar la visualización
  Sys.sleep(1) # pausa de 1 segundo entre gráficas
}

# cerramos la conexión a la base de datos
dbDisconnect(con)
