#install.packages(c("DBI", "tidyr", "RMySQL", "dplyr", "fastDummies", "dendextend", "factoextra", "FactoMineR"))
library(DBI)
library(tidyr)
library(RMySQL)
library(dplyr)
library(fastDummies)
library(dendextend)
library(factoextra)
library(FactoMineR)  # para realizar MCA


# establecemos la conexión a la base de datos
con <- dbConnect(MySQL(),
                 user = 'root',
                 password = 'Soccer.8a',  # reemplaza con tu contraseña
                 host = 'localhost',
                 dbname = 'noticias')

# cargamos los datos de la tabla 'extracciones_filtradas'
datos <- dbGetQuery(con, "SELECT * FROM extracciones_filtradas")

# filtramos los registros donde 'captura' es diferente de 'no especifico'
datos_filtrados <- datos %>%
  filter(captura != 'no especifico')

# paso 1: seleccionar los municipios relevantes

# contamos el número de incidentes por municipio
incidentes_por_municipio <- datos_filtrados %>%
  group_by(municipio) %>%
  summarise(num_incidentes = n()) %>%
  arrange(desc(num_incidentes))

# seleccionamos los 15 municipios con más incidentes
top_15_municipios <- incidentes_por_municipio %>%
  slice(1:15) %>%
  pull(municipio)

# seleccionamos los 5 municipios con menos incidentes (considerando municipios con al menos un incidente)
bottom_5_municipios <- incidentes_por_municipio %>%
  filter(num_incidentes > 0) %>%
  slice_tail(n = 5) %>%
  pull(municipio)

# combinamos los municipios seleccionados
municipios_seleccionados <- c(top_15_municipios, bottom_5_municipios)

# filtramos los datos para incluir solo los municipios seleccionados
datos_filtrados <- datos_filtrados %>%
  filter(municipio %in% municipios_seleccionados)

# verificamos el número de registros
print(paste("Número de registros seleccionados:", nrow(datos_filtrados)))

# paso 2: preparar los datos para la clusterización con MCA

# seleccionamos variables categóricas relevantes, incluyendo 'captura'
datos_categoricos <- datos_filtrados %>%
  select(estado, municipio, captor, lugar, tipo_secuestro, captura)

# convertimos variables categóricas a factores
datos_categoricos[] <- lapply(datos_categoricos, as.factor)

# realizamos el MCA
resultado_mca <- MCA(datos_categoricos, graph = FALSE)

# visualizamos la varianza explicada por los componentes
fviz_screeplot(resultado_mca, addlabels = TRUE, ylim = c(0, 50))

# decidimos cuántas dimensiones retener (por ejemplo, las primeras 5)
num_dimensiones <- 4
datos_reducidos <- resultado_mca$ind$coord[, 1:num_dimensiones]

# paso 3: realizar la clusterización jerárquica con datos reducidos

# calculamos la matriz de distancias
distancias <- dist(datos_reducidos, method = "euclidean")

# aplicamos clusterización jerárquica
cluster_jerarquico <- hclust(distancias, method = "complete")

# creamos etiquetas para el dendrograma
datos_categoricos$etiqueta <- paste(datos_categoricos$estado, datos_categoricos$municipio, sep = "-")

# asignamos las etiquetas al objeto de clusterización
cluster_jerarquico$labels <- datos_categoricos$etiqueta

# convertimos a un objeto dendrograma
dendro <- as.dendrogram(cluster_jerarquico)

# coloreamos las ramas según los clústeres
k <- 7  # ajusta este número según el resultado de la validación
dendro_coloreado <- color_branches(dendro, k = k)

# eliminamos las etiquetas del dendrograma coloreado
dendro_coloreado <- dendro_coloreado %>% set("labels", NULL)

# graficamos el dendrograma coloreado sin etiquetas
plot(dendro_coloreado, main = "Dendrograma de Clusterización Jerárquica con MCA",
     ylab = "Distancia", cex.lab = 0.8, cex.axis = 0.8)

# paso 4: validar el número de clústeres

# validamos el número de clústeres usando el método del codo
fviz_nbclust(datos_reducidos, FUN = hcut, method = "wss", k.max = 10) +
  labs(title = "Determinación del Número Óptimo de Clústeres (Método del Codo)")

# validamos el número de clústeres usando el índice de silueta
fviz_nbclust(datos_reducidos, FUN = hcut, method = "silhouette", k.max = 10) +
  labs(title = "Determinación del Número Óptimo de Clústeres (Índice de Silueta)")

# definimos el número de clústeres según la validación (ya definido arriba)
# k <- 4  # ajusta este número según el resultado de la validación

# cortamos el dendrograma para formar los clústeres
grupos <- cutree(cluster_jerarquico, k = k)

# agregamos los clústeres al conjunto de datos
datos_categoricos$cluster <- grupos

# paso 5: analizar e interpretar los clústeres

# resumimos las características de cada clúster con los valores más frecuentes, incluyendo 'captura'
resumen_clusters <- datos_categoricos %>%
  group_by(cluster) %>%
  summarise(
    n = n(),
    estado = names(sort(table(estado), decreasing = TRUE))[1],
    municipio = names(sort(table(municipio), decreasing = TRUE))[1],
    captor_principal = names(sort(table(captor), decreasing = TRUE))[1],
    lugar_principal = names(sort(table(lugar), decreasing = TRUE))[1],
    tipo_secuestro_principal = names(sort(table(tipo_secuestro), decreasing = TRUE))[1],
    captura_principal = names(sort(table(captura), decreasing = TRUE))[1]
  )

# vemos el resumen
print(resumen_clusters)

# paso 6: generar recomendaciones generales

# función para generar recomendaciones para el estado (sin 'captura_principal')
generar_recomendacion_estado <- function(estado, municipio, captor_principal, lugar_principal, tipo_secuestro_principal) {
  paste0("En el municipio de ", municipio,
         " del estado ", estado,
         ", se observa una alta incidencia de secuestros del tipo '", tipo_secuestro_principal,
         "', frecuentemente perpetrados por '", captor_principal,
         "'. Se recomienda al estado implementar medidas de seguridad enfocadas en '", lugar_principal,
         "'.")
}

# función para generar recomendaciones para la población (incluyendo 'captura_principal' y 'lugar_principal')
generar_recomendacion_poblacion <- function(estado, municipio, captura_principal, lugar_principal) {
  paste0("Se recomienda a la población del municipio de ", municipio,
         " en el estado ", estado,
         ", tener precaución ante posibles secuestros realizados mediante '", captura_principal,
         "' en '", lugar_principal,
         "'. Manténgase alerta y tome medidas preventivas.")
}

# generamos recomendaciones para cada clúster
resumen_clusters <- resumen_clusters %>%
  rowwise() %>%
  mutate(
    recomendacion_estado = generar_recomendacion_estado(estado, municipio, captor_principal, lugar_principal, tipo_secuestro_principal),
    recomendacion_poblacion = generar_recomendacion_poblacion(estado, municipio, captura_principal, lugar_principal)
  )

# paso 7: identificar casos aislados (outliers)

# calculamos las distancias de cada observación a su centroide de clúster
centroides <- datos_reducidos %>%
  as.data.frame() %>%
  mutate(cluster = datos_categoricos$cluster) %>%
  group_by(cluster) %>%
  summarise_all(mean)

# calculamos distancias a centroides
distancias_a_centroide <- mapply(function(row, cluster) {
  centroide <- centroides[centroides$cluster == cluster, -1]
  dist(rbind(as.numeric(row), as.numeric(centroide)))
}, split(datos_reducidos, seq(nrow(datos_reducidos))), datos_categoricos$cluster)

# añadimos las distancias al conjunto de datos
datos_categoricos$distancia_centroide <- distancias_a_centroide

# definimos un umbral para considerar casos aislados (por ejemplo, percentil 95)
umbral_outlier <- quantile(distancias_a_centroide, 0.95, na.rm = TRUE)

# identificamos los casos aislados
datos_outliers <- datos_categoricos %>%
  filter(distancia_centroide > umbral_outlier)

# paso 8: generar recomendaciones para casos aislados

if(nrow(datos_outliers) > 0) {
  # función para generar recomendaciones para casos aislados, incluyendo 'captura'
  generar_recomendacion_outlier <- function(estado, municipio, captor, lugar, tipo_secuestro, captura) {
    paste0("En el municipio de ", municipio, " del estado de ", estado, 
           ", se ha identificado un caso particular de secuestro de tipo '", tipo_secuestro, 
           "', realizado mediante '", captura,
           "', perpetrado por '", captor, 
           "' en el lugar '", lugar, 
           "'. Se recomienda investigar este caso en detalle y considerar medidas específicas.")
  }
  
  # generamos recomendaciones para los casos aislados
  datos_outliers <- datos_outliers %>%
    rowwise() %>%
    mutate(recomendacion = generar_recomendacion_outlier(estado, municipio, captor, lugar, tipo_secuestro, captura))
  
} else {
  print("No se identificaron casos aislados significativos.")
}

# preparamos las recomendaciones generales para insertar en la base de datos
recomendaciones_generales <- resumen_clusters %>%
  select(cluster, estado, municipio, recomendacion_estado, recomendacion_poblacion) %>%
  pivot_longer(
    cols = starts_with("recomendacion_"),
    names_to = "tipo_recomendacion",
    values_to = "recomendacion"
  ) %>%
  mutate(tipo_recomendacion = ifelse(tipo_recomendacion == "recomendacion_estado", "Estado", "Poblacion"))


# función para escapar cadenas de texto
escape_strings <- function(con, vec) {
  sapply(vec, function(x) {
    if(is.na(x)) {
      "NULL"
    } else {
      dbEscapeStrings(con, x)
    }
  })
}

# creamos la tabla 'recomendaciones' si no existe
dbSendQuery(con, "
  CREATE TABLE IF NOT EXISTS recomendaciones (
    id INT AUTO_INCREMENT PRIMARY KEY,
    tipo_recomendacion VARCHAR(50),
    cluster INT,
    estado VARCHAR(100),
    municipio VARCHAR(100),
    recomendacion TEXT
  )
")

# insertamos las recomendaciones generales
for(i in 1:nrow(recomendaciones_generales)) {
  tipo_rec <- escape_strings(con, recomendaciones_generales$tipo_recomendacion[i])
  cluster_rec <- ifelse(is.na(recomendaciones_generales$cluster[i]), "NULL", recomendaciones_generales$cluster[i])
  estado_rec <- escape_strings(con, recomendaciones_generales$estado[i])
  municipio_rec <- escape_strings(con, recomendaciones_generales$municipio[i])
  recomendacion_rec <- escape_strings(con, recomendaciones_generales$recomendacion[i])
  
  query <- sprintf("INSERT INTO recomendaciones (tipo_recomendacion, cluster, estado, municipio, recomendacion)
                    VALUES ('%s', %s, '%s', '%s', '%s')",
                   tipo_rec,
                   cluster_rec,
                   estado_rec,
                   municipio_rec,
                   recomendacion_rec)
  
  dbSendQuery(con, query)
}

# insertamos las recomendaciones de casos aislados
if(nrow(datos_outliers) > 0) {
  recomendaciones_outliers <- datos_outliers %>%
    select(estado, municipio, recomendacion) %>%
    mutate(
      tipo_recomendacion = "Caso Aislado",
      cluster = NA
    ) %>%
    # convertimos todas las columnas a tipo character
    mutate(across(everything(), as.character))
  
  for(i in 1:nrow(recomendaciones_outliers)) {
    tipo_rec <- escape_strings(con, recomendaciones_outliers$tipo_recomendacion[i])
    cluster_rec <- "NULL"
    estado_rec <- escape_strings(con, recomendaciones_outliers$estado[i])
    municipio_rec <- escape_strings(con, recomendaciones_outliers$municipio[i])
    recomendacion_rec <- escape_strings(con, recomendaciones_outliers$recomendacion[i])
    
    query <- sprintf("INSERT INTO recomendaciones (tipo_recomendacion, cluster, estado, municipio, recomendacion)
                      VALUES ('%s', %s, '%s', '%s', '%s')",
                     tipo_rec,
                     cluster_rec,
                     estado_rec,
                     municipio_rec,
                     recomendacion_rec)
    
    dbSendQuery(con, query)
  }
}
