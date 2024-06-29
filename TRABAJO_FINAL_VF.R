

rm(list = ls())

# instalar p necesarios

install.packages("haven")
install.packages("dplyr")
install.packages("FactoMineR")
install.packages("missMDA")
install.packages("factoextra")

# bibliotecas necesarias

library(haven)
library(dplyr)
library(FactoMineR)
library(missMDA)
library(factoextra)

# Bases de datos, descargadas de la pagian oficial del INE BOLIVIA
## fuente: https://anda.ine.gob.bo/index.php/catalog/99

ruta_casadas <- "C:/Users/RODRIGO/Desktop/TRABAJO FINAL/EPCVCM/EPCVCM_Casadas.sav"
ruta_personas <- "C:/Users/RODRIGO/Desktop/TRABAJO FINAL/EPCVCM/EPCVCM_Persona.sav"
ruta_separadas <- "C:/Users/RODRIGO/Desktop/TRABAJO FINAL/EPCVCM/EPCVCM_Separadas.sav"
ruta_solteras <- "C:/Users/RODRIGO/Desktop/TRABAJO FINAL/EPCVCM/EPCVCM_Solteras.sav"
ruta_vivienda <- "C:/Users/RODRIGO/Desktop/TRABAJO FINAL/EPCVCM/EPCVCM_Vivienda.sav"


casadas <- read_sav(ruta_casadas)
personas <- read_sav(ruta_personas)
separadas <- read_sav(ruta_separadas)
solteras <- read_sav(ruta_solteras)
vivienda <- read_sav(ruta_vivienda)

# Función para contar valores NA de las bases 
contar_na <- function(df) {
  sapply(df, function(x) sum(is.na(x)))
}

# Reemplazaremos los valores de "NA" con 0 en cada base de datos, se verifico que todas las variables contengas rangos a partir de 1

casadas[is.na(casadas)] <- 0
personas[is.na(personas)] <- 0
separadas[is.na(separadas)] <- 0
solteras[is.na(solteras)] <- 0
vivienda[is.na(vivienda)] <- 0

# Unificamos bases de datos de vivienda y personas
vivienda_personas <- left_join(vivienda, personas, by = "folio")

# Unificamos bases de datos de vivienda_personas con casadas, separadas y solteras según corresponda
vivienda_personas_casadas <- left_join(vivienda_personas, casadas, by = "folio")
vivienda_personas_separadas <- left_join(vivienda_personas, separadas, by = "folio")
vivienda_personas_solteras <- left_join(vivienda_personas, solteras, by = "folio")

# Combinamos todas las bases de datos en una sola llamada "violencia_final"
violencia_final <- bind_rows(vivienda_personas_casadas, vivienda_personas_separadas, vivienda_personas_solteras)

names(violencia_final)

# Distribución de edades
table(violencia_final$s2_03)
hist(violencia_final$s2_03, main = "Distribución de Edades", xlab = "Edad", ylab = "Frecuencia", col = "blue", breaks = 20)


# Distribución por sexo
table(violencia_final$s2_02)
barplot(table(violencia_final$s2_02), main = "Distribución por Sexo", xlab = "Sexo", ylab = "Frecuencia", col = c("pink", "lightblue"))

# Distribución de tipo de vivienda
table(violencia_final$s1_01)
barplot(table(violencia_final$s1_01), main = "Distribución de Tipo de Vivienda", xlab = "Tipo de Vivienda", ylab = "Frecuencia", col = "green")

# Acceso a servicios (ejemplo con agua potable) 
table(violencia_final$s1_07)
barplot(table(violencia_final$s1_07), main = "Acceso a Agua Potable", xlab = "Acceso a Agua Potable", ylab = "Frecuencia", col = "blue")

# Reemplazamos valores "NA" con 0 en la base de datos combinada
violencia_final[is.na(violencia_final)] <- 0

# Establecemos un vector con las columnas a eliminar, ya que estos no son valores numericos
columnas_a_eliminar <- c(
  "upm.x", "s1_06_otro", "s2_06_b1", "s2_07_3", "s5_04_a", "s5_06",
  "S1_1A_2_o", "S1_1A_6_o", "S1_05_o", "S1_11A_3_o", "S1_12_06_o", "S1_18_9_o",
  "S1_21A_4_o", "S1_22_06_o", "S1_28_9_o", "S2_9A_2_o", "S3_08_12_o", "S3_11_o",
  "S3_19_11_o", "S3_25_11_o", "S4_1_8_o", "S6_08_11_o", "S8_1_6_o", "S8_7A_01_o",
  "S8_7A_05_o", "S8_7A_09_o", "S3_34_7_o_CA_SO", "s1_02_otro", "s1_07_otro",
  "s2_06_c1", "s2_08", "s5_04_b", "s5_12_1k", "S1_1A_3_o", "S1_02_05_o",
  "S1_08_9_o", "S1_11A_4_o", "S1_12_11_o", "S1_21A_1_o", "S1_21A_5_o", "S1_22_11_o",
  "S2_1_1", "S2_9A_3_o", "S3_8A_12_o", "S3_12_9_o", "S3_20_11_o", "S3_27_o",
  "S6_02_o", "S6_09_11_o", "S8_2_5_o", "S8_7A_02_o", "S8_7A_06_o", "S8_7A_10_o",
  "s1_03_otro", "upm.y", "s2_07_1", "s2_09_1", "s5_05_a", "s6_05_otro",
  "S1_1A_4_o", "S1_02_10_o", "S1_11A_1_o", "S1_11A_5_o", "S1_13_9_o", "S1_21A_2_o",
  "S1_21A_6_o", "S1_23_9_o", "S2_1_2", "S3_01_o_CA", "S3_09_8_o", "S3_13_5_o",
  "S3_21_6_o", "S3_31_11_o", "S6_04_11_o", "S6_10_8_o", "S8_3_6_o", "S8_7A_03_o",
  "S8_7A_07_o", "S4_1_7_o", "s1_05_otro", "s2_06_a1", "s2_07_2", "s5_03_otro",
  "s5_05_b", "S1_1A_1_o", "S1_1A_5_o", "S1_03_9_o", "S1_11A_2_o", "S1_11A_6_o",
  "S1_15_o", "S1_21A_3_o", "S1_21A_7_o", "S1_25_o", "S2_9A_1_o", "S3_02_o_CA",
  "S3_9A_8_o", "S3_17_7_o", "S3_23_o", "S3_32_7_o", "S6_05_5_o", "S7_3_o",
  "S8_5_o", "S8_7A_04_o", "S8_7A_08_o", "S3_34_1_o_SE"
)

# Eliminar las columnas no deseadas
violencia_final <- violencia_final[, !names(violencia_final) %in% columnas_a_eliminar]

# Convertir columnas no numéricas a numéricas 
numeric_cols <- sapply(violencia_final, is.numeric)
for (col in names(violencia_final)[!numeric_cols]) {
  if (all(grepl("^\\d+$", as.character(violencia_final[[col]])))) {
    violencia_final[[col]] <- as.numeric(as.character(violencia_final[[col]]))
  }
}

# Reemplazar NA con 0 en columnas numéricas en base combinada
for (col in names(violencia_final)[numeric_cols]) {
  violencia_final[is.na(violencia_final[[col]]), col] <- 0
}

print(head(violencia_final))

numeric_columns_names <- names(Filter(is.numeric, violencia_final))

# Estandarizamos los datos numéricos
violencia_final_numeric <- violencia_final[, numeric_columns_names]
violencia_final_scaled <- scale(violencia_final_numeric)

# Imputamos valores faltantes (si existen)
violencia_final_scaled[is.na(violencia_final_scaled)] <- 0

# Realizamos el Análisis de Componentes Principales (ACP)
pca_result <- PCA(violencia_final_scaled, graph = FALSE)

# Mostramos un resumen de los resultados del ACP
print(summary(pca_result))

# Visualizar la varianza explicada por cada componente principal
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

# Visualizar los individuos en el plano de los dos primeros componentes principales
fviz_pca_ind(pca_result,
             col.ind = "cos2", # Color por calidad de representación
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Evita la sobreposición de texto
)

# Visualizar las variables en el plano de los dos primeros componentes principales
fviz_pca_var(pca_result,
             col.var = "contrib", # Color por contribución a los componentes
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Evita la sobreposición de texto
)

# Visualizar el biplot de individuos y variables
fviz_pca_biplot(pca_result,
                repel = TRUE, # Evita la sobreposición de texto
                col.var = "#2E9FDF", # Color de las variables
                col.ind = "#696969"  # Color de los individuos
)
