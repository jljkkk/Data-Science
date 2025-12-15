
#Importación de datos y asignación de tipos

if(!require(readxl)){install.packages("readxl")}
library(readxl)

if(!require(psych)){install.packages("psych")}
library(psych) 

datos <- read_excel("D:/PrepDatos/DatosEleccionesEspaña.xlsx")

df <- as.data.frame(datos)

#Conocer los tipos de variables
str(df)
#Conocer las distintas categorias en densidad
unique(df$Densidad)

#Convertir las categóricas en factores
df$AbstencionAlta <- as.factor(df$AbstencionAlta)
df$Izquierda <- as.factor(df$Izquierda)
df$Derecha <- as.factor(df$Derecha)

df$CCAA <- as.factor(df$CCAA)
df$CodigoProvincia <- as.factor(df$CodigoProvincia)
df$ActividadPpal <- as.factor(df$ActividadPpal)
df$Densidad <- factor(df$Densidad, 
                         levels = c("MuyBaja", "Baja","Alta"),
                         ordered = TRUE)

#Análisis descriptivo

dim(df)

# 8117 filas y 41 columnas

str(df)

summary(df)

describe(df)

head(df)

#El maximo en SameComAutonPtge es 127.16, con lo que claramente es erróneo,
# y los mínimos en ForeignerPtge y Age_over65_pct son negativos,
#que carece de sentido.

#Corrección de datos erróneos

sum( df$SameComAutonPtge > 100 | df$ForeignersPtge < 0 | df$Age_over65_pct < 0)

#En total hay 657 (puede haber un municipio que contenga varios errores, y
# ha sido contado varias veces) errores.

#Al ser una cantidad considerable de errores, eliminarlos directamente no sería
#adecuado, con lo que vamos a convertirlos en NA's, y posteriormente se valorará
#qué hacer o cómo imputarlos

df$SameComAutonPtge[df$SameComAutonPtge > 100] <- NA
df$ForeignersPtge[df$ForeignersPtge < 0] <- NA
df$Age_over65_pct[df$Age_over65_pct < 0] <- NA

summary(df)



#Detección de outliers


atipicos <- function(x,k){ 
  if(is.data.frame(x)==T){ 
    a <- x[,which(sapply(x, is.numeric)==T)] 
    for (i in 1:ncol(a)) { 
      sup <- c(mean(a[,i],na.rm = T)+(sd(a[,i],na.rm = T)*k)) 
      inf <- c(mean(a[,i],na.rm = T)-(sd(a[,i],na.rm = T)*k)) 
      outliers <- a[a[, i] > sup | a[, i] < inf, i] 
      if (length(outliers) > 0) { 
        cat(" Variable:", colnames(a)[i],"\n","Número de 
outliers:",length(outliers), "\n") 
      }else{next} 
    } 
  }else{ 
    cat("ERROR: El objeto no es un data frame") 
  } 
} 

#Outliers univariantes
atipicos(df,3)

#Visualización de outliers en algunas variables

grafico_control <- function(data,var,sigma,ver){ 
  # data = data.frame 
  # var = nombre de la variable que se quiere 
  # sigma = numero de desv. típicas para el intervalo de control. 
  # ver = mostrar o no por pantalla los valores atípicos. 
  if(is.numeric(data[,var]==F)){ 
    cat("Error: la variable tiene que ser numérica") 
  }else{ 
    valores <- data[,var] 
    up <- mean(valores,na.rm = T)+sigma*sd(valores,na.rm = T) 
    down <- mean(valores,na.rm = T)-sigma*sd(valores,na.rm = T) 
    if(diff(range(valores,na.rm  = T))<(up-down)){ 
      plot(valores,type = "l", ylim = c(down-sd(valores), 
                                        up+sd(valores)),xlab 
           = "Casos", 
           ylab = "", main = var) 
      abline(h=up,col = "red",lty=2, lwd = 2) 
      abline(h=down,col = "red",lty=2, lwd = 2) 
    }else{ 
      plot(valores,type = "l",xlab = "Casos", 
           ylab = "") 
      abline(h=up,col = "red",lty=2, lwd = 2) 
      abline(h=down,col = "red",lty=2, lwd = 2) 
    } 
  } 
  casos_up <- matrix(c(which(valores>up),valores[which(valores>up)]),ncol 
                     = 2) 
  colnames(casos_up) <- c("Casos","Valores") 
  casos_down <- 
    matrix(c(which(valores<down),valores[which(valores<down)]),ncol = 2) 
  colnames(casos_down) <- c("Casos","Valores") 
  dat <- rbind(casos_up,casos_down) 
  if(ver==1&length(c(casos_up,casos_down))>0){ 
    return(dat) 
  }else{return(cat(""))} 
}


out_population <- grafico_control(df,"Population",3,1)
#Municipios que son outliers en población
df$Name[out_population[,1]]

out_Foreigners <- grafico_control(df,"ForeignersPtge",3,1)
#Municipios que son outliers en el porcentaje de extranjeros
df$Name[out_Foreigners[,1]]
df$CCAA[out_Foreigners[,1]]
# Si restringimos a los municipios que tienen un porcentaje mayor
# de 50%, la gran mayoría de ellos pertenecen a la C. Valenciana
df$CCAA[which(df$ForeignersPtge > 50)]

#Los picos más destacados se encuentran en la Comunidad Valenciana,
# y probablemente se debe a la inmensa cantidad de turistas o 
# los que veranenean en la zona


out_empresas <- grafico_control(df,"totalEmpresas",3,1)
#Municipios que son ouliers en numero de empresas
df$Name[out_empresas[,1]]
#Se puede observar que los outliers en este caso son las
#principales (grandes) ciudades de España



#Análisis de los valores perdidos

#Visualización de los NA's
column_missing <- c()
for (i in 1:ncol(df)) { 
  n <- length(which(is.na(df[,i]))) 
  if (n >0){
  cat("La variable",colnames(df)[i],"presenta un total de",n,"NAs\n") 
  column_missing <- cbind(column_missing, colnames(df)[i])
  
  }
}
column_missing
if(!require(visdat)){install.packages("visdat")}
library(visdat) 
vis_miss(df[,column_missing])



#En la variable ForeignerPtge es donde hay más NAs, que son los daots
#"borrados" debido a la imposibilidad de los porcentajes. Primero vamos
#a estudiar si existe alguna relación con otras variables.


cor(df$ForeignersPtge,df$SameComAutonPtge, use = "complete.obs")
# La correlación es de -0.6636446, que es alta, vamos a comprobar
# si hay alguna relación con los valores perdidos

casos_na <-  df[which(is.na(df$ForeignersPtge)),"SameComAutonPtge"]
casos_sin_na <- df[which(!is.na(df$ForeignersPtge)),"SameComAutonPtge"]

#se comprueba el supuesto de normalidad: 
print(ks.test(df$ForeignersPtge,"pnorm")) 

# dado que p < 0.05, se concluye que ForeignerPtge no presenta una 
# distribución normal. 
# se procede a contrastar las medias con la prueba de Mann-Whitney. 
print(wilcox.test(casos_na, casos_sin_na))

# se observa como, en efecto, no se rechaza la hipótesis nula de igualdad 
#de medias, concluyendo que los valores NAs en ForeignerPtge no dependen de 
#los valores de SameComAutonPtge. 

#Probamos también con población

casos_na <-  df[which(is.na(df$ForeignersPtge)),"Population"]
casos_sin_na <- df[which(!is.na(df$ForeignersPtge)),"Population"]

#se comprueba el supuesto de normalidad: 
print(ks.test(df$ForeignersPtge,"pnorm")) 

# dado que p < 0.05, se concluye que Petal Length no presenta una 
# distribución normal. 
# se procede a contrastar las medias con la prueba de Mann-Whitney. 
print(wilcox.test(casos_na, casos_sin_na))

plot(density(casos_na), col = "red", main = "Funciones de densidad para 
Population de acuerdo con la presencia\no no los NAs de ForeignersPtge",
ylab = "", xlab = "") 
lines(density(casos_sin_na), col = "blue") 
legend(7, 0.24, legend=c("Con NAs", "Sin NAs"), 
       col=c("red", "blue"), lty=1:2, cex=0.6) 


library(rpart)
if(!require(rpart.plot)){install.packages("rpart.plot")}
library(rpart.plot) 
if(!require(naniar)){install.packages("naniar")}
library(naniar) 

#Se estudia si hay algún patron sobre los NA's en general.

df_prob_missing <- add_prop_miss(df) 

# se construye el árbol 
arbol <- rpart(prop_miss_all ~ ., data = df_prob_missing) 
print(prp(x = arbol, type = 4, extra = 101, prefix = "Prop. Miss = ")) 

#El árbol obtenido indica que no hay relación entre las variables con 
#la presencia de los NAs

df_foreigners <- df
df_foreigners$fo_missing <- is.na(df$ForeignersPtge)

arbol <- rpart(fo_missing ~ ., data = df_foreigners)
print(prp(x = arbol, type = 4, extra = 101, prefix = "Prop. Miss = ")) 

df_indu <- df
df_indu$indu_missing <- is.na(df$Industria)
arbol <- rpart(indu_missing ~ ., data = df_indu)
print(prp(x = arbol, type = 4, extra = 101, prefix = "Prop. Miss = ")) 

df_PI <- df
df_PI$PI_missing <- is.na(df$PersonasInmueble)
arbol <- rpart(PI_missing ~ ., data = df_PI)
print(prp(x = arbol, type = 4, extra = 101, prefix = "Prop. Miss = ")) 

#Imputación de los valores perdidos

#Hay 3 NAs en Age_over65_pct, pero como conocemos los porcentajes
#de los otros grupos, se puede calcular mediante ellos, con lo que
#no es necesario aplicar los métodos de imputación vistos en clase.

pos_age <- which(is.na(df$Age_over65_pct))

#Hay un problema a la hora de llamar la variable Age_0-4_Ptge por
#el signo de la resta, lo llamamos por su indice
colnames(df)[13]
df[,13][pos_age]

#En estas 3 posiciones suman más del 100%, la segunda sólo 0.002 más
#, que probablemente se debe a los redondeos, pero los otros dos son 
#erróneos. Aunque es un poco raro que solo haya gente de 19-65 años.
df$Age_19_65_pct[pos_age] + df$Age_under19_Ptge[pos_age]

#Vamos a detectar si hay más problemas, dejando un margen de error de 0.1%.
total <- df$Age_19_65_pct + df$Age_over65_pct + df$Age_under19_Ptge
which(total > 100.1 & total < 99.9)

#Para la segunda se mantienen, y el procentaje de los mayores de 65 sería 0.
df$Age_over65_pct[pos_age[2]] <- 0

pos_age <- pos_age[-2]

df$Age_19_65_pct[pos_age] <- NA
df$Age_under19_Ptge[pos_age] <- NA

#Para las otras dos posiciones las vamos a imputar con la media, ya que con 
#otros métodos darían problemas para que sumen 100%

media_19_65 <- mean(df$Age_19_65_pct[which(!is.na(df$Age_19_65_pct))])
media_under_19 <- mean(df$Age_under19_Ptge[which(!is.na(df$Age_under19_Ptge))])
media_over_65 <- mean(df$Age_over65_pct[which(!is.na(df$Age_over65_pct))])

df$Age_19_65_pct[pos_age] <- media_19_65
df$Age_under19_Ptge[pos_age] <- media_under_19
df$Age_over65_pct[pos_age] <- media_over_65


column_missing

#Para la variable ForeignPtge imputamos por la media, pero no la 
#la nacional, sino la media de la CCAA que pertenece
pos_foreign <- which(is.na(df$ForeignersPtge))
CCAA <- unique(df$CCAA[pos_foreign])
media_CCAA <- c()

for ( i in 1:length(CCAA)){
  m <- mean(df$ForeignersPtge[which(df$CCAA == CCAA[i])],na.rm=TRUE)
  media_CCAA <- cbind(media_CCAA,m)
}

media_CCAA

for ( i in 1:length(CCAA)){
  pos_CCAA <- which(df$CCAA==CCAA[i])
  pos_modificar <- pos_CCAA[which(is.na(df$ForeignersPtge[pos_CCAA]))]
  df$ForeignersPtge[pos_modificar] <- media_CCAA[i]
}

which(is.na(df$ForeignersPtge))

imputar_media <- function(data,variable){
  pos_na <- which(is.na(data[[variable]]))
  data[[variable]][pos_na] <- mean(data[[variable]], na.rm = TRUE)
  return (data)
}

imputar_mediana <- function(data,variable){
  pos_na <- which(is.na(data[[variable]]))
  data[[variable]][pos_na] <- median(data[[variable]], na.rm = TRUE)
  return (data)
}

#Para la variable SameComAutonPtge, al presentar sólo 3 NAs, se les imputan
#con la media nacional

df <- imputar_media(df,"SameComAutonPtge")



#Se imputan por la mediana las variables como superficie,
#PobChange_pct, Industria, Construcción,PersonasInmueble, 
#Serivicios, ComercTTEHosteleria

variables_mediana <- c("SUPERFICIE",
                       "PersonasInmueble","totalEmpresas","Industria",
                       "Construccion","Servicios","PobChange_pct")

for (var in variables_mediana) {
  df <- imputar_mediana(df, var)
}

#Las variables relacionadas con números de empresas se puede
#obtener mediante las otras, pero muchas de ellas no se
#pueden calcularlas debido a la presencia de NAs en otras
#variables, se simplifica poniendo la mediana, aunque
#en algunos casos puede dar valores que sean imposibles,
#pero al ser pocos, no afectará mucho a los resultados.

pos <- which(is.na(df$ComercTTEHosteleria))
df$ComercTTEHosteleria[pos] <- df$totalEmpresas[pos] - df$Construccion[pos] - df$Servicios[pos] - df$Industria[pos]

#Las variables Densidad y Pob2010 (con las imputadas) se pueden calcular mediante
#otras variables

pos_den <- which(is.na(df$Densidad))

for (i in pos_den){
  d <- df$Population[i] / df$SUPERFICIE[i]
  if (d < 1){
    df$Densidad[i] <- "MuyBaja"
  }
  else{
    if (d <= 5){
      df$Densidad[i] <- "Baja"
    }
    else{
      df$Densidad[i] <- "Alta"
    }
  }
}

unique(df$Densidad)


pos <- which(is.na(df$Pob2010))
for (i in pos){
  df$Pob2010[i] <- df$Population[i] * (1+df$PobChange_pct[i]/100)
}


pos_Inmue <-which(is.na(df$inmuebles))
pos_Inmue
for (i in pos_Inmue){
  value <- df$Population[i] / df$PersonasInmueble[i]
  df$inmuebles[i] <- trunc(value)
}



#Detección de outliers con isolation forest
if(!require(isotree)){install.packages("isotree")}
library(isotree)

# 1. Preparamos los datos (solo numéricos)
datos_modelo <- df[, c("Pob2010", "Industria", "Construccion", "Servicios")]

# 2. Entrenamos el modelo
iso_model <- isolation.forest(datos_modelo, ntrees = 100, nthreads = 1)

# 3. Predecimos el score de anomalía para cada fila
# El resultado es un valor entre 0 y 1
df$score_anomalia <- predict(iso_model, datos_modelo)

# 4. Decidimos quién es outlier (ejemplo: el top 5% más raro)
umbral <- quantile(df$score_anomalia, 0.95)
df$es_outlier_if <- df$score_anomalia > umbral

# Ver los municipios más raros
head(df[order(-df$score_anomalia), ])

#Mahalanobis

#Primero vamos a detectar que variables siguen mas o menos una distribucion normal.

head(df)

#Transformaciones logaritmicas para algunas variables (se crean nuevas
#variables en vez de reemplazarlas) 


vars_a_log <- c("Population", "TotalCensus", "totalEmpresas", "inmuebles", "SUPERFICIE")

# Creamos nuevas columnas con el prefijo 'log_'
#df[paste0("log_", vars_a_log)] <- lapply(df[vars_a_log], function(x) log1p(x))


#Relaciones con las variables objetivo

if(!require(corrplot)){install.packages("corrplot")}
library(corrplot)

# Selecciona solo numéricas y elimina las del "futuro" 
# El target es Izda_Pct
vars_num <- df[, sapply(df, is.numeric)]
vars_a_borrar <- c("Dcha_Pct", "Otros_Pct", "Izquierda", "Derecha") # Variables trampa
vars_num <- vars_num[, !names(vars_num) %in% vars_a_borrar]

# Calcular correlación
matriz_corr <- cor(vars_num, use = "complete.obs")

# Dibujar
corrplot(matriz_corr, method = "color", type = "upper", tl.col = "black", tl.cex = 0.6)

if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)

# Ejemplo: Voto izquierda según Actividad Principal del municipio
ggplot(df, aes(x = ActividadPpal, y = Izda_Pct, fill = ActividadPpal)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Distribución del Voto de Izquierda por Actividad Económica")

ggplot(df, aes(x = AgricultureUnemploymentPtge, y = Izda_Pct)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") # Línea de tendencia curva

ggplot(df, aes(x = UnemployMore40_Ptge, y = Izda_Pct)) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "loess") # Línea de tendencia curva

#Para la dicotomica

table(df$AbstencionAlta)
prop.table(table(df$AbstencionAlta))

ggplot(df, aes(x = AbstencionAlta, y = Population, fill = AbstencionAlta)) +
  geom_boxplot() +
  scale_y_log10() + # Escala logarítmica para ver mejor las diferencias
  labs(title = "¿Influye el tamaño del municipio en la Abstención?",
       y = "Población (Log)", x = "Abstención Alta") +
  theme_minimal()

ggplot(df, aes(x = AbstencionAlta, y = UnemployMore40_Ptge, fill = AbstencionAlta)) +
  geom_boxplot() +
  labs(title = "Relación entre Desempleo (>40 años) y Abstención",
       y = "% Desempleo >40", x = "Abstención Alta")


tabla_geo <- table(df$CCAA, df$AbstencionAlta) # O usa CodigoProvincia
print(tabla_geo)

# Gráfico de barras apiladas al 100%
ggplot(df, aes(x = CCAA, fill = AbstencionAlta)) +
  geom_bar(position = "fill") +
  labs(y = "Proporción", title = "Abstención por Comunidad Autónoma") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))





#Construcción de los modelos de árboles


#Partición del dataset en train y test
if(!require(caret)){install.packages("caret")}
library(caret)

set.seed(123)

#Se hace una partición intentando que las dos
#tengan distribuciones similares
trainIndex <- createDataPartition(df$Izda_Pct, p = .7, list = FALSE)
train_cuant <- df[ trainIndex,]
test_cuant  <- df[-trainIndex,]

index <- createDataPartition(df$AbstencionAlta, p = 0.7, list = FALSE)
train_cual <- df[index, ]
test_cual  <- df[-index, ]

vars_a_borrar <- c("Dcha_Pct", "Otros_Pct", "Izquierda", "Derecha", "score_anomalia","es_outlier_if","Name")
train_cuant <- train_cuant[, !(names(train_cuant) %in% vars_a_borrar)]
test_cuant <- test_cuant[, !(names(test_cuant) %in% vars_a_borrar)]

vars_a_borrar_2 <- c("AbstentionPtge","score_anomalia","es_outlier_if","Name")
train_cual <- train_cual[, !(names(train_cual) %in% vars_a_borrar_2)]
test_cual <- test_cual[, !(names(test_cual) %in% vars_a_borrar_2)]

#Comprobación de que tienen distribuciones parecidas
ggplot(train_cuant, aes(x = Izda_Pct)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") + # Ajusta 'binwidth' al tamaño de tus datos
  labs(title = "Distribución de columna_variable",
       x = "Valores de la Variable",
       y = "Frecuencia") +
  theme_minimal()


ggplot(test_cuant, aes(x = Izda_Pct)) +
  geom_histogram(binwidth = 5, fill = "skyblue", color = "black") + # Ajusta 'binwidth' al tamaño de tus datos
  labs(title = "Distribución de columna_variable",
       x = "Valores de la Variable",
       y = "Frecuencia") +
  theme_minimal()

prop_train <- prop.table(table(train_cual$AbstencionAlta))
prop_test <- prop.table(table(test_cual$AbstencionAlta))

print(prop_train)
print(prop_test)


# Control de validación cruzada (común para ambos) para la clasificación
ctrl_cv_class <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,               # Solo relevante para clasificación
  summaryFunction = twoClassSummary, 
  savePredictions = "final",
  returnResamp    = "all"  
)

ctrl_cv_reg <- trainControl(
  method = "cv",
  number = 10,
  returnResamp = "all"
)

grid_rpart <- expand.grid(
  cp = seq(0.00, 0.05, by = 0.005) 
)

if(!require(MLmetrics)){install.packages("MLmetrics")}
library(MLmetrics) 

#Modelo de clasificación

levels(train_cual$AbstencionAlta) <- c("No", "Si")
levels(test_cual$AbstencionAlta) <- c("No", "Si")

colnames(train_cual) <- make.names(colnames(train_cual))
fit_rpart <- train(
  AbstencionAlta ~ .,
  data      = train_cual,
  method    = "rpart",
  trControl = ctrl_cv_class,
  tuneGrid  = grid_rpart, 
  metric    = "ROC"
)

print(fit_rpart) 

res_rpart <- fit_rpart$resample

valores_cp <- unique(grid_rpart$cp)
n <- length(valores_cp)
roc_mean <- numeric(n)
roc_sd   <- numeric(n)

for (i in 1:n){
  subset_res <- res_rpart[res_rpart$cp == valores_cp[i], ]
  roc_mean[i] <- mean(subset_res$ROC, na.rm=TRUE)
  roc_sd[i]   <- sd(subset_res$ROC, na.rm=TRUE)
}

resumen_manual <- data.frame(cp = valores_cp, ROC = roc_mean, SD = roc_sd)
print(resumen_manual)

plot(resumen_manual$cp, resumen_manual$ROC, type="b", pch=19, 
     col="blue", main="Evolución CP vs ROC", xlab="CP", ylab="Mean ROC")
# (Opcional) Líneas para ver la desviación típica
lines(resumen_manual$cp, resumen_manual$ROC + resumen_manual$SD, col="red", lty=2)
lines(resumen_manual$cp, resumen_manual$ROC - resumen_manual$SD, col="red", lty=2)
legend("topright", legend=c("Media ROC", "+/- SD"), col=c("blue", "red"), lty=c(1,2))


grid_parametros_extra <- expand.grid(
  maxdepth = c(5, 10, 30),   # Árbol pequeño, mediano, y libre (profundo)
  minsplit = c(10, 20, 50)   # Exigencia baja, media y alta para dividir
)

registro_experimentos <- data.frame()

cat("--- Iniciando búsqueda de hiperparámetros extendida ---\n")

# Bucle para probar cada combinación de maxdepth y minsplit
for(i in 1:nrow(grid_parametros_extra)) {
  
  # Extraemos los parámetros de esta iteración
  d_actual <- grid_parametros_extra$maxdepth[i]
  m_actual <- grid_parametros_extra$minsplit[i]
  
  grid_cp <- expand.grid(cp = seq(0.000, 0.050, length.out = 5))
  
  # Definimos el control del árbol
  control_arbol <- rpart.control(maxdepth = d_actual, minsplit = m_actual)
  
  fit_temp <- train(
    AbstencionAlta ~ .,
    data      = train_cual,
    method    = "rpart",
    trControl = ctrl_cv_class,
    tuneGrid  = grid_cp,
    metric    = "ROC",       
    control   = control_arbol 
  )
  
  # Guardamos el MEJOR resultado de este "mini-entrenamiento"
  best_res <- fit_temp$results[which.max(fit_temp$results$ROC), ] 
  
  # Añadimos al registro general
  resumen_fila <- data.frame(
    ID_Config = i,
    MaxDepth  = d_actual,
    MinSplit  = m_actual,
    Best_CP   = best_res$cp,
    ROC_Train = best_res$ROC,     # Rendimiento en CV (Train)
    SD_ROC    = best_res$ROCSD    # Desviación típica (Robustez)
  )
  
  registro_experimentos <- rbind(registro_experimentos, resumen_fila)
  
  cat("Config", i, ": Depth", d_actual, "| Split", m_actual, "-> ROC:", round(best_res$ROC, 4), "\n")
}


# Ordenar por mejor ROC
ordenado <- registro_experimentos[order(-registro_experimentos$ROC_Train), ]
print(head(ordenado))

# Definimos los parámetros de los 3 elegidos
configs_finales <- list(
  c(depth = 30, split = 50, cp = 0, nombre = "ID_9_Optimo"),
  c(depth = 10, split = 20, cp = 0, nombre = "ID_5_Equilibrado"),
  c(depth = 5,  split = 50, cp = 0, nombre = "ID_7_Simple")
)

resultados_test_final <- data.frame()

colnames(test_cual) <- make.names(colnames(test_cual))
for(conf in configs_finales) {
  
  fit_final <- train(
    AbstencionAlta ~ .,
    data      = train_cual,
    method    = "rpart",
    trControl = trainControl(method="none", classProbs=TRUE), 
    tuneGrid  = expand.grid(cp = as.numeric(conf["cp"])),
    control   = rpart.control(maxdepth = as.numeric(conf["depth"]), 
                              minsplit = as.numeric(conf["split"]))
  )
  
  preds_prob <- predict(fit_final, newdata = test_cual, type = "prob")
  preds_raw  <- predict(fit_final, newdata = test_cual, type = "raw")
  
  roc_obj <- roc(test_cual[["AbstencionAlta"]], preds_prob[,1], levels = rev(levels(test_cual[["AbstencionAlta"]])))
  auc_test <- auc(roc_obj)
  
  cm <- confusionMatrix(preds_raw, test_cual[["AbstencionAlta"]])
  
  # Guardar resultados
  resultados_test_final <- rbind(resultados_test_final, data.frame(
    Modelo = conf["nombre"],
    AUC_Test = as.numeric(auc_test),
    Acc_Test = cm$overall["Accuracy"],
    Kappa_Test = cm$overall["Kappa"]
  ))
  
}

roc_train_values <- c(
  0.7844820, # ID 9 (Optimo)
  0.7723108, # ID 5 (Equilibrado)
  0.7542188  # ID 7 (Simple)
)

comparativa <- resultados_test_final
comparativa$AUC_Train <- roc_train_values

comparativa$Diferencia <- comparativa$AUC_Train - comparativa$AUC_Test
comparativa$Porcentaje_Caida <- (comparativa$Diferencia / comparativa$AUC_Train) * 100

# Reordenamos columnas para ver claro
comparativa <- comparativa[, c("Modelo", "AUC_Train", "AUC_Test", "Diferencia", "Porcentaje_Caida")]

cat("\n--- TABLA DE GENERALIZACIÓN (TRAIN vs TEST) ---\n")
print(comparativa)


library(tidyr) # Para transformar los datos para el gráfico

# Preparamos datos para ggplot (formato largo)
datos_plot <- comparativa[, c("Modelo", "AUC_Train", "AUC_Test")]
datos_long <- pivot_longer(datos_plot, cols = c("AUC_Train", "AUC_Test"), 
                           names_to = "Conjunto", values_to = "AUC")

ggplot(datos_long, aes(x = Modelo, y = AUC, fill = Conjunto)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.6) +
  geom_text(aes(label = round(AUC, 3)), 
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3.5) +
  scale_fill_manual(values = c("AUC_Train" = "#4E84C4", "AUC_Test" = "#C44E52")) +
  ylim(0, 1) +
  labs(title = "Análisis de Sobreajuste: Train vs Test",
       subtitle = "Si la barra roja es mucho más baja que la azul -> OVERFITTING",
       y = "AUC (Area Under Curve)") +
  theme_minimal()




# Regresión
colnames(test_cuant) <- make.names(colnames(test_cuant))
colnames(train_cuant) <- make.names(colnames(test_cuant))

registro_regresion <- data.frame()

for(i in 1:nrow(grid_parametros_extra)) {
  
  d_actual <- grid_parametros_extra$maxdepth[i]
  m_actual <- grid_parametros_extra$minsplit[i]
  
  set.seed(123)
  fit_temp <- train(
    Izda_Pct ~ .,
    data      = train_cuant,
    method    = "rpart",
    trControl = ctrl_cv_reg,
    tuneGrid  = expand.grid(cp = seq(0.000, 0.050, length.out = 5)),
    metric    = "RMSE",       
    control   = rpart.control(maxdepth = d_actual, minsplit = m_actual)
  )
  
  best_res <- fit_temp$results[which.min(fit_temp$results$RMSE), ] 
  
  fila <- data.frame(
    ID = i,
    MaxDepth = d_actual, MinSplit = m_actual, Best_CP = best_res$cp,
    RMSE_Train = best_res$RMSE, 
    SD_RMSE    = best_res$RMSESD, 
    R2_Train   = best_res$Rsquared
  )
  registro_regresion <- rbind(registro_regresion, fila)
}

# Ordenamos por MENOR error (RMSE ascendente)
ordenado_reg <- registro_regresion[order(registro_regresion$RMSE_Train), ]

print(ordenado_reg)

candidatos_finales <- list(
  c(id=8, depth=10, split=50, cp=0.00, nombre="ID_8_Ganador"),
  c(id=1, depth=5,  split=10, cp=0.00, nombre="ID_1_Robusto"),
  c(id=5, depth=10, split=20, cp=0.00, nombre="ID_5_Alternativo")
)


resultados_test_reg <- data.frame()

for(cand in candidatos_finales) {
  
  fit_final <- train(
    Izda_Pct ~ .,
    data      = train_cuant,
    method    = "rpart",
    trControl = trainControl(method="none"), 
    tuneGrid  = expand.grid(cp = as.numeric(cand["cp"])),
    control   = rpart.control(maxdepth = as.numeric(cand["depth"]), 
                              minsplit = as.numeric(cand["split"]))
  )
  
  # 2. Predecir en TEST
  preds <- predict(fit_final, newdata = test_cuant)
  
  diferencia <- test_cuant[["Izda_Pct"]] - preds
  
  mi_rmse <- sqrt(mean(diferencia^2, na.rm = TRUE))
  mi_mae <- mean(abs(diferencia), na.rm = TRUE)
  
  sst <- sum((test_cuant[["Izda_Pct"]] - mean(test_cuant[["Izda_Pct"]], na.rm=TRUE))^2, na.rm=TRUE)
  sse <- sum(diferencia^2, na.rm=TRUE)
  mi_r2 <- 1 - (sse / sst)
  resultados_test_reg <- rbind(resultados_test_reg, data.frame(
    Modelo     = cand["nombre"],
    RMSE_Train = c(10.68, 11.01, 10.85)[which(c(8,1,5) == as.numeric(cand["id"]))], 
    RMSE_Test  = mi_rmse,
    MAE_Test   = mi_mae,
    R2_Test    = mi_r2
  ))
}

# Calcular diferencia (Overfitting)
resultados_test_reg$Diferencia_RMSE <- resultados_test_reg$RMSE_Test - resultados_test_reg$RMSE_Train

print(resultados_test_reg)

# Gráfico comparativo de RMSE
barplot(resultados_test_reg$RMSE_Test, names.arg = resultados_test_reg$Modelo,
        col = c("gold", "lightblue", "orange"),
        main = "RMSE en Test (Menor es mejor)", ylab = "RMSE")








