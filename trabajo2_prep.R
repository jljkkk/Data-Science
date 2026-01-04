

datos <- readRDS("D:/PrepDatos/df_limpio.rds")

datos <- datos[, 1:(ncol(datos)-2)]

imputar_mediana <- function(data,variable){
  pos_na <- which(is.na(data[[variable]]))
  data[[variable]][pos_na] <- median(data[[variable]], na.rm = TRUE)
  return (data)
}

pos_na <- which(datos$Explotaciones == 99999)

datos$Explotaciones[pos_na] <- NA

datos <- imputar_mediana(datos,"Explotaciones")

tipificar <- function(x){
  return((x-mean(x))/sd(x))
}

variables <- colnames(datos)

datos_est <- datos

for (var in variables){
  if (is.numeric(datos[[var]])){
    datos_est[[var]] <- tipificar(datos[[var]])
  }
}

# Para el clustering se utilizan las variables numéricas
# excepto aquellas que indican la variable objetivo


columnas_numericas <- sapply(datos_est, is.numeric)
columnas_exclur <- c("Izda_Pct","Dcha_Pct","Otros_Pct")
columnas_a_mantener <- setdiff(names(df)[columnas_numericas], columnas_exclur)

var_cluster <- datos_est[,columnas_a_mantener]

if(!require(factoextra)){install.packages("factoextra")}
library(factoextra)

fviz_nbclust(var_cluster, kmeans, method = "wss") + 
  labs(subtitle = "Elbow method")

fviz_nbclust(var_cluster, kmeans, method = "silhouette") +
  labs(subtitle = "Método de la Silueta")


set.seed(123) 

# Ejecutar K-means con k = 3
modelo_km <- kmeans(var_cluster, centers = 3, nstart = 25)

# Ver un resumen del resultado
print(modelo_km)

fviz_cluster(modelo_km, data = var_cluster,
             palette = "jco",      
             geom = "point",       
             ellipse.type = "convex", 
             ggtheme = theme_minimal(),
             main = "Clustering con k = 3")

modelo_km$size

clusters <- modelo_km$cluster


datos$Name[which(clusters==3)]


datos_nor <- datos

normalizar <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

for (var in variables){
  if (is.numeric(datos[[var]])){
    datos_nor[[var]] <- normalizar(datos[[var]])
  }
}

var_cluster <- datos_nor[,columnas_a_mantener]
fviz_nbclust(var_cluster, kmeans, method = "wss") + 
  labs(subtitle = "Elbow method")

fviz_nbclust(var_cluster, kmeans, method = "silhouette") +
  labs(subtitle = "Método de la Silueta")

var_log <- c("Population","TotalCensus","totalEmpresas ","Industria",
             "Construccion","ComercTTEHosteleria","Servicios",
             "inmuebles", "Pob2010")

datos_log <- datos

for (var in var_log){
  if (is.numeric(datos[[var]])){
    datos_log[[var]] <- log(datos[[var]]+1)
  }
}
datos_log_est <- datos_log

for (var in variables){
  if (is.numeric(datos[[var]])){
    datos_log_est[[var]] <- tipificar(datos_log[[var]])
  }
}


var_cluster <- datos_log_est[,columnas_a_mantener]
fviz_nbclust(var_cluster, kmeans, method = "wss") + 
  labs(subtitle = "Elbow method")

fviz_nbclust(var_cluster, kmeans, method = "silhouette") +
  labs(subtitle = "Método de la Silueta")

modelo_km <- kmeans(var_cluster, centers = 2, nstart = 25)

table(modelo_km$cluster)

var_cluster$cluster <- modelo_km$cluster

print(modelo_km)

var_p <- var_cluster[var_cluster$cluster==1,]
var_g <- var_cluster[var_cluster$cluster==2,]


fviz_nbclust(var_p, kmeans, method = "wss") + 
  labs(subtitle = "Elbow method")

fviz_nbclust(var_p, kmeans, method = "silhouette") +
  labs(subtitle = "Método de la Silueta")

#se elige k= 2
modelo_km_p <- kmeans(var_p, centers = 2, nstart = 25)

print(modelo_km_p)

var_p$cluster2 <- modelo_km_p$cluster


fviz_nbclust(var_g, kmeans, method = "wss") + 
  labs(subtitle = "Elbow method")

fviz_nbclust(var_g, kmeans, method = "silhouette") +
  labs(subtitle = "Método de la Silueta")

#Se elige k = 7

modelo_km_g <- kmeans(var_g, centers = 7, nstart = 25)

print(modelo_km_g)
var_g$cluster2 <- modelo_km_g$cluster


datos_est$cluster <- rep(0,8117)
datos_log_est$cluster <- rep(0,8117)


var_uni <- rbind(var_p,var_g)
var_uni <- var_uni[order(as.numeric(rownames(var_uni))), ]


for (i in 1:8117){
  if (var_uni[i,"cluster"]==1){
    datos_est$cluster[i] <-  var_uni[i,"cluster2"]
    datos_log_est$cluster[i] <- var_uni[i,"cluster2"]
  }
  else{
    datos_est$cluster[i] <- 2 + var_uni[i,"cluster2"]
    datos_log_est$cluster[i] <- 2 + var_uni[i,"cluster2"]
  }
}

datos_log_est$cluster <- as.factor(datos_log_est$cluster)
datos_est$cluster <- as.factor(datos_est$cluster)

if(!require(class)){install.packages("class")}
library(class)
if(!require(caret)){install.packages("caret")}
library(caret)
if(!require(ggplot2)){install.packages("ggplot2")}
library(ggplot2)



# Se crea una nueva variable

CCAA <- unique(datos_log_est$CCAA)
media_CCAA <- c()

for ( i in 1:length(CCAA)){
  m <- mean(datos_log_est$Izda_Pct[which(datos_log_est$CCAA == CCAA[i])],na.rm=TRUE)
  media_CCAA <- cbind(media_CCAA,m)
}

datos_log_est$CCAA_avg <- rep(0,8117)

for ( i in 1:length(CCAA)){
  pos_CCAA <- which(datos_log_est$CCAA==CCAA[i])
  datos_log_est$CCAA_avg[pos_CCAA] <- media_CCAA[i]
}

var_num <- c()

for (var in colnames(datos_log_est)){
  if (is.numeric(datos_log_est[[var]])){
    var_num <- cbind(var_num,var)
  }
}

var_num <- var_num[!(var_num %in% c("Otros_Pct","cluster","AbstentionPtge",
                                    "Pob2010","Population"))]
var_num

trainIndex <- createDataPartition(datos_log_est$AbstencionAlta, p = .7, list = FALSE)
cual_train <- datos_log_est[trainIndex,var_num]
cual_test <- datos_log_est[-trainIndex,var_num]

cual_train$AbstencionAlta <- datos_log_est[trainIndex,"AbstencionAlta"]
cual_test$AbstencionAlta <- datos_log_est[-trainIndex,"AbstencionAlta"]

prop.table(table(cual_train$AbstencionAlta))
prop.table(table(cual_test$AbstencionAlta))

ctrl_cv_class <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,               # Solo relevante para clasificación
  summaryFunction = twoClassSummary, 
  savePredictions = "final",
  returnResamp    = "all"  
)

levels(cual_train$AbstencionAlta) <- c("No", "Si")
levels(cual_test$AbstencionAlta) <- c("No", "Si")

colnames(cual_train) <- make.names(colnames(cual_train))

modelo_abstencion <- train(
  AbstencionAlta ~ .,                 
  data = cual_train,
  method = "knn",
  trControl = ctrl_cv_class,
  tuneGrid = expand.grid(k = seq(1, 31, by = 2)), # Probar k impares para evitar empates
  metric = "ROC"              
)

print(modelo_abstencion)

plot(modelo_abstencion)

res_knn <- modelo_abstencion$resample

valores_k <- seq(1,31,by=2)
n <- length(valores_k)
roc_mean <- numeric(n)
roc_sd   <- numeric(n)

for (i in 1:n){
  subset_res <- res_knn[res_knn$k == valores_k[i], ]
  roc_mean[i] <- mean(subset_res$ROC, na.rm=TRUE)
  roc_sd[i]   <- sd(subset_res$ROC, na.rm=TRUE)
}

resumen_manual <- data.frame(k = valores_k, ROC = roc_mean, SD = roc_sd)
print(resumen_manual)

plot(resumen_manual$k, resumen_manual$ROC, type="b", pch=19, 
     col="blue", main="Evolución K vs ROC", xlab="K", ylab="Mean ROC")
# (Opcional) Líneas para ver la desviación típica
lines(resumen_manual$k, resumen_manual$ROC + resumen_manual$SD, col="red", lty=2)
lines(resumen_manual$k, resumen_manual$ROC - resumen_manual$SD, col="red", lty=2)
legend("topright", legend=c("Media ROC", "+/- SD"), col=c("blue", "red"), lty=c(1,2))valores_cp <- unique(grid_rpart$cp)


#Se selecciona k = 27 que es el que tiene roc más alto, y k=31 que también tiene roc 
#alto y con menor sd

knn31 <- train(
  AbstencionAlta ~ .,                 
  data = cual_train,
  method = "knn",
  trControl = ctrl_cv_class,
  tuneGrid = expand.grid(k = 31), # Probar k impares para evitar empates
  metric = "ROC"              
)

colnames(cual_test) <- make.names(colnames(cual_test))

roc_train <- c(
  0.7849969, #k=27
  0.7838354 #k=31
)

if(!require(pROC)){install.packages("pROC")}
library(pROC)

prob_best <- predict(modelo_abstencion, newdata = cual_test, type = "prob")
prob_alt <- predict(knn31, newdata = cual_test, type = "prob")

roc_obj_best <- roc(cual_test[["AbstencionAlta"]], prob_best[,1], levels = rev(levels(cual_test[["AbstencionAlta"]])))
auc_test <- auc(roc_obj_best)

roc_obj_alt <- roc(cual_test[["AbstencionAlta"]], prob_alt[,1], levels = rev(levels(cual_test[["AbstencionAlta"]])))
auc_test_alt <- auc(roc_obj_alt)

auc_test <- as.numeric(auc_test)
auc_test_alt <- as.numeric(auc_test_alt)

df_grafico <- data.frame(
  Modelo = c(paste0("Mejor k (k=", 27, ")"), 
             paste0("Mejor k (k=", 27, ")"),
             paste0("k Alternativo (k=",31, ")"), 
             paste0("k Alternativo (k=", 31, ")")),
  Fase = c("Entrenamiento (CV)", "Prueba (Test)", 
           "Entrenamiento (CV)", "Prueba (Test)"),
  ROC = c(roc_train[1], auc_test, roc_train[2], auc_test_alt)
)

ggplot(df_grafico, aes(x = Modelo, y = ROC, fill = Fase)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(ROC, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4) +
  coord_cartesian(ylim = c(0.5, 1.0)) + 
  scale_fill_manual(values = c("#3498db", "#95a5a6")) +
  # Etiquetas y títulos
  labs(title = "Comparación de Capacidad Predictiva: Train vs Test",
       subtitle = "Predicción de Abstención > 30% en Municipios Españoles",
       x = "Configuración del Modelo k-NN",
       y = "Área bajo la curva (ROC)",
       fill = "Fase del Análisis") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        plot.title = element_text(face = "bold"))

pred_best <- predict(modelo_abstencion, newdata = cual_test)
cm_best <- confusionMatrix(pred_best, cual_test$AbstencionAlta)

print(cm_best)


var_num <- c()

for (var in colnames(datos_log_est)){
  if (is.numeric(datos_log_est[[var]])){
    var_num <- cbind(var_num,var)
  }
}

var_num <- var_num[!(var_num %in% c("Otros_Pct","AbstentionPtge","Pob2010",
                                    "Population"))]


cual_train <- datos_log_est[trainIndex,var_num]
cual_test <- datos_log_est[-trainIndex,var_num]

cual_train$AbstencionAlta <- datos_log_est[trainIndex,"AbstencionAlta"]
cual_test$AbstencionAlta <- datos_log_est[-trainIndex,"AbstencionAlta"]

grid <- expand.grid(
  fL = c(0, 1, 2),          
  usekernel = c(TRUE, FALSE),  
  adjust = c(1, 1.5)           
)

levels(cual_train$AbstencionAlta) <- c("No", "Si")
levels(cual_test$AbstencionAlta) <- c("No", "Si")

colnames(cual_train) <- make.names(colnames(cual_train))
colnames(cual_test) <- make.names(colnames(cual_test))

modelo_nb <- train(
  AbstencionAlta ~ ., 
  data = cual_train,
  method = "nb",
  trControl = ctrl_cv_class,
  tuneGrid = grid,
  metric = "ROC"    
)

print(modelo_nb)

#fl=0, usekernel=False y adjust=1

#alt: fl= 0, usekernel=TRUE, adjust=1

modelo_nb_alt <- train(
  AbstencionAlta ~ ., 
  data = cual_train,
  method = "nb",
  trControl = ctrl_cv_class,
  tuneGrid = expand.grid(fL=0,usekernel=TRUE,adjust=1),
  metric = "ROC"    
)


roc_train <- c(
  0.7143336, #the best
  0.7100946 #the alt
)

prob_best <- predict(modelo_nb, newdata = cual_test, type = "prob")
prob_alt <- predict(modelo_nb_alt, newdata = cual_test, type = "prob")

roc_obj_best <- roc(cual_test[["AbstencionAlta"]], prob_best[,1], levels = rev(levels(cual_test[["AbstencionAlta"]])))
auc_test <- auc(roc_obj_best)

roc_obj_alt <- roc(cual_test[["AbstencionAlta"]], prob_alt[,1], levels = rev(levels(cual_test[["AbstencionAlta"]])))
auc_test_alt <- auc(roc_obj_alt)

auc_test <- as.numeric(auc_test)
auc_test_alt <- as.numeric(auc_test_alt)




df_grafico <- data.frame(
  Modelo = c(paste0("Mejor fl=0 usekernel=False  adjust=1"), 
             paste0("Mejor fl=0 usekernel=False  adjust=1"),
             paste0("Alt fl= 0, usekernel=True, adjust=1"), 
             paste0("Alt fl= 0, usekernel=True, adjust=1")),
  Fase = c("Entrenamiento (CV)", "Prueba (Test)", 
           "Entrenamiento (CV)", "Prueba (Test)"),
  ROC = c(roc_train[1], auc_test, roc_train[2], auc_test_alt)
)

ggplot(df_grafico, aes(x = Modelo, y = ROC, fill = Fase)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(ROC, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4) +
  coord_cartesian(ylim = c(0.5, 1.0)) + 
  scale_fill_manual(values = c("#3498db", "#95a5a6")) +
  # Etiquetas y títulos
  labs(title = "Comparación de Capacidad Predictiva: Train vs Test",
       subtitle = "Predicción de Abstención > 30% en Municipios Españoles",
       x = "Configuración del Modelo Naive Bayes",
       y = "Área bajo la curva (ROC)",
       fill = "Fase del Análisis") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        plot.title = element_text(face = "bold"))


pred_best <- predict(modelo_nb, newdata = cual_test)
cm_best <- confusionMatrix(pred_best, cual_test$AbstencionAlta)

print(cm_best)



#Regresion logistica

grid_logistica <- expand.grid(
  alpha = c(0, 0.5, 1),        # 0=Ridge, 1=Lasso, 0.5=Elastic Net
  lambda = seq(0.001, 0.1, by = 0.01)
)

modelo_rl <- train(
  AbstencionAlta ~ ., 
  data = cual_train,
  method = "glmnet",           # Logística avanzada con penalización
  trControl = ctrl_cv_class,
  tuneGrid = grid_logistica,
  metric = "ROC"
)

print(modelo_rl)

#El mejor alpha=0.5 lambda=0.001
#El alternativo alpha=1 lambda =0.001

modelo_rl_alt <- train(
  AbstencionAlta ~ ., 
  data = cual_train,
  method = "glmnet",
  trControl = ctrl_cv_class,
  tuneGrid = expand.grid(alpha=1,lambda=0.001),
  metric = "ROC"    
)

roc_train <- c(
  0.7829077, #the best
  0.7825062 #the alt
)


prob_best <- predict(modelo_rl, newdata = cual_test, type = "prob")
prob_alt <- predict(modelo_rl_alt, newdata = cual_test, type = "prob")

roc_obj_best <- roc(cual_test[["AbstencionAlta"]], prob_best[,1], levels = rev(levels(cual_test[["AbstencionAlta"]])))
auc_test <- auc(roc_obj_best)

roc_obj_alt <- roc(cual_test[["AbstencionAlta"]], prob_alt[,1], levels = rev(levels(cual_test[["AbstencionAlta"]])))
auc_test_alt <- auc(roc_obj_alt)

auc_test <- as.numeric(auc_test)
auc_test_alt <- as.numeric(auc_test_alt)




df_grafico <- data.frame(
  Modelo = c(paste0("Mejor alpha=0.5 lambda=0.001"), 
             paste0("Mejor alpha=0.5 lambda=0.001"),
             paste0("Alt alpha=1 lambda=0.001"), 
             paste0("Alt alpha=1 lambda=0.001")),
  Fase = c("Entrenamiento (CV)", "Prueba (Test)", 
           "Entrenamiento (CV)", "Prueba (Test)"),
  ROC = c(roc_train[1], auc_test, roc_train[2], auc_test_alt)
)

ggplot(df_grafico, aes(x = Modelo, y = ROC, fill = Fase)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  geom_text(aes(label = round(ROC, 3)), 
            position = position_dodge(width = 0.7), 
            vjust = -0.5, size = 4) +
  coord_cartesian(ylim = c(0.5, 1.0)) + 
  scale_fill_manual(values = c("#3498db", "#95a5a6")) +
  # Etiquetas y títulos
  labs(title = "Comparación de Capacidad Predictiva: Train vs Test",
       subtitle = "Predicción de Abstención > 30% en Municipios Españoles",
       x = "Configuración del Modelo Regresión Logística Penalizada",
       y = "Área bajo la curva (ROC)",
       fill = "Fase del Análisis") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(size = 12),
        plot.title = element_text(face = "bold"))



pred_best <- predict(modelo_rl, newdata = cual_test)
cm_best <- confusionMatrix(pred_best, cual_test$AbstencionAlta)

print(cm_best)

# Arbol de regresion

trainIndex <- createDataPartition(datos$Izda_Pct, p = .7, list = FALSE)


var_exluir <- c("Dcha_Pct","Otros_Pct","Izquierda","Derecha",
                "AbstencionAlta","Pob2010","Population","Name")
datos$cluster <- datos_log_est$cluster
vars <- colnames(datos)
vars <- vars[!(vars %in% var_exluir)]
train_cuant <- datos[ trainIndex,vars]
test_cuant  <- datos[-trainIndex,vars]

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

ctrl_cv_reg <- trainControl(
  method = "cv",
  number = 10,
  returnResamp = "all"
)
colnames(test_cuant) <- make.names(colnames(test_cuant))
colnames(train_cuant) <- make.names(colnames(test_cuant))

grid_rpart <- expand.grid(
  cp = seq(0.00, 0.05, by = 0.005) 
)
grid_parametros_extra <- expand.grid(
  maxdepth = c(5, 10, 30),   # Árbol pequeño, mediano, y libre (profundo)
  minsplit = c(10, 20, 50)   # Exigencia baja, media y alta para dividir
)

fit_rpart <- train(
  Izda_Pct ~ .,
  data      = train_cuant,
  method    = "rpart",
  trControl = ctrl_cv_reg,
  tuneGrid  = grid_rpart, 
  metric    = "RMSE"
)

print(fit_rpart) 

res_rpart <- fit_rpart$resample

valores_cp <- unique(grid_rpart$cp)
n <- length(valores_cp)
RMSE_mean <- numeric(n)
RMSE_sd   <- numeric(n)

for (i in 1:n){
  subset_res <- res_rpart[res_rpart$cp == valores_cp[i], ]
  RMSE_mean[i] <- mean(subset_res$RMSE, na.rm=TRUE)
  RMSE_sd[i]   <- sd(subset_res$RMSE, na.rm=TRUE)
}

resumen_manual <- data.frame(cp = valores_cp, RMSE = RMSE_mean, SD = RMSE_sd)
print(resumen_manual)

plot(resumen_manual$cp, resumen_manual$RMSE, type="b", pch=19, 
     col="blue", main="Evolución CP vs RMSE", xlab="CP", ylab="Mean ROC")
# (Opcional) Líneas para ver la desviación típica
lines(resumen_manual$cp, resumen_manual$RMSE + resumen_manual$SD, col="red", lty=2)
lines(resumen_manual$cp, resumen_manual$RMSE - resumen_manual$SD, col="red", lty=2)
legend("topright", legend=c("Media RMSE", "+/- SD"), col=c("blue", "red"), lty=c(1,2))




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
    tuneGrid  = expand.grid(cp = seq(0.00, 0.05, by = 0.005) ),
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

ordenado_reg <- registro_regresion[order(registro_regresion$RMSE_Train), ]

print(ordenado_reg)

candidatos_finales <- list(
  c(id=8,depth = 10, split = 50, cp = 0, nombre = "ID_8_mejor"),
  c(id=9,depth = 30, split = 50, cp = 0.005, nombre = "ID_9_Alt")
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
    RMSE_Train = c(10.82928,10.93087)[which(c(8,9) == as.numeric(cand["id"]))], 
    RMSE_Test  = mi_rmse,
    MAE_Test   = mi_mae,
    R2_Test    = mi_r2
  ))
}


resultados_test_reg$Diferencia_RMSE <- resultados_test_reg$RMSE_Test - resultados_test_reg$RMSE_Train

print(resultados_test_reg)

# Gráfico comparativo de RMSE
barplot(resultados_test_reg$RMSE_Test, names.arg = resultados_test_reg$Modelo,
        col = c("gold", "lightblue", "orange"),
        main = "RMSE en Test (Menor es mejor)", ylab = "RMSE")




fit_final <- train(
  Izda_Pct ~ .,
  data      = train_cuant,
  method    = "rpart",
  trControl = trainControl(method="none"), 
  tuneGrid  = expand.grid(cp = 0.005),
  control   = rpart.control(maxdepth = 30, 
                            minsplit = 50)
)


predicciones_num <- predict(fit_final, 
                            newdata = test_cuant)

valores_reales <- test_cuant$Izda_Pct
residuos <- valores_reales - predicciones_num

# 3. Gráfico: Predicción vs Realidad (Ideal: línea diagonal)
plot(valores_reales, predicciones_num, 
     main = "Predicción vs Realidad",
     xlab = "Valor Real", ylab = "Predicción",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2) # Línea de referencia perfecta

# 4. Gráfico: Distribución de Residuos (Para ver si hay sesgo)
plot(predicciones_num, residuos,
     main = "Residuos vs Predicción",
     xlab = "Predicción", ylab = "Residuo",
     pch = 19, col = "darkgray")
abline(h = 0, col = "red", lwd = 2)

library(rpart.plot)
rpart.plot(fit_temp$finalModel, 
           type = 2, 
           extra = 101, 
           under = TRUE, 
           faclen = 0, 
           box.palette = "GnBu", 
           main = "Árbol de Decisión Óptimo")



#Regresion lineal



var_exluir <- c("Dcha_Pct","Otros_Pct","Izquierda","Derecha",
                "AbstencionAlta","Pob2010","Population","Name","CCAA",
                "UnemployMore40_Ptge", "Age_under19_Ptge",
                "ComercTTEHosteleria",
                "Servicios",
                "inmuebles","Industria","Construccion",
                "SameComAutonPtge", "Age_19_65_pct",
                "totalEmpresas")
datos$cluster <- datos_log_est$cluster
vars <- colnames(datos)
vars <- vars[!(vars %in% var_exluir)]
train_cuant <- datos_log_est[ trainIndex,vars]
test_cuant  <- datos_log_est[-trainIndex,vars]

train_cuant$Izda_Pct <- datos[trainIndex,"Izda_Pct"]
test_cuant$Izda_Pct <- datos[-trainIndex,"Izda_Pct"]

#modelo lineal simple
if(!require(car)){install.packages("car")}
library(car)  
ml <- lm(Izda_Pct ~ ., data = train_cuant)

summary(ml)
car::vif(ml)


predicciones <- predict(ml, newdata = test_cuant)

valores_reales <- test_cuant$Izda_Pct
residuos <- valores_reales - predicciones

# 3. Gráfico: Predicción vs Realidad (Ideal: línea diagonal)
plot(valores_reales, predicciones, 
     main = "Predicción vs Realidad",
     xlab = "Valor Real", ylab = "Predicción",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2) # Línea de referencia perfecta

# 4. Gráfico: Distribución de Residuos (Para ver si hay sesgo)
plot(predicciones, residuos,
     main = "Residuos vs Predicción",
     xlab = "Predicción", ylab = "Residuo",
     pch = 19, col = "darkgray")
abline(h = 0, col = "red", lwd = 2)

if(!require(Metrics)){install.packages("Metrics")}
library(Metrics)

rmse_test <- rmse(test_cuant$Izda_Pct, predicciones)
mae_test <- mae(test_cuant$Izda_Pct, predicciones)

# R-cuadrado para el test
rss <- sum((predicciones - test_cuant$Izda_Pct)^2)
tss <- sum((test_cuant$Izda_Pct - mean(test_cuant$Izda_Pct))^2)
r2_test <- 1 - (rss/tss)

cat("RMSE en Test:", rmse_test, "\n")
cat("R2 en Test:", r2_test, "\n")


grid_linear <- expand.grid(
  alpha = seq(0, 1, by = 0.5),       # De Ridge a Lasso
  lambda = seq(0.001, 0.1, by = 0.01) # Fuerza de la penalización
)

control_cv <- trainControl(
  method = "cv", 
  number = 10,
  verboseIter = TRUE
)

modelo_li <- train(
  Izda_Pct ~ ., 
  data = train_cuant, 
  method = "glmnet",        # Este método combina regresión lineal, Lasso y Ridge
  trControl = control_cv,
  tuneGrid = grid_linear,
)


print(modelo_li)


#El mejor alpha=1 lambda=0.001
#EL alternativo alpha=0.5 lambda=0.001

rmse_train <- c(
  9.848872, #el mejor
  9.848949 #alt
)


modelo_li_alt <- train(
  Izda_Pct ~ ., 
  data = train_cuant, 
  method = "glmnet",        # Este método combina regresión lineal, Lasso y Ridge
  trControl = control_cv,
  tuneGrid = expand.grid(alpha=0.5,lambda=0.001),
)

predicciones_li <- predict(modelo_li, newdata = test_cuant)
predicciones_li_alt <- predict(modelo_li_alt, newdata = test_cuant)

rmse_test_li <- rmse(test_cuant$Izda_Pct, predicciones_li)
mae_test_li <- mae(test_cuant$Izda_Pct, predicciones_li)
rmse_test_li_alt <- rmse(test_cuant$Izda_Pct, predicciones_li_alt)
mae_test_li_alt <- mae(test_cuant$Izda_Pct, predicciones_li_alt)

# R-cuadrado para el test
rss_li <- sum((predicciones_li - test_cuant$Izda_Pct)^2)
tss_li <- sum((test_cuant$Izda_Pct - mean(test_cuant$Izda_Pct))^2)
r2_test_li <- 1 - (rss_li/tss_li)
rss_li_alt <- sum((predicciones_li_alt - test_cuant$Izda_Pct)^2)
tss_li_alt <- sum((test_cuant$Izda_Pct - mean(test_cuant$Izda_Pct))^2)
r2_test_li_alt <- 1 - (rss_li_alt/tss_li_alt)

cat("RMSE en Test del mejor:", rmse_test_li, "RMSE en train:", rmse_train[1],
    "mae_test: ", mae_test_li, "r2:" ,r2_test_li)
cat("RMSE en Test del alternativo:", rmse_test_li_alt, "RMSE en train:", rmse_train[2],
    "mae_test: ", mae_test_li_alt, "r2:" ,r2_test_li_alt)

valores_reales <- test_cuant$Izda_Pct
residuos <- valores_reales - predicciones_li

# 3. Gráfico: Predicción vs Realidad (Ideal: línea diagonal)
plot(valores_reales, predicciones_li, 
     main = "Predicción vs Realidad",
     xlab = "Valor Real", ylab = "Predicción",
     pch = 19, col = "blue")
abline(0, 1, col = "red", lwd = 2) # Línea de referencia perfecta

# 4. Gráfico: Distribución de Residuos (Para ver si hay sesgo)
plot(predicciones_li, residuos,
     main = "Residuos vs Predicción",
     xlab = "Predicción", ylab = "Residuo",
     pch = 19, col = "darkgray")
abline(h = 0, col = "red", lwd = 2)








