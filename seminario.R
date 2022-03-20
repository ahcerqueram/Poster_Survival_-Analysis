
#https://www.cienciadedatos.net/documentos/12_t-test







########%%%%%%%%%%%%%%%%%%%%%%%%%%/(((((((((((((((((((((((((((((((((888)))))))))))))))))))))))))))))))))
BOOSTING


library(gbm)
library(caret) 
View(heart)

str(heart)
heart$evento_muerte<-as.factor(heart$evento_muerte)
indexes = createDataPartition(heart$evento_muerte, p = .70, list = F)
train = heart[indexes, ]
test = heart[-indexes, ]
mod_gbm = gbm(evento_muerte ~ fraccion_eyeccion+creatinina,
              data = train,
              distribution = "multinomial",
              cv.folds = 10,
              shrinkage = .01,
              n.minobsinnode = 10,
              n.trees = 200)

print(mod_gbm)
summary(mod_gbm)




#### test de test

pred = predict.gbm(object = mod_gbm,
                   newdata = test,
                   n.trees = 200,
                   type = "response")

labels = colnames(pred)[apply(pred, 1, which.max)]
result = data.frame(test$evento_muerte, labels)
print(result)

cm = confusionMatrix(test$evento_muerte, as.factor(labels))
print(cm)
str(heart)

#ahora cpn caret

tc = trainControl(method = "repeatedcv", number = 10)
model = train(evento_muerte ~ fraccion_eyeccion+creatinina, data=train, method="gbm", trControl=tc)
pred = predict(model, test)
result = data.frame(test$evento_muerte, pred)
print(result)
cm = confusionMatrix(test$evento_muerte, as.factor(pred))
print(cm)

####$$$$$$$$$$$$$$$$$$ otro
####   https://datascience-enthusiast.com/R/ML_python_R_part2.html


set.seed(100)  # For reproducibility

# Create index for testing and training data

str(heart)
heart$evento_muerte<-as.factor(heart$evento_muerte)
inTrain <- createDataPartition(y = heart$evento_muerte, p = 0.7, list = FALSE)

# subset power_plant data to training
training <- heart[inTrain,]


# subset the rest to test
testing <- heart[-inTrain,]


X_train = xgb.DMatrix(as.matrix(training %>% select(-evento_muerte)))

y_train = training$evento_muerte


X_test = xgb.DMatrix(as.matrix(testing %>% select(-evento_muerte)))
y_test = testing$evento_muerte





xgb_trcontrol = trainControl(
  method = "cv",
  number = 5,  
  allowParallel = TRUE,
  verboseIter = FALSE,
  returnData = FALSE
)

xgbGrid <- expand.grid(nrounds = c(100,200),  # this is n_estimators in the python code above
                       max_depth = c(10, 15, 20, 25),
                       colsample_bytree = seq(0.5, 0.9, length.out = 5),
                       ## The values below are default values in the sklearn-api. 
                       eta = 0.1,
                       gamma=0,
                       min_child_weight = 1,
                       subsample = 1
)


set.seed(0) 

xgb_model = train(
  X_train, y_train,  
  trControl = xgb_trcontrol,
  tuneGrid = xgbGrid,
  method = "xgbTree"
)


xgb_model$bestTune



###### otro blog

library("xgboost")
library("tidyverse")

str(heart)
View(heart)

hongo <- list()
hongo$original <- heart


set.seed(1919)
hongo$train_df <- sample_frac(heart, size = 0.7)
hongo$test_df <- setdiff(hongo$original, hongo$train_df)

dim(hongo$train)

hongo$train_mat <- 
  hongo$train_df %>% 
  select(-evento_muerte) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = hongo$train_df$evento_muerte)


###### ahora datos de entrenamiento

hongo$test_mat <- 
  hongo$test_df %>% 
  select(-evento_muerte) %>% 
  as.matrix() %>% 
  xgb.DMatrix(data = ., label = hongo$test_df$evento_muerte)


hongo$modelo_01 <- xgboost(data = hongo$train_mat, 
                           objective = "binary:logistic",
                           nrounds = 200, max.depth = 10, eta = 0.1,                  
                           nthread = 2)

hongo$predict_01 <- predict(hongo$modelo_01, hongo$test_mat)
head(hongo$predict_01)
head(hongo$predict_01 > 0.5)

cbind(hongo$predict_01 > 0.5, hongo$test_df$evento_muerte) %>% 
  data.frame() %>% 
  table() %>% 
  confusionMatrix()



#######$$$$$$$$$$$$$$$$$$$$%%%%%&&&&/(((()))))))))))))))))))))))))))))$$$$$$$

# regresion logistica

set.seed(340)
indxEntrena <- createDataPartition(y = heart$evento_muerte, p = 0.70, list = FALSE)

heart_entre <- heart[indxEntrena,]
heart_te <- heart[-indxEntrena,] 

modelo_glm <- glm(evento_muerte ~ creatinina+fraccion_eyeccion+tiempo, data = heart_entre, family = "binomial")


summary(modelo_glm)
confint(modelo_glm)

##AIC 167.91 RESIDUAL deviance 167.91
## AIC 216.5 residual deviance 210.50


predicciones <- ifelse(test = modelo_glm$fitted.values > 0.5, yes = 1, no = 0)

matriz_confusion <- table(modelo_glm$model$evento_muerte, predicciones,
                          dnn = c("observaciones", "predicciones"))
matriz_confusion

heart$evento_muerte<-as.factor(heart$evento_muerte)

### #######segundo log

prob.modelo <- predict(modelo_glm, newdata = heart_entre, type = "response")

pred.modelo <- rep("0", length(prob.modelo))
# Sustitución de "Down" por "Up" si la p > 0.5
pred.modelo[prob.modelo > 0.5] <- "1"

# tercer log
y=predict(modelo_glm, type="response")
pred = ifelse(as.double(y)>0.5,1,0)
library(MLmetrics)
Accuracy(y_pred = pred, y_true = heart_entre$evento_muerte)
ConfusionMatrix(y_pred = pred, y_true = heart_entre$evento_muerte)


##
##  PREDICCION

y_t=predict(modelo_glm, heart_te[c("fraccion_eyeccion","creatinina","tiempo")])

pred_test = ifelse(as.double(y_t)>0.5,1,0)

Accuracy(y_pred = pred_test, y_true = heart_te["evento_muerte"])

ConfusionMatrix(y_pred = pred_test, y_true = unlist(heart_te["evento_muerte"] ) )


pred## GRAFICA modelo

modelo_logistico <- glm(evento_muerte ~ creatinina, data = heart, family = "binomial")

# Representación gráfica del modelo.

ggplot(data = heart, aes(x = creatinina, y = evento_muerte)) +
  geom_point(aes(color = as.factor(evento_muerte)), shape = 1) + 
  stat_function(fun = function(x){predict(modelo_logistico,
                                          newdata = data.frame(creatinina = x),
                                          type = "response")}) +
  theme_bw() +
  labs(title = "Regresión logística",
       y = "Probabilidad evento_muerte") +
  theme(legend.position = "none")


###########

modeloo <- glm(evento_muerte ~ fraccion_eyeccion, data = heart, family = "binomial")
summary(modeloo)

nuevos_puntos <- seq(from = min(heart$fraccion_eyeccion), to = max(heart$fraccion_eyeccion), by = 0.5)

predicciones <- predict(modeloo, newdata = data.frame(fraccion_eyeccion= nuevos_puntos), 
                        se.fit = TRUE, type = "response")

names(predicciones)


CI_inferior <- predicciones$fit - 1.96 * predicciones$se.fit
CI_superior <- predicciones$fit + 1.96 * predicciones$se.fit


datos_curva <- data.frame(fraccion_eyeccion = nuevos_puntos, probabilidad = predicciones$fit, 
                          CI.inferior = CI_inferior, CI.superior = CI_superior)


ggplot(heart, aes(x = fraccion_eyeccion, y = evento_muerte)) +
  geom_point(aes(color = as.factor(evento_muerte)), shape = "I", size = 3) +
  geom_line(data = datos_curva, aes(y = probabilidad), color = "firebrick") +
  geom_line(data = datos_curva, aes(y = CI.superior), linetype = "dashed") +
  geom_line(data = datos_curva, aes(y = CI.inferior), linetype = "dashed") +
  labs(title = "Modelo logístico Direction ~ Lag2", 
       y = "P(evento_muerte = muerte | fraccion_eyeccion)", 
       x = "fraccion_eyeccion") +
  scale_color_manual(labels = c("no_muerte", "muerte"), values = c("blue", "red")) +
  guides(color=guide_legend("evento_muerte")) +
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme_bw()









#........................












# arboles

library(tree)
heart$tiempo<-as.numeric(heart$tiempo)
set.seed(524)
traina <- createDataPartition(y = heart$evento_muerte, p = 0.7, list = FALSE, times = 1)

# Datos entrenamiento
datos_traine <- heart[traina, ]


# datos de test
datos_teste <- heart[-traina, ]
dim(datos_teste)

modelo_arbolC <- tree(evento_muerte ~ fraccion_eyeccion+creatinina+tiempo, data = datos_traine)
plot(modelo_arbolC)
text(modelo_arbolC, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")



### KNN


heart <- heart %>% 
  mutate(evento_muerte = ifelse(evento_muerte == "1", "si", "no")) %>% 
  mutate_at(c("evento_muerte"), ~as.factor(.))


set.seed(30)
indxEntrena <- createDataPartition(y = heart$evento_muerte, p = 0.70, list = FALSE)

heart_entrena <- heart[indxEntrena,]
heart_test <- heart[-indxEntrena,] 

set.seed(30)
SP_ctrl <- trainControl(method="cv", number = 10) 


set.seed(30)


#preprocesando datos
set.seed(30)

SP_knnEntrenado <- train(evento_muerte ~fraccion_eyeccion+creatinina+tiempo, 
                         data = heart_entrena, 
                         method = "knn",  
                         tuneLength = 20,
                         trControl = SP_ctrl,
                         preProcess = c("center","scale")
)

set.seed(30)
SP_knnPrediccionn <- predict(SP_knnEntrenado, newdata = heart_entrena)
confusionMatrix(SP_knnPrediccionn, heart_entrena$evento_muerte)


# error de prueba
set.seed(30)
SP_knnPrediccion <- predict(SP_knnEntrenado, newdata = heart_test )
set.seed(30)
confusionMatrix(SP_knnPrediccion, heart_test$evento_muerte)



library(caTools)
library(MLmetrics)
library(tree)
library(fastAdaboost)

# ARBOLES DE DECISION

set.seed(524)
traina <- createDataPartition(y = heart$evento_muerte, p = 0.7, list = FALSE, times = 1)

# Datos entrenamiento
datos_traine <- heart[traina, ]
dim(datos_traine)

# datos de test
datos_teste <- heart[-traina, ]
dim(datos_teste)

heart$tiempo<-as.numeric(heart$evento_muerte)
str(heart)
modelo_arbolC <- tree(evento_muerte ~ creatinina+fraccion_eyeccion, data = datos_traine)
summary(modelo_arbolC)
plot(modelo_arbolC)
text(modelo_arbolC, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")
modelo_arbolC

predicciones <- predict(modelo_arbolC, datos_traine, type="class")
confusionMatrix(predicciones, datos_traine$evento_muerte)

# 0.84 fraccion y creatinina
# 0.81 fraccion+hiper+edad
# 0.81 con edad

set.seed(356)
cv_arboles <- cv.tree(modelo_arbolC, K = 10, FUN = prune.misclass)
cv_arboles
par(mfrow = c(1, 2))

# Cost complexity pruning
plot(cv_arboles$size, cv_arboles$dev, xlab = "nodos terminales", 
     ylab = "error clasificación", type = "b", pch = 19)
plot(x = cv_arboles$k, y = cv_arboles$dev, xlab = "alpha", 
     ylab = "error clasificación", type = "b")

modelo_arbolCpodado <- prune.misclass(tree = modelo_arbolC, best = 2)

par(mfrow = c(1,1))
plot(x = modelo_arbolCpodado, type = "proportional")
text(modelo_arbolCpodado, splits = TRUE, pretty = 0, cex = 0.8, col = "firebrick")

predicciones <- predict(modelo_arbolCpodado, newdata = datos_teste, type = "class")

confusionMatrix(predicciones, datos_teste$evento_muerte)

#con  hipertension+edad+fraccion_eyeccion+creatinina 0.75
# sin hipertension 0.75
# tiempo 



### adabost


# Método de validacion cruzada (10-fold)

fitControl <- trainControl(method = "cv", 
                           number = 10, 
                           search = "grid", 
                           allowParallel = TRUE)

# Hiperparámetros a evaluar
grid <- expand.grid(nIter = c(100, 400, 600),
                    method = "adaboost") #nº de iteraciones (clasificadores)

# Evaluación del mejor conjunto de hiperparámetros
set.seed(356)
modelo_adaboost <- train(evento_muerte ~ tiempo+fraccion_eyeccion+
                         creatinina, data = datos_traine, 
                         method = "adaboost", 
                         tuneGrid = grid, 
                         trControl = fitControl, 
                        
                         verbose = FALSE)

modelo_adaboost
predicciones_2 <- predict(modelo_adaboost, newdata = datos_teste)
confusionMatrix(predicciones_2, datos_teste$evento_muerte)


confusionMatrix(predict(modelo_adaboost, datos_teste), datos_teste[ , 13])





# modelo naive.

datos_traine
# datos de test
datos_teste


mod <- naiveBayes(evento_muerte ~ tiempo+fraccion_eyeccion+creatinina, data = datos_traine)
mod
pred <- predict(mod,datos_teste)
tab <- table(datos_teste$evento_muerte, pred, dnn = c("Actual", "Predicha"))
confusionMatrix(tab)



### red neuronal artificial



install.packages("h2o")
library(h2o)
h2o.init(nthreads = -1)

install.packages("dummies")

library(dummies)

data_dm <- dummy.data.frame(data=heart, names="evento_muerte", sep="_")
str(data_dm)

normaliza <- function(x) {return ((x-min(x))/(max(x)-min(x)))}

heart<-heart[,c(3,5,8,12,13)]



data_norm <- heart
summary(data_norm)
str(data_norm)
set.seed(3141592) #necesario si se quiere reproducir de nuevo el mismo código y obtener los mimsos resultados
index <- sample(nrow(data_norm), round(0.70*nrow(data_norm)))
train <- data_norm[index,] # crea train a partir del indice de la muestra
test <- data_norm[-index,] # crea test a partir del resto de la muestra
head(train)
set.seed(3141592)
library(neuralnet)

ann_model <- neuralnet(evento_muerte~ fraccion_eyeccion+creatinina+tiempo, data=train, hidden= 5, act.fct = "logistic", linear.output = FALSE)
plot(ann_model, rep="best")

ann_pred <- compute(ann_model,test)

head(ann_pred$net.result)

predic<-max.col(ann_pred)
head(predic, 100)


ann_pred <- compute(ann_model,test)
a=ann_pred$net.result
test.result<-ifelse(a>0.5,1,0)
table(test.result,test$evento_muerte)
confusionMatrix(table(test.result,test$evento_muerte))




predict<-max.col(a)
head(predic, 10)
test_res <- max.col(test)
head(test_res,)

library(gmodels)
CrossTable(x=test_res, y=predic)


test_res <- max.col(test)
head(test_res, 100)
confusionMatrix(table(test_res, predic)






heart#https://rpubs.com/Cristina_Gil/SVM maquinas de soporte



#https://www.youtube.com/watch?v=0_c24N7SAYE regresion logistica
#https://www.youtube.com/watch?v=6WAy6l9sWsI regresion logistica

mc<- table(testset[13],prediccion)
mc


#https://www.youtube.com/watch?v=0_c24N7SAYE
#https://www.youtube.com/watch?v=6WAy6l9sWsI


https://www.kaggle.com/andrewmvd/heart-failure-clinical-data/discussion/178372

https://www.kaggle.com/nickkwidzinski/feature-selection-and-svm-with-p arameter-tuning

https://www.kaggle.com/anupamshah/heart-failure-analysis-and-prediction-dt-xgb

https://www.kaggle.com/djellalmohamedaniss/classification-heart-failure-clinical-data


install.packages("survival")
install.packages("KMsurv")
install.packages("survMisc")
install.packages("survminer")
install.packages("flexsurv")
install.packages("actuar")
install.packages("dplyr")
install.packages("car")
install.packages("lmtest")
install.packages("e1071")
install.packages("DescTools")

library(survival)
library(KMsurv)
library(survMisc)
library(survminer)
library(ggfortify)
library(flexsurv)
library(actuar)
library(dplyr)
library(dplyr)
library(car)
library(lmtest)
library(DescTools)




library(readxl)
pepo<- read_excel("C:\\Users\\Andres\\Desktop\\estadistica\\modelos lineales\\heart failure\\heart.xlsx")
colnames(pepo)


mi_model<-glm(data=pepo,evento_muerte~.,family = binomial)
mi_model



timecat<-pepo$tiempo
timecat[pepo$tiempo<30]<-"0"
timecat[pepo$tiempo>=30 & pepo$tiempo<60]<-"1"
timecat[pepo$tiempo>=60 & pepo$tiempo<90]<-"2"
timecat[pepo$tiempo>=90 & pepo$tiempo<120]<-"3"
timecat[pepo$tiempo>=120 & pepo$tiempo<150]<-"4"
timecat[pepo$tiempo>=150 & pepo$tiempo<180]<-"5"
timecat[pepo$tiempo>=180 & pepo$tiempo<210]<-"6"
timecat[pepo$tiempo>=210 & pepo$tiempo<240]<-"7"
timecat[pepo$tiempo>=240 & pepo$tiempo<270]<-"8"
timecat[pepo$tiempo>=270]<-"9"

mam<-timecat

class(mam)


pepo2<-pepo %>% 
  mutate(tiempo=mam)
View(pepo2)
pico<-pepo2[,1:13]
View(pico)

library(scales)
library(ggplot2)


str(pico$tiempo)
pico$tiempo<-as.factor(pico$tiempo)

table(pico$tiempo)
prop.table(table(pico$tiempo))
str(pico$tiempo)


pico$evento_muerte<-factor(pico$evento_muerte, levels = c("0", "1"), 
                       
                       labels = c("no_muerte", "muerte"))

View(pico)



tabula<-table(pico$evento_muerte,pico$tiempo)
tabula


tata <- prop.table(tabula, margin=2)
mico=round(tata*100,2)
mico

barplot(tata, 
        beside = TRUE, las=1, 
        xlab='Estrato', ylab='Frecuencia relativa',
        col = c("lightblue", "mistyrose"),
        ylim = c(0, 1.5))
legend('top', legend=rownames(tabula), bty='n',
       fill=c("lightblue", "mistyrose"))

?legend


luca=tata*100
luca
lila=round(luca,2)
lila

library(readxl)

heart<- read_excel("C:\\Users\\Andres\\Desktop\\estadistica\\modelos lineales\\heart failure\\heart.xlsx")
View(heart)
heart<-heart[,c(5,8,13)]
heart$evento_muerte<-as.factor(heart$evento_muerte)
heartolnames(heart)


pico$tiempo<-as.numeric(pico$tiempo)


View(heart)

pico$tiempo<-as.factor(pico$tiempo)
str(pico)

survfit(Surv(tiempo, evento_muerte) ~ 1, heart, conf.type = "log-log") 





Surv(tiempo,evento_muerte)
survfit(Surv(tiempo,evento_muerte) ~ 1, data = heart)
survfit(Surv(tiempo,evento_muerte) ~ factor, data = heart)


data("diabetic")
View(diabetic)
str(diabetic)
fit <- survfit(Surv(time,status) ~ 1, data = aml)
fit
data("veteran")
str(veteran)
str(heart)


View(heart)


#supervivencia completa aca empezamos


pico$tiempo<-as.numeric(pico$tiempo)
str(heart)
str(pico)
pico$tiempo<-as.factor(pico$tiempo)
pico$evento_muerte<-as.factor(pico$evento_muerte)
str(pico)


#### tenemos la base de pico para sacar la curva

heart$tiempo<-as.numeric(heart$tiempo)
fit <- survfit(Surv(tiempo,evento_muerte) ~ 1, data = pico)
fit
ggsurvplot(fit, data = heart, 
           palette = "blue", 
           conf.int = FALSE, 
           surv.median.line = "hv")



heart$tiempo<-as.numeric(heart$tiempo)
heart$evento_muerte<-as.factor(heart$evento_muerte)

## la base de heart para sacar la curva

fito <- survfit(Surv(tiempo,evento_muerte) ~ 1, data = heart)
fito
str(heart)
summary(fito)

ggsurvplot(fito, data = heart, 
           palette = "red", 
           conf.int = FALSE, 
           surv.median.line = "hv")

data(aml)


# sexo

fit <- survfit(Surv(tiempo,evento_muerte) ~ sexo, data = heart)
fit
ggsurvplot(fit, data = heart, 
           conf.int = TRUE, 
           conf.int.style = "step", 
           surv.median.line = "hv", 
           pval = TRUE)

# fuma

fit <- survfit(Surv(tiempo,evento_muerte) ~ fuma, data = heart)
fit
ggsurvplot(fit, data = heart, 
           conf.int = TRUE, 
           conf.int.style = "step", 
           surv.median.line = "hv", 
           pval = TRUE)

# diabetes
fit <- survfit(Surv(tiempo,evento_muerte) ~ diabetes, data = heart)
fit
ggsurvplot(fit, data = heart, 
           conf.int = TRUE, 
           conf.int.style = "step", 
           surv.median.line = "hv", 
           pval = TRUE)


# anemia

fit <- survfit(Surv(tiempo,evento_muerte) ~ anemia, data = heart)
fit
ggsurvplot(fit, data = heart, 
           conf.int = TRUE, 
           conf.int.style = "step", 
           surv.median.line = "hv", 
           pval = TRUE)

# hipertension

fit <- survfit(Surv(tiempo,evento_muerte) ~ hipertension, data = heart)
fit
ggsurvplot(fit, data = heart, 
           conf.int = TRUE, 
           conf.int.style = "step", 
           surv.median.line = "hv", 
           pval = TRUE)

# edad




fit <- survfit(Surv(tiempo,evento_muerte) ~ edad_1, data = heart)
fit
ggsurvplot(fit, data = heart, 
           conf.int = TRUE, 
           conf.int.style = "step", 
           surv.median.line = "hv", 
           pval = TRUE)

survfit(Surv(tiempo, evento_muerte) ~ sexo + edad_1, heart, conf.type = "log-log") %>% 
  ggsurvplot(title = "Supervivencia entre Género por Edad", conf.int = T, 
             facet.by = "edad_1", legend.title = "Género", panel.labs = list(edadg = c("40-60", 
                                                                                      "50-70", "70-95")), short.panel.labs = T)


# creatinina

heart$creatinina_1<-cut(heart$creatinina,c(0,1.8,9.4))
with(heart, table(creatinina_1, evento_muerte))


fit <- survfit(Surv(tiempo,evento_muerte) ~ creatinina_1, data = heart)
fit
ggsurvplot(fit, data = heart, 
           conf.int = TRUE, 
           conf.int.style = "step", 
           surv.median.line = "hv", 
           pval = TRUE)


# fraccion_eyeccion

heart$fraccion_eyeccion_1<-cut(heart$fraccion_eyeccion,c(14,25,80))
with(heart, table(fraccion_eyeccion_1, evento_muerte))

fit <- survfit(Surv(tiempo,evento_muerte) ~ fraccion_eyeccion_1, data = heart)
fit
ggsurvplot(fit, data = heart, 
           conf.int = TRUE, 
           conf.int.style = "step", 
           surv.median.line = "hv", 
           pval = TRUE)


#Modelos de supervivencia

# https://rstudio-pubs-static.s3.amazonaws.com/375297_34390ade0ddb4dd2bbe3bf1abf884dfe.html#modelos_de_supervivencia

# Resumen del modelo ajustado

View(heart)
str(heart)

heart$hipertension<-factor(heart$hipertension, levels = c("0", "1"), 
                           
                           labels = c("no", "si"))
View(heart)

fit <- coxph(Surv(tiempo, evento_muerte) ~ hipertension, data = heart)

# Resumen del modelo ajustado
fit
ggforest(fit)


heart$hipertension<-as.factor(heart$hipertension)

ggadjustedcurves(fit, data=heart, 
                 variable = "hipertension", 
                 palette = "lancet")

str(heart)
ftest <- cox.zph(fit)
ftest
ggcoxzph(ftest)
ggcoxdiagnostics(fit, type = "deviance",
                 linear.predictions = FALSE)

ggcoxdiagnostics(fit, type = "schoenfeld",
                 ox.scale = "time")


# Gráfico de supervivencia
ggadjustedcurves(fit, data=heart, 
                 variable = "sexo", 
                 palette = "lancet")

sex.df <- with(heart, data.frame(sexo = c("Male", "Female")))
ggsurvplot(survfit(fit,newdata = sex.df), data = heart,
           palette = "lancet",
           conf.int = TRUE, 
           conf.int.style = "step",
           legend.labs=c("Male", "Female"))




datosgraf <- ggadjustedcurves(fit, data=heart, variable = "sexo", palette = "lancet")$dat
datossurv <- spread(datosgraf, key = variable, value =surv)
datossurv <- datossurv %>% mutate(dif = Female - Male)
# Gráfico
ggplot(datossurv,aes(x = time, y = dif))+ geom_smooth() + theme_minimal()


## ajuste del modelo multiples predictoras


data(lung)
summary(lung)
View(heart)
colnames(lung)

fitcom <- coxph(Surv(tiempo, evento_muerte) ~ edad+fuma+creatina_quinasa+sexo+hipertension+anemia+diabetes + creatinina
                +fraccion_eyeccion+nivel_sodio+plaquetas, data = heart)
str(heart)

frito <- coxph(Surv(tiempo, evento_muerte) ~ creatinina, data = heart)

coxph(Surv(tiempo,evento_muerte) ~ fuma + strata(creatinina) + strata(fraccion_eyeccion), data = heart)

# riesgos proporcionales
ftest <- cox.zph(fitcom)
ftest

# Gráfico de riesgos proporcionales
ggcoxzph(ftest)

# Observaciones influyentes
ggcoxdiagnostics(fitcom, type = "deviance",
                 linear.predictions = FALSE)
# no linealidad
ggcoxdiagnostics(fitcom, type = "schoenfeld",
                 ox.scale = "time")




#Estimación no-paramétrica de la función de supervivencia


#Estimador de Kaplan-Meier y Fleming-Harrington.


Surv(tiempo, 1)

fito<-survfit(Surv(tiempo, evento_muerte) ~ 1, heart, type = "kaplan-meier") 
fito
class(heart)
summary(fito)
fortify((fito))


# 2.2 Desviación estándar 
#e Intervalos de confianza de la estimación de la función de supervivencia.


tongue <- survfit(Surv(tiempo,evento_muerte) ~ 1, data = heart, type = "kaplan-meier", 
                     error = "tsiatis", conf.type = "log-log", conf.int = 0.99)
summary(tongue)
fortify(tongue)

# Graficación de la curva de supervivencia

ggsurvplot(fit = tongue, data = heart, conf.int = T, title = "Curva de Supervivencia", 
           xlab = "Tiempo", ylab = "Probabilidad de supervivencia", legend.title = "Estimación", 
           legend.labs = "Kaplan-Meier")

#  Estimación de la función de riesgo acumulado.



tongue <- survfit(Surv(tiempo,evento_muerte) ~ 1, data=heart)
R <- tongue %>% fortify %>% mutate(CumHaz = cumsum(n.event/n.risk))
R

# ejemplo con covariables pendiente......................

heart$hipertension<-factor(heart$hipertension, levels = c("0", "1"), 
                           
                           labels = c("no", "si"))

tongue <- survfit(Surv(tiempo,evento_muerte) ~ hipertension, heart)
tongue
R <- tongue %>% fortify %>% group_by(strata) %>% mutate(CumHaz = cumsum(n.event/n.risk))
print(R, 299)

plot(tongue, fun = "cumhaz", conf.int = F, main = "Riesgo Acumulado", col = 1:2, 
     xlab = "Tiempo (dias)", ylab = "Riesgo Acumulado")

ggsurvplot(tongue, fun = "cumhaz", xlab = "tiempo (dias)", censor = T, 
           ylab = "Riesgo Acumulado", title = "Riesgo Acumulado", legend.title = "Perfil de hipertension", 
           legend.labs = c("no", "si"))


#
tongue <- survfit(Surv(tiempo, evento_muerte) ~ 1, heart)
R <- tongue %>% fortify %>% mutate(CumHaz = cumsum(n.event/n.risk))
R

#
tongue <- survfit(Surv(tiempo, evento_muerte) ~ hipertension, heart)
R <- tongue %>% fortify %>% group_by(strata) %>% mutate(CumHaz = cumsum(n.event/n.risk))
R

qplot(tiempo, CumHaz, col = strata, data = R, geom = "step", xlab = "Tiempo (Semanas)", 
      ylab = "Riesgo Acumulado", main = "Riesgo Acumulado")

#Estimación de la media, mediana y percentiles de los tiempos de supervivencia.

print(tongue, print.rmean = TRUE)
quantile(tongue, c(0.05, 0.5, 0.95))


# 4 Modelo de Riesgo proporcionales de Cox



modelo_1 <- coxph(Surv(tiempo, evento_muerte) ~ edad+fuma+creatina_quinasa+sexo+hipertension+anemia+diabetes + creatinina
                +fraccion_eyeccion+nivel_sodio+plaquetas, data = heart)
summary(modelo_1)

modelo_2<- coxph(Surv(tiempo, evento_muerte) ~ creatinina+fraccion_eyeccion+edad+creatina_quinasa+
                   anemia, data = heart)

modelo_3<- coxph(Surv(tiempo, evento_muerte) ~ creatinina+fraccion_eyeccion
                 , data = heart)
summary(modelo_3)

modelo_4<- coxph(Surv(tiempo, evento_muerte) ~ edad+creatinina+fraccion_eyeccion_1+hipertension
                 , data = heart)

summary(modelo_2)  # 3= 53.66 7 #2=47.47 #1=81

library(lmtest)
anova(modelo_1, modelo_2,modelo_3)

waldtest(modelo_2,modelo_3)
View(lung)



lung$agenew <- cut(lung$age,quantile(lung$age,na.rm = TRUE))
View(lung)
fit1 <- coxph(Surv(time, status) ~ sex, data = lung)
fit2 <- coxph(Surv(time, status) ~ sex + agenew, data = lung)
fit3 <- coxph(Surv(time, status) ~ sex + agenew + wt.loss, data = lung)

anova(fit1,fit2)
str(lung)




#### analisis de base de datos de riñon
library(survival)
library(kidtran)
data(kidtran)
View(kidtran)

kidtran$gender <- factor(kidtran$gender, labels = c("Hombre", "Mujer"))
kidtran$race <- factor(kidtran$race, labels = c("Blanco", "Negro"))

kidtran$edadg <- cut(kidtran$age, c(0, 25, 50, 75))
with(kidtran, table(edadg, delta))

heart$edad_1<-cut(heart$edad,c(40,50,70,95))
with(heart, table(edad_1, evento_muerte))

heart$creatinina_1<-cut(heart$creatinina,c(0,1.8,9.4))
with(heart, table(creatinina_1, evento_muerte))

summary(heart$creatinina)
#Estimación No-Parametríca


### prediccion

datosnuevo <- data.frame(edad = c(70,80,90, 95))
Pred <- survfit(modelo_3, newdata = datosnuevo)
ggsurvplot(Pred, title = "Curva de Supervivencia por Edad")

??pred

# Supervivencia para menores de 70 años


View(heart)
datosnuevo <- data.frame(creatinina = c(1,0.45,1.4),fraccion_eyeccion=c(20,45,67),hipertension=c(0,1,1))
Pred <- survfit(modelo_3, newdata = datosnuevo)
ggsurvplot(Pred, title = "Curva de Supervivencia por Edad")

summary(datosnuevo)

sex.df <- with(heart, data.frame(edad = c("60", "90"))
                                
ggsurvplot(survfit(modelo_2,newdata = datosnuevo), data = heart,
           palette = "lancet",
           conf.int = TRUE, 
           conf.int.style = "step",
         )


creatinina+fraccion_eyeccion+hipertension


# vamos a verificar las hipotesis de riesgos proporcionales
#Utilizamos la función cox.zph() para evaluar mediante tests 
#estadísticos la hipótesis de riesgos proporcionales,
#y la función ggcoxzph() para el análisis gráfico

# Tests de riesgos proporcionales


ftest <- cox.zph(modelo_3)
ftest

ggcoxzph(ftest)

#Observaciones influyentes
#La detección de observaciones influyentes se realiza mediante métodos gráficos

ggcoxdiagnostics(modelo_3, type = "deviance",
                 linear.predictions = FALSE)

# No linealidad

ggcoxdiagnostics(modelo_3, type = "schoenfeld",
                 ox.scale = "time")

ggcoxdiagnostics(modelo_3, type = "martingale")

ggcoxdiagnostics(modelo_3, type = "dfbeta")

# vamos a recodificar fraccion eyeccion y despues plantear otro modelo_3

# Ajuste del modelo


modelo_4<- coxph(Surv(tiempo, evento_muerte) ~ edad+creatinina+fraccion_eyeccion_1+hipertension
                 , data = heart)

heart$fraccion_eyeccion_1<-cut(heart$fraccion_eyeccion,c(14,25,80))
with(heart, table(fraccion_eyeccion_1, evento_muerte))

#vamos a plantear linealidad de modelo_4


ggcoxdiagnostics(modelo_4, type = "schoenfeld",
                 ox.scale = "time")

ggcoxdiagnostics(modelo_4, type = "martingale")

ggcoxdiagnostics(modelo_4, type = "dfbeta")

# Observaciones influyentes
ggcoxdiagnostics(modelo_4, type = "deviance",
                 linear.predictions = FALSE)

# puntos de corte

corte <- surv_cutpoint(heart, time = "tiempo", 
                       event = "evento_muerte", 
                       variables = c("edad","fraccion_eyeccion","creatinina"))
corte


# No linealidad
ggcoxdiagnostics(modelo_4, type = "schoenfeld",
                 ox.scale = "time")

### 

heart$edadnew <- ifelse(heart$edad>=corte$edad$estimate,"+70","-70")
lung$wtnew <- ifelse(lung$wt.loss>=corte$wt.loss$estimate,"+9","-9")
View(heart)

## grafico de supervivencia hipertension


fit <- coxph(Surv(tiempo, evento_muerte) ~ hipertension, data = heart)
ggadjustedcurves(fit, data=heart, 
                 variable = "hipertension", 
                 palette = "lancet")



sex.df <- with(heart, data.frame(hipertension = c("si", "no")))

ggsurvplot(survfit(fit,newdata = sex.df), data = heart,
           palette = "lancet",
           conf.int = TRUE, 
           conf.int.style = "step",
           legend.labs=c("si", "no"))

# Guardamos la información del gráfico
datosgraf <- ggadjustedcurves(fit, data=heart, variable = "hipertension", palette = "lancet")$dat
datossurv <- spread(datosgraf, key = variable, value =surv)
datossurv <- datossurv %>% mutate(dif = si - no)
# Gráfico
ggplot(datossurv,aes(x = time, y = dif))+ geom_smooth() + theme_minimal()

### variale fraccion eyeccion:

heart$fraccion_eyeccion_1<-cut(heart$fraccion_eyeccion,c(14,25,80))
with(heart, table(fraccion_eyeccion_1, evento_muerte))





