rm(list=ls())
#install.packages("readODS")
library(readODS)

dati <- read_ods("sondaggio_mobili_bagno.ods")
head(dati)
View(dati)
str(dati)
##pulizia dataset
dati$dimensione_cm=as.factor(dati$dimensione_cm)
dati$numero_cassetti=as.factor(dati$numero_cassetti)
table(dati$RISPOSTA)


dati$RISPOSTA = as.factor(dati$RISPOSTA)
dati$colore = as.factor(dati$colore)
dati$lavabo = as.factor(dati$lavabo)
dati$all_inclusive = as.factor(dati$all_inclusive)
dati$ante = as.factor(dati$ante)
dati$forma_specchiera = as.factor(dati$forma_specchiera)
dati$type = as.factor(dati$type)
dati$illuminazione = as.factor(dati$illuminazione)
dati$`DAY-Time`=NULL
str(dati)
sum(is.na(dati))

##funzione degli errori
fun.errori=function(previsti,osservati){
  pr_f=factor(previsti,levels=c(F,T))
  true_f=factor(osservati)
  tab=table(pr_f,true_f)
  c(
    "ce"=1-sum(diag(tab))/sum(tab),
    "fp"=tab[2,1]/(tab[2,1]+tab[1,1]),
    "fn"=tab[1,2]/(tab[1,2]+tab[2,2]),
    "F1"=2*tab[2,2]/(2*tab[2,2]+tab[1,2]+tab[2,1])
  )
}
source("lift-roc-tab.R")
str(dati)


##uso tutti gli altri modelli con 0/1 così posso fingere anche che sia numerica (come
#facevamo a data mining)
table(dati$RISPOSTA)
dati$y=ifelse(dati$RISPOSTA=="N",0,1)
table(dati$y)
dati$RISPOSTA=NULL
str(dati)
set.seed(123)
gruppi=sample(1:nrow(dati),0.7*nrow(dati))
stima=dati[gruppi,]
verifica=dati[-gruppi,]
dim(stima)
str(stima)
nomi=names(stima[,-10])
head(stima)
str(stima)


##MODELLO LINEARE GENERALIZZATO, sia senza che successivamente tentativo con interazioni
scope=as.formula(paste("~ ", paste(nomi, collapse=" + ")))
mod_null_glm=glm(y~1,family="binomial",data=stima)
mod_step_forw_glm = step(mod_null_glm, scope = scope, direction = "forward")
summary(mod_step_forw_glm)
pred_glm=predict(mod_step_forw_glm,verifica,type="response")
err_glm=tabella.sommario(pred_glm>0.5,verifica$y)
err_glm=fun.errori(pred_glm>0.5,verifica$y)

lift.roc(pred_glm,verifica$y)

###con interazioni
scope = as.formula(
  paste("~ (", paste(nomi, collapse=" + "), ")^2")
)
mod_null_glm=glm(y~1,family="binomial",data=stima)
mod_step_forw_glm = step(mod_null_glm, scope = scope, direction = "forward")
summary(mod_step_forw_glm)
##nessuna interazione significativa



##lasso logistico
set.seed(123)
x.s=model.matrix(y~.,data=stima)
x.v=model.matrix(y~.,data=verifica)
mod_lasso_bin=glmnet(x.s[,-1],stima$y,alpha=1,family="binomial")
plot(mod_lasso_bin,xvar="lambda")
mod_cv_lasso2=cv.glmnet(x.s[,-1],stima$y,alpha=1,family="binomial")
plot(mod_cv_lasso2)
par(mfrow=c(1,1))
plot(mod_cv_lasso2)
abline(v=log(mod_cv_lasso2$lambda.min))
abline(v=log(mod_cv_lasso2$lambda.1se),col=2)
lambda_ott2=mod_cv_lasso2$lambda.1se

mod_lasso_fin2=glmnet(x.s[,-1],stima$y,alpha=1,lambda=lambda_ott2,family="binomial")
coef(mod_lasso_fin2)
pred_lasso2=predict(mod_lasso_fin2,x.v[,-1])
err_lasso2=tabella.sommario(pred_lasso2>0.5,verifica$y)
lift.roc(pred_lasso2,verifica$y)
err_lasso2=fun.errori(pred_lasso2>0.5,verifica$y)
err_lasso2


##implementazione CART
set.seed(123)
library(tree)
mod_tree=tree(y~.,data=stima,control=tree.control(nobs=NROW(stima),minsize=2,mindev=0.000001),family="binomial")
par(mfrow=c(1,1))
plot(mod_tree)
text(mod_tree)
set.seed(123)
prune=cv.tree(mod_tree,K=10)
plot(prune$size, prune$dev, type = "b", xlab = "Dimensione albero", ylab = "Errore CV")
par(mfrow=c(1,1))
plot(prune,xlim=c(0,50))
str(prune)
J=prune$size[which.min(prune$dev)]
abline(v=J,lwd=2,col=2)
mod_tree_best=prune.tree(mod_tree,best=J)
plot(mod_tree_best)
text(mod_tree_best,pretty=4,cex=0.8)
summary(mod_tree)
summary(mod_tree_best)
pred_tree=predict(mod_tree_best,verifica)
err_tree=tabella.sommario(pred_tree>0.5,verifica$y)
err_tree=fun.errori(pred_tree>0.5,verifica$y)



###implementazione mars (inserimento di interazione fino ad ordine 2)
set.seed(123)
library(earth)
mod_mars2=earth(y~.,data=stima,trace=0,degree=2,nk=300)
summary(mod_mars2)
plot(mod_mars2$gcv.per.subset, pch = 16, xlab = "Numero di basi", ylab = "GCV")
abline(v = which.min(mod_mars2$gcv.per.subset), col = 2)
par(mfrow=c(2,3))
plotmo(mod_mars2,degree1=T,degree2=F,do.par=F)#vedo i grafici per le funzioni di base
#che entrano rispetivamente senza interazioni (e sotto con interazioni)
plotmo(mod_mars2,degree1=F,degree2=T,do.par=F)

par(mfrow=c(1,2))
plot(mod_mars2, which=1)
plot(mod_mars2, which=2)
evimp(mod_mars2)
pred_mars=predict(mod_mars2,verifica)

err_mars=tabella.sommario(pred_mars>0.5,verifica$y)
lift.roc(pred_mars,verifica$y)
err_mars=fun.errori(pred_mars>0.5,verifica$y)


  
############RANDOM FOREST
###fissiamo ntree e ottimizziamo mtry:
library(randomForest)
set.seed(123)
p= ncol(stima) - 1
mtry_grid= unique(c(1, floor(sqrt(p)), floor(p/3), floor(p/2), p))

err= numeric(length(mtry_grid))

for(i in seq_along(mtry_grid)) {
  mod_rf= randomForest(
    factor(y) ~ ., 
    data = stima, 
    ntree = 500,
    mtry = mtry_grid[i]
  )
  
  pred_rf <- predict(mod_rf, newdata = verifica, type = "prob")[,2]
  err[i] <- fun.errori(pred_rf > 0.5, verifica$y)
}
err
plot(mtry_grid,err,type="l")
abline(v=mtry_grid[which.min(err)])
mtry_best=mtry_grid[which.min(err)]##mtry ottimale è 3
set.seed(123)
mod_rf_best=randomForest(factor(y) ~ ., data = stima, ntree = 500, mtry = mtry_best)
pred_rf_best= predict(mod_rf_best, newdata=verifica, type="prob")[,2]##probabilità per ciascuna classe
err_rf_best=tabella.sommario(pred_rf_best>0.5,verifica$y)
lift.roc(pred_rf_best,verifica$y)
err_rf_best=fun.errori(pred_rf_best>0.5,verifica$y)

par(mfrow=c(1,1))
plot(mod_rf_best)
legend("topright",
       legend = c("Errore OOB", "Classe 0", "Classe 1"),
       col = c(1, 2, 3),
       lty = 1, lwd = 2)


var_imp_best <- randomForest::importance(mod_rf_best)[, 1]  
names(var_imp_best) <- rownames(randomForest::importance(mod_rf_best))
##rappresentazione grafico importanza
imp <- sort(var_imp_best, decreasing = TRUE)
df_imp <- data.frame(
  Variabile = factor(names(imp), levels = names(imp)),
  Importanza = as.numeric(imp)
)
ggplot(df_imp,
       aes(x = Variabile,
           y = Importanza)) +
  
  geom_col(fill = "#A6CEE3", width = 0.8) +
  
  coord_flip() +
  
  labs(
    title = "Variable Importance - Random Forest",
    x = "",
    y = "Importance"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    plot.title = element_text(face = "bold"),
    axis.text.y = element_text(size = 10)
  )


#PPR
set.seed(123)
dim(stima)
#ridivido in stima e convalida se ho stima grande
gruppii=sample(1:nrow(stima),0.8*nrow(stima))
err=matrix(NA,1,10)
#e quindi:
grid=1:10
length(grid)
#si scelgono il numero di dorsali ottimali:
for (j in 1:length(grid)){ #le mie funzioni dorsali da 1 a 10
  stimm=stima[gruppii,]
  verr=stima[-gruppii,]
  mod_ppr=ppr(y~.,data=stimm,nterms=grid[j])
  pred=predict(mod_ppr,verr)
  err_ppr=fun.errori(pred>0.5,verr$y)
  err[1,j]=err_ppr["F1"]
}
err
par(mfrow=c(1,1))
plot(grid,err,type="l")
#mod_finale con la funzione dorsale che mi massimizza F1
mod_ppr=ppr(y~.,data=stima,nterms=2)
pred=predict(mod_ppr,verifica)
err_ppr=tabella.sommario(pred>0.5,verifica$y)
lift.roc(pred,verifica$y)
err_ppr=fun.errori(pred>0.5,verifica$y)

####bagging
library(ipred)
err=NA
numero_alberi=c(50,100,150,200,250,300,500,600)
for (alberi in c(50,100,150,200,250,300,500,600)) {
  set.seed(123)
  mbag = bagging(factor(y) ~ ., data = stima,
                 nbagg = alberi, coob = TRUE)
  err = c(err, mbag$err)
  cat("Alberi:", alberi, " - OOB error:", mbag$err, "\n")
}

#plot dell' OOB error
plot(c(50,100,150,200,250,300,500,600), err[-1], type = "b", lwd = 2,
     xlab = "Numero di alberi", ylab = "Errore OOB",
     main = "Stabilizzazione errore OOB")
best_alb=which.min(err[-1])
best_ntree=numero_alberi[best_alb]##numero alberi ottimali 20
abline(v = best_ntree, col = "red", lty = 2)

#errore di classificazione
set.seed(123)
##NOTA:
#sarebbe stato nbagg 150 ma non si stabilizzava l'errore 
#(guardando a occhio il grafico oob abbiamo scelto
#500 alberi)
mbag = bagging(factor(y) ~ ., data = stima,
               nbagg = 500, coob = TRUE)

err_bag = sapply(mbag$mtrees, function(x) {
  pred = predict(x$btree, stima[-x$bindx, ], type = "class")
  mean(pred != stima$y[-x$bindx])
})
err_cumulativa = cumsum(err_bag) / seq_along(err_bag)
plot(err_cumulativa, type = "l", lwd = 2,
     xlab = "Numero di alberi", ylab = "Errore medio cumulativo",
     main = "Evoluzione errore medio (OOB)")

pbagg1 = predict(mbag, verifica, type = "prob")[, 2]
err_bagging=tabella.sommario(pbagg1>0.5,verifica$y)
lift.roc(pbagg1,verifica$y)
err_bagging=fun.errori(pbagg1>0.5,verifica$y)
err_bagging

#LDA
library(MASS)
m_lda = lda(as.factor(y)~., data = stima)
pr_lda=predict(m_lda,verifica)
pr_lda$class
table(pr_lda$class, verifica$y)
pr_lda$posterior[,2]
err_lda=fun.errori(pr_lda$posterior[,2]>0.5,verifica$y)##mi esce NaN, te sai perchè? la tabella sommario
#funziona invece
lift.roc(pr_lda$class,verifica$y)



#XGBOOST
library(gbm)
##CGBOOST STUMP NO TUNING LEARNING RATE
set.seed(123)
mod_gbm=gbm(y ~ ., data=stima, 
            distribution="bernoulli", n.trees=5000, interaction.depth=1)
summary(mod_gbm)
plot(mod_gbm$train.error, type="l", ylab="training error")
yhat1 <- predict(mod_gbm, newdata = verifica, type = "response",n.trees=1:5000)

##XGBOOST DEPTH 4 NO TUNING LEARNING RATE
set.seed(123)
mod_gbm4=gbm(y ~ ., data=stima, 
             distribution="bernoulli", n.trees=5000, interaction.depth=4)
plot(mod_gbm4$train.error, type="l", ylab="training error")
yhat_depth <- predict(mod_gbm4, newdata = verifica, type = "response",n.trees=1:5000)
summary(mod_gbm4)


#XGBOOST STUMP CON TUNING LEARNING RATE
gbm_learn=gbm(y ~ ., data=stima, 
              distribution="bernoulli", n.trees=5000, interaction.depth=1, shrinkage=0.01)
plot(gbm_learn$train.error, type="l", ylab="training error")
yhat_learn <- predict(gbm_learn, newdata = verifica, type = "response",n.trees = 1:5000)


#XGBOOST DEPTH 4 CON TUNING LEARNING RATE
gbm_learn4=gbm(y ~ ., data=stima, 
               distribution="bernoulli", n.trees=5000, interaction.depth=4, shrinkage=0.01)
plot(gbm_learn4$train.error, type="l", ylab="training error")
yhat_learn4 <- predict(gbm_learn4, newdata = verifica, type = "response",n.trees = 1:5000)


plot(mod_gbm4$train.error,type="l",col="red")
lines(mod_gbm$train.error,type="l",col="green")
lines(gbm_learn$train.error,type="l",col="blue")
lines(gbm_learn4$train.error,type="l",col="pink")




err_test1 <- apply(yhat1, 2, function(p) {
  pred_class <- ifelse(p > 0.5, 1, 0)
  mean(pred_class != verifica$y)
})

err_test_depth <- apply(yhat_depth, 2, function(p) {
  pred_class <- ifelse(p > 0.5, 1, 0)
  mean(pred_class != verifica$y)
})

err_test_learn<- apply(yhat_learn, 2, function(p) {
  pred_class <- ifelse(p > 0.5, 1, 0)
  mean(pred_class != verifica$y)
})

err_test_learn4 <- apply(yhat_learn4, 2, function(p) {
  pred_class <- ifelse(p > 0.5, 1, 0)
  mean(pred_class != verifica$y)
})



###grafico andamento degli errori dei 4 modelli a confronto
mins <- data.frame(
  modello = c("depth1","depth4","shrink","learn4"),
  trees = c(which.min(err_test1),
            which.min(err_test_depth),
            which.min(err_test_learn),
            which.min(err_test_learn4))
)
df_plot <- data.frame(
  trees = 1:5000,
  depth1 = err_test1,
  depth4 = err_test_depth,
  shrink = err_test_learn,
  learn4=err_test_learn4
)
df_long <- df_plot %>%
  pivot_longer(cols = -trees,
               names_to = "modello",
               values_to = "error")
ggplot(df_long, aes(x = trees, y = error, color = modello)) +
  geom_line(linewidth = 1) +
  geom_vline(
    data = mins,
    aes(xintercept = trees, color = modello),
    linetype = "dashed",
    show.legend = FALSE
  ) +
  labs(
    title = "Test error vs numero di alberi (GBM)",
    x = "Numero di alberi",
    y = "Test error"
  ) +
  coord_cartesian(ylim = c(0, 0.9)) +
  theme_minimal(base_size = 13)


best1 <- which.min(err_test1)
best_depth <- which.min(err_test_depth)
best_learn <- which.min(err_test_learn)
best_learn4 <- which.min(err_test_learn4)




##troviamo errore minimo ottenuto e numero alberi
#STUMP NO TUNING LR
best1_alberi=which.min(err_test1)
err_test1[which.min(err_test1)]##errore minimo ottenuto
#DEPTH 4 NO TUNING LR
best_depth_alberi=which.min(err_test_depth)
err_test_depth[which.min(err_test_depth)]##errore minimo ottenuto
#STUMP TUNING LR
best_learn_alberi=which.min(err_test_learn)
err_test_learn[which.min(err_test_learn)]
#DEPTH 4 TUNING LR
best_learn4 <- which.min(err_test_learn4)
err_test_learn4[which.min(err_test_learn4)]


##PRED XGBOOST STUMP NO TUNING LR
pred_fin1=predict(mod_gbm, newdata = verifica, type = "response",n.trees=best1_alberi)
err_gbm_tab1=tabella.sommario(pred_fin1>0.5,verifica$y)
err_gbm1 <- fun.errori(pred_fin1>0.5, verifica$y)

#PRED XGBOOST DEPTH 4 NO TUNING LR
pred_fin4=predict(mod_gbm4, newdata = verifica, type = "response",n.trees=best_depth_alberi)
err_gbm_tab4=tabella.sommario(pred_fin4>0.5,verifica$y)
err_gbm4 <- fun.errori(pred_fin4>0.5, verifica$y)



#PRED XGBOOST STUMP TUNING LR
pred_fin_learn=predict(gbm_learn, newdata = verifica, type = "response",n.trees=best_learn_alberi)
err_gbm_tab_learn=tabella.sommario(pred_fin_learn>0.5,verifica$y)
err_gbm_learn <- fun.errori(pred_fin_learn>0.5, verifica$y)

#PRED XGBOOST DEPTH 4 TUNING LR
pred_fin_learn4=predict(gbm_learn4, newdata = verifica, type = "response",n.trees=best_learn4)
err_gbm_tab_learn4=tabella.sommario(pred_fin_learn4>0.5,verifica$y)
err_gbm_learn4 <- fun.errori(pred_fin_learn4>0.5, verifica$y)



###PDP PER XGBOOST CON DEPTH 4 NO TUNING LEARNING RATE
library(ggplot2)
##PDP colore
pd_col <- partial(
  object = mod_gbm4,
  pred.var = "colore",
  train = stima,
  n.trees = 30,
  plot = FALSE
)

ggplot(pd_col, aes(x = colore, y = yhat)) +
  geom_col(fill = "#003366") +
  labs(
    title = "Partial Dependence - Colore",
    x = "Colore",
    y = "Probabilità stimata"
  ) +
  theme_minimal(base_size = 13)


##pdp dimensione
pd_dim <- partial(
  object = mod_gbm4,
  pred.var = "dimensione_cm",
  train = stima,
  n.trees = 30,
  plot = FALSE
)
ggplot(pd_dim, aes(x = dimensione_cm, y = yhat)) +
  geom_col(fill =  "#003366") +
  labs(
    title = "Partial Dependence - Dimensione cm",
    x = "Dimensione",
    y = "Probabilità stimata"
  ) +
  theme_minimal(base_size = 13)


##pdp numero cassetti
pd_col <- partial(
  object = mod_gbm4,
  pred.var = "numero_cassetti",
  train = stima,
  n.trees = 30,
  plot = FALSE
)

ggplot(pd_col, aes(x = numero_cassetti, y = yhat)) +
  geom_col(fill = "#003366") +
  labs(
    title = "Partial Dependence - Numero cassetti",
    x = "Cassetti",
    y = "Probabilità stimata"
  ) +
  theme_minimal(base_size = 13)

###pdp illuminazione
pd_col <- partial(
  object = mod_gbm4,
  pred.var = "illuminazione",
  train = stima,
  n.trees = 30,
  plot = FALSE
)

ggplot(pd_col, aes(x = illuminazione, y = yhat)) +
  geom_col(fill = "#003366") +
  labs(
    title = "Partial Dependence - Illuminazione",
    x = "Illuminazione",
    y = "Probabilità stimata"
  ) +
  theme_minimal(base_size = 13)

