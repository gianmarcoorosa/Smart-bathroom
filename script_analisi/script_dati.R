rm(list=ls())
gc()
library(readxl)
library(DIMORA)
library(ggplot2)
library(tidyr)
library(prophet)


dd=read_xlsx("C:/Users/Utente/Downloads/ss_ide3_sist.xlsx")
head(dd)
dd=data.frame(dd)
head(dd)
dd$...1=dd$X=NULL




plot(dd[, 1], type="l", ylim=c(0, 30), xlab="Mesi", ylab="Vendite mensili")
for(i in 1:16) {
  line_col <- ifelse(i %in% 1:4, "black", 
                     ifelse(i %in% 5:8, "purple",
                            ifelse(i %in% 9:12, "grey", "brown")))
  lines(1:120, dd[, i], col=line_col, lyt=2)
}



################################# GRAFICO
dd <- as.data.frame(dd)
nomi_colonne <- colnames(dd)[1:16]
dd$Mese <- 1:120

dd_long <- pivot_longer(dd, 
                        cols = all_of(nomi_colonne), 
                        names_to = "Serie", 
                        values_to = "Vendite")

gruppi_nomi <- rep(c("1. Prime 4", 
                     "2. Seconde 4", 
                     "3. Terze 4", 
                     "4. Ultime 4"), each = 4)
names(gruppi_nomi) <- nomi_colonne
dd_long$Gruppo <- gruppi_nomi[dd_long$Serie]
colori <- c("1. Prime 4" = "black", 
            "2. Seconde 4" = "black", 
            "3. Terze 4" = "gray", 
            "4. Ultime 4" = "brown")

tipi_linea <- c("1. Prime 4" = "solid", 
                "2. Seconde 4" = "dashed", 
                "3. Terze 4" = "solid", 
                "4. Ultime 4" = "solid")

grafico_base <- ggplot(mapping = aes(x = Mese, y = Vendite, color = Gruppo, linetype = Gruppo, group = Serie)) +
  scale_color_manual(values = colori) +
  scale_linetype_manual(values = tipi_linea) +
  ylim(0, 30) +
  labs(x = "Mesi", y = "Vendite mensili") +
  theme_classic() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "white", color = NA),
    legend.position = "right",           # Mostra la legenda a destra
    legend.title = element_blank()       # Rimuove il titolo per renderla più pulita
  )



p1 <- grafico_base + geom_line(data = subset(dd_long, Gruppo %in% c("1. Prime 4")), linewidth = 0.8)
print(p1)
p2 <- grafico_base + geom_line(data = subset(dd_long, Gruppo %in% c("1. Prime 4", "2. Seconde 4")), linewidth = 0.8)
print(p2)
p3 <- grafico_base + geom_line(data = subset(dd_long, Gruppo %in% c("1. Prime 4", "2. Seconde 4", "3. Terze 4")), linewidth = 0.8)
print(p3)
p4 <- grafico_base + geom_line(data = dd_long, linewidth = 0.8) # Mette tutto
print(p4)


################### CLUSTERING BASATO SUI COEFFICIENTI DI BASS
risultati=matrix(nrow=16, ncol=3)
for(i in 1:16){
  bm_tmp=BM(dd[, i], display=F)
  risultati[i,]=coef(bm_tmp)
}
# PLOT DEI COEFFICIENTI DI BASS
set.seed(34)
res_gbm=matrix(nrow=16, ncol=6)
for(i in 1:16){
  bm=GBM(dd[, i], shock="rett", nshock=1, 
         prelimestimates = c(risultati[i, ], 50, 66, 0.001))
  res_gbm[i, ]=coef(bm)
}
# tolgo quelli con a<0 o con b<a
non_validi_minore_0=which(res_gbm[, 4]<0)   
non_validi_b_piccolo=which(res_gbm[,4]>res_gbm[, 5])
non_validi=c(non_validi_minore_0, non_validi_b_piccolo)
m_p_q=res_gbm[, 1:3]
m_p_q[non_validi,]=risultati[non_validi, ]
# questi sono i valori di m,p,q dove se presente uno shock rettangolare
# sono considerati quei m,p,q, mentre se non presente non sono considerati
rownames(m_p_q)=colnames(dd)[1:16]
p_q=m_p_q[, 2:3]
plot(p_q[,1], p_q[,2])
text(p_q[,1], p_q[,2], labels=rownames(p_q),pos=3, cex=0.6)
df = as.data.frame(p_q)
colnames(df) = c("p", "q")

df$Finitura = gsub("_[0-9]+", "", rownames(p_q))
df$Misura = gsub("[a-zA-Z]+_", "", rownames(p_q))
colori_base = c("bianco" = "lightgrey", "grigio" = "grey", "antracite" = "black", "noce" = "brown")
ggplot(df, aes(x = p, y = q, color = Finitura, label = Misura)) +
  geom_point(size = 4) +                                      
  geom_text(hjust = -0.5, vjust = 0.5, color = "black", size = 4) +         
  scale_color_manual(values = colori_base) + 
  ylim(0.0001, 0.062) +
  xlim(0.0001, 0.0075)+
  theme_bw() +                                                
  theme(
    panel.grid.major = element_blank(),                   
    panel.grid.minor = element_blank(),                      
    panel.border = element_rect(color = "black", size = 1)    
  )   

ggplot(df, aes(x = p, y = q, color = Finitura, label = Misura)) +
  geom_point(size = 4) +                                      
  geom_text(hjust = -0.5, vjust = 0.5, color = "black", size = 4) +         
  geom_vline(xintercept = 0.0029, linetype = "dashed") +      
  geom_hline(yintercept = 0.025, linetype = "dashed") +       
  scale_color_manual(values = colori_base) + 
  ylim(0.0001, 0.062) +
  xlim(0.0001, 0.0075)+
  theme_bw() +                                                
  theme(
    panel.grid.major = element_blank(),                   
    panel.grid.minor = element_blank(),                      
    panel.border = element_rect(color = "black", size = 1)    
  )                                            







## stima dei coefficienti di una spline
## definizione della polinomiale a tratti
## clustering sui coefficienti OLS 
mesi=1:120
X= matrix(nrow=120, ncol=11)
X[, 1]=rep(1, 120)
X[, 2]=mesi
X[, 3]=(mesi^2)
X[, 4]=mesi^3
X[, 5]=pmax(0, (mesi-36))^3
X[, 6]=pmax(0, (mesi-48))^3
X[, 7]=pmax(0, (mesi-60))^3
X[, 8]=pmax(0, (mesi-72))^3
X[, 9]=pmax(0, (mesi-84))^3
X[, 10]=pmax(0, (mesi-96))^3
X[, 11]=pmax(0, (mesi-108))^3

res_spli = matrix(nrow=16, ncol=11) 
for(i in 1:16){
  mod_tmp = lm(dd[, i] ~ X - 1)     
  s = summary(mod_tmp)$coefficients
  coef_tmp = s[, 1]  
  pval_tmp = s[, 4]  
  non_signif = which(pval_tmp >= 0.01)
  coef_tmp[non_signif] = 0
  res_spli[i, ] = coef_tmp   
}
row.names(res_spli)=colnames(dd)[1:16]

distanze <- dist(scale(res_spli), method = "maximum")
hc <- hclust(distanze, method = "ward.D2")
plot(hc, 
     main = "Dendrogramma delle Curve di Vendita", 
     xlab = "Indice della Serie Storica", 
     ylab = "Distanza", 
     sub = "")

colnames(dd)





library(fda)
base_spline <- create.bspline.basis(rangeval = c(1, 120), nbasis = 10)
curve_fd <- smooth.basis(argvals = mesi, y = as.matrix(dd), fdParobj = base_spline)$fd
pca_funzionale <- pca.fd(curve_fd, nharm = 2)
df_scores <- as.data.frame(pca_funzionale$scores)
colnames(df_scores) <- c("Score_1", "Score_2")
rownames(df_scores) <- colnames(dd)
df_scores$Finitura <- gsub("_[0-9]+", "", rownames(df_scores))
df_scores$Misura <- gsub("[a-zA-Z]+_", "", rownames(df_scores))
modello_kmeans <- kmeans(df_scores[, c("Score_1", "Score_2")], centers = 3)
df_scores$Cluster <- as.factor(modello_kmeans$cluster)
colori_base <- c("bianco" = "lightgrey", "grigio" = "grey", "antracite" = "black", "noce" = "brown")
ggplot(df_scores, aes(x = Score_1, y = Score_2, color = Finitura, label = Misura)) +
  
  # Punti ed etichette
  geom_point(size = 4) +
  geom_text(hjust = -0.5, vjust = 0.5, color = "black", size = 4) +
  scale_color_manual(values = colori_base) +
  
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, size = 1)
  ) +
  labs(
    title = "Clustering FDA: Punteggi Funzionali",
    x = "Prima Armonica (Score 1)",
    y = "Seconda Armonica (Score 2)"
  )


###########################################################
###########################################################
########### MIGLIOR MODELLO PER PREVISIONE ################
# approccio expanding window

errori_bass <- numeric(40)
df=dd

for (i in 1:40) {
  x_tmp <- df$grigio_80[1:(80 + i)]
  bass_tmp <- BM(x_tmp, display = F)
  
  pred_bmtw <- predict(bass_tmp, newx = c(1:(80 + i + 2)))
  pred.insttw<- make.instantaneous(pred_bmtw)
  
  errori_bass[i] <- tail(pred.insttw,1) - df$grigio_80[82 + i]
}

mse_bass <- mean(errori_bass[1:38]^2)


## approccio tramite rolling mean
errori_mean <- numeric(40)

for (i in 1:40) {
  x_tmp <-  df$grigio_80[1:(80+i)]
  pred_mean <- mean( df$grigio_80[  (80-12+i):(80+i)] )
  errori_mean[i] <- pred_mean - df$grigio_80[(80 + i + 2)]
  
}
mse_mean <- mean((errori_mean[1:38])^2)

## approccio tramite regressione lineare disponendo di alcuni ritardi
errori_lm <- numeric(38)
serie <- df$grigio_80
idx <- 13:120

dati_lm <- data.frame(
  tempo = idx,
  y = serie[idx],
  lag1 = serie[idx - 1],
  lag2 = serie[idx - 2],
  lag3 = serie[idx - 3],
  lag4 = serie[idx - 4],
  mean_12 = sapply(idx, function(i) mean(serie[(i - 12):(i - 1)])),
  same_month_last_year = serie[idx - 12]
)

for (i in 1:38) {
  
  t_pred <- 80 + i
  
  train <- dati_lm[dati_lm$tempo <= t_pred, ]
  test  <- dati_lm[dati_lm$tempo == t_pred + 2, ]
  
  modello <- lm( y ~ lag2 + lag3 + lag4 + mean_12 + same_month_last_year,
    data = train)
  
  predizione <- predict(modello, newdata = test)
  errori_lm[i] <- predizione - test$y
}

mse_lm <- mean(errori_lm^2)
mse_lm


## previsione tramite xgboost
library(xgboost)
errori_xgb <- numeric(38)

for (i in 1:38) {
  
  t_pred <- 80 + i      # cambiato da 79+i
  train <- dati_lm[dati_lm$tempo <= t_pred, ]
  test  <- dati_lm[dati_lm$tempo == t_pred + 2, ]
  
  X_train <- as.matrix(train[, -c(1,2)])
  y_train <- train$y
  X_test <- as.matrix(test[, -c(1,2)])
  
  modello_xgb <- xgboost(
    data = X_train,
    label = y_train,
    nrounds = 30,
    objective = "reg:squarederror",
    verbose = 0, 
    max_depth = 6,
    learning_rate = 0.3
  )
  
  predizione <- predict(modello_xgb, X_test)
  errori_xgb[i] <- predizione - test$y
}

mse_xgb <- mean(errori_xgb^2)
mse_xgb



errori_prophet = numeric(38)
for(i in 1:38) {
  n_train = 80 + i 
  train_data = data.frame(
    ds = 1:n_train, 
    y = df$grigio_80[1:n_train]
  )
  
  model = prophet(train_data, yearly.seasonality = "auto", weekly.seasonality = FALSE)
  
  future = data.frame(ds = (n_train + 1):(n_train + 2))
  forecast = predict(model, future)
  errori_prophet[i] = tail(forecast$yhat, 1) - df$grigio_80[82 + i]
}

mse_prophet = mean(errori_prophet^2)
print(mse_prophet)



######## prophet
data_inizio <- as.Date("2010-01-01")
df$ds <- seq.Date(
  from = data_inizio,
  by = "month",
  length.out = nrow(df)
)
errori_prophet <- numeric(38)

for(i in 1:38) {
  n_train <- 80 + i
  train_data <- data.frame(
    ds = df$ds[1:n_train],
    y  = df$grigio_80[1:n_train]
  )
  train_data$agosto <- ifelse(format(train_data$ds, "%m") == "08", 1, 0)
  
  model <- prophet(
    yearly.seasonality = TRUE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
    seasonality.mode = "additive"
  )
  
  model <- add_regressor(model, "agosto")
  model <- fit.prophet(model, train_data)
  
  future <- data.frame(
    ds = df$ds[(n_train + 1):(n_train + 2)]
  )
  
  future$agosto <- ifelse(format(future$ds, "%m") == "08", 1, 0)
  forecast <- predict(model, future)
  errori_prophet[i] <- tail(forecast$yhat, 1) - df$grigio_80[n_train + 2]
}

mse_prophet <- mean(errori_prophet^2)

print(mse_prophet)


###### prova con altra serie
df=dd
errori_bass2 <- numeric(40)

for (i in 1:38) {
  x_tmp <- df$noce_120[1:(80 + i)]
  bass_tmp <- BM(x_tmp, display = F)
  
  pred_bmtw <- predict(bass_tmp, newx = c(1:(80 + i + 2)))
  pred.insttw<- make.instantaneous(pred_bmtw)
  
  errori_bass2[i] <- tail(pred.insttw,1) - df$noce_120[82 + i]
}

mse_bass <- mean(errori_bass2^2)


## approccio tramite rolling windows
errori_mean2 <- numeric(40)

for (i in 1:40) {
  x_tmp <-  df$noce_120[1:(80+i)]
  pred_mean <- mean( df$noce_120[  (80-12+i):(80+i)] )
  
  errori_mean2[i] <- pred_mean - df$noce_120[(80 + i + 2)]
  
}

mse_mean <- mean((errori_mean2[1:38])^2)

## approccio tramite regressione lineare disponendo di alcuni ritardi
errori_lm <- numeric(38)

serie <- df$noce_120
idx <- 13:120

dati_lm <- data.frame(
  tempo = idx,
  y = serie[idx],
  lag1 = serie[idx - 1],
  lag2 = serie[idx - 2],
  lag3 = serie[idx - 3],
  lag4 = serie[idx - 4],
  mean_12 = sapply(idx, function(i) mean(serie[(i - 12):(i - 1)])),
  same_month_last_year = serie[idx - 12]
)

for (i in 1:38) {
  
  t_pred <- 80 + i
  
  train <- dati_lm[dati_lm$tempo <= t_pred, ]
  test  <- dati_lm[dati_lm$tempo == t_pred + 2, ]
  
  modello <- lm(
    y ~ lag2 + lag3 + lag4 + mean_12 + same_month_last_year,
    data = train
  )
  
  predizione <- predict(modello, newdata = test)
  
  errori_lm[i] <- predizione - test$y
}

mse_lm <- mean(errori_lm^2)
mse_lm


## previsione tramite xgboost
library(xgboost)
errori_xgb <- numeric(38)

for (i in 1:38) {
  
  t_pred <- 79 + i
  
  train <- dati_lm[dati_lm$tempo <= t_pred, ]
  test  <- dati_lm[dati_lm$tempo == t_pred + 2, ]
  
  X_train <- as.matrix(train[, -c(1,2)])
  y_train <- train$y
  
  X_test <- as.matrix(test[, -c(1,2)])
  
  modello_xgb <- xgboost(
    data = X_train,
    label = y_train,
    nrounds = 50,
    objective = "reg:squarederror",
    verbose = 0, 
    max_depth = 1,
    learning_rate = 0.1
  )
  
  predizione <- predict(modello_xgb, X_test)
  
  errori_xgb[i] <- predizione - test$y
}
mse_xgb <- mean(errori_xgb^2)
mse_xgb








errori_prophet = numeric(38)

for(i in 1:38) {
  n_train = 80 + i - 1
  
  # Prophet vuole dataframe con ds (anche date fittizie)
  train_data = data.frame(
    ds = 1:n_train,  # usa indici come "tempo"
    y = df$noce_120[1:n_train]
  )
  
  model = prophet(train_data, yearly.seasonality = FALSE, weekly.seasonality = FALSE)
  
  future = data.frame(ds = (n_train + 1):(n_train + 2))
  forecast = predict(model, future)
  
  errori_prophet[i] = tail(forecast$yhat, 1) - df$noce_120[82 + i]
}

mse_prophet = mean(errori_prophet^2)
print(mse_prophet)


















risultati_ggm = matrix(NA, nrow = 16, ncol = 5)
colnames(risultati_ggm) = c("k", "pc", "qc", "ps", "qs")

for(i in 1:16){
  bm_tmp=BM(dd[, i], display=F)
  ggm_tmp = GGM(dd[, i], prelimestimates = c(bm_tmp$coefficients[1], 0.001, 0.1, bm_tmp$coefficients[2], bm_tmp$coefficients[3]))
  
  # Estrai stime e p-value direttamente dall'oggetto
  stime = ggm_tmp$Estimate$Estimate
  pvals = ggm_tmp$Estimate$`p-value`
  nomi = rownames(ggm_tmp$Estimate)
  
  # Assegna 0 se p-value >= 0.05, altrimenti la stima
  risultati_ggm[i, "k"]  = ifelse(pvals[nomi == "K  "] < 0.05, stime[nomi == "K  "], 0)
  risultati_ggm[i, "pc"] = ifelse(pvals[nomi == "pc  "] < 0.05, stime[nomi == "pc  "], 0)
  risultati_ggm[i, "qc"] = ifelse(pvals[nomi == "qc  "] < 0.05, stime[nomi == "qc  "], 0)
  risultati_ggm[i, "ps"] = ifelse(pvals[nomi == "ps  "] < 0.05, stime[nomi == "ps  "], 0)
  risultati_ggm[i, "qs"] = ifelse(pvals[nomi == "qs  "] < 0.05, stime[nomi == "qs  "], 0)
}

risultati_ggm





install.packages("prophet")
# Carica librerie
library(prophet)
library(dplyr)


date_seq = seq(as.Date("2010-01-01"), as.Date("2019-12-01"), by = "month")
nrow(dd)
df_prophet = data.frame(
  ds = date_seq,
  y = dd$grigio_80  # Assicurati che la lunghezza corrisponda (132 mesi)
)

# Stima modello Prophet
model = prophet(df_prophet)

# Crea future dates per previsioni (es. 24 mesi avanti)
future = make_future_dataframe(model, periods = 24, freq = "month")

# Previsioni
forecast = predict(model, future)

# Visualizza risultati
plot(model, forecast)

# Componenti (trend, stagionalità)
prophet_plot_components(model, forecast)

# Estrai previsioni per i prossimi 24 mesi
previsioni = forecast %>%
  tail(24) %>%
  select(ds, yhat, yhat_lower, yhat_upper)

print(previsioni)


