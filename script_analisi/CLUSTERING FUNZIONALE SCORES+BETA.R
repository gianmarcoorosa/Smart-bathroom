rm(list=ls())


####CLUSTERING SU SCORES F-PCA
dati=read_excel("ss_ide3_sist.xlsx")
dim(dati)
str(dati)
matplot(dati[,-c(1,2)],type="l")
head(dati)
dat=dati[,-c(1,2)]
dat=as.matrix(dat)
##serie storica è realizzazione discreta di una funzione continua nel tempo-> quindi ok dati funzionali
library(fda)

rangeval <- c(1, 120)
basis <- create.fourier.basis(rangeval = rangeval, nbasis = 10)
D2 = int2Lfd(2)

#penalizzare la derivata seconda 
b_penal=fdPar(basis,D2, 1e-10)
dim(t(dat))
y_smooth = smooth.basis(1:120, dat, b_penal)
par(mfrow=c(1,1))
plot(y_smooth)
plot(y_smooth$fd$basis,type="l")

pca <- pca.fd(y_smooth$fd, nharm = 2)
head(pca$harmonics$coefs)


par(mfrow=c(1,1))

plot(pca$harmonics)
pca$scores
plot(pca$scores,col="blue",pch=16)


par(mfrow=c(1,2))
plot(pca)

grid_eval=seq(1, 120, length.out = 500)
f_mean=eval.fd(grid_eval,mean.fd(y_smooth$fd))
plot(grid_eval, f_mean, type="l")
f_harmonics=eval.fd(grid_eval,pca$harmonics)
head(f_harmonics)
head(pca$harmonics$coefs)

matplot(f_harmonics, type="l")
##se devo costruirle
pca$scores[1,]
#punteggi delle componenti principali associate alla prima unità statistica
#scores più alti incrementi delle vendite
pca$harmonics$coefs
pca$scores
#scores negativi ci sono declini nelle vendite
plot(pca$harmonics)

#f_mean=eval.fd(grid_eval,mean.fd(x_fd$fd))
x1_rec=f_mean+pca$scores[1,1]*f_harmonics[,1]+pca$scores[1,2]*f_harmonics[,2]
f_origin=eval.fd(grid_eval,y_smooth$fd)##funzione valutata sulla griglia
par(mfrow=c(1,1))
##si può vedere quanto bene la prima pca ha approssimato bene tra 
#f origin valutata sulla griglia e quella costruita con le pca
plot(grid_eval, f_origin[,1],type="l")
lines(grid_eval, x1_rec,col=2,type="l")


scores <- pca$scores
#faccio cluster sugli scores delle componenti principali
km <- kmeans(scores, centers = 3)
plot(y_smooth, col = km$cluster)
par(mfrow=c(1,1))
plot(scores, col = km$cluster, pch = 16)
text(scores,labels=colnames(dat),pos=4,cex=0.7)


#scores f-PCA
scores <- as.data.frame(pca$scores)
colnames(scores) <- c("PC1","PC2")
#nomi serie
scores$serie <- colnames(dat)
#etichette 60/80/100/120
scores$label <- sub(".*_", "", scores$serie)
#colori per materiale
scores$gruppo <- case_when(
  grepl("antracite", scores$serie) ~ "Antracite",
  grepl("grigio", scores$serie) ~ "Grigio",
  grepl("noce", scores$serie) ~ "Noce",
  grepl("bianco", scores$serie) ~ "Bianco"
)
cols <- c(
  "Grigio" = "grey50",
  "Bianco" = "grey85",
  "Antracite" = "black",
  "Noce" = "firebrick4"
)

library(ggplot2)
ggplot(scores, aes(x = PC1, y = PC2, color = gruppo)) +
  
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey60") +
  
  geom_point(size = 4) +
  
  geom_text(aes(label = label),
            vjust = -1,
            size = 5) +
  
  scale_color_manual(values = cols) +
  
  labs(
    title = "Clustering sulle Functional PCA scores",
    x = "PC1",
    y = "PC2",
    color = "Finitura"
  ) +
  
  theme_minimal(base_size = 14)




#####CLUSTERING FUNZIONALE SUI COEFFICIENTI FUNZIONALI
beta <- t(y_smooth$fd$coefs)
# clustering sui Beta
km_beta <- kmeans(beta, centers = 3)
beta_df <- as.data.frame(beta)
#rinominiamo primi due coefficienti
colnames(beta_df)[1:2] <- c("Beta1", "Beta2")
#nomi serie
beta_df$serie <- colnames(dat)
#etichette 60/80/100/120
beta_df$label <- sub(".*_", "", beta_df$serie)

beta_df$gruppo <- case_when(
  grepl("antracite", beta_df$serie) ~ "Antracite",
  grepl("grigio", beta_df$serie) ~ "Grigio",
  grepl("noce", beta_df$serie) ~ "Noce",
  grepl("bianco", beta_df$serie) ~ "Bianco"
)

beta_df$cluster <- factor(km_beta$cluster)

cols <- c(
  "Grigio" = "grey50",
  "Bianco" = "grey85",
  "Antracite" = "black",
  "Noce" = "firebrick4"
)

ggplot(beta_df,
       aes(x = Beta1,
           y = Beta2,
           color = gruppo)) +
  
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "grey70") +
  
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = "grey70") +
  
  geom_point(
    size = 5,
    shape = 16
  ) +
  
  geom_text(aes(label = label),
            vjust = -1,
            size = 5) +
  
  scale_color_manual(values = cols) +
  
  labs(
    title = "",
    subtitle = "Clustering sui coefficienti β",
    x = expression(beta[1]),
    y = expression(beta[2]),
    color = "Finitura"
  ) +
  
  theme_minimal(base_size = 14) +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "right"
  )

