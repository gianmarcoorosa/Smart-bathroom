#####MARKET BASKET ANALYSIS
rm(list=ls())
#install.packages("readODS")
library(readODS)

dati <- read_ods("sondaggio_mobili_bagno.ods")
head(dati)
View(dati)
str(dati)
table(dati$RISPOSTA)
table(dati$dimensione_cm)
table(dati$numero_cassetti)

dati$RISPOSTA = as.factor(dati$RISPOSTA)
table(dati$colore)
dati$colore = as.factor(dati$colore)
dati$lavabo = as.factor(dati$lavabo)
dati$all_inclusive = as.factor(dati$all_inclusive)
dati$ante = as.factor(dati$ante)
dati$forma_specchiera = as.factor(dati$forma_specchiera)
dati$type = as.factor(dati$type)
dati$illuminazione = as.factor(dati$illuminazione)

str(dati)
sum(is.na(dati))






##uso tutti gli altri modelli con 0/1 così posso fingere anche che sia numerica (come
#facevamo a data mining)
table(dati$RISPOSTA)
dati$y=ifelse(dati$RISPOSTA=="N",0,1)
table(dati$y)
dati$RISPOSTA=NULL
dati1 = dati[dati$y == 1, ]
str(dati1)
dati1$dimensione_cm=as.factor(dati1$dimensione_cm)
dati1$numero_cassetti=as.factor(dati1$numero_cassetti)

table(dati1$dimensione_cm)
table(dati1$numero_cassetti)
table(dati1$colore)
dati1$dimensione_cm=as.character(dati1$dimensione_cm)
dati1$dimensione_cm[dati1$dimensione_cm %in% "60"] = "dimensione 60 cm"
dati1$dimensione_cm[dati1$dimensione_cm %in% "80"] = "dimensione 80 cm"
dati1$dimensione_cm[dati1$dimensione_cm %in% "100"] = "dimensione 100 cm"
dati1$dimensione_cm[dati1$dimensione_cm %in% "120"] = "dimensione 120 cm"
table(dati1$dimensione_cm)
dati1$dimensione_cm=as.factor(dati1$dimensione_cm)

dati1$numero_cassetti=as.character(dati1$numero_cassetti)
dati1$numero_cassetti[dati1$numero_cassetti %in% "0"]="nessun cassetto"
dati1$numero_cassetti[dati1$numero_cassetti %in% "1"]="un cassetto"
dati1$numero_cassetti[dati1$numero_cassetti %in% "2"]="due cassetti"
dati1$numero_cassetti[dati1$numero_cassetti %in% "3"]="tre cassetti"
dati1$numero_cassetti[dati1$numero_cassetti %in% "4"]="quattro cassetti"
dati1$numero_cassetti=as.factor(dati1$numero_cassetti)
table(dati1$numero_cassetti)
str(dati)
###prendiamo solo le categoriali

dim(dati1)
dim(dati)

# seleziono categoriali
head(dati1)
basket= dati1[, c("colore","dimensione_cm","lavabo",
                  "all_inclusive","numero_cassetti",
                  "ante","forma_specchiera",
                  "type","illuminazione")]

install.packages("arules", dependencies = TRUE)
install.packages(
  "https://cran.r-project.org/src/contrib/Archive/arules/arules_1.7-7.tar.gz",
  repos = NULL,
  type = "source"
)
library(arules)

basket[] <- lapply(basket, as.character)

basket_list <- apply(basket, 1, as.list)

basket_list <- lapply(basket_list, unlist)

basket_trans <- as(basket_list, "transactions")
itemFrequency(basket_trans)
sort(itemFrequency(basket_trans),decreasing=T)
itemFrequencyPlot(basket_trans,support=.1)

set.seed(1234)
musicrules <- apriori(basket_trans,parameter=list(support=.1,confidence=.7)) 
##0.1 supporto voglio che le regole compaiano almeno nel 10% del dataset
#(noi abbiamo 90 ass, vogliamo che la regola compaia almeno 9 volte)

##0.7 confidence dice che quando compare lhs, rhs deve comparire almeno nel 70% dei casi
table(dati$type)
table(dati$dimensione_cm)
table(dati$numero_cassetti)
inspect(sort(musicrules, by="lift")[1:20])
install.packages("arulesViz")
library(arulesViz)

plot(musicrules, method = "graph") 


#I clienti che scelgono mobili sospesi rettangolari da 60 cm (sia all inclusive che non) tendono 
#fortemente a preferire configurazioni senza illuminazione integrata. (vedi le prime due )


#Chi sceglie mobili con due cassetti e specchio sovrapposto tende frequentemente a preferire illuminazione esterna. (vedi 3 e 4)

#I mobili da 100 cm sono quasi sempre scelti nella configurazione a terra. (vedi 9-12)
boxplot(dati$y)
boxplot(dati$dimensione_cm)



######SEGMENTAZIONE DEL MERCATO
###rimetto label dei cassett (arbitraria nella MBA era per far uscire più
#chiaro le etichette algoritmo a priori, non cambia niente)
rm(list=ls())
#install.packages("readODS")
library(readODS)

dati <- read_ods("sondaggio_mobili_bagno.ods")
head(dati)
View(dati)
str(dati)
table(dati$RISPOSTA)
table(dati$dimensione_cm)
table(dati$numero_cassetti)
dati$`DAY-Time`=NULL
dati$RISPOSTA=NULL
table(dati$colore)
dati$colore = as.factor(dati$colore)
dati$lavabo = as.factor(dati$lavabo)
dati$all_inclusive = as.factor(dati$all_inclusive)
dati$ante = as.factor(dati$ante)
dati$forma_specchiera = as.factor(dati$forma_specchiera)
dati$type = as.factor(dati$type)
dati$illuminazione = as.factor(dati$illuminazione)
dati$dimensione_cm=as.factor(dati$dimensione_cm)
dati$numero_cassetti=as.factor(dati$numero_cassetti)
library(cluster)

set.seed(1234)
d <- daisy(dati, metric = "gower")

hc <- hclust(d, method = "complete")
plot(hc, main="", labels=F)
c3 <- cutree(hc, k = 3)
table(c3)
plot(hc,
     main = "Clustering gerarchico (complete method)",
     xlab = "",
     ylab = "Distanza di Gower",
     labels = FALSE)

rect.hclust(hc, k = 3, border = "red")


#1 cluster= -0.07 sotto la media, 0.03 quasi neutro
#2 cluster= 0.14 sopra la media, -0.07 sotto la media
lapply(dati[, sapply(dati, is.factor)], function(x)
  prop.table(table(c3, x), 1)
)##per ogni cluster c'è la percentuale della categoria

dat_cl2 <- model.matrix(~ . - 1, data = dati)
library(flexclust)
set.seed(1234)
MD.km28 <- stepFlexclust(dat_cl2, k = 2:5)
plot(MD.km28, xlab = "number of segments")


MD.k2 <- MD.km28[["3"]]
barchart(MD.k2)



cluster_table <- function(var, cluster){
  round(prop.table(table(cluster, var), 1), 3)
}
vars_factor <- names(dati)[sapply(dati, is.factor)]

resultati <- lapply(vars_factor, function(v){
  cluster_table(dati[[v]], clusters(MD.k2))
})
dim(dati)

names(resultati) <- vars_factor
risultati


