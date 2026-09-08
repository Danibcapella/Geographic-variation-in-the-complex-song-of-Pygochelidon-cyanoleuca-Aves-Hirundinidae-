setwd("~/Artigo andorinha")
library(ggmap)
library(vegan)

dados <- read.csv("andorinha.csv", h = T)

##fazer o mapa
mapa <- borders("world", regions = c("Brazil", "Uruguay", "Argentina", "French Guiana", "Suriname", "Colombia", "Venezuela",
"Bolivia", "Ecuador", "Chile", "Paraguay", "Peru", "Guyana", "Panama", "Costa Rica", 
"Nicaragua", "Honduras", "El Salvador", "Belize", "Guatemala", "Mexico", "Trinidad and Tobago",
"Caribe", "Puerto Rico", "Dominican Republic", "Haiti", "Jamaica", "Cuba", "Bahamas", "Antiles",
"Dominica", "Saba"), 
 fill = "grey70", colour = "black")

mapa.plot <- ggplot() + mapa + theme_bw() + xlab("") + ylab("") + 
theme(panel.border = element_blank(), panel.grid.major = element_line(colour = "grey80"), panel.grid.minor = element_blank())

mapa.plot + 
geom_point(data = dados, aes(x = Longitude, y = Latitude, colour = Grupo, shape = Grupo), size=1.5)+
  scale_colour_manual(values = c("#D81B60", "#1E88E5"))+
theme_bw()


#Função para transformar em presença/ausência
MaiorQ0 <- function(numeros){
  logico <- numeros > 0
  as.numeric(logico)
}

dados[,26:30] <- lapply(dados[,20:25], MaiorQ0)

levels(as.factor(dados$Grupo))
dados$Grupo <- as.factor(dados$Grupo)
class(dados$Grupo)

par(mfrow = c(2,2))

boxplot(log(FMIN) ~ Grupo, dados, col = c("#D81B60", "#1E88E5"), ylab = "Frecuencia mínima")
boxplot(log(FMAX) ~ Grupo, dados, col = c("#D81B60", "#1E88E5"), ylab = "Frecuencia máxima")
boxplot(log(FDOM) ~ Grupo, dados, col = c("#D81B60", "#1E88E5"), ylab = "Frecuencia dominante")
boxplot(log(BAN) ~ Grupo, dados, col = c("#D81B60", "#1E88E5"), ylab = "Banda de frecuencia")



##PCA completa#####

grp <- dados[,3]
song.std <- decostand(dados[,13:17], method = "standardize")
song.std <- cbind(song.std, dados[,26:30])

boxplot(song.std)

song.pca <- rda(song.std, scale = T)
summary(song.pca)

song.ev <- song.pca$CA$eig
n <- length(song.ev)
song.bs <- bstick(n,n)
x11()
barplot(t(cbind(song.ev,song.bs)), beside=T, col=c(7,2))

explic <- (song.pca$CA$eig)/(sum(song.pca$CA$eig))*100 
explic

spe.score <- scores(song.pca, display="wa",choices=1:2)
acu.score <- scores(song.pca, display="sp", choices=1:2)

spe.score <- as.data.frame(spe.score)
acu.score <- as.data.frame(acu.score)

spe.score <- cbind(spe.score, grp)


x11()
plot(song.pca,type="n",cex.lab=1.4,font.lab=2,cex.axis=1.2,
     xlab=paste("PC 1 - ",round(explic[1],1),"%",sep=""),
     ylab=paste("PC 2 - ",round(explic[2],1),"%",sep=""))

points(spe.score$PC1[(spe.score$grp=="Este")],
       spe.score$PC2[(spe.score$grp=="Este")],
       pch=1, col="#D81B60",lwd=2, cex=1.6)

points(spe.score$PC1[(spe.score$grp=="Oeste")],
       spe.score$PC2[(spe.score$grp=="Oeste")],
       pch=2, col="#1E88E5",lwd=2, cex=1.6)


##PCA Notas####
synt <- dados[,20:25]

synt.pca <- rda(synt, scale = T)

explic.synt <- (song.pca$CA$eig)/(sum(song.pca$CA$eig))*100 
explic.synt

spe.score <- scores(synt.pca, display="wa",choices=1:2)
acu.score <- scores(synt.pca, display="sp", choices=1:2)

spe.score <- as.data.frame(spe.score)
acu.score <- as.data.frame(acu.score)

spe.score <- cbind(spe.score, grp)

x11()
plot(synt.pca,type="n",cex.lab=1.4,font.lab=2,cex.axis=1.2,
     xlab=paste("PC 1 - ",round(explic.synt[1],1),"%",sep=""),
     ylab=paste("PC 2 - ",round(explic.synt[2],1),"%",sep=""))

points(spe.score$PC1[(spe.score$grp=="Este")],
       spe.score$PC2[(spe.score$grp=="Este")],
       pch=2, col="#D81B60",lwd=2, cex=1.6)

points(spe.score$PC1[(spe.score$grp=="Oeste")],
       spe.score$PC2[(spe.score$grp=="Oeste")],
       pch=1, col="#1E88E5",lwd=2, cex=1.6)


arrows(x0=0,y0=0,x1=acu.score$PC1,y1=acu.score$PC2,length=0.15,angle=20,
       col="forestgreen")

text(acu.score$PC1, acu.score$PC2,
     labels=rownames(acu.score),
     col="forestgreen",pos=2,offset=0.3, cex=1)

##PCA parametros#####

freq <- decostand(dados[,13:17], method = "standardize")

freq.pca <- rda(freq, scale = T)

explic.freq <- (song.pca$CA$eig)/(sum(song.pca$CA$eig))*100 
explic.freq

spe.score.freq <- scores(freq.pca, display="wa",choices=1:2)
acu.score.freq <- scores(freq.pca, display="sp", choices=1:2)

spe.score.freq <- as.data.frame(spe.score.freq)
acu.score.freq <- as.data.frame(acu.score.freq)

spe.score.freq <- cbind(spe.score.freq, grp)

x11()
plot(freq.pca,type="n",cex.lab=1.4,font.lab=2,cex.axis=1.2,
     xlab=paste("PC 1 - ",round(explic.freq[1],1),"%",sep=""),
     ylab=paste("PC 2 - ",round(explic.freq[2],1),"%",sep=""))

points(spe.score.freq$PC1[(spe.score.freq$grp=="Este")],
       spe.score.freq$PC2[(spe.score.freq$grp=="Este")],
       pch=1, col="#D81B60",lwd=2, cex=1.6)

points(spe.score.freq$PC1[(spe.score.freq$grp=="Oeste")],
       spe.score.freq$PC2[(spe.score.freq$grp=="Oeste")],
       pch=2, col="#1E88E5",lwd=2, cex=1.6)


arrows(x0=0,y0=0,x1=acu.score.freq$PC1,y1=acu.score.freq$PC2,length=0.15,angle=20,
       col="forestgreen")

text(acu.score.freq$PC1, acu.score.freq$PC2,
     labels=rownames(acu.score.freq),
     col="forestgreen",pos=2,offset=0.3, cex=1)

legend("bottomleft", legend=c("Este", "Oeste"),
       pch=c(1,2), col=c("#D81B60", "#1E88E5"), bty="n", pt.cex=1.6, pt.lwd=2,
       cex=1.2, text.font=3)
