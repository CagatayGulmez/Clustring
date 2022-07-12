

#### PACKAGES #######

install.packages("pastecs")
install.packages("ggplot2")   
install.packages("tidyverse")
install.packages("psych")
install.packages("philentropy")
install.packages("usedist")
install.packages("factoextra")
install.packages("ggpubr")
install.packages("cluster")
install.packages("corrplot")
install.packages("GGally")
install.packages(c("fpc", "NbClust"))
install.packages("clValid")
install.packages("dplyr")
install.packages("reshape2")       
install.packages("lattice")
library(lattice) 
library(reshape2)
library(dplyr)
library(fpc)
library(NbClust)
library(GGally)
library(ggplot2)             
library(tidyverse)           
library(psych)
library(pastecs)
library(philentropy)
library(usedist)
library(ggpubr)
library(factoextra)
library(cluster)
library(FactoMineR)
library(corrplot)
library(clValid)

#### DATA#####

df <- read.csv("D:\\Yedek\\\ÝSTATÝSTÝK\\Bitirme Projesi\\BP_Veri_Seti_2021.csv")
View(df)
df_num<-data.frame(df[3:14]) 
Country_Names=df[,1]
rownames(df_num)=Country_Names[as.integer(rownames(df_num))]
View(df_num)

#### DESCRIPTIVE STATISTICS ######

describe(df_num)
corrplot(cor(df_num))  
ggcorr(df_num, method = c("everything", "pearson"))
ggcorr(df_num, label = TRUE,hjust = 1, label_size = 3, label_round = 2, label_alpha = TRUE)
pairs(df_num)
ggpairs(df_num) 
boxplot(df_num)
boxplot(scale(df_num))

#### PCA ####

KMO(df_num)           
bartlett.test(df_num) 


data.pca <- prcomp(df_num, center = TRUE, scale. = TRUE)
summary(data.pca)

sqrt(data.eigen$values)
data.pca$sdev
data.pca$rotation
data.pca$center
data.pca$scale
data.pca$x
data.pca$x [1:2,]
predict(data.pca)[1:2,]
(scale(data)%*%data.pca$rotation)[1:2,]
fviz_eig(data.pca)

# Eigenvalues
eig.val <- get_eigenvalue(data.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(data.pca)
res.var$coord # Coordinates
res.var$contrib # Contributions to the PCs
res.var$cos2 # Quality of representation
data.pca
sum(res.var$contrib[,2])

##Variable contributions to the principal axes:
# Contributions of variables to PC1
fviz_contrib(data.pca, choice = "var", axes = 1, top = 12)
# Contributions of variables to PC2
fviz_contrib(data.pca, choice = "var", axes = 2, top = 12)

# Results for individuals
res.ind <- get_pca_ind(data.pca)
res.ind$coord # Coordinates
res.ind$contrib # Contributions to the PCs

"pc1 Ekonomi ve yönetim
Safety an security
governance
social capital
investment
enterprise
market
economic
education"
"pc2 Yasam kalitesi
personal freedom
living conditions
health
natural enviroment
"
fviz_pca_var(data.pca, axes = c(1, 2), col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE)



fviz_pca_ind(data.pca,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)


df_numpca<- predict(data.pca)[,1:2] #YENI VERI SETI
df_num<-
res.ind$cos2 # Quality of representation

describe(df_numpca)

#### Distances#####

df_num.matrix <- (as.matrix(df_num.scaled))

##EUCLIDEAN##
dist.eucl <- dist(df_num, method = "euclidean" , order("FALSE"))
round(as.matrix(dist.eucl))
t(dist.eucl)
view(round(as.matrix(dist.eucl)))
fviz_dist(dist.eucl)

##MANHATTAN##
dist.manh <- dist(df_num, method = "manhattan")
dist.manh.matrix <- round(as.matrix(dist.manh))
view(dist.manh.matrix)
t(dist.manh)
fviz_dist(dist.manh)

##EUCLIDEAN.tr#
df_num.t <- scale(df_num[150:160,])
dist.eucl <- dist(df_num.t, method = "euclidean")
rounded_dist<-round(as.matrix(dist.eucl))
distname<-dist_setNames(dist.eucl, df[150:160,1])
fviz_dist(distname,show_labels = TRUE)

##MANHATTAN.tr##
dist.manh <- dist(df_num[150:160,], method = "manhattan")
dist.manh.matrix <- round(as.matrix(dist.manh))
distname<-dist_setNames(dist.manh.matrix, df[150:160,1])
fviz_dist(distname,show_labels = TRUE)
fviz_dist(dist.manh)


#### K- MEANS ####



fviz_nbclust((df_numpca),kmeans,method = "wss")
km.res<-kmeans((df_numpca),3,nstart = 25)
print(km.res)
km.clusters<-km.res$cluster
fviz_cluster(list(data=(df_numpca),cluster=km.clusters))


for (i in 1:3){
  a=data.frame()
  a=(df_numpca[which(km.clusters==i),])
  assign(paste("Clusterpca",i,sep="_"),a)
  
}

#### K- Medoids ####

b <- pam(df_numpca,3,metric="euclidean", stand=TRUE)

fviz_cluster(list(data=scale(df_numpca),cluster=b$cluster))

for (i in 1:3){
  a=data.frame()
  a=(df_numpca[which(b$clustering==i),])
  assign(paste("K_MedoidsCluster",i,sep=""),a)
  
}
#### Silhouette and gapstat  ####


fviz_nbclust(scale(df_numpca),kmeans,method = "silhouette")


fviz_nbclust(scale(df_numpca),kmeans,method = "gap_stat")

#### Descriptive of Clusters ####

boxplot(scale(Clusterpca_1))
boxplot(scale(Clusterpca_2))
boxplot(scale(Clusterpca_3))

boxplot(PC1~df_numpca$km.clusters)

describe(Clusterpca_1)
describe(Clusterpca_2)
describe(Clusterpca_3)

#### Hierarchical ####
grp <- cutree(hc.res, k = 3)
fviz_dend(hc.res, k = 3, 
          cex = 0.5, 
          k_colors = c("blue", "green", "red", "black"),
          color_labels_by_k = TRUE, 
          rect = TRUE 
)

fviz_cluster(list(data = df_numpca, cluster = grp),
             palette = c("red", "green", "blue", "black"),
             ellipse.type = "convex", 
             repel = TRUE, 
             show.clust.cent = FALSE, ggtheme = theme_minimal())

fviz_cluster(hc.res, geom = "point",
             palette = "jco", ggtheme = theme_minimal())
hc.res <- eclust((df_numpca), "hclust", k = 3, hc_metric = "euclidean",hc_method = "ward.D2", graph = FALSE)
fviz_dend(hc.res, show_labels = FALSE,  
          palette = "jco", as.ggplot = TRUE)




for (i in 1:3){
  a=data.frame()
  a=(df_numpca[which(hc.res$cluster==i),])
  assign(paste("H_Clust",i,sep=""),a)
}






#### Choosing optimal method ####

#silhouette
fviz_silhouette(hc.res, palette = "jco",
                ggtheme = theme_classic())

silinfo <- hc.res$silinfo 
silinfo
sil <- km.res$silinfo$widths[, 1:3]
neg_sil_index <- which(sil[, "sil_width"] < 0)
sil[neg_sil_index, , drop = FALSE]

#dunn index
km_stats <- cluster.stats(dist(df_numpca), hc.res$cluster) 
km_stats$dunn # Dun index


clmethods <- c("hierarchical","kmeans","pam")
intern <- clValid(df_numpca, nClust = (3:6),
                  clMethods = clmethods, validation = "internal")
summary(intern) # En Uygun Yöntemin belirlenmesi


#30 indices for choosing the best number of clusters
nb <- NbClust(df_numpca, distance = "euclidean", min.nc = 2,
              max.nc = 9, method = "kmeans")
fviz_nbclust(nb)

nb$All.index
nb$All.CriticalValues
nb$Best.nc
nb$Best.partition

#### Conclusion ####
df_num$Clusters <- hc.res$cluster

describe(H_Clust1)
boxplot(H_Clust1)
describe(H_Clust2)
boxplot(H_Clust2)
describe(H_Clust3)
boxplot(H_Clust3)


Cluster1 <- df_num[which(df_num[,13]==1),1:12]
Cluster2 <- df_num[which(df_num[,13]==2),1:12]
Cluster3 <- df_num[which(df_num[,13]==3),1:12]

describe(Cluster1)
describe(Cluster2)
describe(Cluster3)

boxplot(Cluster1)
boxplot(Cluster2)
boxplot(Cluster3)

df_nummelt <- melt(df_num, id = "Clusters")

ggplot(df_nummelt, aes(x = variable, y = value, fill=factor(Clusters))) +  # ggplot function
  geom_boxplot()

bwplot(value ~ variable, df_nummelt)







