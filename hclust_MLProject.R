library(stats)  ## for dist
library(NbClust)
library(cluster)
library(mclust)
library(amap)  ## for using Kmeans (notice the cap K)
library(factoextra) ## for cluster vis, silhouette, etc.
library(purrr)
library(philentropy)  ## for distance() which offers 46 metrics
library(SnowballC)
library(caTools)
library(dplyr)
library(textstem)
library(stringr)
library(wordcloud)

setwd(getwd())
Record_3D_DF<-read.csv("ClusterTrainData.csv")

head(Record_3D_DF)
str(Record_3D_DF)

## remove columns which are not numerical
Record_3D_DF <- Record_3D_DF[ ,-c(2:3) ]
head(Record_3D_DF)
Record_3D_DF <- sample_n(Record_3D_DF,100)

Dist2<- dist(Record_3D_DF, method = "minkowski", p=2) #Euclidean

## Create a normalized version of Record_3D_DF
Record_3D_DF_Norm <- as.data.frame(apply(Record_3D_DF[,1:2 ], 2, ##2 represents operations column wise
                                          function(x) (x - min(x))/(max(x)-min(x))))
# 1:4 above represents the column numbers - first four columns

## Look at scaled distances
Dist_norm<- dist(Record_3D_DF_Norm, method = "minkowski", p=2) #Euclidean

# ward.D2" = Ward's minimum variance method - however dissimilarities are **squared before clustering. 
# "single" = Nearest neighbours method. 
# "complete" = distance between two clusters is defined as the maximum distance between an observation in one.

## Now run hclust...you may use many methods - Ward, Ward.D2, complete, etc..
HClust_Ward_Euc_N_3D <- hclust(Dist_norm, method = "average" )
plot(HClust_Ward_Euc_N_3D, cex=0.9, hang=-1, main = "Minkowski p=2 (Euclidean)")
#rect.hclust(HClust_Ward_Euc_N_3D, k=4)

## Using Cosine similarity
#dist_C <- stats::dist(Record_3D_DF_Norm, method="cosine")
#HClust_Ward_CosSim_N_3D <- hclust(dist_C, method="ward.D2")

similarity_matrix <- tcrossprod(scale(Record_3D_DF_Norm, center = TRUE, scale = TRUE))

# perform hierarchical clustering using cosine similarity
hclust_results <- hclust(as.dist(1 - similarity_matrix), method = "ward.D2")

# plot the dendrogram
plot(hclust_results, main = "Hierarchical Clustering using Cosine Similarity")

#plot(HClust_Ward_CosSim_N_3D, cex=.7, hang=-30,main = "Cosine")
#rect.hclust(HClust_Ward_CosSim_N_3D, k=2)
