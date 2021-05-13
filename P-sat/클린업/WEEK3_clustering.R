setwd("C:/Users/samsung/Documents/DATAMINING_PSAT")

#패키지 쪼로록 불러오자
library(tidyverse)
library(cluster)
library(factoextra)

#오늘 사용할 데이터는 USArrests!
df=USArrests
df=na.omit(USArrests)

#스케일링부터 해보자
df=scale(df)
head(df)

#유사도(거리) 시각화해보자
distance=get_dist(df)
fviz_dist(distance, gradient=list(low="#00AFBB",mid = "white", high = "#FC4E07"))

#군집 개수 k 결정하기 - kmeans 이용
set.seed(19981204)
wss<-function(k){
  kmeans(df,k,nstart=10)$tot.withinss
}

k.values=1:15
wss_values=map_dbl(k.values,wss)

plot(k.values, wss_values, type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


#시각화하기 - kmeans 이용
k2=kmeans(df,centers=2,nstart=25)
k2
fviz_cluster(k2,data=df)
