data = read.csv("optdigits.csv")    # read input from csv file
mdata = subset(data, select = -c(digit))
# part a
# parameters : 
# 1. n = 10 as number of clusters is the same as number of digits
# 2. nstart = 20 i.e 20 different starting assignments will be tested and the one with
#    lowest will be seleted withing cluster variaion.

set.seed(10)    # ensure reproducibility
digits_cluster = kmeans(mdata, 10, nstart = 20,  iter.max = 200)     # Apply kmeans clustering
tab = table(digits_cluster$cluster, data$digit)   # print result in tabular form
 

# part b
library(dendextend)
hdata = data.frame()    # Data frame for all the values in cluster 9 
for(x in 1:nrow(data)){     
    if(digits_cluster$cluster[x] == 1){
        hdata = rbind(hdata, data.frame(data[x,]))
    }
}

ndata = subset(hdata, select=-c(digit))
h_clusters = hclust(dist(ndata))    # hierarchical clustering
newcut_f = as.dendrogram(h_clusters)  # create dendrogram
plot(cut(newcut_f, h = 50)$upper, main = "Upper Tree (cut at height 50)") # plot the dendrogram above height 50
newcut_t = cutree(h_clusters, 2)    # bring number of clusters down to 2
table(newcut_t, hdata$digit)    # plot table

# part c 
test_data = read.csv("optdigits_test.csv")   # read input from csv
centers = digits_cluster$centers

test_result = data.frame("Image Number" = c(), "Cluster Number" = c(), "Digit" = c())

for(x in 1:nrow(test_data)){
    d = c()
    for(y in 1:10){
        d = c(d, dist(rbind(subset(test_data, select=-c(imageno))[x,], centers[y,])))   # calculate eucledian distance from each center
    }
    cluster_number = which.min(d)       # find the cluster number which has minimum distance
    digit = which.max(tab[cluster_number, ])[[1]] - 1       # find the digit corresponding to that cluster
    test_result = rbind(test_result, data.frame("Image Number" = c(test_data$imageno[x]), "Cluster Number" = c(cluster_number), "Digit" = c(digit)))
}
print(test_result)  # print the result

# part d
classtab = data     # make a copy of data
for(x in 1:nrow(data)){
    d = c()
    for(y in 1:10){
        d = c(d, dist(rbind(mdata[x,], centers[y,])))   # calculate eucledian distance from each center
    }
    cluster_number = which.min(d)       # find the cluster number which has minimum distance
    digit = which.max(tab[cluster_number, ])[[1]] - 1       # find the digit corresponding to that cluster
    if(data$digit[x] == 9 && cluster_number == 1){
        cluster_number = 1
    } 
    classtab$cluster_number[x] = cluster_number     # add corresponding cluster number to the table
} 

newtab = subset(classtab, select=-c(digit, cluster_number))
library(class)

fitknn = knn(train = newtab,test = subset(test_data, select=-c(imageno)), cl = classtab$cluster_number, k = 7)  # fit data using knn
knn_result = data.frame("Image Number" = c(), "Cluster Number" = c(), "Digit" = c())
for(x in 1:nrow(test_data)){
    digit = which.max(tab[fitknn[x], ])[[1]] - 1       # find the digit corresponding to that cluster
    knn_result = rbind(knn_result, data.frame("Image Number" = c(test_data$imageno[x]), "Cluster Number" = c(fitknn[x]), "Digit" = c(digit)))   
}
print(knn_result)   # print knn result
clustable = data.frame("Cluster Number" = c(), "Digit" = c())   # table containing correct labels corresponding to digit
for(x in 1:10){
    if(x == 1){     # if cluster number is 1, corresponding digit is 9
        digit = 9
    }
    else{
        digit = which.max(tab[x, ])[[1]] - 1    
    }
    clustable = rbind(clustable, data.frame("Cluster Number" = c(x), "Digit" = c(digit)))
}
print(clustable)    # print the table
