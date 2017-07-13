#libraries and options
library(pastecs)
options(scipen=9999)


####separate out input/descriptor columns; --------------------------------------------------------###
    #input= columns capturing business processes and modeled during clustering, 
    #desc= columns overlayed to clusters to find add'l patterns

#getting all column names of dataset & splitting into input and desc
colnames(data_id)

input<-data_id[,c(2:36,44,49)]

desc<-data_id[,-c(2:36,44,49)]


####Principal Components Analysis: elbow chart, cum prop graph, subset to 95% variation-------------###

names(input[, sapply(input, function(v) var(v, na.rm=TRUE)==0)])

#input.pca <- (prcomp(input, scale. = T, retx = TRUE))

#actually let's use princomp instead as test
input.pca <- (princomp(input, scale. = T, retx = TRUE))

#determine optimal number of PCs; plotting some of the components
std_dev <- input.pca$sdev
var <- std_dev^2
prop_varex <- var/sum(var)

#plot 'elbow' chart of proportion of variance explained
plot((prop_varex), xlab = "Principal Component",
     ylab = "Variance Explained",
     main = "Proportion of Variance Explained",
     type = "b") ##5pcs

#plot 'cumulative proportion graph'
plot(cumsum(prop_varex), xlab = "Principal Component",
     ylab = "Cumulative variance",
     main = "Cumulative Proportion of Variance Explained",
     type = "b") #let's go with 26 this time

cumsum(prop_varex)


#Setting dataset for k means to PCA dataset
input.pca <- data.frame(input.pca$x)

#Subset to 95% variation (5 components below is what we get from plot of above step)
#for testing let's also change number of PCAs taken to 6
input.pca <- input.pca[,1:6]  


#### set up k-means -----------------------------------------------------------------------####

# Check for the optimal number of clusters given the data
mydata <- input.pca
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:30) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss) 
#plot elbow chart
plot(1:30, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares",
     main="Assessing the Optimal Number of Clusters with the Elbow Method",
     pch=20, cex=2)  #graphs suggests using x clusters


# library(fpc)
# pamk.best <- pamk(input.pca)
# cat("number of clusters estimated by optimum average silhouette width:", pamk.best$nc, "\n")
# plot(pam(d, pamk.best$nc))


####running k-means ----------------------------------------------------------------------####
#running initial k-means
set.seed(1234)
km3_30 = kmeans(mydata, 10, iter.max=30, nstart=100) #10 clusters

#recentering cluster centers on iniitial run
km3_30v2 = kmeans(mydata, 10, iter.max=30, centers=km3_30$centers)


#### post run data formatting and sum stats---------------------------------------------------####
#adding cluster assignments to desc and input datasets
desc_post<-cbind(desc,km3_30v2$cluster)
input_post<-cbind(input, km3_30v2$cluster)


#get initial sum stats of clusters as needed...
input_all_vars<-aggregate(input_post[,1:35], list(input_post$cluster), mean) #entire 'input sums by cluster' info
input_table<-as.data.frame(table(input_post$cluster))
  #many more sum stats of clusters as needed