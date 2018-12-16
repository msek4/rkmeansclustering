#################################################################################################
#################################################################################################
## Install and import library
#################################################################################################
install.packages('stringr')
install.packages('VIM')
install.packages('factoextra')
install.packages('NbClust')
install.packages('data.table')

library('stringr') # stringr library for using string handler function
library('VIM') # VIM library for using 'aggr'
library('cluster') # Cluster analytics
library('factoextra') # Compute and visualize distance matrix using the functions get_dist() and fviz_dist()
library('NbClust') # Determining the optimal number of clusters
library('data.table') # Fast aggregation of large data
library('dplyr') # A grammar of data manipulation

###########################################################################################
###########################################################################################
# Data processing
###########################################################################################
# df <- read.table("BlackFriday.csv", header=TRUE, sep=",")
df <- read.csv("BlackFriday.csv")

# Plots the amount of missing/imputed values in each column
png("/var/www/html/graphcluster/MissingValue.Purchase.png")
aggr(df)
dev.off()

# Remove unnecessary columns
df <- df[,c("User_ID","Product_ID","Gender","Age","Occupation","City_Category","Stay_In_Current_City_Years","Marital_Status","Product_Category_1","Purchase")]

png("/var/www/html/graphcluster/RemoveMissingValue.Purchase.png")
aggr(df)
dev.off()

# Convert Gender, Age, City Category, Stay in current city years columns to number
df$Gender=as.numeric(df$Gender)
df$Age=as.numeric(df$Age)
df$City_Category=as.numeric(df$City_Category)
df$Stay_In_Current_City_Years=as.numeric(df$Stay_In_Current_City_Years)

# Remove first character in Product_ID then convert to number
df$Product_ID <- str_sub(df$Product_ID, 2)
df$Product_ID=as.numeric(df$Product_ID)

head(df)

###########################################################################################
###########################################################################################
#### Clustering : Age - Purchase 
###########################################################################################
set.seed(123)
# Group by User_ID and sumarize Purchase
df.AgePurchase <- df[,c("User_ID","Age","Purchase")]
df.AgePurchase <- df.AgePurchase %>% group_by(User_ID,Age) %>% summarize(Purchase = sum(Purchase))
df.AgePurchase.scaled <- scale(df.AgePurchase)

# Compute and visualize distance matrix
png("/var/www/html/graphcluster/DistanceCluster.Age.Purchase.png")
fviz_dist(dist(df.AgePurchase.scaled), show_labels = FALSE,gradient = list(low = "red", mid = "white", high = "blue"))+ labs(title = "Black Friday")
dev.off()

# png("/var/www/html/graphcluster/DistanceCluster.Age.Purchase.SteelBlue.png")
# fviz_dist(dist(df.AgePurchase.scaled), show_labels = FALSE,gradient = list(low = "red",  mid = "white", high = "steelblue"))+ labs(title = "Black Friday")
# dev.off()

# Find optimal cluster
png("/var/www/html/graphcluster/OptimalCluster.Age.Purchase.silhouette.png")
fviz_nbclust(df.AgePurchase.scaled, kmeans, method = "silhouette") +   labs(subtitle = "Silhouette method")
dev.off()

png("/var/www/html/graphcluster/OptimalCluster.Age.Purchase.wss.png")
fviz_nbclust(df.AgePurchase.scaled, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2)+ labs(subtitle = "Elbow method")
dev.off()

png("/var/www/html/graphcluster/OptimalCluster.Age.Purchase.GapStatistic.png")
# fviz_nbclust(df.AgePurchase.scaled, kmeans, nstart = 25, method = "gap_stat", nboot = 30)+ labs(subtitle = "Gap statistic method")
fviz_nbclust(df.AgePurchase.scaled, kmeans, method = "gap_stat")+ labs(subtitle = "Gap statistic method")
dev.off()


# Run clustering with Kmeans
k.means.fit <- kmeans(df.AgePurchase.scaled, 4)
png("/var/www/html/graphcluster/KmeansClustering.Age.Purchase.png")
fviz_cluster(list(data = df.AgePurchase.scaled, cluster = k.means.fit$cluster), ellipse.type = "norm", geom = "point", stand = FALSE, palette = "jco", ggtheme = theme_classic())
# fviz_cluster(k.means.fit, data = df.AgePurchase.scaled, ellipse.type = "convex")+ theme_minimal()
dev.off
