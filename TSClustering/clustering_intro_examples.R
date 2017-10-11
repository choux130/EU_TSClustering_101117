#### SIMULATED DATA ####
library(MixSim)

set.seed(3)
Q <- MixSim(BarOmega = 0.00002, MaxOmega = NULL , K = 4, p = 2, hom = TRUE)
A = simdataset(n = 50, Pi = Q$Pi, Mu = Q$Mu, S = Q$S)
sim_d = data.frame(A$X)
sim_d$true_cluster = A$id
plot(sim_d[,1:2], main = "Simulated Clustering Data")
plot(sim_d[,1:2], col = sim_d$true_cluster, main = "Simulated Clustering Data")

#### K-means Animation ####
# https://yihui.name/animation/example/kmeans-ani/
library(animation)
ani.options(interval = 3)
set.seed(123)
kmeans.ani(sim_d[,1:2], centers = 4, pch = 1:4,  col = 1:4, 
           hints = c("Get the new center points", 
                     "Update cluster to the nearest one"))
# save the animation
saveGIF({
  set.seed(123)
  kmeans.ani(sim_d[,1:2], centers = 4, pch = 1:4,  col = 1:4, 
             hints = c("Get the new center points", "Update cluster to the nearest one"))
}, movie.name = "kmeans.gif", interval = 3, nmax = 30, ani.width = 600)

#### Things need to be noticed ####
#### You won’t always get the same results. So, do it more times! ####
set.seed(2223) # good case
k  = kmeans(sim_d[,1:2], centers=4, nstart = 1) 
plot(sim_d[,1:2], col = k$cluster, main = "Good Casse")

set.seed(14) # bad case
k  = kmeans(sim_d[,1:2], centers=4, nstart = 1) 
plot(sim_d[,1:2], col = k$cluster, main = "Bad Case")

# so we need to try multiple time 
set.seed(14) 
k  = kmeans(sim_d[,1:2], centers=4, nstart = 30) 
plot(sim_d[,1:2], col = k$cluster)

#### How to choose the correct number of clusters ? ####
set.seed(14) # 2 clusters
k  = kmeans(sim_d[,1:2], centers=2, nstart = 1) 
plot(sim_d[,1:2], col = k$cluster)

set.seed(14) # 6 clusters
k  = kmeans(sim_d[,1:2], centers=6, nstart = 1) 
plot(sim_d[,1:2], col = k$cluster)

# find the best number of cluster
k.max <- 10
wss <- sapply(1:k.max, function(k){
  kmeans(sim_d, k, nstart=30, iter.max = 15 )$tot.withinss
})
plot(1:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")

#### Standardize the data ####
# https://stats.stackexchange.com/questions/41704/how-and-why-do-normalization-and-feature-scaling-work

#### Hierarchical Clustering ####

#### Dissimilarity Matrix ####
library(corrplot)

dst = dist(sim_d[,1:2], method = "euclidean")
corrplot(as.matrix(dst), is.corr = FALSE, method = "color", 
         tl.cex = 0.5, cl.cex = 0.5, mar = c(0.5, 0.5, 0.5, 0.5))

#### Algorithm ####
dst = dist(sim_d[,1:2], method = "euclidean")
hc = hclust(dst)
plot(hc, cex = 0.6)

#### Find the best cluster ####
dst = dist(sim_d[,1:2], method = "euclidean")
hc = hclust(dst)

wss <- function(d) {
  sum(scale(d, scale = FALSE)^2)
}
wrap = function(i, hc, x) {
  cl = cutree(hc, i)
  spl = split(x, cl)
  wss = sum(sapply(spl, wss))
  wss
}

res = sapply(seq.int(1, 10), wrap, h = hc, x = sim_d[,1:2])
plot(seq_along(res), res, type = "b", pch = 19)

k = 4
plot(hc, cex = 0.6)
rect.hclust(hc, k, border = 2:5)
cutree(hc, k)

#### Time Series Data ####
#### Example Data - stock price ####
library(tidyverse)
# temp = list.files(path = "FreshData", pattern="*.csv")
# myfiles = lapply(temp, function(x) read.csv(paste("FreshData", x, sep="/"), header=TRUE))
# name = sapply(temp, function(x) unlist(strsplit(x[1], split='.', fixed=TRUE))[1])
# names(myfiles) = name
# 
# newfiles = lapply(myfiles, function(df) {
#   cbind(df, price = apply(df[c("Open", "High", "Low", "Close")], 1, mean))
# })
# newfiles = lapply(newfiles, function(df) df[which(names(df) %in% c('Date','price'))])
# newfiles = lapply(newfiles, function(df) df[complete.cases(df),])
# 
# # change the name of 'price' column to the stock name
# for (i in names(newfiles)){
#   names(newfiles[[i]]) = c('Date', i)
# }
# 
# d_all= Reduce(function(dtf1, dtf2) full_join(dtf1, dtf2), newfiles[c(1:37)])
# d_all$Date = as.Date(d_all$Date, "%Y-%m-%d")
# d_all = d_all[order(d_all$Date),]
# 
# d_all = d_all[order(d_all$Date),]
# dim(d_all) # 251  38
# View(d_all)
# write.csv(d_all, file = "stock_37.csv")

d_all = read.csv("stock_37.csv", header = TRUE)
d_all = d_all[-which(names(d_all) %in% c("ADS", "AGN", "AES", "AMD"))]
d_all = d_all[complete.cases(d_all),]
d_all = d_all[,-1]
num_col = dim(d_all)[2]
d_all$Date = as.Date(d_all$Date, "%Y-%m-%d")
d_all = subset(d_all, Date >= "2017-01-01")
d_all_long = gather(d_all, key = stock, value = price, 2:num_col)
ggplot(d_all_long, aes(x = Date, y = price, group = stock, color = stock)) + 
  geom_line() + guides(col = guide_legend(keywidth = 1, keyheight = 0.5) )

saveGIF({
  for (i in 1:length(unique(d_all_long$stock))) 
    print(ggplot(subset(d_all_long, stock %in% unique(d_all_long$stock)[1:i]), 
           aes(x = Date, y = price, group = stock, color = stock)) + 
    geom_line() + guides(col = guide_legend(keywidth = 1, keyheight = 0.5) ))
}, movie.name = "stock.gif", interval = 0.5)

#### Results ####
# data preparation
time = as.Date(d_all$Date, '%Y-%m-%d')
d_all_t = data.frame(t(d_all))
d_all_t = d_all_t[-1,]
d_all_t[1:dim(d_all_t)[2]] = lapply(d_all_t[1:dim(d_all_t)[2]], 
         function(df) as.numeric(as.character(df)))
d_all_t = rownames_to_column(d_all_t)

# euclidean - amap::Kmeans
library(amap)

d = as.matrix(d_all_t[,-1])
k.max <- 10
wss <- sapply(2:k.max, function(k){
  set.seed(11*k)
  sum(Kmeans(d, k, nstart=30, iter.max = 30, method = "euclidean")$withinss)
})
plot(2:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
k=2
set.seed(11*k)
d_all_t$clust_eu = Kmeans(d, k, nstart=30, iter.max = 15)$cluster
clust_eu  = d_all_t[,c('rowname','clust_eu')]
names(clust_eu) = c("stock", "clust_eu")

# correlation - amap::Kmeans
k.max <- 10
wss <- sapply(2:k.max, function(k){
  set.seed(33*k)
  sum(Kmeans(d, k, nstart=30, iter.max = 30, method = "correlation")$withinss)
})
plot(2:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")
k=4
set.seed(33*k)
d_all_t$clust_cor = Kmeans(d, k, nstart=30, iter.max = 30)$cluster
clust_cor  = d_all_t[,c('rowname','clust_cor')]
names(clust_cor) = c("stock", "clust_cor")

# dtw 
dst_dtw  = dist(d, method= "DTW")
hc <- hclust(dst_dtw)
plot(hc, cex = 0.6)

wss <- function(d) {
  sum(scale(d, scale = FALSE)^2)
}
wrap <- function(i, hc, x) {
  cl <- cutree(hc, i)
  spl <- split(x, cl)
  wss <- sum(sapply(spl, wss))
  wss
}

res <- sapply(seq.int(1, 10), wrap, h = hc, x = d)
plot(seq_along(res), res, type = "b", pch = 19)

k = 4
plot(hc, cex = 0.6)
rect.hclust(hc, k, border = 2:5)

d_all_t$clust_dtw = cutree(hc, k)
clust_dtw  = d_all_t[,c('rowname','clust_dtw')]
names(clust_dtw) = c("stock", "clust_dtw")

# merge data for plotting
d_clust = Reduce(function(x, y) merge(x, y, all=TRUE), 
                list(clust_eu, clust_cor, clust_dtw))
d_all_long_clust = merge(d_all_long, d_clust, by = "stock", all.x = TRUE)

p_eu = ggplot(d_all_long_clust, aes( x = Date, y = price, color = stock )) + 
  geom_line() + facet_grid(clust_eu ~., scales = "free") +
  guides(col = guide_legend(keywidth = 1, keyheight = 0.5) ) + 
  labs(title = "Clusters using Euclidean Distance") +
  theme(legend.position="none")
p_eu

p_cor = ggplot(d_all_long_clust, aes(x = Date, y = price, color = stock )) + 
  geom_line() + facet_grid(clust_cor ~., scales = "free") +
  guides(col = guide_legend(keywidth = 1, keyheight = 0.5) ) +
  labs(title = "Clusters using Correlation Distance") +
  theme(legend.position="none")
p_cor

p_dtw = ggplot(d_all_long_clust, aes(x = Date, y = price, color = stock )) + 
  geom_line() + facet_grid(clust_dtw ~., scales = "free") +
  guides(col = guide_legend(keywidth = 1, keyheight = 0.5) ) +
  labs(title = "Clusters using DTW Distance") +
  theme(legend.position="none")
p_dtw



