CreateData = function(dd){
  dd_wide = spread(dd[,c("Date", "stock", "price")], key = stock, value = price) 
  dd_wide = dd_wide[complete.cases(dd_wide), ] # 250 125
  dd_long = gather(dd_wide, key = stock, value = price, -Date)
  
  dd_norm_wide = dd_wide
  dd_norm_wide[,2:125] = apply(dd_norm_wide[,2:125], 2, scale)
  dd_norm_long = gather(dd_norm_wide, key = stock, value = price, -Date)
  
  #### DATA PREPARATION ####
  dd_wide_t = data.frame(t(dd_wide))
  dd_wide_t = dd_wide_t[-1,]
  dd_wide_t[1:dim(dd_wide_t)[2]] = lapply(dd_wide_t[1:dim(dd_wide_t)[2]], 
                                          function(df) as.numeric(as.character(df)))
  dd_wide_t = rownames_to_column(dd_wide_t)
  rowname = dd_wide_t$rowname
  d = as.matrix(dd_wide_t[,-1]) # 124 250
  rownames(d) = rowname
  
  dd_norm_wide_t = data.frame(t(dd_norm_wide))
  dd_norm_wide_t = dd_norm_wide_t[-1,]
  dd_norm_wide_t[1:dim(dd_norm_wide_t)[2]] = lapply(dd_norm_wide_t[1:dim(dd_norm_wide_t)[2]], 
                                                    function(df) as.numeric(as.character(df)))
  dd_norm_wide_t = rownames_to_column(dd_norm_wide_t)
  d_norm = as.matrix(dd_norm_wide_t[,-1]) # 124 250
  rownames(d_norm) = rowname
  
  return(list(d = d, d_norm = d_norm, dd_long = dd_long, dd_norm_long = dd_norm_long))
}

# Examples : 
# kmeans_alg(k.max = 30, seed = 123, data = d_norm, nstart = 30, iter.max = 50, method = "euclidean")
# kmeans_alg(k.max = 30, seed = 123, data = d, nstart = 30, iter.max = 50, method = "correlation")
kmeans_alg = function(k.max, seed, data, nstart, iter.max, method){
  wss <- sapply(2:k.max, function(k){
    set.seed(seed*k)
    sum(Kmeans(data, k, nstart = nstart, iter.max = iter.max, method = method)$withinss)
  })
  p = plot(2:k.max, wss, type="b", pch = 19, frame = FALSE, xlab="Number of clusters K",
           ylab="Total within-clusters sum of squares")
  return(list(p, wss = data.frame(clust = 2:k.max, wss = wss)))
}

# Examples :
# hc_alg(data = d, method = "euclidean", k.max = 30) 
# hc_alg(data = d, method = "correlation", k.max = 50) 
# hc_alg(data = d, method = "DTW", k.max = 30) 
hc_alg = function(data, method, k.max){
  
  wss <- function(d) {
    sum(scale(d, scale = FALSE)^2)
  }
  wrap <- function(i, hc, x, norm) {
    cl <- cutree(hc, i)
    spl <- split(x, cl)
    wss <- sum(sapply(spl, wss))
    wss
  }
  
  dst  = dist(data, method= method)
  hc <- hclust(dst)
  res <- sapply(2:k.max, wrap, h = hc, x = data, norm = norm)
  p = plot(2:k.max, res, type = "b", pch = 19)
  return(list(p, wss = data.frame(clust = 2:k.max, wss = res)))
}

GetClustKmeans = function(k, seed, stocknames, data, nstart, iter.max, method){
  set.seed(seed*k)
  clust_cor_kmeans = data.frame(stock = stocknames)
  clust_cor_kmeans$clust = Kmeans(data, k, nstart, iter.max, method = method)$cluster
  
  return(clust_cor_kmeans)
}

