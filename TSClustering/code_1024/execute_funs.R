library(amap)
library(dtw)

#### LOAD DATA ####
dd_all = read.csv("stocks_124.csv", header = TRUE)
dd_all = dd_all[,-1]
dd_all$Date = as.Date(dd_all$Date, "%Y-%m-%d")

# dd_4 = subset(dd_all, Date >= "2016-10-03" & Date <= "2016-12-31") # 7688    6
# dd_1 = subset(dd_all, Date >= "2017-01-01" & Date <= "2017-03-31") # 7688    6
# dd_2 = subset(dd_all, Date >= "2017-04-01" & Date <= "2017-06-30") # 7812    6
dd_3 = subset(dd_all, Date >= "2017-07-01" & Date <= "2017-09-30") # 7727    6
dd = dd_3

all_dd = CreateData(dd)
d = all_dd$d
d_norm = all_dd$d_norm
dd_long = all_dd$dd_long
dd_norm_long = all_dd$dd_norm_long
stocknames = rownames(d)

kmeans_alg(k.max = 30, seed = 123, data = d_norm, nstart = 30, iter.max = 50, method = "correlation")

k = 2
clust_cor_kmeans = GetClustKmeans(k, seed=123, stocknames, data = d_norm, nstart=30, iter.max=50, 
                                  method = "correlation")


dd_long_clust = merge(dd_long, clust_cor_kmeans, by = "stock", all.x = TRUE)
dd_norm_long_clust = merge(dd_norm_long, clust_cor_kmeans, by = "stock", all.x = TRUE)

clust_cor_kmeans[clust_cor_kmeans$stock == "SP500",]
table(clust_cor_kmeans$clust)

write.csv(clust_cor_kmeans, "all_clust_list.csv")


# Q4 : k = 16, sp500 = 7, c(1,5,7,10)
# Q1 : k = 4, sp500 = 4, c(1:4)
# Q2 : k = 4, sp500 = 4, c(1:4)
# Q3 : k = 2, sp500 = 1, c(1:2)
# All : k = 6, sp500 = 1, c(1:6)

# only S&P 500
if (!require("grid")) install.packages("grid")
if (!require("gridExtra")) install.packages("gridExtra")

p_500 = ggplot(subset(dd_norm_long_clust, stock == "SP500"), aes(x = Date, y = price, group = stock, color = stock)) + geom_line() +
  theme(legend.position="none") + facet_grid(clust ~., scales = "free") +
  labs(title = "S&P 500") 
p_500

p_all = ggplot(subset(dd_norm_long_clust, stock != "SP500" & clust %in% c(1:6)), 
               aes(x = Date, y = price, color = stock )) + 
  geom_line() + facet_grid(clust ~., scales = "free") +
  guides(col = guide_legend(keywidth = 1, keyheight = 0.5) ) + 
  labs(title = "Clusters using Correlation Distance") + 
  theme(legend.position="none")
p_all

g = grid.arrange(p_500, p_all , nrow = 2, heights = c(1,4)) 
ggsave("plots/q3_comp.png", g)

p_500_no = ggplot(subset(dd_norm_long_clust, stock == "SP500"), 
                  aes(x = Date, y = price, group = stock, color = stock)) + 
  geom_line() + theme(legend.position="none") +
  facet_grid(clust ~., scales = "free") +
  labs(title = "S&P 500") 
p_500_no

p_cor = ggplot(subset(dd_norm_long_clust, stock != "SP500" & clust== 1), 
               aes(x = Date, y = price, color = stock )) + 
  geom_line() + facet_grid(clust ~., scales = "fix") +
  theme(legend.position="none") + 
  guides(col = guide_legend(keywidth = 0.5, keyheight = 0.5) ) + 
  labs(title = "Clusters using Correlation Distance") 
p_cor

gg = grid.arrange(p_500_no, p_cor , nrow = 2, heights = c(1,1)) 
ggsave("plots/all_sp.png", gg)

p_raw = ggplot(dd_long, aes(x = Date, y = price, group = stock, color = stock)) + geom_line() +
  theme(legend.position="none") + 
  labs(title = "Raw 124 stocks data") 
p_raw

p_norm = ggplot(dd_norm_long, aes(x = Date, y = price, group = stock, color = stock)) + geom_line() +
  theme(legend.position="none") + 
  labs(title = "Normalized 124 stocks data") 
p_norm

pp = grid.arrange(p_raw, p_norm , nrow = 2, heights = c(1,1)) 
ggsave("plots/raw_norm.png", pp)

