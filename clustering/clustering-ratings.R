library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

# https://bookdown.org/rdpeng/exdata/hierarchical-clustering.html 

###### data preparation (not for class)

load("clustering/data/movielens-small.RData")

# N users with most ratings
users_frq <- ratings %>% group_by(userId) %>% summarize(count = n()) %>% arrange(desc(count))
my_users <- users_frq$userId[1:100]

# M movies with most ratings
movies_frq <- ratings %>% group_by(movieId) %>% summarize(count = n()) %>% arrange(desc(count))
my_movies <- movies_frq$movieId[1:30]

# reduced dataset
ratings_red <- ratings %>% filter(userId %in% my_users, movieId %in% my_movies)
ratings_red <- droplevels(ratings_red)
ratings_red <- ratings_red %>% complete(userId, movieId) %>% replace_na(list(rating = 3))

# add movie title
ratings_red <- left_join(ratings_red, movies)

# clean up the movie titles
ratings_red <- ratings_red %>% 
  mutate(title = str_remove_all(title, " \\(.*")) %>%
  mutate(title = str_remove_all(title, " -.*")) %>%
  mutate(title = str_remove_all(title, "The.*Ring")) 

# save
saveRDS(ratings_red, file = "clustering/data/movieratings.Rds")

#######

# class example starts here

# read in movie ratings
ratings <- readRDS(file = "clustering/data/movieratings.Rds")

# reshape long to wide
ratings_wide <- ratings %>% 
  dplyr::select(userId, title, rating) %>% 
  pivot_wider(names_from = title, values_from = rating)

# visualize the raw data
par(mai = c(2,.5,.5,.5))
image(t(ratings_wide[,-1])[, (nrow(ratings_wide)-1):1], axes = F)
axis(1, at = 0:29/29, names(ratings_wide)[-1], las = 3, cex.axis = 0.7)
axis(2, at = 0:99/99, rownames(ratings_wide), cex.axis = 0.6)

# cluster rows and columns
heat <- heatmap(as.matrix((ratings_wide[,-1])), scale = "none", margins = c(12, 5))
row_ordering <- heat$rowInd
col_ordering <- heat$colInd
# manual-ish heatmap
ratings_mat <- as.matrix(ratings_wide[,-1])
image(t(ratings_mat[row_ordering, col_ordering]))

# how does it cluster?

# hierarchical clustering with hclust
hcl_complete <- dist(ratings_wide[,-1]) %>% hclust(method = "complete")
hcl_single <- dist(ratings_wide[,-1]) %>% hclust(method = "single")
hcl_centroid <- dist(ratings_wide[,-1]) %>% hclust(method = "centroid")

plot(hcl_complete)
plot(hcl_single)
plot(hcl_centroid)

user_clusters <- cutree(hcl_complete, h = 9)
user_clusters_df <- data.frame(userId = ratings_wide$userId, 
                               user_clus = user_clusters, row.names = NULL)
table(user_clusters)
ratings_wide <- ratings_wide %>% mutate(user_clusters = user_clusters)
table(ratings_wide$user_clusters)

# plot mean movie ratings within each cluster
cluster_means <- ratings_wide %>% 
  group_by(user_clusters) %>%
  summarise_at(vars(-starts_with("user")), mean, na.rm = TRUE) %>% 
  ungroup()

cluster_means %>% mutate(user_clusters = factor(user_clusters)) %>%
  pivot_longer(cols = -1, names_to = "title", values_to = "rating") %>%
  ggplot(aes(x = title, y = rating, colour = user_clusters, group = user_clusters)) + 
  geom_line() + coord_flip() + xlab(element_blank()) + ylab(element_blank())

cluster_means %>% mutate(user_clusters = factor(user_clusters)) %>%
  pivot_longer(cols = -1, names_to = "title", values_to = "rating") %>%
  ggplot(aes(x = title, y = rating, colour = user_clusters, group = user_clusters)) +
  facet_grid(. ~ user_clusters) +
  geom_line() + coord_flip() + xlab(element_blank()) + ylab(element_blank())

# redo heatmap after grouping users into clusters
ratings_userclus <- ratings_wide %>% 
  dplyr::select(-userId) %>% group_by(user_clusters) %>%
  summarise_all(mean, na.rm = TRUE) 
heatmap(as.matrix((ratings_userclus[,-1])), scale = "none", margins = c(12, 5))

# or keep users separate but reorder them using cluster results 
hcl_row_ordering <- hcl_complete$order
hcl_row_ordering
row_ordering
par(mfrow = c(1,2))
image(t(as.matrix(ratings_mat)), main = "Original Matrix")
image(t(ratings_mat[hcl_row_ordering, ncol(ratings_mat):1]), main = "Reordered rows")

# can also cluster the movies 

# transpose and leave out Id and cluster memb columns
ratings_wide_t <- t(ratings_wide[,-c(1,32)]) %>% as.data.frame()
names(ratings_wide_t) <- ratings_wide$userId

# hclust
hcl_complete <- dist(ratings_wide_t) %>% hclust(method = "complete")
hcl_single <- dist(ratings_wide_t) %>% hclust(method = "single")
hcl_centroid <- dist(ratings_wide_t) %>% hclust(method = "centroid")

plot(hcl_complete)
plot(hcl_single)
plot(hcl_centroid)

title_clusters <- cutree(hcl_complete, k = 6)
title_clusters_df <- data.frame(title = names(title_clusters), title_clus = title_clusters, row.names = NULL)
table(title_clusters)
# movies in cluster 1
str_c(title_clusters_df[title_clusters_df$title_clus == 1, "title"], collapse = "; ")
# movies in cluster 2
str_c(title_clusters_df[title_clusters_df$title_clus == 2, "title"], collapse = "; ")
# movies in cluster 3
str_c(title_clusters_df[title_clusters_df$title_clus == 3, "title"], collapse = "; ")

ratings_wide_t <- ratings_wide_t %>% mutate(title_clusters = title_clusters)
table(ratings_wide_t$title_clusters)

ratings_titleclus <- ratings_wide_t %>% group_by(title_clusters) %>%
  summarise_all(mean, na.rm = TRUE) 

# plot mean movie ratings within each cluster
cluster_means <- ratings_wide_t %>% 
  group_by(title_clusters) %>%
  summarise_all(mean, na.rm = TRUE) %>% 
  ungroup()

cluster_means %>% mutate(title_clusters = factor(title_clusters)) %>%
  pivot_longer(cols = -1, names_to = "user", values_to = "rating") %>%
  ggplot(aes(x = user, y = rating, colour = title_clusters, group = title_clusters)) + 
  geom_line() + coord_flip() + xlab(element_blank()) + ylab(element_blank())

cluster_means %>% mutate(title_clusters = factor(title_clusters)) %>%
  pivot_longer(cols = -1, names_to = "user", values_to = "rating") %>%
  ggplot(aes(x = user, y = rating, colour = title_clusters, group = title_clusters)) + 
  geom_line() + coord_flip() + xlab(element_blank()) + ylab(element_blank()) +
  facet_grid(. ~ title_clusters)

# not sure if this works
usedId_ordered <- ratings_wide$userId[hcl_row_ordering]
cluster_means[, c(1, hcl_row_ordering + 1)] %>% mutate(title_clusters = factor(title_clusters)) %>%
  pivot_longer(cols = -1, names_to = "user", values_to = "rating") %>%
  mutate(user = factor(user, levels = usedId_ordered)) %>%
  ggplot(aes(x = user, y = rating, colour = title_clusters, group = title_clusters)) + 
  geom_line() + coord_flip() + xlab(element_blank()) + ylab(element_blank()) 

# redo heatmap after grouping movies into clusters
heatmap(as.matrix((ratings_titleclus[,-1])), scale = "none", margins = c(12, 5))

# or keep users separate but reorder them using cluster results 
hcl_col_ordering <- hcl_complete$order
hcl_col_ordering
col_ordering
par(mfrow = c(1,2))
image(t(as.matrix(ratings_mat)), main = "Original Matrix")
image(t(ratings_mat[nrow(ratings_mat):1, col_ordering]), main = "Reordered columns")

# put it all together

par(mfrow = c(1,4))
image(t(as.matrix(ratings_mat)), main = "Original Matrix")
image(t(ratings_mat[hcl_row_ordering, ncol(ratings_mat):1]), main = "Reordered rows")
image(t(ratings_mat[nrow(ratings_mat):1, hcl_col_ordering]), main = "Reordered columns")
image(t(ratings_mat[hcl_row_ordering, hcl_col_ordering]), main = "Reordered both")
# compare to the original heatmap
heatmap(as.matrix((ratings_wide[,-c(1,32)])), scale = "none", margins = c(12, 5))

# merge in cluster memberships for users and movies
ratings <- ratings %>% 
  left_join(user_clusters_df, by = "userId") %>% 
  left_join(title_clusters_df, by = "title")

# summarize
ratings_bothclus <- ratings %>% 
  group_by(title_clus, user_clus) %>%
  summarize(rating = mean(rating, na.rm = TRUE)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = title_clus, values_from = rating)

heatmap(as.matrix((ratings_bothclus[,-1])), scale = "none", margins = c(5, 5))

## kmeans

df <- ratings_wide %>% dplyr::select_at(vars(-starts_with("user")))
kmeansObj <- kmeans(df, centers = 6)
names(kmeansObj)
kmeansObj$cluster

# cluster means

kmeansObj$centers %>% as.data.frame() %>% 
  mutate(user_clus_km = paste0("clust",1:6)) %>%
  pivot_longer(-user_clus_km, names_to = "title", values_to = "rating") %>%
  ggplot(aes(x = title, y = rating, colour = user_clus_km, group = user_clus_km)) + 
  geom_line() + coord_flip() + xlab(element_blank()) + ylab(element_blank())


# or manually
ratings_wide <- ratings_wide %>% mutate(user_clus_km = kmeansObj$cluster)

cluster_means <- ratings_wide %>% 
  group_by(user_clus_km) %>%
  summarise_at(vars(-starts_with("user")), mean, na.rm = TRUE) %>% 
  ungroup()

cluster_means %>% mutate(user_clus_km = factor(user_clus_km)) %>%
  pivot_longer(cols = -1, names_to = "title", values_to = "rating") %>%
  ggplot(aes(x = title, y = rating, colour = user_clus_km, group = user_clus_km)) + 
  geom_line() + coord_flip() + xlab(element_blank()) + ylab(element_blank())

# similarity between kmeans and hclust
xx <- table(ratings_wide$user_clusters, ratings_wide$user_clus_km)
image(xx)

# heatmap (cluster rows only)
image(t(df)[, nrow(df):1], yaxt = "n", main = "Original Data")
image(t(df)[, order(kmeansObj$cluster)], yaxt = "n", main = "Clustered Data")



#### pca/svd

# scale and reorder ratings
ratings_scaled_ordered <- scale(ratings_mat)[hcl_row_ordering, hcl_col_ordering]
ratings_ordered <- ratings_mat[hcl_row_ordering, hcl_col_ordering]

svd1 <- svd(ratings_scaled_ordered)

u <- svd1$u
v <- svd1$v
d <- diag(svd1$d)

# approximate original data with outer product of first N singular vectors
approx1 <- u[,1] %*% matrix(d[1,1],nrow=1) %*% t(v[,1])
approx2 <- u[,1:2] %*% d[1:2,1:2] %*% t(v[,1:2])
approx5 <- u[,1:5] %*% d[1:5,1:5] %*% t(v[,1:5])
approx30 <- u %*% d %*% t(v)

# plot original data and approximated data
par(mfrow = c(1, 5))
image(t(ratings_scaled_ordered[100:1,]), main = "Original Matrix")
image(t(approx1)[, nrow(approx1):1], main = "Approximation (1 SV)")
image(t(approx2)[, nrow(approx2):1], main = "Approximation (2 SV)")
image(t(approx5)[, nrow(approx5):1], main = "Approximation (5 SV)")
image(t(approx30)[, nrow(approx30):1], main = "Approximation (30 SV)")

par(mfrow = c(1, 3))
image(t(ratings_scaled_ordered[100:1,]), main = "Original Data")
plot(u[, 1], 100:1, ylab = "Row", xlab = "First left singular vector", pch = 19)
plot(v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)

# PCs are equal to the right SVs if you scale the data to have mean 0 and sd 1

plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d / sum(svd1$d), xlab = "Column", ylab = "% variance explained", pch = 19)
plot(cumsum(svd1$d) / sum(svd1$d), xlab = "Column", ylab = "Cumulative % variance explained", pch = 19)

pca1 <- prcomp(ratings_ordered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, 
     xlab = "Principal Component 1", ylab = "Right Singular Vector 1") 
abline(c(0, 1), col = "red")

par(mfrow = c(1,2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(pca1$sdev, xlab = "Column", ylab = "Std dev of PCs", pch = 19)

library(imager)

mona <- load.image("clustering/data/lowres_mona.png")
plot(mona)

# transform into matrix form
mona_mat <- as.data.frame(mona) %>% 
  pivot_wider(names_from = y, values_from = value) %>%
  dplyr::select(-x) %>%
  as.matrix()

image(mona_mat[, resized_x:1], col = gray.colors(100))

# svd
svd1 <- svd(mona_mat)

u <- svd1$u
v <- svd1$v
d <- diag(svd1$d)

# number of singular values
nsv <- 5

# approximate original data with outer product of first N singular vectors
approx <- u[,1:nsv] %*% matrix(d[1:nsv,1:nsv],nrow=nsv) %*% t(v[,1:nsv])

# plot approximated mona
image(approx[, nrow(approx):1], col = gray.colors(30))

plot(svd1$d / sum(svd1$d), xlab = "Column", ylab = "% variance explained", pch = 19)

