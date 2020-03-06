# In this exercise you'll use longitudinal brand purchase data to look for 
# clusters of people with similar weekly shopping patterns. To do this, we
# 1) wrangle the original data on purchase times into a data frame containing
#    the number of times each consumer made a purchase (of any brand) on each
#    day of the week (i.e. on a a M, Tu, W, Th, F, Sa, Su).
# 2) fit a k-means clustering model
# 3) look at different ways of choosing k

library(dplyr)
library(tidyr)
library(ggplot2)

load("timeseries/data/purchase_data.rdata")

### 1) wrangle the original data on purchase times

# number of times each customer bought on a M, Tu, W, Th, F, Sa, Su
cust_pch_per_wday <- purchase_data %>% 
  count(id, day) 

# remove purchase data, don't need
rm(purchase_data)

# reshape long to wide
cust_pch_per_wday <- cust_pch_per_wday %>%
  pivot_wider(names_from = day, values_from = n) 

# replace any missing values with zeros
cust_pch_per_wday <- cust_pch_per_wday %>%
  mutate_all(replace_na, replace = 0)

# drop the id column
kmeans_data <- cust_pch_per_wday %>% dplyr::select(-id)

### 2) fit a k-means clustering model

# k = 2 
k2res <- kmeans(kmeans_data, centers = 2, nstart = 5)

# % variance explained
k2res$betweenss / k2res$totss

# plot cluster centroids

# first do some cleaning up; rename variables and reshape wide to long
k2res$centers_long <- k2res$centers %>% data.frame() %>% 
  rename(Sun = X1, Mon = X2, Tue = X3, Wed = X4, Thu = X5, Fri = X6, Sat = X7) %>%
  mutate(cluster = paste0("Clus", 1:2)) %>%
  pivot_longer(cols = -cluster, names_to = "Day", values_to = "Purchases") %>%
  mutate(Day = factor(Day, levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")))

# now ggplot
k2res$centers_long %>% 
  ggplot(aes(x = Day, y = Purchases, colour = cluster)) +
  geom_point() + geom_line(aes(group = cluster))

#### EXERCISE: try some other values of k. What do you find?

### 3) look at different ways of choosing k

# Approach 1: "by eye" (use the results from the last exercise to make some kind
# of subjective choice of what seems sensible)

# Approach 2: Plot % variance explained (between-cluster SS / total SS) against k

kres <- list()
varexp <- c()
for(k in 1:10){
  kres[[k]] <- kmeans(kmeans_data, centers = k, nstart = 5)
  # % variance explained
  varexp <- c(varexp, kres[[k]]$betweenss / kres[[k]]$totss)
}
plot(1:10, varexp)

# Approach 3: Cross validation

# Partition data into m = 10 parts ("folds")
kmeans_data <- kmeans_data %>% 
  mutate(fold_id = sample(x = 1:10, size = nrow(kmeans_data), replace = TRUE))

# For each of the m parts ((( just showing for one part below )))

# keep that part aside as test data,
kmeans_data_test <- kmeans_data %>% filter(fold_id == 1)

# fit clustering model on the rest of the data
kres_train <- kmeans(kmeans_data %>% filter(fold_id != 1) %>% dplyr::select(-fold_id),
       centers = 2)

# calculate the sum of the squared distances to the centroids for the test set

# here's a fn that works out the distance between a new observation and each 
# of the cluster centroids in an already fitted kmeans model
dist_to_clusters <- function(km_fitted, newdata){
  d <- apply(km_fitted$centers, 1, function(x) sqrt(sum((newdata - x)^2)))
  return(d)
}

# for example
dist_to_clusters(kres_train, newdata = kmeans_data_test[1,-8])

# that's not really what we need
# we want a fn that works out the MINIMUM distance between a new observation and each 
# of the cluster centroids in an already fitted kmeans model
dist_to_closest_cluster <- function(km_fitted, newdata){
  d <- apply(km_fitted$centers, 1, function(x) sqrt(sum((newdata - x)^2)))
  return(min(d))
}

# for example
dist_to_closest_cluster(kres_train, newdata = kmeans_data_test[1,-8])

# apply to all rows in the test data
# this gives what we want: distances to the (closest) centroids for the test set
test_dists <- apply(kmeans_data_test[,-8], 1, dist_to_closest_cluster, 
                    km_fitted = kres_train)
# sum up over all users
sum_test_dists <- sum(test_dists)

## this is just for ONE of the 10 folds, and only for ONE possible value of k. So 
## we would need to do the above for each of the other 9 folds, and take the mean.
## That's the cross-validated "accuracy" for the 2-cluster model. Then we need to 
## do the same for k = 3, 4, 5, etc. We'd choose the value of k that gives the 
## smallest value of sum_test_dists. Try this to complete the exercise.



