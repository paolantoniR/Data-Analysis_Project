library(tidyverse)
library(dbplyr)
library(ggplot2)
library(GGally)
library(ggcorrplot)
library(gridExtra)
library(dataPreparation)
library(leaps)
library(glmnet)
library(tree)
require(car)
library(randomForest)
require(factoextra)


# data import and 1st cleaning
Starter <- read.csv('BrazHousesRent.csv')
glimpse(Starter)
summary(Starter)

sum(is.na(Starter))

sum(duplicated(Starter))

data <- distinct(Starter)
#visualize categorical variables
p1 <- ggplot(data, aes(x = city)) + 
  geom_bar()
p2 <- ggplot(data, aes(x = rooms)) + 
  geom_bar()
p3 <- ggplot(data, aes(x = animal)) + 
  geom_bar()
p4 <- ggplot(data, aes(x = furniture)) + 
  geom_bar()
p5 <- ggplot(data, aes(x = floor)) + 
  geom_bar()

grid.arrange(p1,p2,p3,p4,p5)


#visualize target variable
ggplot(data, aes(x = rent_amount)) + 
  geom_histogram()

# data manipulation
data <- data %>% 
  mutate_if(is.character, as.factor)

data <- remove_percentile_outlier(data)

data <-  data %>% 
  mutate(house_size = rooms + bathroom + parking_spaces)

house_size = data %>%
  cut(x = data$house_size,
      breaks = c(min(data$house_size),3,4,6,7,max(data$house_size)),
      labels = c("Very_Small","Small","Normal","Big","Very_Big"),
      include.lowest = T,
      right = T)
table(house_size)
data$house_size = house_size

data %>%
  select(where(is.numeric)) %>% 
  ggpairs()

data <- data %>% 
  select(-rooms,-bathroom, -parking_spaces, -fire_insurance, -floor)

data %>%
  select(where(is.numeric)) %>% 
  ggpairs()

data <- data %>%
  mutate(across(c(area, hoa, property_tax, rent_amount),function(x)sqrt(x)))

data %>%
  select(where(is.numeric)) %>% 
  ggpairs()

data %>% 
  select_if(is.numeric) %>%  
  cor() %>%  
  ggcorrplot(lab = TRUE, legend.title = 'Correlation')


# start with task 1
data <- data[,c(6,1,2,3,4,5,7,8)]
idx_train = sample.int(n = nrow(data), size = floor(.7*nrow(data)), replace = F)
data.train = data[idx_train, ]
data.valid  = data[-idx_train, ]

#multiple linear regression with all predictors
lm.fit <- lm(rent_amount ~ ., data.train)
summary(lm.fit)

pred_lm.fit <-  predict(lm.fit, data.valid[,-1])
mean((data.valid$rent_amount - pred_lm.fit)^2)

par(mfrow = c(2,2))
plot(lm.fit)

sort(vif(lm.fit)) #no multicollinearity

lm.fit <- lm(log(rent_amount) ~ ., data.train)
summary(lm.fit)
plot(lm.fit)

#multiple linear regression with stepwise selection

regfit.fwd <- regsubsets(rent_amount ~ ., data = data.train,
                         nvmax = 8, method = "forward")
summary(regfit.fwd)

regfit.bwd <- regsubsets(rent_amount ~ ., data = data.train,
                         nvmax = 8, method = "backward")
summary(regfit.bwd)

#regression with the best predictor resulting from fwd stepwise selection
lm.fit <- lm(rent_amount ~ poly(property_tax,3), data = data.train)
summary(lm.fit)
pred_lm.fit = predict(lm.fit, data.valid[,-1])
mse(data.valid$rent_amount, pred_lm.fit)  
par(mfrow = c(2,2))
plot(lm.fit)



# Ridge regression ------DA FINIRE-------

x <- model.matrix(rent_amount ~ ., data, subset = train)[, -1] 
y <- data$rent_amount

grid <- 10^seq(10, -2, length = 100)
ridge.mod <- glmnet(x, y, alpha = 0, lambda = grid)



#TREE Models

tree.fit <- tree(rent_amount ~ . , data = data.train)
summary(tree.fit)
par(mfrow = c (1,1))
plot(tree.fit)
text(tree.fit, pretty=0, cex = 0.7)

pred_tree.fit = predict(tree.fit, data.valid)
mean((pred_tree.fit - data.valid$rent_amount)^2)


#RANDOM FOREST

rf.fit <-  randomForest(rent_amount ~. , data = data.train, mtry = 5, importance = T, ntree=1e3 )
pred_rf <-  predict(rf.fit, data.valid)
mean((pred_rf - data.valid$rent_amount)^2)
plot(pred_rf, data.valid$rent_amount, col="#00000060", xlab="predicted", ylab="test")
abline(0,1, col = "red")





# clustering ------- Da finire
set.seed(1)
ClusteringData <- data %>%
  dplyr::select(where(is.numeric))

ClusteringData <- sample_n(ClusteringData, 1000)
dissEu <- get_dist(ClusteringData, method = "euclidean")
dissPe <- get_dist(ClusteringData, method = "pearson")

fviz_nbclust(ClusteringData, FUNcluster = hcut, diss = dissEu, method = "wss", k.max = 20)
fviz_nbclust(ClusteringData, FUNcluster = hcut, diss = dissEu, method = "silhouette", k.max = 20)

hierEu <- hcut(x = dissEu, isdiss = T, k = 2, hc_method = "ward.D2") # Ward.D2 is the most statistically valid method

fviz_dend(x=hierEu) + theme_bw()
fviz_cluster(object=hierEu, ClusteringData) + theme_bw()


fviz_nbclust(ClusteringData, FUNcluster = hcut, diss = dissPe, method = "wss", k.max = 20)
fviz_nbclust(ClusteringData, FUNcluster = hcut, diss = dissPe, method = "silhouette", k.max = 20)
hierPe <- hcut(x = dissPe, isdiss = T, k = 2, hc_method = "ward.D2") # Ward.D2 is the most statistically valid method

fviz_dend(x=hierPe) + theme_bw()
fviz_cluster(object=hierPe, ClusteringData) + theme_bw()

fviz_nbclust(ClusteringData, FUNcluster = kmeans, method = "wss")
fviz_nbclust(ClusteringData, FUNcluster = kmeans, method = "silhouette")


