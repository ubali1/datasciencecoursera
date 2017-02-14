angiogenesis <- read.csv("Angiogenesis.csv", sep=",", na.strings = c("", "NA"), head = TRUE)
dim(angiogenesis)
str(angiogenesis)
colnames(angiogenesis)
colnames(angiogenesis) <- c("compound", "concentration", "cell_count", "fiber_count", "signal_count", "positive_fiber", "total_tube_length", 
                            "mean_tube_length", "total_tube_area", "mean_tube_area", "pct_tube_area", "tube_thickness", "segments", "branch_points",
                            "nodes", "total_node_area", "mean_node_area", "node_pct_area", "connected_sets", "tube_length_per_set")
colnames(angiogenesis)

library(dplyr)

by_cmp_conc <- group_by(angiogenesis, compound, concentration)

angiogenesis_summary <- summarize(by_cmp_conc, mean_cell_count = mean(cell_count, na.rm = TRUE), mean_fiber_count = mean(fiber_count, na.rm = TRUE), 
                        mean_signal_count = mean(signal_count, na.rm = TRUE), mean_positive_fiber = mean(positive_fiber, na.rm = TRUE), 
                        mean_total_tube_length = mean(total_tube_length, na.rm = TRUE), mean_mean_tube_length = mean(mean_tube_length, na.rm = TRUE), 
                        mean_total_tube_area = mean(total_tube_area, na.rm = TRUE), mean_mean_tube_area = mean(mean_tube_area, na.rm = TRUE), 
                        mean_pct_tube_area = mean(pct_tube_area, na.rm = TRUE), mean_tube_thickness = mean(tube_thickness, na.rm = TRUE),
                        mean_segments = mean(segments, na.rm = TRUE), mean_branch_points = mean(branch_points, na.rm = TRUE),mean_nodes = mean(nodes, na.rm = TRUE),
                        mean_total_node_area = mean(total_node_area, na.rm = TRUE), mean_mean_node_area = mean(mean_node_area, na.rm = TRUE),
                        mean_node_pct_area = mean(node_pct_area, na.rm = TRUE),mean_connected_sets = mean(connected_sets, na.rm = TRUE), mean_tube_length_per_set = mean(tube_length_per_set, na.rm = TRUE))


colnames(angiogenesis_summary)

library(ggplot2)

boxplot(cell_count ~ concentration, data = angiogenesis, main = "distribution of cell counts", xlab = "Concentration code", ylab = "cell count") + ylim(1000, 2000)
stripchart(cell_count ~ concentration, data = angiogenesis, vertical = TRUE, method = "jitter", add = TRUE, pch = 20, col = 'blue', alpha = 5)

qplot(concentration, cell_count, data = angiogenesis, geom = c("boxplot", "jitter"), facets = .~ concentration, color = concentration, main = "distribution of cell counts", xlab = "Concentration code", ylab = "cell count")

library(gridExtra)
library(grid)

g <- ggplot(angiogenesis, aes(concentration, cell_count))
g <- g + geom_point(position = position_jitter(w = 0.3, h = 0), size = 3, alpha = 0.2, color = "blue")
g <- g + facet_grid(.~ compound)
g <- g + xlab ("concentration") + ylab ("cell count") + ylim(1000, 2000)
g <- g + ggtitle ("distribution of cell counts (jitter plot)")

# qplot(concentration, mean_cell_count, data = angiogenesis_summary, xlab = "Compound concentration code", 
# ylab = "Mean cell count", color = concentration, size = mean_cell_count, facets = .~ compound) + ylim(1000, 2000)

g1 <- ggplot(angiogenesis_summary, aes(concentration, mean_cell_count))
g1 <- g1 + geom_point(position = position_jitter(w = 0.3, h = 0), size = 3, alpha = 0.2, color = "blue")
g1 <- g1 + facet_grid(.~ compound) + geom_smooth()
g1 <- g1 + xlab ("concentration") + ylab (" mean cell count") + ylim(1000, 2000)
g1 <- g1 + ggtitle ("distribution of mean cell counts")

grid.arrange(g, g1, ncol = 2)

# identify predictors that contain greater than 20% zero values
zero_values <- sapply(angiogenesis, function(x) mean(x == 0)) > 0.2
zero_values
# we should remove nodes, total_node_area, mean_node_area and node_pct_area as predictors containing zero values

# let's calculate the variance manually using the data set grouped by concentration and compound
by_cmp_conc_df <- cbind(round(sapply(by_cmp_conc[,-1], var), digits = 2), round(sapply(by_cmp_conc[,-1], mean), digits = 2))
by_cmp_conc_df <- cbind(by_cmp_conc_df, "var as % of mean" = round((by_cmp_conc_df[,1]/by_cmp_conc_df[,2])*100))
colnames(by_cmp_conc_df) <- c("variance", "mean", "var as % of mean")
by_cmp_conc_df 

# and identify predictors that have an overall variance less than 100
low_variance <- sapply(angiogenesis, function(x) var(x)) < 100
low_variance 

# we can see that "mean tube length", "pct_tube_area", tube_thickness", "nodes", "node_pct_area", and "connected_sets" have been correctly identified
# as predictors with lowest overall variance in the angiogenesis data set and should, therefore, be removed from the final analysis

# what do these look like?


A_plot <- qplot(concentration, mean_tube_length, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "mean_tube_length", color = concentration, size = mean_tube_length, facets = .~ compound)
B_plot <- qplot(concentration, pct_tube_area, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "pct_tube_area", color = concentration, size = pct_tube_area, facets = .~ compound)
C_plot <- qplot(concentration, tube_thickness, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "tube_thickness", color = concentration, size = tube_thickness, facets = .~ compound)
D_plot <- qplot(concentration, nodes, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "node", color = concentration, size = nodes, facets = .~ compound)
E_plot <- qplot(concentration, node_pct_area, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "node_pct_area", color = concentration, size = node_pct_area, facets = .~ compound)
F_plot <- qplot(concentration, connected_sets, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "connected_sets", color = concentration, size = connected_sets, facets = .~ compound)

suppressWarnings(grid.arrange(A_plot, B_plot, C_plot, D_plot, E_plot, F_plot, nrow = 3, ncol = 2), heights=c(4,1), widths=c(2,1))

# What do the predictors with the highest variance look like?

G_plot <- qplot(concentration, total_tube_length, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "total_tube_length", color = concentration, size = total_tube_length, facets = .~ compound)
H_plot <- qplot(concentration, total_tube_area, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "total_tube_area", color = concentration, size = total_tube_area, facets = .~ compound)
I_plot <- qplot(concentration, segments, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "segments", color = concentration, size = segments, facets = .~ compound)
J_plot <- qplot(concentration, branch_points, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "branch_points", color = concentration, size = branch_points, facets = .~ compound)
K_plot <- qplot(concentration, total_node_area, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "total_node_area", color = concentration, size = total_node_area, facets = .~ compound)
L_plot <- qplot(concentration, mean_node_area, data = angiogenesis, geom = "smooth", xlab = "Compound concentration code", ylab = "mean_node_area", color = concentration, size = mean_node_area, facets = .~ compound)

grid.arrange(G_plot, H_plot, I_plot, J_plot, K_plot, L_plot, nrow = 3, ncol = 2)

library(caret)

# find columns that are correlated greater than 90%
correlation_index <- findCorrelation(abs(cor(angiogenesis[,-1])), cutoff = 0.90)
correlation_names <- names(angiogenesis)[correlation_index]
correlation_names

angiogenesis_correlation <- angiogenesis[,correlation_names]
pairs(angiogenesis_correlation, main = "Simple Scatterplot Matrix")
# let's look at the correlation coefficients
cor(angiogenesis_correlation)

nocorrelation_names <- names(angiogenesis)[-correlation_index]
# names of predictors that are not correlated
nocorrelation_names

# remove the first, second and the node and the node % area predictors
nocorrelation_names <- nocorrelation_names[-c(1,2, 9, 11)]
nocorrelation_names

angiogenesis_nocorrelation <- angiogenesis[,nocorrelation_names]
pairs(angiogenesis_nocorrelation, main = "Simple Scatterplot Matrix of non correlated predictors")
# the correlation coefficients for the non highly correlated predictors
cor(angiogenesis_nocorrelation)

findCorrelation(abs(cor(angiogenesis_nocorrelation)), cutoff = 0.90)
colnames(angiogenesis_nocorrelation)[c(1,2,3,4,5)]


# Let's create a new data frame containing our refined list of predictors which include total_tube_length, total_tube_area, segments, branch_points

angiogenesis_final <- subset(angiogenesis)[,c(1,2,7,9,13,14)]

angiogenesis_final_grouped <- group_by(angiogenesis_final, compound, concentration)

angiogenesis_final_grouped_summary <- summarize(angiogenesis_final_grouped, mean_total_tube_length = mean(total_tube_length, na.rm = TRUE), 
                                                mean_total_tube_area = mean(total_tube_area, na.rm = TRUE), 
                                                mean_segments = mean(segments, na.rm = TRUE), 
                                                mean_branch_points = mean(branch_points, na.rm = TRUE))

# let's visualize these predictors

M_plot <- qplot(concentration, mean_total_tube_length, data = angiogenesis_final_grouped_summary, geom = c("point", "smooth"), xlab = "Compound concentration code", ylab = "mean_total_tube_length", color = concentration, size = mean_total_tube_length, facets = .~ compound)
N_plot <- qplot(concentration, mean_total_tube_area, data = angiogenesis_final_grouped_summary, geom = c("point", "smooth"), xlab = "Compound concentration code", ylab = "mean_total_tube_area", color = concentration, size = mean_total_tube_area, facets = .~ compound)
O_plot <- qplot(concentration, mean_segments, data = angiogenesis_final_grouped_summary, geom = c("point", "smooth"), xlab = "Compound concentration code", ylab = "mean_segments", color = concentration, size = mean_segments, facets = .~ compound)
P_plot <- qplot(concentration, mean_branch_points, data = angiogenesis_final_grouped_summary, geom = c("point", "smooth"), xlab = "Compound concentration code", ylab = "mean_branch_points", color = concentration, size = mean_branch_points, facets = .~ compound)

grid.arrange(M_plot, N_plot, O_plot, P_plot, nrow = 2, ncol = 2)

# it is apparent that ACT01, ACT03 and ACT05 give a steady increase in predictor response over concentrattion, whereas ACT02 and ACT04 do not.

# Let's create a data partition using the preprocessed data frame "angiogenesis_final"
set.seed(123)
train_partition <- createDataPartition(y = angiogenesis_final$concentration, p = 0.8, list = FALSE)
train <- angiogenesis_final[train_partition, ]
test <- angiogenesis_final[-train_partition, ]


library(rpart)
cross_validation <- trainControl(method = "cv", number=3, repeats=3)

cart_model <- train(concentration ~ ., data = train, trControl = cross_validation, method = 'rpart')
library(rpart.plot)
library(rattle)
fancyRpartPlot(cart_model$finalModel,cex=.5,under.cex=1,shadow.offset=0)
varImp(cart_model)

library(caret)

# prediction of test data using 'cart_model'
CART_prediction <- predict(cart_model, newdata = test)
CART_confusion_matrix <- confusionMatrix(CART_prediction, test$concentration)
CART_confusion_matrix$overall[1]

# prediction of test data using 'gbm_model'
gbm_model <- train(compound ~ ., data = train, trControl = cross_validation, method='gbm')
summary(gbm_model)
gbm_prediction <- predict(gbm_model, newdata = test)
gbm_confusion_matrix <- confusionMatrix(gbm_prediction, test$concentration)
gbm_confusion_matrix$overall[1]

# prediction of test data using 'lm'
lm_model <- train(concentration ~., data = train, trControl = cross_validation, method = 'lm')
summary(lm_model)
lm_prediction <- predict(lm_model, newdata = test)
lm_prediction_values <- as.integer(lm_prediction)
lm_confusion_matrix <- confusionMatrix(test$concentration, lm_prediction_values)

rbind(test$concentration, lm_prediction_values)

# we can see that the linear model predictions 10 out of 16 answers accurately - an accuracy of-
lm_confusion_matrix$overall[1]
cbind(lm_model$results[2], lm_model$results[3], lm_model$results[4], lm_model$results[5])

par(mfrow = c(2, 2))
plot(lm_model$finalModel,pch=19,cex=0.5,col="#00000010")


# Let's try PCA
pairs(train[,-c(1,2)])
cbind(mean = sapply(train[,-c(1,2)], mean), sd = sapply(train[,-c(1,2)], sd))
#we can see that the standard deviation varies quite a bit between the four variables by as much as 40-fold. Running a PCA on this data set would
# inevitably identify the variable with the highest sd as the first principal component. To remedy this, we shall scale the data set 
# (center around mean and normalize to sd) first as follows:

train_scaled <- as.data.frame(scale(train[3:6]))
cbind(mean = sapply(train_scaled, mean), sd = sapply(train_scaled, sd))

# we can see that the mean of the variables is near zero and the sd is all 1.

train_scaled <- mutate(train, scale_total_tube_length = scale(train[3]), scale_total_tube_area = scale(train[4]), scale_segments = scale(train[5]), scale_branch_points = scale(train[6]))
train_scaled <- select(train_scaled, concentration, scale_total_tube_length, scale_total_tube_area, scale_segments, scale_branch_points)

# Let's apply the same transformation to the test data set as follows:
test_scaled <- mutate(test, scale_total_tube_length = scale(test[3]), scale_total_tube_area = scale(test[4]), scale_segments = scale(test[5]), scale_branch_points = scale(test[6]))
test_scaled <- select(test_scaled, concentration, scale_total_tube_length, scale_total_tube_area, scale_segments, scale_branch_points)

train_pca <- prcomp(train_scaled[,-1])
train_pca_data <- cbind(train_scaled[,1], train_pca$x[,1], train_pca$x[,2],train_pca$x[,3],train_pca$x[,4])
colnames(train_pca_data) <- c("concentration", "PC1", "PC2", "PC3", "PC4")


test_pca <- prcomp(test_scaled[,-1])
test_pca_data <- cbind(test_scaled[,1], test_pca$x[,1], test_pca$x[,2],test_pca$x[,3],test_pca$x[,4])
colnames(test_pca_data) <- c("concentration", "PC1", "PC2", "PC3", "PC4")
  
training_pca <- train(train_pca_data[,1] ~., method = "lm", data = train_pca_data)
screeplot(prcomp(train_scaled), type = "lines")
prcomp(train_scaled)$sdev
sum(prcomp(train_scaled)$sdev)^2
# print results of model
confusionMatrix(as.integer(predict(train_pca,test_scaled)), test_scaled$concentration)
