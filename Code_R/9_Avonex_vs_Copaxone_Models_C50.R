
# Remove doc_id
avocop_model <- read.csv('Avo_Cop_partial_tidy.csv')
avocop_model <- avocop_model[,-1]
dim(avocop_model)

# Splitting Dataset
# Train vs Test
require(caret)
set.seed(123)
trainIndex_caret_1 <- createDataPartition(y = avocop_model$y_drug_name, p = 0.8, list = F)
avocop_train_1 <- avocop_model[trainIndex_caret_1,]
avocop_train_1$y_drug_name <- as.factor(avocop_train_1$y_drug_name)
avocop_test_1 <- avocop_model[-trainIndex_caret_1, -69]
avocop_test_y_1 <- avocop_model[-trainIndex_caret_1, 69]
avocop_test_y_1 <- as.factor(avocop_test_y_1)

# Numeric vs Categorical
# Just in Case !
avocop_classes <- avocop_model %>% sapply(FUN = class)
avocop_numeric <- names(avocop_classes)[avocop_classes == 'numeric' | avocop_classes == 'integer']
avocop_categorical <- names(avocop_classes)[avocop_classes == 'character' | avocop_classes == 'factor']
avocop_numeric_df <- avocop_model[,avocop_numeric]
avocop_categorical_df <- avocop_model[,avocop_categorical]

# Decision Trees

# C5.0
# Factors are not converted to dummy vars !
# Can handle NAs !
# Rules can be generated !
# Default trials for boosting = 1
require(C50)
# Caret + C5.0
#
set.seed(123)
control <- trainControl(method="cv", number=10, verboseIter = T)
gridSearch <- expand.grid(.trials = seq(5,100,5),
                          .model = c("tree", "rules"),
                          .winnow = c(TRUE, FALSE))
caret_c50_1 <- train(x = avocop_train_1[,-69], y = avocop_train_1[,69], method = 'C5.0',
                    trControl = control, tuneGrid = gridSearch, metric = "Accuracy")
summary(caret_c50_1)
caret_c50_1_final_model_1 <- caret_c50_1$finalModel
# trials model winnow
# tree   FALSE    80     0.9480972  0.9150182
# Plot grid
plot(caret_c50_1, metric = 'Accuracy')
# Variable Importance
var_imp_caret_c50_1_1 <- C5imp(caret_c50_1_final_model_1)
var_imp_caret_c50_1_1$variable_name <- rownames(var_imp_caret_c50_1_1)
var_imp_caret_c50_1_1 <- arrange(var_imp_caret_c50_1_1, Overall)
var_imp_caret_c50_1_1$variable_name <- factor(var_imp_caret_c50_1_1$variable_name, levels = var_imp_caret_c50_1_1$variable_name)
levels(var_imp_caret_c50_1_1$variable_name)
ggplot(data = var_imp_caret_c50_1_1, aes(x = variable_name, y = Overall)) +
  geom_bar(stat = 'identity') + coord_flip()
# Prediction
#
prediction_caret_c50_1_final_model_1 <- predict(caret_c50_1_final_model_1, avocop_test_1)
(confMat_caret_c50_1 <- confusionMatrix(table(prediction_caret_c50_1_final_model_1, avocop_test_y_1))) # (0.9262, 0.9624)
# C5.0 + cost matrix
#
cost_matrix_c50 <- matrix(c(0,3,10,3,0,3,10,3,0), ncol = 3, byrow = T) # arbitrary
rownames(cost_matrix_c50) <- c('Avonex', 'Avonex / Copaxone', 'Copaxone')
colnames(cost_matrix_c50) <- c('Avonex', 'Avonex / Copaxone', 'Copaxone')
c50_cost_sensitive_1 <- C5.0(x = avocop_train_1[,-69], y = avocop_train_1[,69], costs = cost_matrix_c50,
                             trials = 80, rules = F, control = C5.0Control(winnow = FALSE)) # values from caret based model
# Prediction
#
predict_c50_cost_sensitive_1 <- predict(c50_cost_sensitive_1, avocop_test_1)
(confMat_c50_cost_sensitive_1 <- confusionMatrix(table(predict_c50_cost_sensitive_1, avocop_test_y_1))) # cost ineffective !! # same as above !
