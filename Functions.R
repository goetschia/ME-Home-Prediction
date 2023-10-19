Model_Quality <- function(model, data, y_variable, k){
  print("Hoslem Test for Goodness of Fit:")
  hoslem <- hoslem.test(model$y, fitted(model))
  cat("P-value:", hoslem$p.value, " - Good if bigger than 0.05!\n")
  yvec <-unlist(data[, colnames(data) == y_variable])
  
  cat("AIC: ", AIC(model), " \n")
  
  print("Confusion Matrix:")
  prediction <- predict(model, newdata = data, type = "response")
  conf_matr <- caret::confusionMatrix(as.factor(as.numeric(prediction > 0.5)), as.factor(yvec), positive = "1") 
  cat("\nAccuracy (correct TP & TN out of all):" , conf_matr$overall["Accuracy"])
  cat("\nSpecificty (correct TN in all negatives):", conf_matr$byClass["Specificity"]) 
  cat("\nPrecision (correct TP of all samples predicted to be  positive): ", conf_matr$byClass["Precision"] )
  cat("\nRecall (correct TP of all truly positive samples): ",conf_matr$byClass["Recall"]) 
  cat("\nF1 (harmonic mean of Recall & Precision):",conf_matr$byClass["F1"], "\n") 
  
  print("ROC:")
  roc <- roc(response = unlist(data[, colnames(data) == y_variable]), predictor = prediction)
  cat("\nArea under the curve: ", roc$auc, "\n0.7-0.8: acceptable; 0.8-0.9: excellent; > 0.9: outstanding\n")
  plot(roc)
  
  cat("Diagnostic plots:\n")
  cat("Predicted vs real values\n")
  data.frame(prediction, yvec) %>% ggplot(aes(x = prediction, y = yvec)) + geom_jitter() -> p1
  data.frame(prediction,  yvec) %>% ggplot(aes(x = prediction)) + geom_histogram() + facet_grid(~yvec) -> p2
  print(p1)
  print(p2)
  
  print("Cross-validation:")
  n <- nrow(data) #number of rows in data sat
  fold_size <- floor(n/k) #folds
  
  rsq_list <- rep(NA, k)
  rmse_list <- rep(NA, k )
  accuracy_list <- rep(NA, k)
  spec_list <- rep(NA, k)
  prec_list <- rep(NA, k)
  rec_list <- rep(NA, k)
  f1_list <- rep(NA, k)
  formula.cv <- formula(model)
  
  
  for (i in 1:k) {
    test_id <- ((i-1)*fold_size + 1):(i*fold_size)
    train_id <- setdiff(1:n, test_id)
    train_data <- data[train_id,]
    test_data <- data[test_id,]
    model.k <- glm(formula.cv, data = train_data, family = binomial)
    pred.k <- predict(model.k, newdata = test_data)
    rsq_list[i] <- cor(unlist(test_data[, colnames(test_data) == y_variable]), pred.k)^2
    rmse_list[i] <- sqrt(mean((unlist(test_data[, colnames(test_data) == y_variable]) - pred.k)^2))
    
    
    # conf_cv <- caret::confusionMatrix( as.factor(as.numeric(pred.k > 0.5)), as.factor(train_data[,colnames(train_data) == y_variable]) ,positive = "1")
    # accuracy_list[i] <- conf_cv$overall["Accuracy"]
    # spec_list[i] <- conf_cv$byClass["Specificity"]
    # prec_list[i] <- conf_cv$byClass["Precision"]
    # rec_list[i] <- conf_cv$byClass["Recall"]
    # f1_list[i] <- conf_cv$byClass["F1"]
  }
  
  cat("\nR-squared of cross-validation with ", k, "folds (fold size = ", n, "): ", mean(rsq_list))
  cat("\nRMSE of cross-validation with ", k, "folds (fold size = ", n, "): ", mean(rmse_list))
  # cat("\nMean Accuracy: ", mean(accuracy_list), " +/- ", sd(accuracy_list))
  # cat("\nMean Specificity: ", mean(spec_list), " +/- ", sd(spec_list))
  # cat("\nMean Precision: ", mean(prec_list), " +/- ", sd(prec_list))
  # cat("\nMean Recall: ", mean(rec_list), " +/- ", sd(rec_list))
  # cat("\nMean F1: ", mean(f1_list), " +/- ", sd(f1_list))
}

coef_abs <- function(model1, model2){
  coef1 <- model1$coefficients %>% as.data.frame() %>% rownames()
  coef2 <- model2$coefficients %>% as.data.frame() %>% rownames()
  both <- intersect(coef1, coef2)
  coef1.1 <- coef(model1)[coef1 %in% both]
  coef2.1 <- coef(model2)[coef2 %in% both]
  
  result <- abs((coef1.1 - coef2.1)/coef1.1)
  change <- result[result >=0.2]
  cat("Following parameters changed more than 20%: ", rownames(as.data.frame(change)), " ", change, ".")
}

p.value.max <- function(model){
  p.value <- coef(summary(model))[,4]
  para <- sort(p.value, TRUE)[1:3]
  cat("Three largest p-values:\n")
  print(para)
}

GetOR <- function(model){
  oddsratios_model <- exp(coef(model))
  ci_model <- exp(confint.default(model))
  df_ci_model <- data.frame(OR = oddsratios_model, LowerCI = ci_model[,1], UpperCI = ci_model[,2])
  df_ci_model <- round(df_ci_model, digits = 3)
  print(df_ci_model)
}

csv.write <- function(model){
  csv <- GetOR(model)
  write.csv(csv, file = "logOR.csv")
}

unadjusted_ORs <- function(model){
  variables <- all.vars(formula(model))[-1]
  unadj_OR <- numeric(length(variables))
  lower_CI <- numeric(length(variables))
  upper_CI <- numeric(length(variables))
  
  for (i in seq_along(variables)) {
    formula_single <- reformulate(variables[i], response = all.vars(formula(model))[1])
    model_unadj <- glm(formula_single, data = model$model, family = binomial)
    coef_unadj <- coef(model_unadj)[2]
    unadj_OR[i] <- exp(coef_unadj)
    
    ci_unadj <- confint.default(model_unadj)[2,]
    lower_CI[i] <- exp(ci_unadj[1])
    upper_CI[i] <- exp(ci_unadj[2])
  }
  results = data.frame(Variables = variables, 
                       unadjOR = unadj_OR,
                       lowerCI = lower_CI,
                       upperCI = upper_CI)
  formula.null <- paste(as.character(formula(model)[2]), "~1")
  null.model <- glm(formula.null, data = model$model, family = binomial)
  
  coef.null <- exp(coef(null.model))
  lower_CI.null <- exp(confint.default(null.model)[1])
  upper_CI.null <- exp(confint.default(null.model)[2])
  null.vect <- c(as.character(formula(model)[2]),coef.null, lower_CI.null, upper_CI.null)
  
  results <- rbind(results, null.vect)
  results[2:4] <- results %>% select(-Variables) %>% mutate_all(as.numeric)
  results[,2:4] <- round(results[,2:4], digits = 3)
  return(results)
}

csv.write.unadj <- function(model){
  csv <- unadjusted_ORs(model)
  write.csv(csv, file = "log_unadj_OR.csv")
}


compare_to_reference <- function(data, outcome, predictors) {
  data[[outcome]] <- factor(data[[outcome]])
  
  outcome_levels <- levels(data[[outcome]])
  
  models_list <- list()
  
  reference_level <- outcome_levels[1]
  
  for (i in 2:length(outcome_levels)) {
    level_of_interest <- outcome_levels[i]
    subset_data <- data[data[[outcome]] %in% c(reference_level, level_of_interest), ]
    
    model <- glm(formula = paste(outcome, "~", paste(predictors, collapse = " + ")),
                 data = subset_data,
                 family = binomial)
    
    models_list[[i-1]] <- model
  }
  
  return(models_list)
}
