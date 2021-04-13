require(dplyr)
require(rpart)

calc_mae_reduction <- function(y_hat, y_actual) {
  model_error <- mean(abs(y_hat - y_actual))
  baseline <- mean(y_actual, na.rm = TRUE)
  baseline_error <-  mean(abs(baseline - y_actual))
  result <- 1 - model_error/baseline_error
  # cat("MAE - baseline:", baseline_error, "\n")
  # cat("MAE - model:", model_error, "\n")
  # cat("MAE - before cleaning up:", result, "\n")
  result <- max(0.0, min(result, 1.0))
  round(100*result, 2)
}

calc_misclass_reduction <- function(y_hat, y_actual) {
  tab <- table(y_hat, y_actual)
  model_error <- 1 - sum(diag(tab))/sum(tab)
  majority_class <- names(which.max(table(y_actual)))
  baseline.preds <- rep(majority_class, length(y_actual))
  baseline_error <- mean(baseline.preds != y_actual)
  result <- 1 - model_error/baseline_error
  # cat("MISCLASS - baseline:", baseline_error, "\n")
  # cat("MISCLASS - model:", model_error, "\n")
  # cat("MISCLASS - before cleaning up:", result, "\n")
  result <- max(0.0, min(result, 1.0))
  round(100*result, 2)
}

x2y_inner <- function(x, y) {
  
  if (length(unique(x)) == 1 |
      length(unique(y)) == 1 ) {
    return(NA)
  } 
  # if y is continuous
  if (is.numeric(y)) {
    preds <- predict(rpart(y ~ x, method = "anova"), type = 'vector')
    calc_mae_reduction(preds, y)
  }
  # if y is categorical
  else {
    preds <- predict(rpart(y ~ x, method = "class"), type = 'class')
    calc_misclass_reduction(preds, y)
  }
}


simple_boot <- function(x,y) {
  ids <- sample(length(x), replace = TRUE)
  x2y_inner(x[ids], y[ids])
}

x2y <- function(x, y, confidence = FALSE) {
  results <- list()
  
  missing <-  is.na(x) | is.na(y)
  results$perc_of_obs <- round(100 * (1 - sum(missing) / length(x)), 2)
  
  x <- x[!missing]
  y <- y[!missing]
  
  results$x2y <- x2y_inner(x, y)
  
  if (confidence) {
    results$CI_95_Lower = NA
    results$CI_95_Upper = NA
    if (!is.na(results$x2y) & results$x2y > 0) {
      n <- length(x)
      draws <- replicate(1000, simple_boot(x, y))
      errors <- draws - results$x2y
      results$CI_95_Lower <- results$x2y - round(quantile(errors,
                                                          probs = 0.975,
                                                          na.rm = TRUE), 2)
      results$CI_95_Upper <- results$x2y - round(quantile(errors,
                                                          probs = 0.025,
                                                          na.rm = TRUE), 2)
    }
  }
  results
}

dx2y <- function(d,
                 target = NA,
                 confidence = FALSE) {
  if (is.na(target)) {
    pairs <- combn(ncol(d), 2)
    pairs <- cbind(pairs, pairs[2:1, ])
  }
  else {
    n <- 1:ncol(d)
    idx <- which(target == names(d))
    n <- n[n != idx]
    pairs <- cbind(rbind(n, idx), rbind(idx, n))
  }
  
  n <- dim(pairs)[2]
  
  results <- data.frame(x = names(d)[pairs[1,]],
                        y = names(d)[pairs[2,]],
                        perc_of_obs = rep(0.00, n),
                        x2y = rep(0.00, n),
                        CI_95_Lower = rep(NA, n),
                        CI_95_Upper = rep(NA, n))
  
  for (i in 1:n) {
    x <- d %>% pull(pairs[1, i])
    y <- d %>% pull(pairs[2, i])
    if (confidence) {
      results[i, 3:6] <- x2y(x, y, confidence = TRUE)
    }
    else {
      results[i, 3:4] <- x2y(x, y)
    }
  }
  
  if (!confidence) {
    results$CI_95_Lower <- NULL
    results$CI_95_Upper <- NULL
  }
  
  results <- results %>% arrange(desc(x2y), desc(perc_of_obs))
  
  results
}
