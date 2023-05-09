model_result
================
Yi Huang
2023-05-09

## Load data, data partition

``` r
# load data
load("data/covid_recovery.Rdata")

#Split data into 70-300, using the third member's uni
set.seed(2337)
indexTrain <- createDataPartition(y = data$recovery_time, p = 0.7, list = FALSE)

# training data
train_data <- data[indexTrain,]
# matrix of predictors
train_x <- model.matrix(recovery_time~.,train_data)[,-1]
# vector of response
train_y <- train_data$recovery_time

# test data
test_data <- data[-indexTrain,]
# matrix of predictors
test_x <- model.matrix(recovery_time~.,test_data)[,-1]
# vector of response
test_y <- test_data$recovery_time

# 10-fold cv
ctrl <- trainControl(method = "cv",
                     number = 10)
```

## Exploratory Data Analysis on train data

Here is the exploratory analysis and data visualization from train data
set. we have 15 variables in total, 8 factors and 7 numeri from 1600
participants. Base on the underlay true of some variables, I had convert
character variable gender, race, smoking, hypertension, diabetes,
vaccine, severity and study into factors to add them into our model and
proceed with the analysis. Because id cannot be regarded as a predictor,
I removed it for analysis. Then we have 14 predictors for consideration
and recovery time is our outcome. Recovery time has a mean of 43.17
days, but some patients have very long recovery time.

``` r
#continuous
ggplot(train_data, aes(x = age))+geom_density()+labs(title="Distribution for age")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-1.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = height))+geom_density()+labs(title="Distribution for height")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-2.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = weight))+geom_density()+labs(title="Distribution for weight")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-3.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = bmi))+geom_density()+labs(title="Distribution for bmi")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-4.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = SBP))+geom_density()+labs(title="Distribution for SBP")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-5.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = LDL))+geom_density()+labs(title="Distribution for LDL")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-6.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = recovery_time))+geom_density()+labs(title="Distribution for recovery_time")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-7.png" width="90%" style="display: block; margin: auto;" />

``` r
#discrete
ggplot(train_data) + geom_bar(aes(x=gender))+labs(title="Bar plot for different gender")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-8.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data) + geom_bar(aes(x=race))+labs(title="Bar plot for different race")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-9.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data) + geom_bar(aes(x=smoking))+labs(title="Bar plot for different smoking")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-10.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data) + geom_bar(aes(x=hypertension))+labs(title="Bar plot for different hypertension")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-11.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data) + geom_bar(aes(x=diabetes))+labs(title="Bar plot for different diabetes")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-12.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data) + geom_bar(aes(x=vaccine))+labs(title="Bar plot for different vaccine")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-13.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data) + geom_bar(aes(x=severity))+labs(title="Bar plot for different severity")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-14.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data) + geom_bar(aes(x=study))+labs(title="Bar plot for different study")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-15.png" width="90%" style="display: block; margin: auto;" />

``` r
num_df <- 
  train_data %>% 
  dplyr::select(age, height, weight, bmi, SBP, LDL, recovery_time) 


# calulate the correlations
res <- cor(num_df, use="complete.obs")

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-3-16.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = age, y = recovery_time))+geom_point()+geom_smooth(method = 'gam', se = TRUE, color = 'red')+labs(title="Recovery time against age")
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

<img src="model_result_files/figure-gfm/unnamed-chunk-4-1.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = height, y = recovery_time))+geom_point()+geom_smooth(method = 'gam', se = TRUE, color = 'red')+labs(title="Recovery time against height")
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

<img src="model_result_files/figure-gfm/unnamed-chunk-4-2.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = weight, y = recovery_time))+geom_point()+geom_smooth(method = 'gam', se = TRUE, color = 'red')+labs(title="Recovery time against weight")
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

<img src="model_result_files/figure-gfm/unnamed-chunk-4-3.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = bmi, y = recovery_time))+geom_point()+geom_smooth(method = 'gam', se = TRUE, color = 'red')+labs(title="Recovery time against bmi")
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

<img src="model_result_files/figure-gfm/unnamed-chunk-4-4.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = SBP, y = recovery_time))+geom_point()+geom_smooth(method = 'gam', se = TRUE, color = 'red')+labs(title="Recovery time against SBP")
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

<img src="model_result_files/figure-gfm/unnamed-chunk-4-5.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(train_data, aes(x = LDL, y = recovery_time))+geom_point()+geom_smooth(method = 'gam', se = TRUE, color = 'red')+labs(title="Recovery time against LDL")
```

    ## `geom_smooth()` using formula = 'y ~ s(x, bs = "cs")'

<img src="model_result_files/figure-gfm/unnamed-chunk-4-6.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(data, aes(x = as.factor(gender), y = recovery_time))+geom_boxplot()+labs(title="Box plot for Recovery time with different gender")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-5-1.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(data, aes(x = as.factor(race), y = recovery_time))+geom_boxplot()+labs(title="Box plot for Recovery time with different race")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-5-2.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(data, aes(x = as.factor(smoking), y = recovery_time))+geom_boxplot()+labs(title="Box plot for Recovery time with different smoking")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-5-3.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(data, aes(x = as.factor(hypertension), y = recovery_time))+geom_boxplot()+labs(title="Box plot for Recovery time with different hypertension")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-5-4.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(data, aes(x = as.factor(diabetes), y = recovery_time))+geom_boxplot()+labs(title="Box plot for Recovery time with different diabetes")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-5-5.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(data, aes(x = as.factor(vaccine), y = recovery_time))+geom_boxplot()+labs(title="Box plot for Recovery time with different vaccine")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-5-6.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(data, aes(x = as.factor(severity), y = recovery_time))+geom_boxplot()+labs(title="Box plot for Recovery time with different severity")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-5-7.png" width="90%" style="display: block; margin: auto;" />

``` r
ggplot(data, aes(x = as.factor(study), y = recovery_time))+geom_boxplot()+labs(title="Box plot for Recovery time with different study")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-5-8.png" width="90%" style="display: block; margin: auto;" />

## Regression

## primary analysis (Regression)

- **Linear regression**
- **K-Nearest Neighbors (KNN)**
- **Elastic net**
- **Partial least squares (PLS)**
- **Generalized Additive Model (GAM)**
- **Multivariate Adaptive Regression Splines (MARS)**
- **Boosting**
- **Random forest**

### Linear regression

``` r
## fit linear model on train data
set.seed(2337)
linear_model <- train(train_x,
                      train_y,
                      method = "lm", 
                      trControl = ctrl)
summary(linear_model)
```

    ## 
    ## Call:
    ## lm(formula = .outcome ~ ., data = dat)
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -73.848 -13.472  -1.414   9.399 257.759 
    ## 
    ## Coefficients:
    ##                 Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)   -2.743e+03  1.295e+02 -21.171  < 2e-16 ***
    ## age           -1.353e-02  1.177e-01  -0.115 0.908505    
    ## gender1       -4.538e+00  9.432e-01  -4.812 1.59e-06 ***
    ## race2          5.460e-01  2.166e+00   0.252 0.800997    
    ## race3         -1.639e+00  1.193e+00  -1.374 0.169550    
    ## race4         -9.671e-01  1.653e+00  -0.585 0.558518    
    ## smoking1       3.052e+00  1.066e+00   2.863 0.004236 ** 
    ## smoking2       7.843e+00  1.563e+00   5.018 5.58e-07 ***
    ## height         1.608e+01  7.612e-01  21.120  < 2e-16 ***
    ## weight        -1.733e+01  8.043e-01 -21.541  < 2e-16 ***
    ## bmi            5.195e+01  2.300e+00  22.587  < 2e-16 ***
    ## hypertension1  3.229e+00  1.560e+00   2.070 0.038547 *  
    ## diabetes1     -2.210e-01  1.277e+00  -0.173 0.862608    
    ## SBP            3.232e-04  1.012e-01   0.003 0.997453    
    ## LDL           -1.013e-02  2.499e-02  -0.405 0.685287    
    ## vaccine1      -8.167e+00  9.684e-01  -8.434  < 2e-16 ***
    ## severity1      5.717e+00  1.534e+00   3.726 0.000199 ***
    ## studyB         4.315e+00  1.215e+00   3.552 0.000389 ***
    ## studyC        -3.479e-01  1.500e+00  -0.232 0.816566    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 23.58 on 2498 degrees of freedom
    ## Multiple R-squared:  0.2754, Adjusted R-squared:  0.2702 
    ## F-statistic: 52.75 on 18 and 2498 DF,  p-value: < 2.2e-16

``` r
# view performance on the test set (RMSE)
lm_test_pred <- predict(linear_model, newdata = test_x) # test dataset
lm_test_rmse <- sqrt(mean((lm_test_pred - test_y)^2))
sprintf("test error for lm is: %.3f",lm_test_rmse)
```

    ## [1] "test error for lm is: 23.569"

### K-Nearest Neighbors (KNN)

``` r
# fit knn on train data use caret
kGrid <- expand.grid(k = seq(1, to = 40, by = 1))
knn_model <- train(train_x,
                   train_y,
                   method = "knn",
                   trControl = ctrl,
                   tuneGrid = kGrid)
ggplot(knn_model, highlight = TRUE)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-7-1.png" width="90%" style="display: block; margin: auto;" />

``` r
# knn with K = 18 was selected as the final model

# view performance on the test set (RMSE)
knn_test_pred <- predict(knn_model, newdata = test_x) # test dataset
knn_test_rmse <- sqrt(mean((knn_test_pred - test_y)^2))
sprintf("test error for K-Nearest Neighbors is: %.3f", knn_test_rmse)
```

    ## [1] "test error for K-Nearest Neighbors is: 25.277"

### Enet

``` r
#tuning glmnet alpha 0 and 0.05 have large RMSE. So alpha start from 0.1
set.seed(2337)
enet.fit.min <- train(train_x, train_y,
                  method = "glmnet",
                  tuneGrid = expand.grid(alpha = seq(0.1, 1, length = 21), 
                                         lambda = exp(seq(-10, -5, length = 100))),
                  trControl = ctrl)
enet.fit.min$bestTune
```

    ##     alpha      lambda
    ## 100   0.1 0.006737947

``` r
plot(enet.fit.min)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-8-1.png" width="90%" style="display: block; margin: auto;" />

``` r
# view performance on the test set (RMSE)
enet_test_pred <- predict(enet.fit.min, newdata = test_x) # test dataset
enet_test_rmse <- sqrt(mean((enet_test_pred - test_y)^2))
sprintf("test error for enet is: %.3f",enet_test_rmse)
```

    ## [1] "test error for enet is: 23.498"

### PLS

``` r
set.seed(2337)
pls_model <- train(train_x,
                   train_y,
                   method = "pls",
                   tuneGrid = data.frame(ncomp = 1:16),
                   trControl = ctrl,
                   preProcess = c("center", "scale"))
ggplot(pls_model, highlight = TRUE)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-9-1.png" width="90%" style="display: block; margin: auto;" />

``` r
ggsave(file = "image/pls_number_of_component.png", width = 10, height = 7)

# view performance on the test set (RMSE)
pls_test_pred <- predict(pls_model, newdata = test_x) # test dataset
pls_test_rmse <- sqrt(mean((pls_test_pred - test_y)^2))
sprintf("test error for pls is: %.3f",pls_test_rmse)
```

    ## [1] "test error for pls is: 23.567"

### Generalised additive regression (GAM)

``` r
# gam use caret
# fit GAM model using all predictors
# parallel computing
no_cores <- detectCores() - 1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
gam_model <- train(train_x, train_y, # training dataset
                   method = "gam",
                   trControl = ctrl,
                   control = gam.control(maxit = 200)) 
stopCluster(cl)
registerDoSEQ()

gam_model$bestTune
```

    ##   select method
    ## 2   TRUE GCV.Cp

``` r
gam_model$finalModel
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## .outcome ~ gender1 + race2 + race3 + race4 + smoking1 + smoking2 + 
    ##     hypertension1 + diabetes1 + vaccine1 + severity1 + studyB + 
    ##     studyC + s(age) + s(SBP) + s(LDL) + s(bmi) + s(height) + 
    ##     s(weight)
    ## 
    ## Estimated degrees of freedom:
    ## 0.00 0.00 0.00 7.78 2.65 4.50  total = 27.93 
    ## 
    ## GCV score: 448.2538

``` r
plot(gam_model)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-10-1.png" width="90%" style="display: block; margin: auto;" />

``` r
summary(gam_model$finalModel)
```

    ## 
    ## Family: gaussian 
    ## Link function: identity 
    ## 
    ## Formula:
    ## .outcome ~ gender1 + race2 + race3 + race4 + smoking1 + smoking2 + 
    ##     hypertension1 + diabetes1 + vaccine1 + severity1 + studyB + 
    ##     studyC + s(age) + s(SBP) + s(LDL) + s(bmi) + s(height) + 
    ##     s(weight)
    ## 
    ## Parametric coefficients:
    ##               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)    43.7183     1.3232  33.040  < 2e-16 ***
    ## gender1        -4.6981     0.8434  -5.570 2.82e-08 ***
    ## race2           0.5405     1.9332   0.280 0.779829    
    ## race3          -1.5605     1.0659  -1.464 0.143332    
    ## race4          -2.6252     1.4791  -1.775 0.076054 .  
    ## smoking1        4.3355     0.9556   4.537 5.98e-06 ***
    ## smoking2        8.0325     1.3975   5.748 1.01e-08 ***
    ## hypertension1   3.0143     0.8471   3.558 0.000380 ***
    ## diabetes1       0.6857     1.1425   0.600 0.548453    
    ## vaccine1       -7.7120     0.8649  -8.916  < 2e-16 ***
    ## severity1       5.9722     1.3716   4.354 1.39e-05 ***
    ## studyB          4.1736     1.0846   3.848 0.000122 ***
    ## studyC         -0.6406     1.3400  -0.478 0.632678    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Approximate significance of smooth terms:
    ##                 edf Ref.df       F p-value    
    ## s(age)    1.123e-06      9   0.000 0.55847    
    ## s(SBP)    8.397e-07      9   0.000 0.68712    
    ## s(LDL)    1.176e-06      9   0.000 0.51661    
    ## s(bmi)    7.775e+00      9 111.700 < 2e-16 ***
    ## s(height) 2.649e+00      9   1.019 0.00269 ** 
    ## s(weight) 4.501e+00      9   1.049 0.04744 *  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## R-sq.(adj) =  0.418   Deviance explained = 42.5%
    ## GCV = 448.25  Scale est. = 443.28    n = 2517

``` r
# view performance on the test set (RMSE)
gam_test_pred <- predict(gam_model, newdata = test_x) # test dataset
gam_test_rmse <- sqrt(mean((gam_test_pred - test_y)^2))
sprintf("test error for Generalised additive regression is: %.3f", knn_test_rmse)
```

    ## [1] "test error for Generalised additive regression is: 25.277"

### Multivariate adaptive regression

``` r
set.seed(2337)

# create dummy variables for categorical variables
df_dummies <- data.frame(model.matrix(~ . - 1, 
                                      # exclude ID and continuous variables
                                      data = data[, c("gender", "race", "smoking", "hypertension", "diabetes",
                                                     "vaccine", "severity", "study")]), 
                         # add continuous variables back to the data frame
                         age = data$age,
                         height = data$height,
                         weight = data$weight,
                         bmi = data$bmi,
                         SBP = data$SBP,
                         LDL = data$LDL,
                         recovery_time = data$recovery_time) 

# rename df_dummies dataset as dat
dat_mars <- df_dummies

# training data
dat_train_mars <- dat_mars[indexTrain, ]
x_mars <- model.matrix(recovery_time~.,dat_mars)[indexTrain,-1]
y_mars <- dat_mars$recovery_time[indexTrain]

# test data
dat_test_mars <- dat_mars[-indexTrain, ]
x2_mars <- model.matrix(recovery_time~.,dat_mars)[-indexTrain,-1]
y2_mars <- dat_mars$recovery_time[-indexTrain]


# create grid of all possible pairs that can take degree and nprune values
mars_grid <- expand.grid(degree = 1:3, # number of possible product hinge functions in 1 term
                         nprune = 2:20) # upper bound of number of terms in model
# parallel computing
no_cores <- detectCores() - 1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)

mars_model <- train(x_mars, y_mars, # training dataset
                  method = "earth",
                  tuneGrid = mars_grid,
                  trControl = ctrl)

stopCluster(cl)
registerDoSEQ()

ggplot(mars_model)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-11-1.png" width="90%" style="display: block; margin: auto;" />

``` r
print(plot(mars_model))
```

<img src="model_result_files/figure-gfm/unnamed-chunk-11-2.png" width="90%" style="display: block; margin: auto;" />

``` r
summary(mars_model$finalModel)
```

    ## Call: earth(x=matrix[2517,19], y=c(14,36,50,65,3...), keepxy=TRUE, degree=2,
    ##             nprune=5)
    ## 
    ##                      coefficients
    ## (Intercept)             -1.264634
    ## vaccine1                -7.897168
    ## h(bmi-25.9)              8.611315
    ## h(32.6-bmi)              5.699561
    ## studyB * h(bmi-32.6)    41.724058
    ## 
    ## Selected 5 of 24 terms, and 3 of 19 predictors (nprune=5)
    ## Termination condition: Reached nk 39
    ## Importance: studyB, bmi, vaccine1, gender0-unused, gender1-unused, ...
    ## Number of terms at each degree of interaction: 1 3 1
    ## GCV 413.5903    RSS 1031928    GRSq 0.4574914    RSq 0.4617953

``` r
# view performance on the test set (RMSE)
mars_test_pred <- predict(mars_model, newdata = x2_mars) # test dataset
mars_test_rmse <- sqrt(mean((mars_test_pred - test_y)^2))
sprintf("test error for MARS is: %.3f", mars_test_rmse)
```

    ## [1] "test error for MARS is: 22.586"

### Random Forest - Regression

``` r
set.seed(2337)
rf.grid <- expand.grid(mtry = 1:14,
                       splitrule = "variance",
                       min.node.size = seq(from = 1, to = 50, by = 5))

# parallel computing
no_cores <- detectCores() - 1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)

set.seed(2337)
rf_model <- train(recovery_time ~ . , 
                  train_data, 
                  method = "ranger",
                  tuneGrid = rf.grid,
                  trControl = ctrl)

stopCluster(cl)
registerDoSEQ()
ggplot(rf_model, highlight = TRUE)
```

    ## Warning: The shape palette can deal with a maximum of 6 discrete values because
    ## more than 6 becomes difficult to discriminate; you have 10. Consider
    ## specifying shapes manually if you must have them.

    ## Warning: Removed 56 rows containing missing values (`geom_point()`).

<img src="model_result_files/figure-gfm/unnamed-chunk-12-1.png" width="90%" style="display: block; margin: auto;" />

``` r
rf_model$bestTune
```

    ##     mtry splitrule min.node.size
    ## 102   11  variance             6

``` r
# variable importance
rf.final.per <- ranger(recovery_time ~ . , 
                       train_data,
                       mtry = rf_model$bestTune[[1]], 
                       splitrule = "variance",
                       min.node.size = rf_model$bestTune[[3]],
                       importance = "permutation",
                       scale.permutation.importance = TRUE) 

barplot(sort(ranger::importance(rf.final.per), decreasing = FALSE), 
        las = 2, horiz = TRUE, cex.names = 0.7,
        col = colorRampPalette(colors = c("cyan","blue"))(19))
```

<img src="model_result_files/figure-gfm/unnamed-chunk-12-2.png" width="90%" style="display: block; margin: auto;" />

``` r
# view performance on the test set (RMSE)
rf_test_pred <- predict(rf_model, newdata = test_data) # test dataset
rf_test_rmse <- sqrt(mean((rf_test_pred - test_y)^2))
sprintf("test error for Random Forest is: %.3f", rf_test_rmse)
```

    ## [1] "test error for Random Forest is: 21.919"

### Boosting

``` r
gbm.grid <- expand.grid(n.trees = c(500,1000,2000,3000,4000,5000),
                        interaction.depth = 1:5,
                        shrinkage = c(0.005,0.01),
                        n.minobsinnode = c(1))
no_cores <- detectCores() - 1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
set.seed(2337)
gbm.fit = train(recovery_time~. ,
                  train_data,
                  tuneGrid = gbm.grid,
                  trControl = ctrl,
                  method = "gbm",
                  verbose = FALSE)
stopCluster(cl)
registerDoSEQ()

ggplot(gbm.fit, highlight = TRUE)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-13-1.png" width="90%" style="display: block; margin: auto;" />

``` r
gbm.fit$bestTune
```

    ##    n.trees interaction.depth shrinkage n.minobsinnode
    ## 15    2000                 3     0.005              1

``` r
# test error
gbm.fit.predict = predict(gbm.fit, newdata = test_data)
RMSE(gbm.fit.predict, test_y)
```

    ## [1] 21.33985

## resampling on train

``` r
set.seed(2337)
resamp <- resamples(list(lm = linear_model,
                         knn = knn_model,
                         enet = enet.fit.min,
                         pls = pls_model,
                         gam = gam_model,
                         mars = mars_model,
                         rf = rf_model,
                         boosting = gbm.fit))
summary(resamp)
```

    ## 
    ## Call:
    ## summary.resamples(object = resamp)
    ## 
    ## Models: lm, knn, enet, pls, gam, mars, rf, boosting 
    ## Number of resamples: 10 
    ## 
    ## MAE 
    ##              Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lm       13.92045 14.83631 15.70615 15.86320 17.11461 17.98634    0
    ## knn      15.37075 16.08924 16.27729 16.43150 16.73887 18.30159    0
    ## enet     13.75661 14.78158 15.51474 15.72501 16.89604 18.07575    0
    ## pls      13.91136 14.83702 15.70282 15.86059 17.10895 17.98698    0
    ## gam      13.91937 14.22080 14.77448 14.71540 15.11166 15.73245    0
    ## mars     13.16081 13.78606 14.37446 14.40883 14.90154 16.14824    0
    ## rf       12.88480 13.25494 14.28194 14.35238 15.10200 16.81740    0
    ## boosting 12.87423 13.14708 13.93877 13.95774 14.40141 15.67332    0
    ## 
    ## RMSE 
    ##              Min.  1st Qu.   Median     Mean  3rd Qu.     Max. NA's
    ## lm       18.77627 20.33513 21.86867 23.48139 25.53443 35.65489    0
    ## knn      21.33260 22.70396 24.32635 24.80976 26.40794 30.60127    0
    ## enet     18.66444 20.35663 21.70232 23.45010 25.32954 36.20936    0
    ## pls      18.77159 20.33757 21.86645 23.47877 25.53555 35.65638    0
    ## gam      18.50087 20.64363 21.41164 21.59688 23.04463 24.37753    0
    ## mars     17.90391 19.23699 20.37222 20.57945 21.21665 25.56186    0
    ## rf       17.19551 18.55079 20.27562 20.91333 22.18751 29.98525    0
    ## boosting 17.59902 18.52951 19.77556 20.48785 21.51046 27.10191    0
    ## 
    ## Rsquared 
    ##                 Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## lm       0.165660213 0.1862176 0.2236317 0.2579309 0.2907474 0.5119799    0
    ## knn      0.004646629 0.0681859 0.1452300 0.1781345 0.1917208 0.4966265    0
    ## enet     0.166425299 0.1896596 0.2246024 0.2562765 0.2874691 0.4928261    0
    ## pls      0.165927044 0.1863135 0.2240227 0.2580736 0.2908591 0.5118804    0
    ## gam      0.158119658 0.2623102 0.3796382 0.3702098 0.4633351 0.5898515    0
    ## mars     0.170545013 0.3371873 0.3535206 0.3990474 0.4869190 0.7315694    0
    ## rf       0.186380668 0.3111663 0.3864993 0.3927711 0.4373603 0.6420149    0
    ## boosting 0.240938777 0.3452425 0.3903808 0.4173522 0.4527714 0.7393036    0

``` r
parallelplot(resamp, metric = "RMSE")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-14-1.png" width="90%" style="display: block; margin: auto;" />

``` r
bwplot(resamp, metric = "RMSE")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-14-2.png" width="90%" style="display: block; margin: auto;" />

## Classification

### Data Manipulation

We consider time to recovery as a binary outcome as long(\>30 days) and
short (\<= 30 days).

``` r
#consider time to recovery as a binary outcome (>30 days vs. <= 30 days) and develop a prediction model for this binary outcome.
train_class = 
  train_data %>% 
  mutate(recovery_time = ifelse(recovery_time>30, "long", "short")) 
test_class = 
  test_data %>% 
  mutate(recovery_time = ifelse(recovery_time>30, "long", "short")) 

# matrix of predictors
x_train_class <- model.matrix(recovery_time~.,train_class)[,-1]
# vector of response
y_train_class <- train_class$recovery_time


# matrix of predictors
x_test_class <- model.matrix(recovery_time~.,test_class)[,-1]
# vector of response
y_test_class <- test_class$recovery_time


# ctrl for class
ctrl_class <- trainControl(method = "cv",
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)
```

#### Modeling Strategy

- **Penalized logistic regression**
- **Generalized additive model (GAM)**
- **Multivariate adaptive regression splines (MARS)**
- **Linear discriminant analysis (LDA)**
- **Classification and Regression Tree (CART)**
- **Random Forest**
- **Boosting**
- **Support Vector Machine (SVM with Linear and Radial Kernels)**

### Penalized logistic regression

``` r
glmnGrid <- expand.grid(.alpha = seq(0, 1, length = 21),
                        .lambda = exp(seq(-12, -7, length = 50)))
set.seed(2337)
model.glmn <- train(x = x_train_class,
                    y = y_train_class,
                    method = "glmnet",
                    tuneGrid = glmnGrid,
                    metric = "ROC",
                    trControl = ctrl_class)

model.glmn$bestTune
```

    ##     alpha       lambda
    ## 720   0.7 4.270372e-05

``` r
myCol<- rainbow(25)
myPar <- list(superpose.symbol = list(col = myCol),
              superpose.line = list(col = myCol))

plot(model.glmn, par.settings = myPar, xTrans = function(x) log(x))
```

<img src="model_result_files/figure-gfm/unnamed-chunk-16-1.png" width="90%" style="display: block; margin: auto;" />

``` r
# view performance on the test set (accuracy)
glmn_class_test_pred <- predict(model.glmn, newdata=x_test_class) # test dataset
glmn_class_test_acc <- sum(glmn_class_test_pred==y_test_class)/length(y_test_class)
sprintf("test error for penalized logistic is: %.3f",1-glmn_class_test_acc)
```

    ## [1] "test error for penalized logistic is: 0.286"

Since the boundary value for $\lambda$ is exp(-10)=4.54e-5 and
exp(-5)=0.007, and the optimal lambda is not on the boundary. So local
minimum is achieved.

### Gam

``` r
# gam
set.seed(2337)
model.gam <- train(x = x_train_class,
                   y = y_train_class,
                   method = "gam",
                   tuneGrid = data.frame(method = "GCV.Cp", select = c(TRUE,FALSE)),
                   metric = "ROC",
                   trControl = ctrl_class)

model.gam$bestTune
```

    ##   select method
    ## 2   TRUE GCV.Cp

``` r
model.gam$finalModel
```

    ## 
    ## Family: binomial 
    ## Link function: logit 
    ## 
    ## Formula:
    ## .outcome ~ gender1 + race2 + race3 + race4 + smoking1 + smoking2 + 
    ##     hypertension1 + diabetes1 + vaccine1 + severity1 + studyB + 
    ##     studyC + s(age) + s(SBP) + s(LDL) + s(bmi) + s(height) + 
    ##     s(weight)
    ## 
    ## Estimated degrees of freedom:
    ## 0.0002 3.0898 0.1982 3.8717 1.9808 0.0001  total = 22.14 
    ## 
    ## UBRE score: 0.07604226

``` r
#discover bmi and recovery time
plot(model.gam$finalModel, select = 4)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-17-1.png" width="90%" style="display: block; margin: auto;" />

``` r
#train error
train.pred.gam <- predict(model.gam, newdata = x_train_class)
train.error.gam = 1-sum(train_class$recovery_time == train.pred.gam)/length(train_data$recovery_time)
sprintf("The train error for GAM is %.3f", train.error.gam)
```

    ## [1] "The train error for GAM is 0.266"

``` r
#test error
test.pred.gam <- predict(model.gam, newdata = x_test_class)
test.error.gam = 1-sum(test_class$recovery_time == test.pred.gam)/length(test_class$recovery_time)
sprintf("The test error for GAM is %.3f", test.error.gam)
```

    ## [1] "The test error for GAM is 0.283"

### MARS

``` r
set.seed(2337)
mars.fit = train(x = x_train_class,
                        y =  y_train_class,
                         method = "earth",
                         tuneGrid = expand.grid(degree = 1:3, 
                                                nprune = 2:20),
                         metric = "ROC",
                         trControl = ctrl_class)
```

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

    ## Warning: glm.fit: fitted probabilities numerically 0 or 1 occurred

``` r
plot(mars.fit)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-18-1.png" width="90%" style="display: block; margin: auto;" />

``` r
#train error
train.pred.mars = predict(mars.fit , newdata = x_train_class)
train.error.mars = 1-sum(train_class$recovery_time == train.pred.mars)/length(train_data$recovery_time)
sprintf("The train error for MARS is %.3f", train.error.mars)
```

    ## [1] "The train error for MARS is 0.270"

``` r
#test error
test.pred.mars = predict(mars.fit , newdata = x_test_class)
test.error.mars = 1-sum(test_class$recovery_time == test.pred.mars)/length(test_class$recovery_time)
sprintf("The test error for MARS is %.3f", test.error.mars)
```

    ## [1] "The test error for MARS is 0.274"

### Linear discriminant analysis (LDA)

``` r
set.seed(2337)
# Using MASS
lda.fit <- lda(recovery_time~., 
               data = train_class)
plot(lda.fit)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-19-1.png" width="90%" style="display: block; margin: auto;" />

``` r
lda.fit$scaling
```

    ##                        LD1
    ## age           -0.005230071
    ## gender1        0.582852115
    ## race2          0.111644453
    ## race3         -0.015332625
    ## race4          0.078103928
    ## smoking1      -0.395459244
    ## smoking2      -0.628005874
    ## height        -0.497778576
    ## weight         0.538847538
    ## bmi           -1.636992668
    ## hypertension1 -0.368808467
    ## diabetes1     -0.053193708
    ## SBP            0.010681574
    ## LDL           -0.003381055
    ## vaccine1       0.854566384
    ## severity1     -0.726871127
    ## studyB         1.410574304
    ## studyC        -0.044449352

``` r
set.seed(2337)
# Using caret
lda_model <- train(x_train_class, #train x
                   y_train_class, #train y 
                   method = "lda", 
                   metric = "ROC", 
                   trControl = ctrl_class)
summary(lda_model)
```

    ##             Length Class      Mode     
    ## prior        2     -none-     numeric  
    ## counts       2     -none-     numeric  
    ## means       36     -none-     numeric  
    ## scaling     18     -none-     numeric  
    ## lev          2     -none-     character
    ## svd          1     -none-     numeric  
    ## N            1     -none-     numeric  
    ## call         3     -none-     call     
    ## xNames      18     -none-     character
    ## problemType  1     -none-     character
    ## tuneValue    1     data.frame list     
    ## obsLevels    2     -none-     character
    ## param        0     -none-     list

``` r
#train error
train.pred.lda <- predict(lda_model, newdata = x_train_class)
train.error.lda = 1 - sum(train_class$recovery_time == train.pred.lda)/length(train_data$recovery_time)
sprintf("The train error for LDA is %.3f", train.error.lda)
```

    ## [1] "The train error for LDA is 0.277"

``` r
#test error
test.pred.lda <- predict(lda_model, newdata = x_test_class)
test.error.lda = 1 - sum(test_class$recovery_time == test.pred.lda)/length(test_class$recovery_time)
sprintf("The test error for LDA is %.3f", test.error.lda)
```

    ## [1] "The test error for LDA is 0.287"

### Random Forest - Classification

``` r
set.seed(2337)
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
rf.grid <- expand.grid(mtry = 1:18,
                       splitrule = "gini",
                       min.node.size = seq(from = 1, to = 50, by = 5))

set.seed(2337)
rf_class_model <- train(x_train_class,
                        y_train_class,
                        method = "ranger",
                        importance = "permutation",
                        tuneGrid = rf.grid,
                        metric = "ROC",
                        trControl = ctrl_class)
stopCluster(cl)
registerDoSEQ()

ggplot(rf_class_model, highlight = TRUE)
```

    ## Warning: The shape palette can deal with a maximum of 6 discrete values because
    ## more than 6 becomes difficult to discriminate; you have 10. Consider
    ## specifying shapes manually if you must have them.

    ## Warning: Removed 72 rows containing missing values (`geom_point()`).

<img src="model_result_files/figure-gfm/unnamed-chunk-20-1.png" width="90%" style="display: block; margin: auto;" />

``` r
rf_class_model$bestTune
```

    ##   mtry splitrule min.node.size
    ## 5    1      gini            21

``` r
#train error
train.pred.rf <- predict(lda_model, newdata = x_train_class)
train.error.ef = 1 - sum(train_class$recovery_time == train.pred.lda)/length(train_data$recovery_time)
sprintf("The train error for LDA is %.3f", train.error.lda)
```

    ## [1] "The train error for LDA is 0.277"

``` r
#test error
test.pred.rf <- predict(rf_class_model, newdata = x_test_class)
test.error.rf = 1 - sum(test_class$recovery_time == test.pred.rf)/length(test_class$recovery_time)
sprintf("The test error for Random Forest is %.3f", test.error.rf)
```

    ## [1] "The test error for Random Forest is 0.293"

### Boosting

``` r
gbmA.grid = expand.grid(n.trees = c(10000,20000,30000,40000),
                         interaction.depth = 1:3,
                         shrinkage = c(0.0003,0.0005,0.001),
                         n.minobsinnode = 1)

cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)
set.seed(2337)
gbmA.fit = train(recovery_time~. ,
                  train_class,
                  tuneGrid = gbmA.grid,
                  trControl = ctrl_class,
                  method = "gbm",
                  distribution = "adaboost",
                  metric = "ROC",
                  verbose = FALSE)

stopCluster(cl)
registerDoSEQ()

ggplot(gbmA.fit, highlight = TRUE)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-21-1.png" width="90%" style="display: block; margin: auto;" />

``` r
#train error
train.pred.boosting = predict(gbmA.fit , newdata = train_class, type = "raw")
train.error.boosting = 1-sum(train_class$recovery_time == train.pred.boosting)/length(train_data$recovery_time)
# sprintf("The train error for boosting is %.3f", train.pred.boosting)

#test error
test.pred.boosting = predict(gbmA.fit , newdata = test_class, type = "raw")
test.error.boosting = 1-sum(test_class$recovery_time == test.pred.boosting)/length(test_class$recovery_time)
# sprintf("The test error for boosting is %.3f", train.pred.boosting)
```

### linear SVM

``` r
# parallel computing
no_cores <- detectCores() - 1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)

# kernlab
set.seed(2337)
svml.fit <- train(recovery_time ~ . , 
                  data = train_class, 
                  method = "svmLinear",
                  tuneGrid = data.frame(C = exp(seq(-5,3,len=30))),
                  trControl = ctrl_class)
```

    ## maximum number of iterations reached 0.008388346 0.00775115

``` r
plot(svml.fit, highlight = TRUE, xTrans = log)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-22-1.png" width="90%" style="display: block; margin: auto;" />

``` r
stopCluster(cl)
registerDoSEQ()

svml.fit$bestTune
```

    ##            C
    ## 6 0.02676421

``` r
#train error
train.pred.svml <- predict(svml.fit, newdata = train_class)
train.error.svml = 1-sum(train_class$recovery_time == train.pred.svml)/length(train_data$recovery_time)
sprintf("The train error for Support vector classifier is %.3f", train.error.svml)
```

    ## [1] "The train error for Support vector classifier is 0.292"

``` r
#test error
test.pred.svml <- predict(svml.fit, newdata = test_class)
test.error.svml = 1-sum(test_class$recovery_time == test.pred.svml)/length(test_class$recovery_time)
sprintf("The test error for Support vector classifier is %.3f", test.error.svml)
```

    ## [1] "The test error for Support vector classifier is 0.294"

### SVM with radial kernel

``` r
svmr.grid <- expand.grid(C = exp(seq(-1,5,len=20)),
                         sigma = exp(seq(-10,-2,len=10)))

# parallel computing
no_cores <- detectCores() - 1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)

# tunes over both cost and sigma
set.seed(2337)             
svmr.fit <- train(recovery_time ~ . , 
                  data = train_class,
                  method = "svmRadialSigma",
                  tuneGrid = svmr.grid,
                  trControl = ctrl_class)
```

    ## Warning in train.default(x, y, weights = w, ...): The metric "Accuracy" was not
    ## in the result set. ROC will be used instead.

    ## Warning in nominalTrainWorkflow(x = x, y = y, wts = weights, info = trainInfo,
    ## : There were missing values in resampled performance measures.

``` r
stopCluster(cl)
registerDoSEQ()



myCol<- rainbow(25)
myPar <- list(superpose.symbol = list(col = myCol),
              superpose.line = list(col = myCol))

plot(svmr.fit, highlight = TRUE, par.settings = myPar)
```

<img src="model_result_files/figure-gfm/unnamed-chunk-23-1.png" width="90%" style="display: block; margin: auto;" />

``` r
svmr.fit$bestTune
```

    ##          sigma        C
    ## 176 0.00386592 78.91892

``` r
#train error
train.pred.svmr <- predict(svmr.fit, newdata = train_class)
train.error.svmr = 1-sum(train_class$recovery_time == train.pred.svmr)/length(train_class$recovery_time)
sprintf("The train error for Support vector Machine with radial kernel is %.3f", train.error.svmr)
```

    ## [1] "The train error for Support vector Machine with radial kernel is 0.252"

``` r
#test error
test.pred.svmr <- predict(svmr.fit, newdata = test_class)
test.error.svmr = 1-sum(test_class$recovery_time == test.pred.svmr)/length(test_class$recovery_time)
sprintf("The test error for Support vector Machine with radial kernel is %.3f", test.error.svmr)
```

    ## [1] "The test error for Support vector Machine with radial kernel is 0.288"

After tuning, the cost in this range achieves local maximum accuracy.
The value is not on the boundary, hence the local maximum accuracy is
achieved. It takes a long time (aprox 30 min) to run so it may be hard
to refine into a smaller grid.

## Resampling Results and Model Selection

``` r
# parallel computing
no_cores <- detectCores() - 1
cl <- makePSOCKcluster(no_cores)
registerDoParallel(cl)

set.seed(2337)
resamp_class <- resamples(list(enet_logistic = model.glmn,
                         gam = model.gam,
                         mars = mars.fit,
                         lda = lda_model,
                         rf = rf_class_model,
                         boosting = gbmA.fit,
                         linear_svm = svml.fit,
                         radial_svm = svmr.fit))
stopCluster(cl)
registerDoSEQ()

summary(resamp_class)
```

    ## 
    ## Call:
    ## summary.resamples(object = resamp_class)
    ## 
    ## Models: enet_logistic, gam, mars, lda, rf, boosting, linear_svm, radial_svm 
    ## Number of resamples: 10 
    ## 
    ## ROC 
    ##                    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## enet_logistic 0.6378682 0.6902541 0.6988395 0.7043575 0.7102212 0.7767234    0
    ## gam           0.6582269 0.6979388 0.7078518 0.7137858 0.7137554 0.7971455    0
    ## mars          0.6541527 0.6992141 0.7135143 0.7135352 0.7187120 0.7921348    0
    ## lda           0.6377164 0.6883298 0.6975023 0.7035379 0.7099743 0.7763438    0
    ## rf            0.6540389 0.6831443 0.7005770 0.7057080 0.7063120 0.7725104    0
    ## boosting      0.6484531 0.6867057 0.7033979 0.7094265 0.7205468 0.7817454    0
    ## linear_svm    0.6208250 0.6432502 0.6679673 0.6637873 0.6821406 0.7217947    0
    ## radial_svm    0.6667932 0.6859264 0.6986712 0.7042262 0.7150980 0.7666262    0
    ## 
    ## Sens 
    ##                    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. NA's
    ## enet_logistic 0.8483146 0.9129213 0.9245810 0.9214331 0.9325843 0.9606742    0
    ## gam           0.8707865 0.9002809 0.9103791 0.9163832 0.9314622 0.9775281    0
    ## mars          0.8483146 0.8946629 0.9047769 0.9107683 0.9325843 0.9719101    0
    ## lda           0.8707865 0.9269663 0.9325843 0.9298443 0.9413408 0.9719101    0
    ## rf            1.0000000 1.0000000 1.0000000 1.0000000 1.0000000 1.0000000    0
    ## boosting      0.9213483 0.9396067 0.9523727 0.9500471 0.9649426 0.9662921    0
    ## linear_svm    0.9775281 0.9887797 0.9971910 0.9938265 1.0000000 1.0000000    0
    ## radial_svm    0.8876404 0.9298616 0.9438202 0.9433118 0.9622199 0.9943820    0
    ## 
    ## Spec 
    ##                     Min.   1st Qu.    Median        Mean    3rd Qu.       Max.
    ## enet_logistic 0.12162162 0.1898371 0.2094595 0.208237690 0.24315068 0.26027397
    ## gam           0.20270270 0.2465753 0.2652721 0.274916698 0.29729730 0.36986301
    ## mars          0.18918919 0.2465753 0.2635135 0.266753054 0.27397260 0.35616438
    ## lda           0.13513514 0.1808589 0.1972418 0.201351351 0.23898556 0.27027027
    ## rf            0.00000000 0.0000000 0.0000000 0.000000000 0.00000000 0.00000000
    ## boosting      0.09459459 0.1491577 0.1631803 0.171584598 0.18641244 0.28767123
    ## linear_svm    0.00000000 0.0000000 0.0000000 0.006812292 0.01013514 0.04109589
    ## radial_svm    0.09459459 0.1330063 0.1643836 0.172861903 0.20830248 0.25675676
    ##               NA's
    ## enet_logistic    0
    ## gam              0
    ## mars             0
    ## lda              0
    ## rf               0
    ## boosting         0
    ## linear_svm       0
    ## radial_svm       0

``` r
parallelplot(resamp_class, metric = "ROC")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-24-1.png" width="90%" style="display: block; margin: auto;" />

``` r
bwplot(resamp_class, metric = "ROC")
```

<img src="model_result_files/figure-gfm/unnamed-chunk-24-2.png" width="90%" style="display: block; margin: auto;" />
