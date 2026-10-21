######################################################################################
#### zvm_models(): fit one coverage model (glmnet / rf / xgboost) and score it on
#### test_data. Shared by 04_zvm_cv_runs.R and 05_zvm_team_analyses.R.
######################################################################################

library(dplyr)
library(tidyr)
library(pROC)
library(glmnet)
library(glmnetUtils)
library(ranger)
library(xgboost)

zvm_models <- function(model_data,
                       test_data,
                       ml_type = c("glmnet","rf","xgboost"),
                       full_cv = TRUE,
                       excl_vars = NULL,
                       param_grid = NULL,
                       seed = 123,
                       return_preds = FALSE,
                       nthread = 1,
                       ...){
  dta <- model_data |>
    select(-gameId,-playId,
           -gameplayId,-pff_passCoverage,-c({{excl_vars}}))
  
  dta_test <- test_data |>
    select(-gameId,-playId,
           -gameplayId,-pff_passCoverage,-c({{excl_vars}}))
  
  ml_type = match.arg(ml_type)
  
  if(ml_type == "glmnet"){
    set.seed(seed)
    if(full_cv){
      mods <- cva.glmnet(x = as.matrix(dta |> select(-pff_manZone_num)),y = dta$pff_manZone_num,family = binomial(),...)
      mins <- sapply(mods$modlist,\(x) min(x$cvm))
      modu <- mods$modlist[[which.min(mins)]]
    }else{
      modu <- cv.glmnet(x = as.matrix(dta |> select(-pff_manZone_num)),y = dta$pff_manZone_num,family = binomial(),...)
    }
    coefs <- coef(modu)
    newxu <- as.matrix(dta_test |> select(-pff_manZone_num))
    preds <- predict(modu,newx = newxu, s = "lambda.min",type = "response")
    out <- preds > 0.5 
    acc <- mean(out == dta_test$pff_manZone_num)
    
    m_roc <- roc(dta_test$pff_manZone_num,c(preds))
    m_auc <- auc(m_roc)
    #plot(m_lasso_roc)
    logloss <- -mean((dta_test$pff_manZone_num*log(preds)+(1-dta_test$pff_manZone_num)*log(1-preds)))
  }else if(ml_type == "rf"){
    modu <- ranger(pff_manZone_num ~ .,data = dta,
                   num.trees = 1000, importance = "permutation",...)  
    
    preds <- predict(modu,data = dta_test,type = "response")
    out <- preds$predictions > 0.5
    acc <- mean(out == dta_test$pff_manZone_num)
    
    preds <- preds$predictions
    m_roc <- roc(dta_test$pff_manZone_num,preds)
    m_auc <- auc(m_roc)
    #lines(m_rf_roc,col = "skyblue")
    logloss <- -mean((dta_test$pff_manZone_num*log(preds)+(1-dta_test$pff_manZone_num)*log(1-preds)))
  }else if(ml_type == "xgboost"){
    
    xgb_mat <- xgboost::xgb.DMatrix(as.matrix(dta |> select(-pff_manZone_num)),
                                    label = dta$pff_manZone_num)
    
    if(full_cv){
      if(is.null(param_grid)){
        # eta 0.01 needs very many rounds and eta >= 0.5 / depth 10 overfit
        # on ~3,000 training plays; this grid keeps the useful region
        param_grid <- expand_grid(eta = c(0.05,0.1,0.3),
                                  max_depth = c(1,3,5))
      }
      xgb_list <- list()
      set.seed(seed)
      for(i in 1:nrow(param_grid)){
        params <- list(eval_metric = c("logloss"),
                       objective = "binary:logistic",
                       eta = param_grid[i,1],
                       max_depth = param_grid[i,2],
                       colsample_bynode = 0.3,
                       nthread = nthread)
        xgb_list[[i]] <- xgb.cv(params = params,data = xgb_mat,
                                nfold = 5,showsd = T, nrounds = 1500,
                                print_every_n = 100,early_stopping_rounds = 15,...)
        
      }
      cv_losses <- sapply(xgb_list,function(r){
        iter <- r$best_iteration
        r$evaluation_log[iter]$test_logloss_mean
      })
      bpi <- which.min(cv_losses)
      bp <- xgb_list[[bpi]]$params
      nr <- xgb_list[[bpi]]$best_iteration
      modu <- xgboost(xgb_mat, max_depth = param_grid[bpi,2], eta = param_grid[bpi,1],
                      nrounds = nr, gamma = 0.5,colsample_bynode = 0.3, objective = "binary:logistic",
                      nthread = nthread,
                      ...)
    }else{
      modu <- xgboost(xgb_mat,
                      nrounds = nr, gamma = 0.5,colsample_bynode = 0.3, objective = "binary:logistic",
                      ...)
    }
    
    xgb_test <- xgboost::xgb.DMatrix(as.matrix(dta_test |> select(-pff_manZone_num)),
                                     label = dta_test$pff_manZone_num)
    
    preds <- predict(modu,newdata = xgb_test)
    out <- as.numeric(preds > 0.5)
    acc <- mean(out == dta_test$pff_manZone_num)
    
    m_roc <- roc(dta_test$pff_manZone_num,preds)
    m_auc <- auc(m_roc)
    logloss <- -mean((dta_test$pff_manZone_num*log(preds)+(1-dta_test$pff_manZone_num)*log(1-preds)))
  }
  if(return_preds){
    return(list(model = modu,
                acc = acc,
                roc = m_roc,
                auc = m_auc,
                preds = preds,
                logloss = logloss,
                npred = ncol(dta)))
  }else{
    return(list(model = modu,
                acc = acc,
                roc = m_roc,
                auc = m_auc,
                #preds = preds,
                logloss = logloss,
                npred = ncol(dta)))
  }
  
}
