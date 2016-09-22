xgb_train_binary_tree <- function(Xtrain,Xtest,y,iter,pct_train){

  n_train <- nrow(Xtrain)
  n_test <- nrow(Xtest)

  OUTPUT <- init_OUPUT(n_train=n_train,n_test=n_test,iter=iter,pct_train=pct_train)

  # get missing attr
  missing <-  NA

  if(!is.null(n_test)){dtest <- xgb.DMatrix(as.matrix(Xtest), missing = missing)}
  mean_pred <- vector(mode = 'numeric',length = iter)
  for(i in 1:iter){
  cat('building model',i, '\n\n')
  idx <- OUTPUT$IDX[,i]

  # get unique stopping samples
  idx_stop  <- sample((1:n_train)[-idx] ,length(idx)) #stopping index same length
  idx_valid <- (1:n_train)[-c(idx,idx_stop)] #everything left is validation

  #initialize 3 xgb.Dmatrices
  dtrain   <-   xgb.DMatrix( as.matrix(Xtrain[idx,])       ,  missing = missing, label = y[idx] )
  dstop    <-   xgb.DMatrix( as.matrix(Xtrain[idx_stop,])  ,  missing = missing, label = y[idx_stop] )
  dvalid   <-   xgb.DMatrix( as.matrix(Xtrain[idx_valid,]) ,  missing = missing, label = y[idx_valid] )

  param = list(
    booster          =   'gbtree',
    objective        =   'binary:logistic',
    eval_metric      =   'logloss',
    max.depth        =   sample(3:12, 1),
    eta              =   runif(1,.05,.1),
    gamma            =   runif(1,0,2),
    min_child_weight =   runif(1,0,2),
    subsample        =   .75,
    colsample_bytree =   .75,
    nrounds          =   5000,
 #   lambda           =   runif(1,0,2),  ##tree default 1 related?
 #  alpha            =   0,                 ## tree related?
    base_score       =   mean(y),
    nthread          =   30 )

  model <- xgb.train(
    early_stopping_rounds  = 20,
    watchlist         = list(stop_err = dstop),
    print_every_n     = 50,
    param             = param,
    data              = dtrain,
    nrounds           = param$nrounds,
    maximize          = F,
    verbose  =  1 )

  OUTPUT$PCV[idx_valid,i]    <-   predict(model, newdata = dvalid, ntreelimit = model$best_iteration)

  if(!is.null(n_test)){ OUTPUT$PT[,i] <-   predict(model, newdata = dtest,  ntreelimit = model$best_iteration)}

  OUTPUT$DATA[[i]]   <-  list(param  = model$params,
                         rounds   = model$best_iteration,
                         cv_score = model$best_score)

}

return(OUTPUT)
}
