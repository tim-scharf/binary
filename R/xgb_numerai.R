xgb_train_binary_tree_weighted <- function(Xtrain,Xtest,y,iter,pct_train,w){

  n_train <- nrow(Xtrain)
  n_test <- nrow(Xtest)

  OUTPUT <- init_OUPUT(n_train=n_train,n_test=n_test,iter=iter,pct_train=pct_train)

  # get missing attr
  missing <-  NA
  dtest <- xgb.DMatrix(as.matrix(Xtest), missing = missing)

for(i in 1:iter){
  cat('building model',i, '\n\n')
  idx0 <- OUTPUT$IDX[,i]
  size = 4000
  scale = 1.25
  idx1 <- sample(idx0 , size = size, prob = w[idx0]**scale)
  # index madness, lets do this


  idx <- idx1[idx1%%2==1]
  idx_stop <- idx1[idx1%%2==0]

  idx_valid <- sample((1:length(y))[-idx0] , size = size, prob = w[-idx0]**scale)

  #initialize 3 xgb.Dmatrices
  dtrain   <-   xgb.DMatrix( as.matrix(Xtrain[idx,])        ,  missing = missing, label = y[idx] )
  dstop    <-   xgb.DMatrix( as.matrix(Xtrain[idx_stop,])   , missing = missing, label = y[idx_stop] )
  dvalid   <-   xgb.DMatrix( as.matrix(Xtrain[idx_valid,])  ,  missing = missing, label = y[idx_valid] )


  param = list(
    booster          =   'gbtree',
    objective        =   'binary:logistic',
    eval_metric      =   'logloss',
    max.depth        =   sample(2:8, 1),
    eta              =   .001,
    gamma            =   1,
    min_child_weight =   1,
    subsample        =   .75,
    colsample_bytree =   .75,
    nrounds          =   5000,
    #   lambda           =   runif(1,0,2),  ##tree default 1 related?
    #  alpha            =   0,                 ## tree related?
    base_score       =   mean(y),
    nthread          =   30 )

  model <- xgb.train(
    early_stopping_rounds  = 20,
    watchlist         = list(train_err = dtrain, valid_err = dvalid, stop_err = dstop),
    print_every_n     = 1,
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
