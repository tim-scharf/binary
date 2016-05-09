#' Title
#'
#' @param Xtrain.feather
#' @param y.feather
#' @param Xtest.feather
#' @param data_checks_func
#' @param train_func
#' @param iter
#' @param pct_train
#'
#' @return
#' @export
#'
#' @examples
classifier_loop <- function(Xtrain.feather,
                            y.feather,
                            Xtest.feather,
                            data_checks_func,
                            train_func,
                            iter,
                            pct_train){

#get data and do checks
Xtrain <- readRDS(Xtrain.feather)
Xtest  <- readRDS(Xtest.feather)
y <- readRDS(y.feather)
data_checks_func(Xtrain,Xtest,y)

#initialize output

OUTPUT <- train_func(Xtrain,Xtest,y,iter,pct_train)

}



