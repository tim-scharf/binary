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
                            y.rds,
                            train_func,
                            iter,
                            pct_train,
                            Xtest.feather = NULL,
                            w = NULL){

#get data and do checks
Xtrain <- setDT(read_feather(Xtrain.feather))
y <- readRDS(y.rds)
Xtest <- NULL
if(! is.null(Xtest.feather)) { Xtest <- setDT(read_feather(Xtest.feather)) }

#initialize output
OUTPUT <- train_func(Xtrain = Xtrain, Xtest = Xtest,y = y ,iter = iter,pct_train = pct_train,w = w)

}



