

#' init_OUPUT
#'
#' @param n_train_full number of training examples
#' @param n_test_full  number of test examples
#' @param iter  how many iterations to run
#' @param pct_train what PCT 0,1 to train on)
#'
#' @return list PCV cv predictions
#' @export
#'
#' @examples OUPUT <- init_OUPUT(1000,1000,100,.25)
#'
#'
init_OUPUT <- function(n_train,iter,pct_train,n_test=NULL){
  #initialize input matrices
  n_samp  <- round( pct_train * n_train )

  PCV   = matrix(NA_real_,  nrow  =  n_train, ncol = iter)
  IDX   = sapply( 1:iter,function(z) sample(n_train,n_samp))
  DATA  = vector(mode='list',length=iter)
  PT    = NULL
  if(!is.null(n_test)){  PT    = matrix(NA_real_,  nrow =  n_test , ncol = iter)}
  L = list(PCV=PCV,
           IDX=IDX,
           DATA=DATA,
           PT=PT)
  return(L)
}
