filterDataset <- function(df) {
  numeric.colums <- gtools::mixedsort(colnames(df)[grepl("X", colnames(df))])
  df <- df[,numeric.colums]
  dft <- t(df)
  colnames(dft) <- paste0("g", seq(1,nrow(df)))
  dft <- as.data.frame(dft)
  return(dft)
}


leukemia <- function(model, X_pls, threshold=0.5, ncomp=NULL) {
  if (!is.null(ncomp)) {
    predicted <- predict(model, newdata=X_pls, ncomp=ncomp, type="response")
  } else {
    predicted <- predict(model, newdata=X_pls, type="response")
  }
  
  predicted <- sapply(as.numeric(predicted), function(i) {return(if(i <= threshold) 0 else 1)})
  return(predicted)
}
