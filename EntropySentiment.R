#calculate entropy shannon for all sentiment
entropySent <- function(x) {
  #y<- data.frame()
  totalSum<-rowSums(x)
  probi<-x/totalSum
  logpi<-log(probi)
  Hlocal<- (-probi*logpi)
  #y$H<-rowSums(y)
  result <- rowSums(Hlocal,na.rm = TRUE)
  return(result)
}
