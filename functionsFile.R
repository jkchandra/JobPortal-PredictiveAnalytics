#########################################################################################################
# BC2407 Semester Analytics Project: Functions
# Team Number 8
# Members: Christopher Gerard Toh, Teo Tian Shun Kenneth, Lim De Quan, Jonathan Kevin Chandra
# Datasets: 
# Library: 
####################################################################################################


which.max2 <- function(x){
  n <- length(x)
  return (which(x == sort(x,partial=n-1)[n-1]))
}
which.max3 <- function(x){
  n <- length(x)
  return (which(x == sort(x,partial=n-2)[n-2]))
}
which.max4 <- function(x){
  n <- length(x)
  return (which(x == sort(x,partial=n-3)[n-3]))
}
which.max5 <- function(x){
  n <- length(x)
  return (which(x == sort(x,partial=n-4)[n-4]))
}