get_closest <- function(vector, target) {
  
  if (target <= vector[1])
    return(vector[1])
  if (target >= vector[length(vector)])
    return(vector[length(vector)])
  
  Rfast::binary_search(vector, target, index = T)
  
}
