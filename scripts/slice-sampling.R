
.small[
slice_sample <- function(f, x_0, w) {
  N <- length(x_0)
  x_1 <- vector(mode = "numeric", length = N)
  seq_dfrow <- function(df) { seq_len(nrow(df)) }
  #print(x_0)
  #cat("Initial point =", x_0, "\n")
  #cat("Scales =", w, "\n")

  # Step (a): Find the value of y that defines the slice
  y <- runif(n = 1, min = 0, max = f(x_0))
  #cat("Slice defining value, y =", y, "\n\n")
  
  # Step (b): Randomly position the hyperrectangle
  H <- data.frame(L = rep(NA, N), R = rep(NA, N))
  for (i in seq_dfrow(H)) {
    H[i, "L"] <- x_0[i] - runif(n = 1, min = 0, max = w[i])
    H[i, "R"] <- H[i, "L"] + w[i]
  }
  #cat("Hyperrectangle\n")
  #print(H)
  #cat("\n")
  # Step (c): Sample from H, shrinking when points are rejected
  repeat {
    for (i in seq_along(x_1)) {
      x_1[i] <- H[i, "L"] + runif(n = 1) * (H[i, "R"] - H[i, "L"])
    }
    
    #cat("Computed x_1 = ", x_1, "\n")
    #cat("Testing stopping condition: y = ", y, " < f(x_1) =", f(x_1), ".")
    if (y < f(x_1)) break
   # cat("False.\n")
    
    for (i in seq_dfrow(H)) {
      if (x_1[i] < x_0[i]) {
        H[i, "L"] <- x_1[i]
      } else {
        H[i, "R"] <- x_1[i]
      }
    }
    
  } 
  #cat("True.\n")
  return(x_1)
}

slice_sampling <- function(posterior, initial_point, scale_estimates, iter = 2000, warmup = floor(iter/2)) {
  chain_length <- iter - warmup
  samples <- data.frame(matrix(vector(), iter, length(initial_point)))
  
  samples[1,] <- slice_sample(f = posterior, x_0 = initial_point, w = scale_estimates)
  for (i in 2:iter) {
    samples[i, ] <- slice_sample(f = posterior, x_0 = unlist(samples[i - 1,]), w = scale_estimates)    
  }
  
  colnames(samples) <- paste0("param", seq_along(colnames(samples)))
  return(tail(samples, n = chain_length))
}