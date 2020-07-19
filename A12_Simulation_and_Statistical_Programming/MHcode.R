## Perform one M-H step
## Y  - current state of chain
## r,c  - row/column of bit to try to flip
mh_step <- function(Y, r, c, theta) {
  
  ## work out indicies of adjacent rows
  ## and columns
  r_adj = r + c(-1,1)   # one row above and below
  c_adj = c + c(-1,1)   # columns
  
  ## if on the boundary then adjust indices appropriately
  if (r == 1) r_adj = r_adj[-1]
  else if (r == nrow(Y)) r_adj = r_adj[-2]
  if (c == 1) c_adj = c_adj[-1]
  else if (c == ncol(Y)) c_adj = c_adj[-2]
  
  ## change in pi
  ????
  log_alpha = ????
  
  ## if U < acceptance ratio then flip entry
  ## otherwise keep as is
  if (log(runif(1)) < log_alpha) {
    Y[r,c] = 1-Y[r,c]  # flip this bit
  }
  
  return(Y)
}
