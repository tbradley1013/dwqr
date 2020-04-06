#===============================================================================
# Nitrification Potential Curves
# See:
# https://doi.org/10.1002/j.1551-8833.2005.tb07453.x
# https://doi.org/10.1002/j.1551-8833.2008.tb09749.x
#
# Tyler Bradley
# 2020-04-06
#===============================================================================

get_kinetic_conts <- function(tcl_1, fa_1, tcl_2, fa_2, max_ks = 0.1){
  if (!is.numeric(tcl_1)) stop("tcl_1 must be numeric")
  if (!is.numeric(fa_1)) stop("fa_1 must be numeric")
  if (!is.numeric(tcl_2)) stop("tcl_2 must be numeric")
  if (!is.numeric(fa_2)) stop("fa_2 must be numeric")

  obj_in <- c(fa_2, -1*tcl_2)

  const_mat <- matrix(c(fa_1, -1*tcl_1, 1, 0, 1, 0, 0, 1), nrow = 4, byrow = TRUE)

  const_1 <- fa_1*tcl_1
  const_2 <- tcl_1
  const_3 <- tcl_2
  const_4 <- max_ks

  const_rhs <- c(const_1, const_2, const_3, const_4)

  const_dir <- c("=", ">=", "<=", "<=")

  opt <- lpSolve::lp(direction = "max", obj_in, const_mat, const_dir, const_rhs, compute.sens = 5)

  return(opt)

}

