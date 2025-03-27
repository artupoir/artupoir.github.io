sigmoid = function(z) {
  1 / (1 + exp(-z))
}

regresi_logistik = function(X, y, metode = "Newton_Raphson", 
                            max_iter = 100, 
                            toleransi = 1e-4) {
  
  if (!is.matrix(X)) stop("X as matriks")
  if (!is.vector(y)) stop("y as vektor")
  
  n = nrow(X)
  p = ncol(X)
  beta = rep(0, p)
  
  if (metode == "Newton_Raphson") {
    for (iterasi in 1:max_iter) {
      pi = sigmoid(X %*% beta)
      
      gradien = t(X) %*% (y - pi)
      
      W = diag(as.vector(pi * (1 - pi)))
      hessian = t(X) %*% W %*% X
      
      delta_beta = solve(hessian) %*% gradien
      beta_baru = beta + delta_beta
      
      if (max(abs(beta_baru - beta)) < toleransi) break
      
      beta = beta_baru
    }
  } else if (metode == "IRLS") {
    for (iterasi in 1:max_iter) {
      pi = sigmoid(X %*% beta)
      
      z = X %*% beta + (y - pi) / (pi * (1 - pi))
      
      W = diag(as.vector(pi * (1 - pi)))
      
      beta_baru = solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z
      
      if (max(abs(beta_baru - beta)) < toleransi) break
      
      beta = beta_baru
    }
  } else {
    stop("Harus Metode 'Newton_Raphson' dan 'IRLS'")
  }
  
  probabilitas_fit = sigmoid(X %*% beta)
  
  return(list(
    metode = metode,
    beta = beta,
    probabilitas_fit = probabilitas_fit
  ))
}

# How to use
set.seed(123)

# Input data or create data
X = cbind(1, matrix(rnorm(100 * 2), ncol = 2))
y = rbinom(100, 1, 0.5)

# Running Newton-Raphson method
hasil_nr = regresi_logistik(X, y, metode = "Newton_Raphson")
hasil_nr

# Running IRLS method
hasil_irls = regresi_logistik(X, y, metode = "IRLS")
hasil_irls