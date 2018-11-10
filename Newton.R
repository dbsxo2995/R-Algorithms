#sample mean method 표본평균법 
#Q1. integral 0 to 1 4*sqrt(1-x^2) dx

SM_Method <- function(g, a, b, N) {
  sum_g = 0 
  for(i in 1:N){
    x = runif(1, min= a, max= b)
    sum_g = sum_g + g(x)
  }
  I_SM <- (b-a)* sum_g / N
  return(I_SM)
}

set.seed(1234)  
g <- function(x) {4*sqrt(1-x^2)}
est_SM_method <- rep(0, 100)

for(i in 1:100){
  est_SM_method[i] <- SM_Method(g, 0, 1, 1000)
}

summary(est_SM_method)


#Newton Algorithm
#Q2. x^3 - 7x^2 - 7x -8 = 0 prove

tol = 1e-6 #허용오차
x_new = 10 #seed value

for(i in 1:1000){
  x_old = x_new
  
  fx = x_old^3 - 7*x_old^2 -7*x_old -8 #f(x)
  dfx = 3*x_old^2 - 14*x_old - 7 #df(x)/dx
  
  x_new = x_old - fx/dfx
  change_x = abs((x_new- x_old)/ x_old)
  
  cat(i, x_new, change_x, "\n")
  if (change_x < tol) break
} 
#허용오차가 주어졌을 때, 5번째에서 중단하고 방정식의해는 8


