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


#단일품목 판매 통계적 모의실험
##################################################################
#한 신물팔이 소년이 신문 1부에 150원으로 사서 300원에 판매한다.###
#팔지못한 신문은 1부당 100원에 환불받으며, 돈을 벌고 있다. 이 때##
#10부 단위로 신문을 받아온다고 할 때, 매일 몇부씩 받아오는것이 ###
#최대의 이익이 될것인가? 다음은 100일동안 매일의 판매 부수를 조사#
#하여 확률 표로 나타내었다.                                      #
#X : 10 20 30 40 50 60 70 80 90 판매부수 #
#P(x) 4 6  12 20 25 18 10  3  2 단위(%)  #


result <- matrix(0, nrow = 9, ncol = 2)
sum_profit <- matrix(0, nrow = 9, ncol = 1)
colnames(result) <- c("구입","이익")
F <- c(0,00, 0,04, 0.10, 0.22, 0.42, 0.67, 0.85, 0.95, 0.98, 1.00) #누적확률
set.seed(12345)

for(i in 1:9){
  buy = 10 * i
  for(day in 1:1000){
    u = runif(1, 0, 1)
    for(j in 1:9){
      sale = 10 * j
      if(u >= F[j]){if(buy >= sale) profit = ((150 * sale) - (50 * (buy - sale))) else profit = 150 * buy}
    }
  }
  sum_profit[i] = sum_profit[i] + profit
  result[i, 1] <- buy
  result[i, 2] <- sum_profit[i]
}
result



