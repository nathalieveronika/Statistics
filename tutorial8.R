# TUTORIAL 8: 
#   
#   Criticl regions given
# 1) Find probability of type I error
# 2) Find analytical expression for the power function
# 3) Produce seperate plotes of these power functions 
# 
# To plot the power functiony ou may find it
# useful to use the curve function in R
# The following code illustrates how curve can be used
# 
# xpower <- function(x,power) {
#   x^power
# }
# curve(xpower(x,power=2), xlim=c(-1,1))

mu_0 <- 0
sigma <- 1
alpha <- 0.05 
n <- 50
# Plot critical region 1
c_minus <- function(x, sigma, mu_0, alpha, n) {
  f <- pnorm(-qnorm(1-alpha) + (mu_0 - x)/(sigma/sqrt(n)))
  return(f)
}
curve(c_minus(x, sigma=1, mu_0=0, alpha=0.05, n=50), xlim=c(-1,1))

# plot critical region 2
c_plus <- function(x, sigma, mu_0, alpha, n) {
  f <- 1 - pnorm(qnorm(1-alpha) + (mu_0 - x)/(sigma/sqrt(n)))
  return(f)
}
curve(c_plus(x, 1, 0, 0.05, 50), xlim=c(-1,1))

# plot critical region 3
c_star <- function(x, sigma, mu_0, alpha, n) {
  f <- 1 - pnorm(qnorm(1-alpha/2) + (mu_0 - x)/(sigma/sqrt(n))) + pnorm(-qnorm(1-alpha/2) + (mu_0 - x)/(sigma/sqrt(n)))
  return(f)
}
curve(c_star(x, 1, 0, 0.05, 50),  xlim=c(-1,1))


# from plots you see that: 
# c_minus has good power when mu < 0,
# c_plus has good power when mu > 0,
# c_star has good power but is not uniformly most powerful
# since, for example the test based on c_minus has uniformley
# better power for mu < 0 or the test on c_plus has uniformly
# better power for mu > 0 
# we see that the two sided test only has slightly lower 
# than the most powerful on-sided test


