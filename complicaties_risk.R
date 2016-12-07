# vorige groep
# ------------

# aantal jaren observatie
years <- 797
# aantal gebeurtenissen (complicaties)
events <- 78

# verwachtingswaarde frequentie
f_expected <- events / years
print(paste('f_expected:', f_expected))

# ondergrens van het 95% betrouwbaarheidsinterval
f_lower <- qchisq(.025, df=2*events)/2/years
print(paste('f_lower:', f_lower))


# bovengrens van het 95% betrouwbaarheidsinterval
f_upper <- qchisq(.975, df=2*(events+1))/2/years
print(paste('f_upper:', f_upper))

# nieuwe groep

# aantal jaren observatie
years <- 92.3 #130 old value revised later to 92.3
# aantal gebeurtenissen
events <- 4

# verwachtingswaarde frequentie
f_expected <- events / years
print(paste('f_expected:', f_expected))

# ondergrens van het 95% betrouwbaarheidsinterval
f_lower <- qchisq(.025, df=2*events)/2/years
print(paste('f_lower:', f_lower))


# bovengrens van het 95% betrouwbaarheidsinterval
f_upper <- qchisq(.975, df=2*(events+1))/2/years
print(paste('f_upper:', f_upper))
                 

# Monte Carlo way to determine CIs
# old data

n.pompjaren <- 797
n.events <- 78
lambda.pomp <- n.events / n.pompjaren

nmc <- 50000
res <- c()
for (i in 1:nmc) {
  res[i] <- sum(rpois(n = n.pompjaren, lambda = lambda.pomp)) / n.pompjaren
}
print('old data')
quantile(res, c(0.025, 0.975))
mean(res)

# new data
n.pompjaren <- 93
n.events <- 4
lambda.pomp <- n.events / n.pompjaren
nmc <- 50000
res <- c()
for (i in 1:nmc) {
  res[i] <- sum(rpois(n = n.pompjaren, lambda = lambda.pomp)) / n.pompjaren
}
print('old data')
quantile(res, c(0.025, 0.975))
mean(res)

