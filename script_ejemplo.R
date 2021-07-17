N <- 10000

df <- data.frame(x=c(rnorm(N), rnorm(N, 5, 2), rnorm(N, 10, 3)),
                 y=c(rnorm(N), rnorm(N, 4, 2), rnorm(N, 10, 3)),
                 g=rep(c('A', 'B', 'C'), each=N))

ggplot(df, aes(y, fill=g)) +
  geom_density(alpha=0.8)

