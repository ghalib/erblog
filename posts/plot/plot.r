library(ggplot2)

## Histogram of standard normal
plot_hist <- function() {
  x <- rnorm(10000)
  df <- data.frame(values = x)
  h <- ggplot(df, aes(x=df$values), environment=environment()) +
    geom_histogram()
  ggsave(h, filename="r_hist.png") 
}

## Line plot of a sine wave
plot_sine <- function() {
  x <- seq(0, 20, 0.1)
  df <- data.frame(values=x)
  s <- ggplot(df, aes(x=df$values, y=sin(df$values)),
              environment=environment()) + geom_line()
  ggsave(s, filename="r_sine.png")
}

## Random scatterplot
plot_scatter <- function() {
  x <- 0.9 * runif(30)
  y <- 0.9 * runif(30)
  df <- data.frame(x=x, y=y)
  sc <- ggplot(df, aes(x=df$x, y=df$y), environment=environment()) +
    geom_point()
  ggsave(sc, filename="r_scatter.png")
}
