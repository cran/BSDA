"normarea" <-
function(lower =  - Inf, upper = Inf, mu, sigma)
{
    # For R 
    Altblue <- "#A9E2FF"
    area <- pnorm(upper, mu, sigma) - pnorm(lower, mu, sigma)
    x <- seq(mu - 4 * sigma, mu + 4 * sigma, length = 1000)
    y <- dnorm(x, mu, sigma)
    par(pty = "m")
    plot(x, y, type = "n", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    title(main = paste("The area between", lower, "and", upper, "is",
        round(area, 4)), sub = paste(
        "Normal distribution with a mean of", mu, 
        "and a standard deviation of", sigma))
    if(lower ==  - Inf || lower < mu - 4 * sigma) {
        lower <- mu - 4 * sigma
    }
    if(upper == Inf || upper > mu + 4 * sigma) {
        upper <- mu + 4 * sigma
    }
    axis(1, at = c(mu, lower, upper), labels = c(mu, lower, upper))
    xaxis1 <- seq(lower, upper, length = 200)
    yaxis1 <- dnorm(xaxis1, mu, sigma)
    xaxis1 <- c(lower, xaxis1, upper)
    yaxis1 <- c(0, yaxis1, 0)
    polygon(xaxis1, yaxis1, density = -1, col = Altblue)
    lines(x, y, lwd = 2)
    lines(c(mu - 4 * sigma, mu + 4 * sigma), c(0, 0), lwd = 2)
    lines(c(lower, lower), c(0, dnorm(lower, mu, sigma)), lwd = 2)
    lines(c(upper, upper), c(0, dnorm(upper, mu, sigma)), lwd = 2)
}

