Ele <- c(180,305,381,488,549,640,762,883)
Ele <- Ele /1000
Temp <- c(13.3,12.2,13.3,10.0,8.3,9.4,8.3,7.2)

plot(Ele, Temp, xlab = 'Elevation(km)', ylab = 'Temperature(degrees C)',
     pch = 20,
     cex = 2,
     col = "grey")
fit <- lm(Temp~Ele)
summary(fit)
abline(fit, lwd = 2, col = "red")