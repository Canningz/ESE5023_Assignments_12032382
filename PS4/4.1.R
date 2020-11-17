#1
library(ggplot2)
Data<-midwest
theme_set(theme_classic())
# Plot
g <- ggplot(midwest, aes(state, percprof))
g + geom_boxplot(varwidth=T, fill="yellow") + 
  labs(title="Box plot", 
       subtitle="Percent profession grouped by different state",
       caption="Source: midwest",
       x="State",
       y="Percent profession")

#2
library(quantmod)
library(ggplot2)
getSymbols('^SSEC',src='yahoo',from = '1997-01-01')
close <- (Cl(SSEC))
time <- index(close)
value <- as.vector(close)
ggplot(data.frame(time,value),aes(time,value))+
  geom_line(colour='green')+
  geom_line(colour='green') + 
  geom_area(colour='green',alpha=0.3)+
  labs(title = "The Value")+
  xlab('The Time Series of Date')+ 
  ylab('The Time Series of Value')+
  geom_smooth(method = "loess")+
  theme_bw()

#3
library(ggplot2)
theme_set(theme_classic())

# Histogram on a Categorical variable
g <- ggplot(midwest, aes(category))
g + geom_bar(aes(fill=state), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="category across state") 

#4
# turn-off scientific notation like 1e+48
options(scipen = 999)
library(ggplot2)
library(ggalt)
h <- mpg
mpg_select <- mpg[mpg$year > 1999 & 
                    mpg$year <= 2008 & 
                    mpg$displ > 1 & 
                    mpg$displ < 9, ]

# Plot
ggplot(mpg, aes(x=displ, y=year)) + 
  geom_point(aes(col=class, size=cyl)) +   # draw points
  geom_smooth(method="loess", se=F) + 
  xlim(c(1, 8)) + 
  ylim(c(1998, 2015)) +   # draw smoothing line
  geom_encircle(aes(x=displ, y=year), 
                data=mpg_select, 
                color="red", 
                size=0.5, 
                expand=0.08) +   # encircle
  labs(subtitle="year Vs displ", 
       y="year", 
       x="displ", 
       title="Scatterplot + Encircle", 
       caption="Source: mpg")

#5
library(ggplot2)
library(forecast)
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity))
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "fill")
ggplot(data = diamonds) + 
  geom_bar(mapping = aes(x = cut, fill = clarity), position = "dodge")
bar <- ggplot(data = diamonds) + 
  geom_bar(
    mapping = aes(x = cut, fill = cut), 
    show.legend = FALSE,
    width = 1
  ) + 
  theme(aspect.ratio = 1) +
  labs(x = NULL, y = NULL)

bar + coord_flip()
bar + coord_polar()
