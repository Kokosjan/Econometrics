library("memisc")
library("lmtest")
library("ggplot2")
library("dplyr")
library("foreign")
library("vcd")
library("devtools")
library("hexbin")
library("pander")
library("sjPlot")
library("knitr")

# load data
h <- diamonds
glimpse(h)
help(diamonds)

# diagram hexagons
qplot(data=h, carat, price)
bg <- qplot(data=h, log(carat), log(price))
bg + geom_hex() 


# load flat's price data
f <- read.csv("flats_moscow.txt", sep="\t", header=TRUE, dec=".")
glimpse(f) 
qplot(data=f, totsp, price) 
qplot(data=f, log(totsp), log(price)) 
mosaic(data=f, ~walk+brick + floor, shade=TRUE)

# mutate walk, brick, floor, code into factors
f <- mutate_each(f, "factor", walk, brick, floor, code)
qplot(data=f, log(price), fill=brick, position="dodge") 
# density functions
g2 <- qplot(data=f, log(price), fill=brick, geom="density", alpha=0.5)

# three regressions
model_0 <- lm(data=f, log(price)~log(totsp))
model_1 <- lm(data=f, log(price)~log(totsp)+brick)
model_2 <- lm(data=f, log(price)~log(totsp)+brick+brick:log(totsp))
# ":" is a product operation
sjp.lm(model_2)

# create a new data for prediction
nw <- data.frame(totsp=c(60,60), brick=factor(c(1,0)))
nw

# confidence interval
exp(predict(model_2, newdata=nw, interval="confidence"))

# prediction interval
exp(predict(model_2, newdata=nw, interval="prediction"))

# F-тест
waldtest(model_0, model_1) # H_0: model_0 H_a: model_1 
# H_0 rejected

# add regression to diagram
gg0 <- qplot(data=f, log(totsp), log(price))
gg0 + stat_smooth(method="lm")
gg0 + stat_smooth(method="lm") + facet_grid(~walk)
gg0 + aes(col=brick) + stat_smooth(method="lm") + facet_grid(~walk)

# RESET test
resettest(model_2)
