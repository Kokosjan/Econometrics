library("HSAUR") 
library("dplyr") 
library("psych") 
library("lmtest")
library("glmnet") 
library("ggplot2") 
library("car") 

# load data
h <- cars
qplot(data=h, speed, dist)

# add squares and cubes
h <- mutate(h, speed2=speed^2, speed3=speed^3)


# regression
model_mcl <- lm(data=h, dist~speed+speed2+speed3)
summary(model_mcl)
# see the multicollinearity

# variance inflation factor
vif(model_mcl) 

# new data point for prediction
nd <- data.frame(speed=10,speed2=100,speed3=1000)
predict(model_mcl,newdata=nd,interval="prediction")

# for comparisson with no multicollinearity
model <- lm(data=h, dist~speed)
predict(model,newdata=nd,interval="prediction")

# confidence intervals
confint(model) 
confint(model_mcl) 


# Ridge and LASSO regs
y <- h$dist
X0 <- model.matrix(data=h, dist~0+speed+speed2+speed3)

# lambdas for LASSO and Ridge
lambdas <- seq(50,0.1,length=30)

# LASSO reg
m_lasso <- glmnet(X0,y,alpha=1, lambda=lambdas)

# lambda dependance of coeffs
plot(m_lasso,xvar="lambda",label=TRUE)

# Ridge reg
m_rr <- glmnet(X0,y,alpha=0, lambda=lambdas)

# cross-validation
cv <- cv.glmnet(X0,y,alpha=1)
plot(cv)


# Principal Components Analysis
h <- heptathlon # load another data
help(heptathlon)
glimpse(h) 
h <- select(h,-score) # remove final score
describe(h) 

# correlation
cor(h)

# principal components analysis with variable scaling
h.pca <- prcomp(h,scale=TRUE)

# first PC
pca1 <- h.pca$x[,1]
head(pca1)

# weights of variables in fisrt PC
w1 <- h.pca$rotation[,1]
w1

summary(h.pca)
plot(h.pca)
biplot(h.pca,xlim=c(-1,1))