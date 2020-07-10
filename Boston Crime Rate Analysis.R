#############################Boston Housing Data############################

#crim per capita crime rate by town.
#zn proportion of residential land zoned for lots over 25,000 sq.ft.
#indus proportion of non-retail business acres per town.
#chas Charles River dummy variable (= 1 if tract bounds river; 0 otherwise).
#nox nitrogen oxides concentration (parts per 10 million).
#rm average number of rooms per dwelling.
#age proportion of owner-occupied units built prior to 1940.
#dis weighted mean of distances to five Boston employment centres.
#rad index of accessibility to radial highways.
#tax full-value property-tax rate per \$10,000.
#ptratio pupil-teacher ratio by town.
#black 1000(Bk − 0.63)2 where Bk is the proportion of blacks by town.
#lstat lower status of the population (percent).
#medv median value of owner-occupied homes in \$1000s.

library(MASS)
library(ggplot2)
library(reshape2)
library(corrplot)
library(plotly)
library(dplyr)
require(ModelMetrics)
####################Data are initially stored in MASS package##################
Boston = Boston
names(Boston) ##506 rows,14 columns
dim(Boston)
summary(Boston)
#Mean crime rate: 3.61
#Median crime rate: 0.26


############################Visiualize by ggplot2 and plotly###############################

###Find correlations - Method 1
bosmelt <- melt(Boston, id="crim")
ggplot(bosmelt, aes(x=value, y=crim))+
  facet_wrap(~variable, scales="free")+
  geom_point()+geom_smooth(method=lm, color="green")
#Positive: indus/nox/age/rad/tax/ptratio/lstat
#Negative: zn/chas/rm/dis/black/mediv

###Find correlations - Method 2
corr_matrix<-cor(Boston)
corrplot(corr_matrix, type="upper")

###Crime Rate by Home Value
ggplot(Boston, aes(x=medv, y=crim)) + 
  geom_point()+
  geom_smooth(method=lm, color="green")+
  labs(title="Crime Rate by Home Value",
       x='Median value of owner-occupied homes', y = "Crime Rate")+
  theme_classic() 

###Find suburbs with high crime rate (higher than 80%)
quantile(Boston$crim, .80)
hcrim<-subset(Boston, crim>quantile(Boston$crim, .80))
sum(Boston$crim>quantile(Boston$crim, .80)) #101 dangerous suburbs
plot_ly(data=Boston, y = ~lstat, name = "Boston", type="box")  %>%
  add_boxplot(data=hcrim, y= ~lstat, name = "Area with 80th percentile crime rate", type="box")
#Obviously, suburbs with more lower status of the population tend to have
#higher chance of crime rate

###Find the relationship between crime rate and median house value
plot_ly(data = Boston,y=~medv,name='Boston',type='box') %>%
  add_boxplot(data=hcrim,y=~medv,name='Area with 80th percentile crime rate',type='box')
#Suburbs with high crime rate tend to have lower median house value 
#(37% lower than Boston Median)

###Find the relationship between crime rate and weighted mean of distances to five Boston employment centres
plot_ly(data = Boston,y=~dis,name = 'Boston',type = 'box') %>%
  add_boxplot(data = hcrim,y=~dis,name='Area with 80th percentile crime rate',type='box')
#Closer to five Boston employment centers, higher crime rate

###Information about lower class
ggplot(data = Boston, aes(x=lstat)) +
  geom_histogram( binwidth=1, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  ggtitle("Bin size = 1") +
  theme(plot.title = element_text(size=15))
ggplot(data = Boston, aes(x=lstat,y=age))+
  geom_point()+geom_abline()
#More lower class in the community, more old houses built prior to 1940
ggplot(data = Boston, aes(x=lstat,y=black))+
  geom_point()+geom_abline()
#More lower class in the community, higher proportion of black
hlstat = subset(Boston,lstat>quantile(Boston$lstat,0.8))
count(hlstat)
summary(hlstat)
plot_ly(data=Boston, y = ~nox, name = "Boston", type="box")  %>%
  add_boxplot(data=hlstat, y= ~nox, name = "Area with 80th percentile lstat", type="box")
#More pollution in lower class community
plot_ly(data=Boston, y = ~zn, name = "Boston", type="box")  %>%
  add_boxplot(data=hlstat, y= ~zn, name = "Area with 80th percentile lstat", type="box")
plot_ly(data=Boston, y = ~rm, name = "Boston", type="box")  %>%
  add_boxplot(data=hlstat, y= ~rm, name = "Area with 80th percentile lstat", type="box")
#Less proportion of land for lots and rooms in lower class community


#####################Run Regression and Predict Median House Price############################
any(is.na(Boston))
Boston$chas = as.factor(Boston$chas)
summary(lm(crim~.,data = Boston))
#zn+dis+rad+black+medv are significant
summary(lm(crim~zn+dis+rad+black+medv,data = Boston))

set.seed(123)
train_index<-sample(seq_len(nrow(Boston)), size=floor(0.8*nrow(Boston)))
train = Boston[train_index,]
test = Boston[-train_index,]
lm1 = lm(medv~lstat,data = train)
summary(lm1) #RMSE of train set:6.154
evaluate = predict(lm1,test)
rmse(evaluate,test[,14]) #RMSE of test set: 6.46
test$pred = evaluate
test$diff = abs(test$pred - test$medv)
ggplot(test, aes(x=diff)) + 
  geom_histogram(color="black", fill="red")+
  geom_vline(aes(xintercept=mean(diff)), color="blue", linetype="dashed", size=1)
#mean difference between prediction and actual value is 5

#####Use non-linear regression again
Test = Boston[-train_index,]
lm2 = lm(medv~age+lstat+I(lstat^2),data = train)
summary(lm2) #RMSE of train set:5.21
evaluate2 = predict(lm2,Test)
rmse(evaluate2,Test[,14]) #RMSE of test set: 5.71
#Non-linear regression makes a huge progress in RMSE,which fits data better
Test$pred = evaluate2
Test$diff = abs(Test$pred - Test$medv)
ggplot(Test, aes(x=diff)) + 
  geom_histogram(color="black", fill="red")+
  geom_vline(aes(xintercept=mean(diff)), color="blue", linetype="dashed", size=1)
#mean difference between prediction and actual value is 4
