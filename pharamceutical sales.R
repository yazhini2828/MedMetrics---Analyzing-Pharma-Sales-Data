#ANOVA
crop.data <- read.csv("averages1.csv", header = TRUE, colClasses = c("factor","numeric"))
print(crop.data)
summary(crop.data)
one.way <- aov(VOLUME~TYPE , data = crop.data)
summary(one.way)
TukeyHSD(one.way)
#---------------------------------------------------------------------------------------------------------------
install.packages('multcomp')

library(multcomp)

#general linear hypothesis test ,specification of linear hypothesis to be tested, multiple change points
summary(glht(one.way, linfct = mcp(TYPE = "Tukey")))
pairwise.t.test(crop.data$VOLUME, crop.data$TYPE,
                p.adjust.method = "BH")  #benjamin hochberg method
plot(one.way, 1)
#----------------------------------------------------------------------------------------------------------------
#companion to applied regression
install.packages('car')
library(car)
leveneTest(VOLUME ~ TYPE, data = crop.data)
plot(one.way, 2)


#---------------------------------------------------------------------------------------------------------------
### NON PARAMETRIC TESTS ###

DET<-read.csv('salesdailydata.csv')
DET
before<-print(DET[1097:1461, 2])  #data in 2017
after<-print(DET[1462:1826, 2])   #data in 2018
summary(DET)

boxplot(after)
boxplot(before)

signCounts = function(before, after) {
  n <- length(before)
  result <- vector(mode="integer", length=3)
  # [1] = num neg (fail), [2] = num zero, [3] = num pos (success)
  for (i in 1:n) {
    if (before[i] - after[i] < 0) {
      result[1] <- result[1] + 1
    }
    else if (before[i] - after[i] > 0) {
      result[3] <- result[3] + 1
    }
    else {
      result[2] <- result[2] + 1
    }
  }
  return(result)
}
signCounts = function(before, after) {
  result <- vector(mode="integer", length=3)
  signedVals <- before - after
  result[1] <- sum(signedVals < 0)
  result[2] <- sum(signedVals = 0)
  result[3] <- sum(signedVals > 0)
  return(result)
} 
cat("Before: ", before, "\n")
cat("After : ", after, "\n\n")
sc <- signCounts(before, after)
numFail <- sc[1]
numZero <- sc[2]
numSucc <- sc[3]
N <- numFail + numSucc  # ignoring sign=0 cases
X <- numSucc

cat("Num success = ", numSucc, "\n")
cat("Num failure = ", numFail, "\n")
cat("\nExecuting binom.test \n")
model <- binom.test(x=X, n=N, alternative="greater")  # 'exact'

cat("The prob of 'no effect' is ", model$p.value, "\n")
cat("So the prob there was 'an effect' is ", 1 - model$p.value, "\n")


#---------------------------------------------------------------------------------------------------------------------
### CONTROL CHARTS ###
# Vector of 31 subgroup sizes (average = 15.5)
sizes <- rpois(31, 15.5)

# Vector of dates identifying subgroups
datum <- seq(as.Date('2018-01-01'), length.out = 31, by = 'day')
datum <- rep(datum,sizes)

# Vector of volume of N02BE
N02BE <- round(rnorm(sum(sizes), 37, 16))

# Data frame of N02BE volume and dates
d <- data.frame(N02BE, datum)
head(d, 31)

install.packages('qicharts2')
library(qicharts2)

#X BAR CHART
qic(N02BE, 
    x     = datum, 
    data  = d,
    chart = 'xbar',
    ylab  = 'millilitre',
    xlab  = 'Date')


#S CHART
qic(N02BE, 
    x = datum, 
    data = d,
    chart = 's',
    ylab = 'millilitre',
    xlab = 'Date')



a<-print(DET[1097:1461, 2]) 
b<-print(DET[1462:1826, 2])   
boxplot(a)


#--------------------------------------------------------------------------------------------------------------
#quantity index
library(IndexNumber)
data<-read.csv('ctrlchartsdata.csv')
quantity<-print(data[2:31, 2])
index.number.serie(quantity,name="quantity",opt.plot=TRUE,opt.summary=FALSE)

#-----------------------------------------------------------------------------------------------------------------
##twoway anova

##You can use a two-way ANOVA to 
##find out if medicine type and Production have an effect on average medicine sales
crop.data<-read.csv("twowayanova.csv")
head(crop.data)
str(crop.data)
crop.data$PRODUCTION <- factor(crop.data$PRODUCTION, 
                               levels = c(6.1, 7, 8),
                               labels = c("A1", "B1", "C1"))

table(crop.data$TYPE, crop.data$PRODUCTION)


two.anova<-aov(VOLUME.S ~ TYPE + PRODUCTION, data = crop.data)
summary(two.anova)

#-------------------------------------------------------------------------------------------------------------------
##NON PARAMETRIC- WILCOXON TEST##
install.packages("ggpubr")
library("ggpubr")
wilco<-read.csv("wilcoxondata.csv")
ggdensity(wilco$M01AB.1., 
          main = "Volume of M01AB IN THE YEAR 2017",
          xlab = "VOLUME")

wilco<-read.csv("wilcoxondata.csv")
ggdensity(wilco$M01AB.2., 
          main = "Volume of M01AB IN THE YEAR 2018",
          xlab = "VOLUME")

library("MASS")
wilco<-read.csv("wilcoxondata.csv")
head(wilco)
wilcox.test(wilco$M01AB.1.,wilco$M01AB.2.,alternative="two.sided")

#-----------------------------------------------------------------------------------------------------------------
#trend line fitting#
install.packages("rmeta")
library("rmeta")
data<-read.csv("averages1.csv")
data
plot(data$TYPE,data$VOLUME,type = "l")
my_mod <- lm(VOLUME ~ TYPE, data)
plot(data$TYPE,                       # Draw line plot with regression line
     data$VOLUME,
     type = "l")
lines(data$TYPE,
      predict(my_mod),
      col = 2,
      lwd = 2)
my_coef <- coef(my_mod)            # Extract coefficients of model
my_coef 
my_equation <- paste("y =",        # Extract equation of model
                     coef(my_mod)[[1]],
                     "+",
                     coef(my_mod)[[2]],
                     "* x")
my_equation