#Dataset from Surrey Data
newdata <- read.csv("https://data.surrey.ca/dataset/20f169b5-3441-42ca-a4e6-ad300433256d/resource/b5bf9696-6e66-460f-8026-c957fc992f5a/download/waterlevels2015april")

street192 <- newdata[newdata$Station == "192",]
streetmanson <- newdata[newdata$Station == "manson",]

nrow(street192)
nrow(streetmanson)

filternewdata <- newdata[newdata$Station == "192" | newdata$Station == "manson",]

filternewdata$Station <- as.factor(filternewdata$Station)


## Calculations to get the SEM for both variables (Street 192 and Street Manson) before moving on to the t-test

s <- sd(street192$Water.Level.Reading)
n <- nrow(street192)
semstreet192 <- s/sqrt(n)

s2 <- sd(streetmanson$Water.Level.Reading)
n2 <- nrow(streetmanson)
semstreetmanson <- s2/sqrt(n2)

## CI levels for each variable at 95% in order to make a barplot, and before continuing to t-test 

street192.mean <- mean(street192$Water.Level.Reading)
highCIstreet192<- street192.mean+ (1.96 ^semstreet192)
lowCIstreet192 <- street192.mean - (1.96 ^semstreet192)

streetmanson.mean <- mean(streetmanson$Water.Level.Reading)
highCIstreetmanson <- streetmanson.mean + (1.96 ^semstreetmanson)
lowCIstreetmanson <- streetmanson.mean - (1.96 ^semstreetmanson)

## Histogram for each variable, followed by a barplot below to look at the differences between the two variables. Then, carry on to inspect normality. 

hist(x = street192$Water.Level.Reading,
     main = "Histrogram of Street 192 Water Level Readings",
     xlab = "Water Levels",
     ylab = "Frequency",
     ylim = c(0,90),
     )

hist(x = streetmanson$Water.Level.Reading,
     main = "Histogam of Manson Street Water Level Reading",
     xlab = "Water Levels",
     ylab = "Frequency",
     ylim = c(0,80),
)


## Barplot for both variables 

graph.midpoints <- barplot(c(street192.mean, streetmanson.mean), ylim = c(0,1.2))
                                  
title(main = "Water Level Readings", 
      xlab = "Conditions", 
      ylab = "Mean Score",
)

## Added whiskers to the barplot for easier interpretation 

arrows(
  graph.midpoints, c(street192.mean - semstreet192, streetmanson.mean - semstreetmanson),
  graph.midpoints, c(street192.mean + semstreet192, streetmanson.mean + semstreetmanson),
  length = 0.05,
  angle = 90,
  code = 3
)

#Check for normality as described in the textbook

normality.filternewdata <- rnorm( n = 716)

hist(x = normality.filternewdata,
     main = "Histogram of both Station Variables",
     )

qqnorm(y = normality.filternewdata)

skew(normality.filternewdata)

shapiro.test( x = normality.filternewdata)

## The Shapiro-Wilk normality test has a W value of 0.99 which, as per the textbook, indicates non-normality. 


## You then need to determine the effect size/cohens D for comparison before moving forward

street192.deviation.score <- street192$Water.Level.Reading - street192.mean
streetmanson.deviation.score <- streetmanson$Water.Level.Reading - streetmanson.mean

street192.squared.deviation.score <- street192.deviation.score^2
streetmanson.squared.deviation.score <- streetmanson.deviation.score^2

street192.ss <- sum(street192.squared.deviation.score)
streetmanson.ss <- sum(streetmanson.squared.deviation.score)

street192.df <- n-1
streetmanson.df <- n2-1

pooledvariance <- (street192.ss + streetmanson.ss) / (street192.df + streetmanson.df)

cohensd <- (street192.mean - streetmanson.mean) / pooledvariance

#Conduct a t-test

independentSamplesTTest(
  formula = Water.Level.Reading ~ Station,
  data = filternewdata,
  var.equal = TRUE
)

#Interpret the meaning of the results 

## The population means are significantly different between the two variables, so you would reject the null hypothesis. 

# Boxplot

boxplot(c(x = con1, y = con2))

boxplot(newdata$Water.Level.Reading)

title(main = "Boxplot Distribution of Water Level Readings",
      ylab = "Water Level Readings",
      )

boxplot(x = filternewdata$Water.Level.Reading,
        main)
