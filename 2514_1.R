#Jess, Blain, David
#############PROBLEM 1###################
wine <- read.csv('Wine.csv')

summary(wine)

cor(WINE, MORTALITY)
plot(WINE, MORTALITY)

lm(MORTALITY ~ WINE, wine)

qqplot(WINE, MORTALITY)


#############PROBLEM 2###################
####part a####
flowers <- read.csv('flowers.csv')
flowers[,4] <- rep(c(1,2))
names(flowers)[4] <- "REPLICATE"

####part b####
min(flowers$INTENS)
max(flowers$INTENS)
flowers[,5] <- NA
names(flowers)[5] <- "INTENS_CAT"
for (i in 1:nrow(flowers)) {
  if(flowers$INTENS[i] < 275) {
    flowers$INTENS_CAT[i] <- 1
  }
  else if (flowers$INTENS[i] < 400) {
    flowers$INTENS_CAT[i] <- 2
  }
  else if (flowers$INTENS[i] < 525) {
    flowers$INTENS_CAT[i] <- 3
  }
  else if (flowers$INTENS[i] < 650) {
    flowers$INTENS_CAT[i] <- 4
  }
  else if (flowers$INTENS[i] < 775) {
    flowers$INTENS_CAT[i] <- 5
  }
  else { 
    flowers$INTENS_CAT[i] <- 6
  }
}

####part d####
summary(aov(FLOWERS ~ TIME + INTENS_CAT, flowers))
m1 <- lm(FLOWERS ~ TIME + INTENS_CAT, flowers)
####part e####
summary(aov(FLOWERS ~ TIME + INTENS_CAT + TIME*INTENS_CAT, flowers))
m2 <- lm(FLOWERS ~ TIME + INTENS_CAT + TIME*INTENS_CAT, flowers)
####part f####
summary(aov(FLOWERS ~ TIME + INTENS, flowers))
m3 <- lm(FLOWERS ~ TIME + INTENS, flowers)
summary(aov(FLOWERS ~ TIME + INTENS + TIME*INTENS, flowers))
m4 <- lm(FLOWERS ~ TIME + INTENS + TIME*INTENS, flowers)
####part g####
anova(m1, m2, m3, m4)

####part h####
inputs <- data.frame()
