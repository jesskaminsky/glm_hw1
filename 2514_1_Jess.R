#Jess, Blain, David
#############PROBLEM 1###################
wine <- read.csv('Wine.csv')
attach(wine)
summary(wine)

cor(WINE, MORTALITY)
plot(WINE, MORTALITY)
lm(MORTALITY ~ WINE, wine)
qqplot(WINE, MORTALITY)

cor(log(WINE), log(MORTALITY))
plot(log(WINE), log(MORTALITY))
lm(log(MORTALITY) ~ log(WINE), wine)
qqplot(log(WINE), log(MORTALITY))

### add a column for wine consumption level - a categorical variable
wine$level[1:6] <- "low"
wine$level[7:12] <- "medium"
wine$level[13:18] <- "high"
#look for a significant difference in mean death rate within each level group
#then

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
####part c####
#The research questions are: What are effects of intensity and timing? 
#Is there an interaction between the two factors?

####part d####
#First create an analysis of variance using timing and the categorical 
#form of the light intensity variable. Determine if there is an effect of each factor.
summary(aov(FLOWERS ~ TIME + INTENS_CAT, flowers))
m1 <- lm(FLOWERS ~ TIME + INTENS_CAT, flowers)

####part e####
#Then create an interaction between light intensity and timing by 
#multiplying the two variables and test for the presence of an interaction.
summary(aov(FLOWERS ~ TIME + INTENS_CAT + TIME*INTENS_CAT, flowers))
m2 <- lm(FLOWERS ~ TIME + INTENS_CAT + TIME*INTENS_CAT, flowers)

####part f####
#Now repeat the process but using light intensity as a continuous variable.
summary(aov(FLOWERS ~ TIME + INTENS, flowers))
m3 <- lm(FLOWERS ~ TIME + INTENS, flowers)
summary(aov(FLOWERS ~ TIME + INTENS + TIME*INTENS, flowers))
m4 <- lm(FLOWERS ~ TIME + INTENS + TIME*INTENS, flowers)

####part g####
#Then perform F-tests to compare the four model you have created 
#(light as continuous and categorical with and without the interaction)
anova(m1, m2, m3, m4)

####part h####
#Predict the number of flowers grown at each combination 
#of light and timing for each of the four models.
intenscat_inputs <- data.frame(TIME = c(rep(1,6), rep(2,6)), INTENS_CAT = c(rep(seq(1,6), 2)))
m1_predict <- predict(m1, intenscat_inputs)
m2_predict <- predict(m2, intenscat_inputs)

intens_inputs <- data.frame(TIME = c(1,1,1,1,1,1,2,2,2,2,2,2), INTENS = c(150,300,450,600,750,900,150,300,450,600,750,900))
m3_predict <- predict(m3, intens_inputs)
m4_predict <- predict(m4, intens_inputs)

####part i####
#Compare each prediction to the observed number of flowers and calculate the 
#difference (observed – predicted). This is the residual. Calculate the residual mean
#squared error for each model by adding the squared residuals together and dividing by 
#the number of residual degrees of freedom. This should equal the mean squared error in each 
#ANOVA table.

####part j####
#Now plot the residuals vs. the predicted for each model and see if there are any patterns. 
#If you see any, what might you do to remove them?

####part k####
#Finally, take the model you think describes the data the best and write a short report for 
#your grandmother who would like to grow these flowers carefully explaining to her how she 
#should best grow them and why. Note that your grandmother is curious about how much changes 
#in light and timing might affect her flowers and how sensitive her results will be to the 
#settings she makes.

