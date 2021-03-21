#setting working directory
current_wd <- getwd()
setwd(current_wd)

#read the .csv file
europeanSalesData <- read.csv("EuropeanSales.csv", header = TRUE)

#dropping down the 'Country' column for calculation of correlation
#matrix and further exploratory analysis
europeanSDcor <- subset(europeanSalesData, select = -c(Country))

#correlation matrix
cor(europeanSDcor[,])

#scatter diagram
plot(europeanSDcor)

#I used Performance Analytics package for this section
#it has a quite handy correlation chart, the bottom left of the chart
#consists of scatter diagrams with a fitted line
#and the diagonal consists of the histograms of the attributes
chart.Correlation(europeanSDcor, histogram = TRUE, method = "pearson")
#this second correlation chart is to show the effect of the ln transformation
#on the data set.it transforms the distributions more akin to normal distribution.
chart.Correlation(log1p(europeanSalesData[2:7]), histogram = TRUE, method = "pearson")

#here I create interaction terms which are later to be used in the models
#the first block is for SalesPerCapita model
europeanSDint <- subset(europeanSalesData, select = -c(Country))
europeanSDint$PGCi <- (europeanSDint$Population)*
                      (europeanSDint$GDPperHead)*
                      (europeanSDint$ComputerSales)
europeanSDint$PGi <-  (europeanSDint$Population)*
                      (europeanSDint$GDPperHead)
europeanSDint$PCi <-  (europeanSDint$Population)*
                      (europeanSDint$ComputerSales)
europeanSDint$GCi <-  (europeanSDint$GDPperHead)*
                      (europeanSDint$ComputerSales)

#the second block is for ComputerSales model
europeanSDint2 <- subset(europeanSalesData, select = -c(Country))
europeanSDint2$PGSi <- (europeanSDint2$Population)*
                       (europeanSDint2$GDPperHead)*
                       (europeanSDint2$SalesPerCapita)
europeanSDint2$PGi <- (europeanSDint2$Population)*
                      (europeanSDint2$GDPperHead)
europeanSDint2$PSi <- (europeanSDint2$Population)*
                      (europeanSDint2$SalesPerCapita)
europeanSDint2$GSi <- (europeanSDint2$GDPperHead)*
                      (europeanSDint2$SalesPerCapita)

#train-test splitting the dataframe for SalesPerCapita model
smp_size2 <- floor(0.80 * nrow(europeanSDint))
set.seed(42)
train_ind2 <- sample(seq_len(nrow(europeanSDint)), size = smp_size2)

train_SDint <- europeanSDint[train_ind2,]
test_SDint <- europeanSDint[-train_ind2,]

#train-test splitting the dataframe for ComputerSales model
smp_size3 <- floor(0.80 * nrow(europeanSDint2))
train_ind3 <- sample(seq_len(nrow(europeanSDint2)), size = smp_size3)

train_SDint2 <- europeanSDint2[train_ind3,]
test_SDint2 <- europeanSDint2[-train_ind3,]

#here is the ComputerSales best performing model
model21 <- lm(ComputerSales ~ PGi + PSi, data = train_SDint2)
summary(model21)
#The best performing model consists of two interaction term attributes.
#These interaction terms were created beforehand.

#test part of the data is used for predictions which are later evaluated.
europeanSDint2_predictions <- predict(model21, test_SDint2)
ggplot_df2 <- subset(test_SDint2, select = c(ComputerSales))
ggplot_df2$Predictions <- europeanSDint2_predictions

#initial evaluation of the test set with a visualization of how well it performed
ggplot(ggplot_df2, aes(x = Predictions, y = ComputerSales)) +
      geom_point() +
      geom_abline(color = "darkblue") +
      ggtitle("ComputerSales vs Model Predictions")

#best performing model for SalesPerCapita is this
model23 <- lm(log1p(SalesPerCapita) ~ log10(Population) + log1p(GDPperHead) +log1p(ComputerSales) +
                log1p(GCi), data = train_SDint)
summary(model23)

#test set is used for predictions for SalesPerCapita model
europeanSDint3_predictions <- predict(model23, test_SDint)
ggplot_df3 <- subset(log1p(test_SDint), select = c(SalesPerCapita))
ggplot_df3$Predictions <- europeanSDint3_predictions

#again a visualization of how well the model performed on test set
ggplot(ggplot_df3, aes(x = Predictions, y = SalesPerCapita)) +
  geom_point() +
  geom_abline(color = "darkblue") +
  ggtitle("SalesPerCapita vs Model Predictions")

#defining R-Squared and Adjusted R-Squared functions for further evaluation of
#the test set.
rsq_test <- function(x,y) cor(x,y)^2
arsq_test <- function(x,y) {
  tmp_ar <- (1-((1-x)*(nobs(y)-1)/(nobs(y)-length(y$coefficients)-2)))
  return(tmp_ar)
}

#R-Squared and Adjusted R-Squared calculations for model23 (SalesPerCapita model)
rsq_m23_test <- rsq_test(ggplot_df3$SalesPerCapita, ggplot_df3$Predictions)
arsq_m23_test <- arsq_test(rsq_m23_test, model23)

#R-Squared and Adjusted R-Squared calculations for model 21 (ComputerSales model)
rsq_m21_test <- rsq_test(ggplot_df2$ComputerSales, ggplot_df2$Predictions)
arsq_m21_test <- arsq_test(rsq_m21_test, model21)

#Findings: Both models have performed outstandingly in both the training and the
#test sets. The R-Squared and Adjusted R-Squared values of both the training sets
#of each model are 1 which is perfect, and the residual standard error is also
#quite low on each of them. Test set R-Squared and Adjusted R-Squared values are
#also close to 1 which means that the model doesn't overfit and is quite simple
#in terms of attributes that were used in the model. This is I think mainly due
#to the fact that the dataset is quite small (miniscule infact) that the models
#had an outshining performance.
