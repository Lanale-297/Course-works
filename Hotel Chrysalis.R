install.packages("caret")
install.packages("ISLR")
install.packages("tidyverse")
install.packages("plotrix")
library(plotrix)
library(caret)
library(ISLR)
library(tidyverse)
ncol(Rate)
nrow(Rate)
summary(Rate)
# Create a boxplot for the "score" variable and confident interval.
boxplot(Rate$Score, main = "Distribution of Score", ylab = "Score", xlab = "Score", col = "purple", boxwex = 1, outline = TRUE)
summary(Rate$Score)
meanscore <- mean(Rate$Score)
sdscore <- sd(Rate$Score)
Varscore <- var(Rate$Score)
st_errorScore <- std.error(Rate$Score)
confidence_intervalScore <- t.test(Rate$Score, conf.level = 0.95)$conf.int
meanscore
sdscore
Varscore
st_errorScore
confidence_intervalScore
## create barplot for score and user continent.
mean_score_by_continent <- aggregate(Score ~ User_continent, data = Rate, mean)
Continent<-barplot(mean_score_by_continent$Score, names.arg = mean_score_by_continent$User_continent,
                   xlab = "User Continent", ylab = "Mean Score", main = "Mean Score by User Continent",col = blues9,
                   ylim = c(0, max(mean_score_by_continent$Score) * 1.2))
text(Continent,mean_score_by_continent$Score, labels = round(mean_score_by_continent$Score, digits = 2))
##  Travel type
mean_score_by_traveller <- aggregate(Score ~ Traveler_type, data = Rate, mean)
Traveler <- barplot(mean_score_by_traveller$Score,names.arg = mean_score_by_traveller$Traveler_type, xlab = "Traveler Type", ylab = "Mean Score", main = "Mean Score by Traveler Type", col= "#4d8c57", ylim = c(0, max(mean_score_by_continent$Score) * 1.2))
text(Traveler,mean_score_by_traveller$Score, labels = round(mean_score_by_traveller$Score, digits = 2))
## Period of Stay
boxplot(data=Rate, Score ~ Period_of_stay, main = "Distribution of Score by Period of Stay", ylab = "Score", xlab = "Period of Stay", col = "#ffb6c1", boxwex = 0.5, outline = TRUE)
## number of reviews
mean_review_by_stars <- aggregate(Nr.reviews ~ Hotel_stars, data = Rate, mean)
mean_htreview_by_stars <- aggregate(Nr.hotel_reviews ~ Hotel_stars, data = Rate, mean)
mean_votes_by_stars <- aggregate(Helpful_votes ~ Hotel_stars, data = Rate, mean)
par(mfrow = c(1, 3))
hotelreview <-barplot(mean_htreview_by_stars$Nr.hotel_reviews, names.arg = mean_htreview_by_stars$Hotel_stars, ylab = "Mean number hotel review", main = "Mean Hotel review by Hotel Stars", col= "#4d8c57",ylim =c(0, max(mean_review_by_stars$Nr.reviews) * 1.2) )
text(hotelreview,mean_htreview_by_stars$Hotel_stars, labels = round(mean_htreview_by_stars$Nr.hotel_reviews, digits = 2))
helpfulvotes <-barplot(mean_votes_by_stars$Helpful_votes,names.arg = mean_votes_by_stars$Hotel_stars, ylab = "Mean number helpful votes", xlab = "Hotel stars", main = "Mean helpful votes by Hotel Stars", col= "#4d8c57",ylim =c(0, max(mean_review_by_stars$Nr.reviews) * 1.2))
text(helpfulvotes,mean_votes_by_stars$Hotel_stars, labels = round(mean_votes_by_stars$Helpful_votes, digits = 2))
reviews <- barplot(mean_review_by_stars$Nr.reviews,names.arg = mean_review_by_stars$Hotel_stars, main = "Mean Number reviews by Hotel Star", ylab = "Mean number of review", col= "#4d8c57", ylim =c(0, max(mean_review_by_stars$Nr.reviews) * 1.2))
text(reviews,mean_review_by_stars$Hotel_stars, labels = round(mean_review_by_stars$Nr.reviews, digits = 2))
hotelreview <-barplot(mean_htreview_by_stars$Nr.hotel_reviews, names.arg = mean_htreview_by_stars$Hotel_stars, ylab = "Mean", main = "Mean Hotel review by Hotel Stars", col= "#4d8c57",ylim =c(0, max(mean_review_by_stars$Nr.reviews) * 1.2) )
helpfulvotes <-barplot(mean_votes_by_stars$Helpful_votes,names.arg = mean_votes_by_stars$Hotel_stars,main = "Mean vote by Hotel Stars", col= "#4d8c57",ylim =c(0, max(mean_review_by_stars$Nr.reviews) * 1.2))

##linear regression 
#model1
set.seed(3)
trainSetting <- trainControl(method="cv", number=10)
model <- train(Score~Tennis_court+Pool+Spa+Gym+Casino+Free_internet, data=Rate, method ="lm", trControl=trainSetting)
model$resample
summary(model)
#model2
set.seed(3)
trainSetting2 <- trainControl(method="cv", number=10)
model2 <- train(Score~Pool+Free_internet, data=Rate, method ="lm", trControl=trainSetting2)
model2$resample
summary(model2)
#model3 
set.seed(3)
trainSetting3 <- trainControl(method="cv", number=10)
model3 <- train(Score~Spa+Gym+Tennis_court+Casino, data=Rate, method ="lm", trControl=trainSetting3)
model3$resample
summary(model3)

