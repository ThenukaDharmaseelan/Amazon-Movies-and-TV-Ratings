
setwd("C:/Users/Dell/Desktop")
getwd()
Rating_Animated_Movies <- read.csv("Amazon - Movies and TV Ratings.csv",header = T)
View(Rating_Animated_Movies)
Row_names <- Rating_Animated_Movies[,1]
Row_names
rownames(Rating_Animated_Movies) <- Row_names
Rating_Animated_Movies <-  Rating_Animated_Movies[,-1]
View(Rating_Animated_Movies)
summary(Rating_Animated_Movies)
NotNA_Counts <- function(x){
    sum(!is.na(x))
}
NotNA_Counts
### Max rating counts
rating_counts <- apply(Rating_Animated_Movies, 2, NotNA_Counts)
rating_counts
rating_counts <-as.data.frame(rating_counts)
View(rating_counts)

library(dplyr)
x = rownames(rating_counts)
x
rating_counts_max <- transform(rating_counts,Movie = c(x))
View(rating_counts_max)
rating_counts_max1 <- arrange(rating_counts_max,desc(rating_counts))
View(rating_counts_max1)
### Avg Movie ratings
Avg_movie_Ratings <- apply(Rating_Animated_Movies,2,mean,na.rm = T)
Avg_movie_Ratings <- as.data.frame(Avg_movie_Ratings)
View(Avg_movie_Ratings)
### max 5 movie ratings
library(dplyr)
x = rownames(Avg_movie_Ratings)
Avg_movie_Ratings_maxs <- transform(Avg_movie_Ratings,Movie = c(x))
Avg_movie_Ratings_max5 <- arrange(Avg_movie_Ratings_maxs,desc(Avg_movie_Ratings))[1:5,]
View(Avg_movie_Ratings_max5)

### least 5 movie ratings
least_audiences <- filter(rating_counts_max1,rating_counts != 0)
View(least_audiences)
least_audiences1 <- arrange(least_audiences,rating_counts)[1:5,]
View(least_audiences1)


### Recommendation model
Rating_Animated_Movies_recommand <- as(Rating_Animated_Movies,"matrix")
library(recommenderlab)

Rating_Animated_Movies_recommand1 <- as(Rating_Animated_Movies_recommand,"realRatingMatrix")
class(Rating_Animated_Movies_recommand1)

### Divide data into training and test data
n = nrow(Rating_Animated_Movies_recommand1)
split = sample(c(TRUE, FALSE), n, replace=TRUE, prob=c(0.8, 0.2))

training = Rating_Animated_Movies_recommand1[split, ]
testing = Rating_Animated_Movies_recommand1[!split, ]

training
testing

our_model <- Recommender(training, method = "UBCF")
our_model 

#Make predictions on the test data
pre <- predict(our_model,testing, n = 10)
pre

View(as(pre,"matrix"))

