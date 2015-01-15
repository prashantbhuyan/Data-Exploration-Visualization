# Prashant B. Bhuyan
# Exploration & Visualization IMBD Database

##### Problem 1 #####

# Solution (See attached image 'Movie Titles By Decade.png')

# Certainly according to the visualization the number of movies produced 
# grows substantially each decade.  Perhaps this is due to better production and 
# distribution technology and perhaps due to population growth and globalization.

library(ggplot2)
library(plyr)
data <- movies
movies$decade <- round_any(movies$year, 10)
g <- ggplot(movies, aes(x = title, y = year, color = decade, group = decade))
x <- geom_line() 
g <- g + x


##### Problem 2 #####

# Solution (See attached image 'Avg Rating By Genre.png' and 'Avg Rating Over the Years.png').

# Documentaries get the highest average rating of any genre whereas
# Action gets the lowest average rating of any genre. 
#
# Over the years, ratings became much more varied.  In the 1920's,
# most films got ratings better than a 5.  You could infer that 
# the overall quality of films was better until the 1970's or that 
# critics became more lax in their critiques.  It seems that
# you could delve further into the question by analyzing the 
# important factors like technological advancement and population growth
# and globalization to see if there are just more movies being produced
# for all types of people some of which are low budget 'bollywood' types of 
# movies that are mass produced to entertain large swaths of the global population.
# Maybe the cost of making movies in the 1920's was so high that
# the quality of the movie was very important to producers.

library(ggplot2)
library(reshape2)

data <- movies
size <- rep(NA, nrow(movies))
genres <- rowSums(movies[,18:24])
size[which(genres==1 & movies$Action == 1)] = "Action"
size[which(genres==1 & movies$Animation == 1)] = "Animation"
size[which(genres==1 & movies$Comedy == 1)] = "Comedy"
size[which(genres==1 & movies$Drama == 1)] = "Drama"
size[which(genres==1 & movies$Documentary == 1)] = "Documentary"
size[which(genres==1 & movies$Romance == 1)] = "Romance"
size[which(genres==1 & movies$Short == 1)] = "Short"
size[which(genres > 1)] = "MultiGenre"
size[which(genres < 1)] = "NoGenre"
movies$size <- as.factor(size)
# Avg Ratings by Genre
plot <- ggplot(movies,aes(x = size, y = rating))+geom_boxplot()+labs(title = "Avg Rating By Genre", x = "Genre", y = "Avg Rating")
# Avg Ratings Over the Years
plot_z <- ggplot(movies, aes(x = year, y = rating))+geom_point()+labs(title = "Avg Ratings Over the Years", x = "Years", Y = "Avg Ratings")


##### Problem 3 #####

# Solution
# please see attached image 'Rating vs Length.png'.
library(ggplot2)
library(reshape2)
library(plyr)
data <- movies

rating_vs_length <- ggplot(data, aes(rating, length)) + geom_point() + labs(title = "Rating vs Length", x = "Rating", y = "Length") 


# Answer: There is a slight positive relationship between rating and length of movies.  There are certainly outliers where very long movies get ratings below 5.  Further 
# there are very short movies that get very high ratings.  As such, the relationship is very weak but positive nevertheless.

##### Problem 4 #####

# Solution
# 
# There is a relationship between Genres and Length.  Shorts are 
# the shortest in length and Documentaries, Dramas and Multi Genre 
# films are the longest in length.  Animations are also fairly short
# movies perhaps due to high cost of development.  Films that have
# no genre seem to be very long but I'm not sure why that is. 

# Please see 'Movie Genres vs Length.png'.

library(ggplot2)
data <- movies
size <- rep(NA, nrow(movies))
genres <- rowSums(movies[,18:24])
size[which(genres == 1 & movies$Action == 1)] = "Action"
size[which(genres==1 & movies$Animation == 1)] = "Animation"
size[which(genres==1 & movies$Comedy == 1)] = "Comedy"
size[which(genres== 1 & movies$Drama == 1)] = "Drama"
size[which(genres == 1 & movies$Documentary == 1)] = "Documentary"
size[which(genres == 1 & movies$Romance == 1)] = "Romance"
size[which(genres == 1 & movies$Short == 1)] = "Short"
size[which(genres > 1)] = "MultiGenre"
size[which(genres < 1)] = "NoGenre"
movies$size <- as.factor(size)
movies_length_vs_genres <- ggplot(data, aes(movies$size, movies$length)) + geom_line() + labs(title = "Genres vs Length", x = "Genres", y = "Movie Length")


##### Problem 5 #####

# Solution 
#
# Clearly, the variable that best predicts votes is the rating of the movie.  Please see
# attached scatter plot called 'Votes vs Rating.png' and you'll see a strong positive relationship.  

library(ggplot2)
data <- movies
votes_vs_rating = ggplot(data, aes(rating, votes)) + geom_point() + labs(title = "Rating vs Votes", x = "Rating", y = "Votes")



