library(recommenderlab)
library(data.table)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(DT)
library(knitr)
library(grid)
library(gridExtra)
library(methods)
library(Matrix)


#loading the datasets
books <- read.csv(file.choose(),stringsAsFactors = T)
ratings <- read.csv(file.choose(),stringsAsFactors = T)
book_tags <- read.csv(file.choose(),stringsAsFactors = T)
tags <- read.csv(file.choose(),stringsAsFactors = T)

View(books)
View(rating)
View(book_tags)
View(tags)

#Remove duplicates(duplicate ratings)
ratings <- ratings %>% group_by(user_id,book_id) %>% mutate(N=n())
table(ratings$N)           
duplicate_rating <- ratings %>% filter(N>1)

ratings <- ratings %>% filter(N==1)
View(ratings)

# User rated less than 3 books
ratings <- ratings %>% group_by(user_id) %>% mutate(Ratings_Given=n())
View(ratings)
ratings <- ratings %>% filter(Ratings_Given>3)
View(ratings)

#______________________________________________________________________________#


# sampling 2% records
set.seed(1)
user_fraction <- 0.02
users <- unique(ratings$user_id)
sample_users <- sample(users,round(user_fraction*length(users)))
nrow(ratings)
ratings <- ratings %>% filter(user_id %in% sample_users)
nrow(ratings)

# barplot for ratings distribution
ratings %>% ggplot(aes(x=rating, fill=factor(rating)))+
            geom_bar(color = 'grey20')+
            scale_fill_brewer(palette = "YlGnBu")+
            guides(fill=FALSE)

# no of times books being rated
ratings %>% group_by(book_id) %>% 
            summarize(number_of_ratings_per_book =n()) %>%
            ggplot(aes(number_of_ratings_per_book))+
            geom_bar(fill='orange',color='grey20',width = 1)+
            coord_cartesian(c(0,40))

# % distribution for different genres
genres <- str_to_lower(c("Art","Biography","Business","Chick lit","children's",
                         "Christian","classics","comics","cookbooks","crime","Fantasy",
                         "Gay and Lesbian","Grapic Novels","Historical Fiction","History","Horror",
                          "Humor and Comedy","Manga", "Memoir","Music","Mystery","Paranomal","Philosophy",
                           "Poetry","Psycology","Religion","Romance","Science", "Science Fiction",
                           "Self Help", "Suspense","Spirituality","Sports","Thriller","Travel","Young Adult") )
available_genres <- genres[str_to_lower(genres) %in% tags$tag_name]
available_tags <- tags$tag_id[match(available_genres,tags$tag_name)]

tmp <- book_tags %>% filter(tag_id %in% available_tags) %>%
                     group_by(tag_id) %>%
                     summarize(n = n()) %>%
                     ungroup() %>%
                     mutate(sumN = sum(n), percentage = n/sumN) %>%
                     arrange(-percentage) %>%
                     left_join(tags, by = "tag_id")
tmp %>% ggplot(aes(reorder(tag_name, percentage), percentage, fill = percentage))+
        geom_bar(stat = "identity") +
        coord_flip()+
        scale_fill_distiller(palette = "YlOrRd")+
        labs(y = "percentage", x = "Genre")


# Top 10 book
books %>% arrange(-average_rating) %>%
          top_n(10,wt = average_rating)%>%
          select(title,ratings_count,average_rating) ->top10
top10

# most popular book
popularbook <- books %>% arrange(-ratings_count) %>%
               top_n(10,wt=ratings_count) %>%
               select(title,ratings_count,average_rating)
popularbook


#____________________________________________________________________________#

#user-based collaborative filtering
dimension_names <- list(user_id = sort(unique(ratings$user_id)),
                       book_id = sort(ratings$book_id))
ratingmat <- spread(select(ratings,book_id,user_id,rating), book_id,rating) %>%
                    select(-user_id)
class(ratingmat)
ratingmat <- as.matrix(ratingmat)
ratingmat[1:5,1:5]
ratingmat<- ratingmat[,-1] 
dimnames(ratingmat) <- dimension_names
ratingmat[1:5,1:5]
dim(ratingmat)

#  6 new books for two different readers
ratingmat0 <- ratingmat
dim(ratingmat0)
0 ->  ratingmat0[is.na(ratingmat0)] 
sparse_ratings <- as(ratingmat0,"sparseMatrix")
real_ratings <- new("realRatingMatrix", data = sparse_ratings)
real_ratings

sample(x=c(T,F),size = nrow(real_ratings),replace = T, prob = c(0.8,0.2)) ->split_book
recc_train <- real_ratings[split_book,]
recc_test <- real_ratings[!split_book,]

Recommender(data = recc_train, method = 'UBCF') -> recc_model_ubcf
n_recommended_ubcf <- 6

predict(object = recc_model_ubcf, newdata = recc_test, n=n_recommended_ubcf) -> recc_predicted_ubcf
recc_predicted_ubcf@items[[1]]-> user1_book_numbers
recc_predicted_ubcf@itemLabels[user1_book_numbers]

books %>% filter(id==6343) %>% select(title,authors)
books %>% filter(id==7482) %>% select(title,authors)
books %>% filter(id==7677) %>% select(title,authors)
books %>% filter(id==4217) %>% select(title,authors)

recc_predicted_ubcf@items[[5]]-> user5_book_numbers
recc_predicted_ubcf@itemLabels[user5_book_numbers]

books %>% filter(id==4609) %>% select(title,authors)
books %>% filter(id==5448) %>% select(title,authors)
books %>% filter(id==606) %>% select(title,authors)
books %>% filter(id==3186) %>% select(title,authors)
books %>% filter(id==780) %>% select(title,authors)
