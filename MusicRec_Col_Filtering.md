    library(softImpute)

    Songs <- read.csv("Songs.csv", sep = ",")
    MusicRatings <- read.csv("MusicRatings.csv", sep = ",")
    Users <- read.csv("Users.csv", sep = ",")

Load the data and calculate some summary statistics

    paste("Total Songs:", nrow(unique(MusicRatings["songID"])))

    ## [1] "Total Songs: 807"

    paste("Total users:", nrow(unique(Users)))

    ## [1] "Total users: 2421"

    paste("Range of ratings: (" ,min(MusicRatings["rating"]),",",max(MusicRatings["rating"]),")")

    ## [1] "Range of ratings: ( 1 , 3.43296929087441 )"

1.  Train Test Split
    ----------------

<!-- -->

1.  Training set with 84% of the observations.<br/>
2.  Validation set A to be used for tuning the collaborative filtering
    model, with 4% of the observations.<br/>
3.  Validation set B to be used for blending, with 4% of the
    observations.
4.  Testing set with 8% of the observations

<!-- -->

    set.seed(345)
    train.ids <- sample(nrow(MusicRatings), 0.92*nrow(MusicRatings))
    train <- MusicRatings[train.ids,]
    test <- MusicRatings[-train.ids,]


    ## split out the validation part
    val.ids <- sample(nrow(train), (8/92)*nrow(train))
    val <- train[val.ids,]

    ## update train set
    train <- train[-val.ids,]

    ## get valA and val B
    valA.ids <- sample(nrow(val), 0.5*nrow(val))
    valA <- val[valA.ids,]
    valB <- val[-valA.ids,]

1.  construct an incomplet training set ratings matrix.

<!-- -->

    mat.train <- Incomplete(train$userID, train$songID, train$rating)

    paste("Dimension of the matrix", dim(mat.train)[1], "*", dim(mat.train)[2])

    ## [1] "Dimension of the matrix 2421 * 807"
