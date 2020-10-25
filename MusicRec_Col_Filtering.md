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

    #### a) Training set with 84% of the observations.<br/>

    #### b) Validation set A to be used for tuning the collaborative filtering model, with 4% of the observations.<br/>

    #### c) Validation set B to be used for blending, with 4% of the observations.

    #### d) Testing set with 8% of the observations

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

#### e) construct an incomplet training set ratings matrix.

    mat.train <- Incomplete(train$userID, train$songID, train$rating)

    paste("Dimension of the matrix", dim(mat.train)[1], "*", dim(mat.train)[2])

    ## [1] "Dimension of the matrix 2421 * 807"

2. Models
---------

Let *X* denote the “complete” ratings matrix, i.e.,
*X*<sub>*i*, *j*</sub> denotes either the observed rating if user i
actually rated song j or the “hypothetical” such rating if user i has
not yet rated song j. We are interested in predicting the values of
*X*<sub>*i*, *j*</sub> that are not observed. Let us first consider the
following model: <br/>
*X*<sub>*i*, *j*</sub> = *α*<sub>*i*</sub> + *β*<sub>*j*</sub> + *ϵ*<sub>*i*, *j*</sub>
<br/>

where *α*<sub>*i*</sub> is a coefficient that depends only on the
particular row i (i.e., user), "j is a coefficient that depends only on
the particular column j (i.e., song), and *ϵ*<sub>*i*, *j*</sub> is a
noise term.

#### i) Parameters

There are two types of parameters in this model *α* and *β*; A total of
2421 *α* and 807 *β*. So there are 3228 parameters to fit. <br/>
There’re 243104 observations in the training set, so we’ll train the
model with 243104 observations and fill in the rest of the matrix.
