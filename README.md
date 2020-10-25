# ML_Music_Recommending_System
This is project about the Music Recommending system by collaborative filtering, it's also a class project of UC Berkeley IEOR142, fall2019


## Background

Recommendation engines are vital to many of today’s most successful firms. From Amazon to Netflix
to Spotify, companies have realized the value of automatically showing customers products they
are more interested in purchasing based on the collaborative evaluations of products by customers,
and based also on customer-specific and product-specific data as well. 

In this problem, we are provided with a dataset consisting of three .csv files, obtained from the Million Song Dataset Project,
of derived user ratings of songs based on each user’s listening behavior. (users do not
actually rate songs – rather the “rating” has been determined from their listening behaviors – but
we will still use phrases like “user i has rated song j”, etc.) Each user only listens to a few songs,
and therefore for each user we only have rating data concerning these few songs. Nevertheless, we
would like to infer each user’s ratings of all songs based on the entirety of the data. The summary
goal of this problem will be to recommend specific songs to specific users.

## Data

• Songs.csv: each observation corresponds to a song and it includes the song’s ID, name, year,
artist, and genre. Observations are sorted by song ID, in ascending order. <br/>
• Users.csv: a list of all user (listener) IDs, in ascending order. <br/>
• MusicRatings.csv: derived ratings of songs based on each user’s listening history. Each
observation contains the user ID, the song ID, and the derived rating


