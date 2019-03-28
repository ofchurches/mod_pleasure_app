# mod_pleasure_app
This is the pleasure recommendation app for the Museum of Discovery's Hedonism exhibition 

1) For the app there is the R file that executes it, the rds file with the ego network and the Rmd file that shows the steps taken in creating the rds file. THe app itsef is here: https://ofchurches.shinyapps.io/its_my_pleasure/.

2) The video is available here: https://www.dropbox.com/s/qmhaisn7707xf2v/pleasure_ego_1080.mov?dl=0. It was built using the top 20 degree nodes from the network. The size of the nodes is the degree, the colour is community (using modularity) and the connections in time are random. It was graphed using Gephi (https://gephi.org/).

3) Then there are two csv files that are useful for making better versions of all this. These are "pleasure_words.csv" and "pleasure_connections.csv". The "words" has the 100 words that are included in the app. The collums are name (string - unique), comunity (factor - 1:5),  centrality (numeric - 1:30) and id (factor - 1:100). THe "connections" has the 574 connection between the 100 words. The collums are from (factor - 1:100) and to (factor - 1:100) and weight (numeric - 11:232).
