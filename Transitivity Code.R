
##========================================================##
##                                                        ##
##   [2017Fall: PLSC 597] Network Analysis                ##
##   9/28 Transitivity                                    ##
##   Sam Wilmer                                           ##
##                                                        ##
##========================================================##



# CONTENTS
#
#   1. Preparing your R workspace
#   2. Reading in and managing data
#   3. Plotting the network
#   4. Traditional measure of transitivity
#   5. 
#   6. 
#   7. 




# ================ 1. Preparing R ================ 
install.packages("RCurl")
library(RCurl)
install.packages("igraph")
library(igraph)
install.packages("reshape2")
library(reshape2)

seed <- 1337

#================ 2. Reading in and Managing Data =============

url1 <- getURL("https://raw.githubusercontent.com/swilmer/Network-Analysis/master/Alliance1990.csv")
Alliance1990 <- read.csv(text = url1) # read the 1990 Aliiance data

url2 <- getURL("https://raw.githubusercontent.com/swilmer/Network-Analysis/master/Alliance1995.csv")
Alliance1995 <- read.csv(text = url2) # read the 1995 Aliiance data

url3 <- getURL("https://raw.githubusercontent.com/swilmer/Network-Analysis/master/Alliance2000.csv")
Alliance2000 <- read.csv(text = url3) # read the 2000 Aliiance data

