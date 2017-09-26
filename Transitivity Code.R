
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
#   5. Calculate Transitivity of the individual nodes fo the network 
#   6. Plotting Local Transitivity




# ================ 1. Preparing R ================ 
##Set Working Directory to whatever you want###
setwd()

###Install Relevant Packages###

install.packages("RCurl")###This Package Lets you pull URLs and use them as objects in R, We will use this to get our data
library(RCurl)
install.packages("igraph")
library(igraph)
install.packages("reshape2")#This package lets us reshape the data from a dyadic format to an adjacency matrix
library(reshape2)

###Set seed so plots are consistent###
seed <- 1337

#================ 2. Reading in and Managing Data =============
# The Data I am using is data on formal alliances between states from 
# Gibler, Douglas M., and Meredith Reid Sarkees. 2004. "Measuring Alliances: 
# The Correlates of War Formal Interstate Alliance Dataset, 1816-2000." 
# Journal of Peace Research 41: 211-22.
# A formal alliance is a written treaty that passes ratification

# The data sets also include data on MIDs and contiguity(also from the correlates of war project), 
# Polity III Scores(Jaggers and Gurr 1995), and trade and GDP data(Gleditsch 2002). 
# The Data set is dyadic and longitudinal which I have subset into three years 1990,1995,2000.



#These lines of code call in the data sets which I have posted to my github https://github.com/swilmer/Network-Analysis#
url1 <- getURL("https://raw.githubusercontent.com/swilmer/Network-Analysis/master/Alliance1990.csv")
Alliance1990 <- read.csv(text = url1) # read the 1990 Aliiance data

url2 <- getURL("https://raw.githubusercontent.com/swilmer/Network-Analysis/master/Alliance1995.csv")
Alliance1995 <- read.csv(text = url2) # read the 1995 Aliiance data

url3 <- getURL("https://raw.githubusercontent.com/swilmer/Network-Analysis/master/Alliance2000.csv")
Alliance2000 <- read.csv(text = url3) # read the 2000 Aliiance data


###Creating the Network Data###

###Create Adjacency Matrix 1990###
#This line creates the adjacency matrix 
adjmat1990<-as.matrix(acast(Alliance1990, ccode1~ccode2, value.var="allies")) 
adjmat1990

#There are some NA's in the matrix which represent states that do not have a formal alliance
#In order to make the object later readable as a graph we replace the NA's with 0's
adjmat1990[is.na(adjmat1990)]<-0

#Create the Graph object with undirected ties
gtest1990<-graph_from_adjacency_matrix(adjmat1990, mode="undirected",diag=F)


###Create Adjacency Matrix 1995### 
#REPEAT FOR 1995 #
adjmat1995<-as.matrix(acast(Alliance1995, ccode1~ccode2, value.var="allies"))
adjmat1995 #Note that the number of nodes increases due to the Soviet Union collapsing
adjmat1995[is.na(adjmat1995)]<-0

gtest1995<-graph_from_adjacency_matrix(adjmat1995, mode="undirected",diag=F)


###Create Adjacency Matrix 2000###
#REPEAT FOR 2000#
adjmat2000<-as.matrix(acast(Alliance2000, ccode1~ccode2, value.var="allies"))
adjmat2000
adjmat2000[is.na(adjmat2000)]<-0

gtest2000<-graph_from_adjacency_matrix(adjmat2000, mode="undirected",diag=F)



#================ 3. Plotting the Networks =============
#1990#
#Currently the Vertex Labels are COW Country Codes which are not useful unless one knows them by heart
#This code changes the Country codes to country abbreviations
V(gtest1990)$name
V(gtest1990)$Country=as.character(Alliance1990$ida[match(V(gtest1990)$name,Alliance1990$ccode1)])

set.seed(seed)
dev.new()
plot(gtest1990, vertex.size = 5, vertex.label = V(gtest1990)$Country , edge.color = "black", 
     layout = layout.fruchterman.reingold, vertex.color = "light blue", vertex.label.color = "orangered4", 
     vertex.label.cex = .75, label.dist = 1)

#1995#
V(gtest1995)$name
V(gtest1995)$Country=as.character(Alliance1995$ida[match(V(gtest1995)$name,Alliance1995$ccode1)])

set.seed(seed)
dev.new()
plot(gtest1995, vertex.size = 5, vertex.label = V(gtest1995)$Country ,edge.color = "black", 
     layout = layout.fruchterman.reingold, vertex.color = "salmon", vertex.label.color = "blue",
     vertex.label.cex = .75, label.dist = 1)

#Notice there are more isolates and a lack of cluster as a result of the soviet union dissolving

#2000#
V(gtest2000)$name
V(gtest2000)$Country=as.character(Alliance2000$ida[match(V(gtest2000)$name,Alliance2000$ccode1)])

set.seed(seed)
dev.new()
plot(gtest2000, vertex.size = 5, vertex.label = V(gtest2000)$Country , edge.color = "black", 
     layout = layout.fruchterman.reingold, vertex.color="yellow3", vertex.label.color = "royalblue4", 
     vertex.label.cex = .75, label.dist = 1)

#Notice A new Russia/CHina Cluster

#===================4. Calculate Transitivity of the network =====================
#This function measures the probability that the adjacent vertices of a vertex are connected in the overall Network#
#Also known as the clustering coefficient#
#1990#
transitivity(gtest1990, type = "undirected")
# .9121187 #

#1995#
transitivity(gtest1995, type = "undirected")
# .9318984 #

#2000#
transitivity(gtest2000, type = "undirected")
#.9274173

#The Networks are all similar but it's easy to see how the 1990 network has the lowest transitivity
#The group of states allied with russia are not all necessarily allied with eachother
# and this portion of the network drops in in 1995

#===================5. Calculate Transitivity of the individual nodes fo the network =====================
#Also known as the clustering coefficient of each node#
#1990#
t1990 <- transitivity(gtest1990, type = "local")
t1990[is.na(t1990)]<-.00001 #I give the NA's(Isolates) some value so I will be able to plot with node size)
#1995#
t1995 <- transitivity(gtest1995, type = "local")
t1995[is.na(t1995)]<-.00001

#2000#
t2000<- transitivity(gtest2000, type = "local")
t2000[is.na(t2000)]<-.00001


#===================6. Plot Local Transitivity  =====================
#1990#
set.seed(seed)
dev.new()
plot(gtest1990, vertex.size = t1990*4, vertex.label = V(gtest1990)$Country , edge.color="black", 
     layout = layout.fruchterman.reingold, vertex.color = "light blue", vertex.label.color = "orangered4", 
     vertex.label.cex = .75, vertex.label.dist = .5)##In this plot I scale up the vertex size due to transitivity being a fraction##
#The DRC, Rwanda and Burundi have perfect transitiviy as the states are all allies with eachother#
#Very Central Nodes are as transitive as they have many connections which decreases the likelihood that they all share alliances
#States that belong to defensive pacts such as NATO have high transitivity which drives the shape of the networks#

#Plot without labels to better see what is going on in the graph###
set.seed(seed)
dev.new()
plot(gtest1990, vertex.size = t1990*5, edge.color="black", 
     layout = layout.fruchterman.reingold, vertex.color = "light blue", vertex.label = NA)

#1995#
set.seed(seed)
dev.new()
plot(gtest1995, vertex.size = t1995*4, vertex.label = V(gtest1995)$Country , edge.color="black", 
     layout = layout.fruchterman.reingold, vertex.color = "salmon", vertex.label.color = "blue", 
     vertex.label.cex = .75, vertex.label.dist = .5)##In these plot I scale up the vertex size due to transitivity being a fraction##
#The (DRC, Rwanda and Burundi cluster), (Chad, Central African Republic, and Congo), and another african alliance has
#perfect transitiviy as the states are all allies with eachother#
#States that belong to defensive pacts such as NATO have high transitivity which drives the shape of the networks#

#Plot without labels to better see what is going on in the graph###
set.seed(seed)
dev.new()
plot(gtest1995, vertex.size = t1995*5, edge.color="black", 
     layout = layout.fruchterman.reingold, vertex.color = "salmon", vertex.label = NA)

#2000#
set.seed(seed)
dev.new()
plot(gtest2000, vertex.size = t2000*4, vertex.label = V(gtest2000)$Country , edge.color="black", 
     layout = layout.fruchterman.reingold, vertex.color = "yellow3", vertex.label.color = "royalblue4", 
     vertex.label.cex = .75, vertex.label.dist = .5)##In these plot I scale up the vertex size due to transitivity being a fraction##
#Few african states have perfect transitivity as the alliances that existed in previous years crossed over to an extent
#States that belong to defensive pacts such as NATO have high transitivity which drives the shape of the networks#


#Plot without labels to better see what is going on in the graph###
set.seed(seed)
dev.new()
plot(gtest2000, vertex.size = t2000*5, edge.color="black", 
     layout = layout.fruchterman.reingold, vertex.color = "yellow3", vertex.label = NA)

