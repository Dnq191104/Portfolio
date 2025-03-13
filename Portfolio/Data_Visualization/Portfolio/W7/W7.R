
# Loading packages

library(igraph)
library(network)
library(sna)
library(ndtv)
library(tidyverse)
library(EpiContactTrace)
library(RColorBrewer)
library(viridis)
library(circlize)
library(alluvial)


# 1. Media Dataset

# 1.1 Setting up data
# 1.1.1 Load data

media_vertices <- read.csv("W7/data/Dataset1-Media-Example-NODES.csv", header=T, as.is=T)
media_edges <- read.csv("W7/data/Dataset1-Media-Example-EDGES.csv", header=T, as.is=T)


head(media_vertices)
head(media_edges)

#or 

glimpse(media_vertices)
glimpse(media_edges)


# 1.1.2 Create an igraph object

net <- graph_from_data_frame(d = media_edges, vertices = media_vertices, directed= T)
net

# Access to the net

E(net) # The edges of the "net" object
V(net) # The vertices of the "net" object
E(net)$type # Edge attribute "type"
V(net)$media # Vertex attribute "media"

# Find vertices and edges by attribute:
# (that returns oblects of type vertex sequence/edge sequence)
V(net)[media == "BBC"]
E(net)[type == "mention"]

# You can also examine the network matrix directly:
net[1,]
net[5,7]


# Get an edge list or a matrix:
as_edgelist(net, names = T)
as_adjacency_matrix(net, attr = "weight")

# Or data frames describing vertices and edges:
igraph::as_data_frame(net, what = "edges")
   igraph::as_data_frame(net, what = "vertices" )


# 1.2 Explore the network plot

plot.igraph(net)

# Remove the loops to create a simple, or acyclic network.
net <- igraph::simplify(net, remove.multiple = F, remove.loops = T)

# Also reduce the arrow size and remove the uninformative and cluttering labels by setting them to NA (which in R basically means ‘this is empty’):
plot.igraph(net, edge.arrow.size = .4, vertex.label = NA)


# 1.3 Plotting parameters

# Using plot.igraph() to make changes

# Use curved edges (edge.curved=.1) and reduce arrow size (edge.arrow.size= .4)
# Note that using curved edges will allow you to see multiple edges between two vertices
# (e.g. edges going in either direction, or multiplex edges)
plot.igraph(net, edge.arrow.size = .4, edge.curved = .1)


# Set edge color to light gray, the vertex & border color to orange.
# Replace the vertex label with the vertex names stored in "media".
plot.igraph(net, edge.arrow.size = .2, edge.color = "orange", vertex.color = "orange", 
            vertex.frame.color = "#ffffff", vertex.label = V(net)$media, 
            vertex.label.color = "black")



# Using igraph() to make changes

# Let’s take a look at the variable we want to use to assign colours: media type.
V(net)$media.type

# First, save a vector of colour names that you want to use:
colrs <- c("gray50", "tomato", "gold") # I haven't grabbed these from ColorBrewer2, but I should have!

# Then, make a new variable for the vertices of net that points to one of those colours depending on the media type
V(net)$color <- colrs[V(net)$media.type]

# Compute vertex degrees (#edges) and use that to set vertex size:
deg <- igraph::degree(net, mode="total")
V(net)$size <- deg*3

# We could also use the audience size value:
# V(net)$size <- V(net)$audience.size*0.6


V(net)$label <- NA

# Set edge width based on weight:
E(net)$width <- E(net)$weight/6

# Change arrow size and edge color:
E(net)$arrow.size <- .2
E(net)$edge.color <- "gray80"


plot.igraph(net) 

# We can also override the attributes
plot.igraph(net, edge.color = "orange", vertex.color = "gray50")

plot.igraph(net)
legend(x = -1.5, y = -1.1, # this sets the position of the legend
       c( "Newspaper", "Television", "Online News"), # we need to know about the data to know what labels to use here
       pch = 21, 
       col = "#777777", 
       pt.bg = colrs, # note that this is the vector of colours that we defined earlier
       pt.cex = 2, 
       cex = .8, 
       bty = "n", 
       ncol =  1)



# 2. Network layouts

# 2.1 Basic network layouts 

random.net <- sample_pa(100) 
plot.igraph(random.net)



V(random.net)$size <- 8
V(random.net)$frame.color <- "white"
V(random.net)$color <- "orange"
V(random.net)$label <- "" 
E(random.net)$arrow.mode <- 0
plot.igraph(random.net)

# Set the layout for the plots
plot.igraph(random.net, layout=layout_randomly)

# Set up layout
l <- layout_in_circle(random.net)
plot.igraph(random.net, layout=l)

# Randomly placed vertices
l <- layout_randomly(random.net)
plot.igraph(random.net, layout=l)

# Circle layout
l <- layout_in_circle(random.net)
plot.igraph(random.net, layout=l)

# 3D sphere layout
l <- layout_on_sphere(random.net)
plot.igraph(random.net, layout=l)


l <- layout_with_kk(random.net)
plot.igraph(random.net, layout=l)

plot.igraph(random.net, layout=layout_with_lgl)

plot.igraph(random.net, layout=layout_with_mds)


# 2.2 Bipartite networks layout

media_vertices2 <- read.csv("W7/data/Dataset2-Media-User-Example-NODES.csv", header = T, as.is = T)
media_edges2 <- read.csv("W7/data/Dataset2-Media-User-Example-EDGES.csv", header = T, row.names = 1)


# Read the matrices
net2 <- graph_from_biadjacency_matrix(media_edges2) # build the igraph object
V(net2) # take a look at just the vertices using the V() function
head(V(net2)$type) # take a look at the `type` attributes of the vertices
table(V(net2)$type) # summarise the numbers of each type


plot.igraph(net2, vertex.label=NA)

plot.igraph(net2, vertex.label=NA, vertex.size=7, layout=layout.bipartite)

l <- layout_on_sphere(random.net)


# 2.3 Using layout to zoom
plot.igraph(random.net, rescale=F, layout=l*0.4)
plot.igraph(random.net, rescale=F, layout=l*0.6)
plot.igraph(random.net, rescale=F, layout=l*0.8)
plot.igraph(random.net, rescale=F, layout=l*1.0)
plot.igraph(random.net, rescale=F, layout=l*3.0)


# 3.0 Highlight aspects of the network

# 3.1 Reducing graph

hist(media_edges$weight)
mean(media_edges$weight)
sd(media_edges$weight)


# Just keep the weight higher than mean

cut.off <- mean(media_edges$weight) 
net.sparse <- delete_edges(net, E(net)[weight<cut.off])
plot.igraph(net, layout=layout_with_kk)
plot.igraph(net.sparse, layout=layout_with_kk) 


# 3.2 Clustering

net.communities <- cluster_leading_eigen(net)
plot(net.communities, net)


# Create communities inside the network

net.communities$membership
V(net)$community <- net.communities$membership
colrs <- adjustcolor( c("gray50", "tomato", "gold", "yellowgreen"), alpha=.6)

plot.igraph(net, vertex.color=colrs[V(net)$community])


# 3.3 Highlight specific vertices or edges

# Show how many vertices the New York Times have to walk through if they want connect

dist.from.NYT <- distances(net, v=V(net)[media=="NY Times"], 
                           to=V(net), weights=NA)

# Set colors to plots the distance

oranges <- colorRampPalette(c("darkred", "gold"))
col <- oranges(max(dist.from.NYT)+1)
col <- col[dist.from.NYT+1]

plot.igraph(net, vertex.color=col, vertex.label=dist.from.NYT, edge.arrow.size=.6, 
            vertex.label.color="white")



#The neighbors function in igraph finds all vertices that are directly linked to the chosen vertex
neigh.vertices <- neighbors(net, V(net)[media=="Wall Street Journal"], mode="out")

# Make the default colour for all of the vertices a grey
vertex.col <- rep("grey40", vcount(net)) # vcount() just tells us how many vertices there are

# Set colors to plot the neighbors:

vertex.col[neigh.vertices] <- "#ff9d00"

plot.igraph(net, vertex.color=vertex.col)




# 4.0 Plotting multiple networks

# 2 types of edges
E(net)[E(net)$type=="hyperlink"]
E(net)[E(net)$type=="mention"]

net.mentions <- net - E(net)[E(net)$type=="hyperlink"] 
net.hyperlinks <- net - E(net)[E(net)$type=="mention"] 

# Plot the two edges separately:
plot.igraph(net.hyperlinks, vertex.color="orange", layout=layout_with_fr, main="Edge: Hyperlink")
plot.igraph(net.mentions, vertex.color="lightsteelblue2", layout=layout_with_fr, main="Edge: Mention")


# 5.0 Other ways to represent a network

# Networks as heatmaps

netm <- as_adjacency_matrix(net, attr="weight", sparse=F)
colnames(netm) <- V(net)$media
rownames(netm) <- V(net)$media

palf <- colorRampPalette(c("gold", "dark orange")) 
heatmap(netm[,17:1], Rowv = NA, Colv = NA, col = palf(100), 
        scale="none", margins=c(10,10) )

# 5.1 Networks as chord diagrams

# Load dataset from github
data <- read.table("https://raw.githubusercontent.com/holtzy/data_to_viz/master/Example_dataset/13_AdjacencyDirectedWeighted.csv", header=TRUE)


# short names
colnames(data) <- c("Africa", "EAsia", "Europe", "LatinAm.",   "NorthAm.",   "Oceania", "SAsia", "SEAsia", "Sov.Un.", "WAsia")
rownames(data) <- colnames(data)

# The data is in the form of an adjacency matrix, but we need it in a three column matrix instead: first column origin, second column destination, third column flow. We will use simple re-shaping commands for this:
data_long <- gather(rownames_to_column(data), key='key', value='value', -rowname)


# color palette
mycolor <- viridis(nrow(data)) # generates 10 colours from the viridis palette, one for each node in the network

# Base plot
chordDiagram(
   x = data_long, 
   grid.col = mycolor,
   transparency = 0.25,
   directional = 1,
   direction.type = c("arrows", "diffHeight"),
   diffHeight  = -0.04,
   #  annotationTrack = "grid",
   #  annotationTrackHeight = c(0.05, 0.1),
   link.arr.type = "big.arrow",
   link.sort = TRUE)



# 6.0 Exploring Dataset 2: Cattle Network Example

# 6.1 Load data

attr1 <- read.csv("W7/data/Farms/attr_farms.csv", stringsAsFactors = F) #load the edgelist_farms.Rdata
edges <- read.csv("W7/data/Farms/Edgelist_farms.csv", stringsAsFactors = F) #load attr.farms.Rdata


# 6.2 Create an igraph object.
net_edges <- graph.data.frame(edges,directed=T)

V(net_edges)$type <- as.character(attr1$type[match(V(net_edges)$name,attr1$farm.id)])

V(net_edges)$farm.size <- attr1$size[match(V(net_edges)$name,attr1$farm.id)]


# 6.3 Plot the network

# Color them by type
V(net_edges)$color <- V(net_edges)$type
head(V(net_edges)$color)
V(net_edges)$color <- gsub("Fattening","steelblue1",V(net_edges)$color)
head(V(net_edges)$color)
V(net_edges)$color <- gsub("Dairy","darkgoldenrod1",V(net_edges)$color)
head(V(net_edges)$color)
V(net_edges)$color <- gsub("Small farm","mediumpurple",V(net_edges)$color)
V(net_edges)$color <- gsub(pattern="Breeding",replacement="navy",x=V(net_edges)$color)
V(net_edges)$color <- gsub(pattern="Complete cycle",replacement="magenta2",x=V(net_edges)$color)
V(net_edges)$color <- gsub(pattern="Growing",replacement="green4",x=V(net_edges)$color)


# Plot the new, improved network

plot.igraph(simplify(net_edges),
            layout=layout.fruchterman.reingold,
            vertex.label=NA,
            vertex.color=V(net_edges)$color,
            vertex.size=10,
            edge.arrow.size=.5)

#make a legend
legend("bottomleft",
       legend=c("Breeding","Growing","Complete cycle","Fattening","Dairy","Small farm"),
       col=c("navy","green4","magenta2","steelblue1","darkgoldenrod1","mediumpurple"),
       pch=19,cex=1,bty="n")

# Task: Try any four different layouts for this network. How do the different layouts convey different impressions about the structure of the network?



# 6.4 Edge criteria

head(edges)

e2 <- edges %>% 
   filter('batch.size' > 50 ) 

net.c <- graph.data.frame(e2)

net.c


V(net.c)$farm.size <- (attr1$size[match(V(net.c)$name,attr1$farm.id)] )
V(net.c)$type <- as.character(attr1$type[match(V(net.c)$name,attr1$farm.id)] )
V(net.c)$color <- V(net.c)$type
V(net.c)$color <- gsub("Breeding","navy",V(net.c)$color)
V(net.c)$color <- gsub("Complete cycle","magenta2",V(net.c)$color)
V(net.c)$color <- gsub("Growing","green4",V(net.c)$color)
V(net.c)$color <- gsub("Fattening","steelblue1",V(net.c)$color)
V(net.c)$color <- gsub("Dairy","darkgoldenrod1",V(net.c)$color)
V(net.c)$color <- gsub("Small farm","mediumpurple",V(net.c)$color)


plot.igraph(simplify(net.c),
            layout=layout.fruchterman.reingold,
            vertex.label=NA,
            vertex.color=V(net.c)$color,
            vertex.size=10,
            edge.arrow.size=.3)



