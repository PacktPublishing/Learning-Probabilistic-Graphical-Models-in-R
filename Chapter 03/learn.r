library(graph)
library(Rgraphviz)
library(plyr)

data0 <- data.frame(
	x=c("a","a","a","a","b","b","b","b"),
	y=c("t","t","u","u","t","t","u","u"),
	z=c("c","d","c","d","c","d","c","d"))

edges0 <- list(x=list(edges=2),y=list(edges=3),z=list())
g0 <- graphNEL(nodes=names(data0),edgeL=edges0,edgemod="directed") 
plot(g0)

data1 <- read.csv("http://archive.ics.uci.edu/ml/machine-learning-databases/nursery/nursery.data", col.names=c("parents","has_nurs","form","children","housing","finance","social","health","class"))
edges1 <- list( parents=list(), has_nurs=list(), form=list(), children=list(), 
	     housing=list(), finance=list(), social=list(), health=list(),
	     class=list(edges=1:8) )
g1 <- graphNEL(nodes=names(data1), edgeL=edges1,edgemod="directed")
plot(g1)

make_cpt<-function(df,pa)
{
	prob <- nrow(df)
	parents <- data.frame(df[1,pa])
	names(parents) <- pa

	data.frame(parents,prob)
}

learn <- function(g,data)
{
	rg <- reverseEdgeDirections(g)
	result <- list()

	for(var in rg@nodes)
	{
		pa <- unlist(adj(rg,var))
		if(length(pa)>0)
		{
			X <- ddply(data, c(var,pa), make_cpt, pa)
			Y <- ddply(data, pa, make_cpt, pa)
			for(i in 1:nrow(Y))
			{
				c <- sapply(1:nrow(X), function(j) all(X[j,pa] == Y[i,pa]))
				c <- which(c)
				X$prob[c] <- X$prob[c]/Y$prob[i]
			}
		}
		else
		{
			X <- ddply(data,var, function(df) c(prob=nrow(df)))
			X$prob <- X$prob/sum(X$prob)
		}

		result[[length(result)+1]] <- X
	}

	return(result)
}
