"splits.rpart" <-
function(tree, print.it=FALSE){

frame <- tree$frame
nodes <- as.numeric(dimnames(frame)[[1]])
paths <- path.rpart(tree, nodes, pretty=NULL, print.it=FALSE)
splits.left <- splits.right <- rep("", nrow(frame))

#attributes(tree$terms)$dataClasses["Country"] == "factor"

for(i in 2:length(nodes))	{
	nodeid <- nodes[i]

	if (nodeid %% 2 == 0)	{
		even <-TRUE
		parentid <- nodeid/2
		parent.node.id <- which(parentid==nodes)
		vtype <- attributes(tree$terms)$dataClasses[frame$var[parent.node.id]]
		l <- paths[[as.character(nodeid)]]
		splits.left[parent.node.id] <- PTprocstring(l[length(l)], vtype)
		}
	else	{
		even <-FALSE
		parentid <- (nodeid-1)/2
		parent.node.id <- which(parentid==nodes)
		vtype <- attributes(tree$terms)$dataClasses[frame$var[parent.node.id]]
		l <- paths[[as.character(nodeid)]]
		splits.right[parent.node.id] <- PTprocstring(l[length(l)], vtype)
		}

	if (print.it==TRUE)	{
	  cat("Node: ", nodeid, " Parent ", parentid, "Parent node id ", parent.node.id, "\n")
	  cat("Variable for parentid: ", frame$var[parent.node.id], "\n")
	  cat("Variable type: ", attributes(tree$terms)$dataClasses[frame$var[parent.node.id]] , "\n")
	}
	
	if (print.it==TRUE)	{
		if (even == TRUE)	
		  cat(" Even: splits.left[ ", parent.node.id, "] = ", splits.left[parent.node.id], "\n")
		else 
		  cat(" Odd: splits.right[ ", parent.node.id, "] = ", splits.right[parent.node.id], "\n")
		}
	
	}

splits <- data.frame(splits.cutleft = splits.left, splits.cutright=splits.right)
splits
}
