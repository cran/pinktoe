"pinktoe" <-
function (treeobj, textfn, tittext, treeid = "", cgibindir = paste("/~magpn/cgi-bin/", 
    treeid, "/", sep = ""), htmldir = paste("/home/magpn/public_html/Research/Politics/TREE/", 
    treeid, "/", sep = ""), localdir = "Tree/", stateprintfn = partyprint, 
    requirelib = "../party.lib", commonhtml) 
{
    require("rpart")
    require("sfsmisc")
    nnodes <- nrow(treeobj$frame)
#
# Since R rpart does not add in the splits to the frame we'll have
# to do it here
#
    s <- splits.rpart(treeobj)
    treeobj$frame<- cbind(treeobj$frame, s)

    for (i in 1:nnodes) {
        nlabel <- as.character(treeobj$frame[i, "var"])
        if (nlabel != "<leaf>") 
            genedmhtml(treeobj, noderow = i, textfn, tittext, 
                cgibindir, htmldir, localdir, stateprintfn = stateprintfn, 
                requirelib = requirelib, commonhtml)
    }
}
