"genedmhtml" <-
function (treeobj, noderow, edmtextfn, edmtittext, cgibindir, 
    htmldir, localdir, stateprintfn, requirelib, commonhtml) 
{
    edmnames <- as.character(treeobj$frame[, 1])
    edmix <- as.numeric(dimnames(treeobj$frame)[[1]])
    edmlabel <- as.character(treeobj$frame[noderow, "var"])
    uniqedmlab <- paste(edmlabel, "_", as.character(edmix[noderow]), 
        sep = "")
    split.type <- treeobj$frame[, "splits.cutright"][noderow]
    if (substring(split.type, 1, 1) != ":") {
        numvar <- TRUE
        numstring <- split.type
    }
    else {
        numvar <- FALSE
        numstring <- split.type
    }
    perlname <- writehtml(edmlabel = edmlabel, uniqedmlab = uniqedmlab, 
        edmtextfn = edmtextfn, edmtittext = edmtittext, cgibindir = cgibindir, 
        localdir = localdir, commonhtml = commonhtml, numvar = numvar, 
        numstring = numstring, treeobj = treeobj)
    perlname <- paste(localdir, perlname, sep = "")
    cat("Frame row number is ", noderow, "\n")
    cat("Node number is ", edmix[noderow], "\n")
    nodenum <- edmix[noderow]
    childleft <- 2 * nodenum
    childright <- childleft + 1
    childleftrow <- match(childleft, edmix)
    childrightrow <- match(childright, edmix)
    writeperl(childleftrow, childrightrow, treeobj, perlname, 
        edmlabel, htmldir, edmix, stateprintfn = stateprintfn, 
        requirelib = requirelib)
}
