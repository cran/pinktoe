"writeperloption" <-
function (childvar, childrow, childnodeid, treeobj, perlname, 
    append = TRUE, htmldir, stateprintfn) 
{
    if (childvar == "<leaf>") {
        stateprintfn(treeobj$frame[childrow, "yval"], file = perlname, 
            append = append)
    }
    else {
        cat(paste("cat('", htmldir, childvar, "_", as.character(childnodeid), 
            ".htm', \\*STDOUT)\n", sep = ""), file = perlname, 
            append = append)
        cat(paste("	or die \"Can't cat ", htmldir, childvar, 
            "_", as.character(childnodeid), ".htm: $!\";\n", 
            sep = ""), file = perlname, append = TRUE)
    }
}
