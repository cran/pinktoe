"writehtml" <-
function (edmlabel, uniqedmlab, edmtextfn, edmtittext, cgibindir, 
    localdir, commonhtml, numvar, numstring, treeobj) 
{
    fn <- paste(localdir, uniqedmlab, ".htm", sep = "")
    perlname <- paste(uniqedmlab, ".pl", sep = "")
    cat("<HTML>\n<HEAD>\n", file = fn, append = FALSE)
    cat(paste("<TITLE>", edmtittext(edmlabel), ": ", edmlabel, 
        "</TITLE>\n", sep = ""), file = fn, append = TRUE)
    cat("</HEAD>\n<BODY>\n", file = fn, append = TRUE)
    cat(paste("<FORM ACTION=\"", cgibindir, perlname, "\" METHOD=POST>\n", 
        sep = ""), file = fn, append = TRUE)
    cat("<HR>\n<P>\n", file = fn, append = TRUE)
    if (numvar == TRUE) {
        cat(paste("Is ", edmlabel, " ", numstring, "?\n", sep = ""), 
            file = fn, append = TRUE)
        edmtextfn(edmlabel, file = fn, append = TRUE)
    }
    else {
        cat(paste(edmlabel, ": ", sep = ""), file = fn, append = TRUE)
        cat("\n Select the following categories:", file = fn, 
            append = TRUE)
        levs <- attr(treeobj, "xlevels")[edmlabel][[1]]
        numstring <- substring(numstring, 2, nchar(numstring))
        cats <- AsciiToInt(numstring) - 96
        for (i in 1:length(cats)) cat(levs[cats[i]], " ", file = fn, 
            append = TRUE)
        cat("<BR>\n", file = fn, append = TRUE)
        edmtextfn(edmlabel, file = fn, append = TRUE)
    }
    cat("<P>\n", file = fn, append = TRUE)
    cat("<STRONG>Check the box if this is true or you agree", 
        "</STRONG>\n", file = fn, append = TRUE)
    cat(paste("<INPUT TYPE=\"checkbox\" NAME=\"EDMQ\" VALUE=\"", 
        edmlabel, "\">\n<BR>\n", sep = ""), file = fn, append = TRUE)
    cat("<INPUT TYPE=\"submit\" VALUE=\"Next\">\n", file = fn, 
        append = TRUE)
    cat("<INPUT TYPE=\"reset\" VALUE=\"Reset\">\n", file = fn, 
        append = TRUE)
    cat("<P>\n", file = fn, append = TRUE)
    commonhtml(file = fn, append = TRUE)
    cat("</FORM>\n</BODY>\n</HTML>\n", file = fn, append = TRUE)
    return(perlname)
}
