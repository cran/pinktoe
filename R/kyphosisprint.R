"kyphosisprint" <-
function (yval, file = "", append = FALSE) 
{
    if (yval == "present") 
        cat(" &present;\n", file = file, append = append)
    else if (yval == "absent") 
        cat(" &absent;\n", file = file, append = append)
    else cat("print \"Unknown kyphosis state<BR>\"\n", file = file, 
        append = append)
}
