"kyphosis.text" <-
function (n, file = "", append = FALSE) 
{
    cat("<BR>\n", file = file, append = append)
    cat("Variable: ", n, ": ", file = file, append = append)
    if (n == "Start") 
        cat("The beginning of the range of vertabrae involved<BR>\n", 
            file = file, append = append)
    else if (n == "Age") 
        cat("The age of the child in months<BR>\n", file = file, 
            append = append)
    else if (n == "Number") 
        cat("The number of vertebrae involved in the operation<BR>\n", 
            file = file, append = append)
    else cat("print \"Unknown variable<BR>\"\n", file = file, 
        append = append)
}
