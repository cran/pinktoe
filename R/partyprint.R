"partyprint" <-
function(yval, file = "", append = FALSE)
{
	if(yval == "Lab")
		cat(" &labour;\n", file = file, append = append)
	else if(yval == "Con")
		cat(" &tory;\n", file = file, append = append)
	else if(yval == "LDem")
		cat(" &libdem;\n", file = file, append = append)
	else if(yval == "UU")
		cat(" &uup;\n", file = file, append = append)
	else if(yval == "PC")
		cat(" &pc;\n", file = file, append = append)
	else if(yval == "SNP")
		cat(" &snp;\n", file = file, append = append)
	else cat("print \"Unknown party<BR>\"\n", file = file, append = append
			)
}
