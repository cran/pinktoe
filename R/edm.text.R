"edm.text" <-
function(n, web=TRUE, file = "", append = FALSE)
{
	if (web == FALSE)
		etext <- ""
	nc <- nchar(n)
	n <- as.numeric(substring(n, 4., nc))
	nl <- 8.
	biglength <- length(edmbigtext)
	numix <- match(as.character(n), edmbigtext)
	if(is.na(numix))
		stop(paste("No EDM with number ", n))
	#	cat("Found EDM ", n, " at index ", numix, "\n")
	if(edmbigtext[numix - 1.] != "EDMNUM:") stop(paste("Found number ",
			n, " in edmbigtext but not EDM number at position ",
			numix))
	localstring <- edmbigtext[numix:biglength]
	llocal <- length(localstring)
	nextix <- match("EDMNUM:", localstring)
	if(is.na(nextix))
		nextix <- llocal
	else nextix <- nextix - 1.
	localstring <- localstring[2.:nextix]
	titstart <- match("EDMTITLE:", localstring) + 1.
	titend <- match("EDMTEXT:", localstring) - 1.
	texstart <- titend + 2.
	texend <- length(localstring)
	if (web==TRUE)
		cat("<EM>", file = file, append = append)
	cnt <- 0.
	for(i in titstart:titend) {
		if (web==TRUE)
			cat(localstring[i], " ", file = file, append = append)
		else
			etext <- paste(etext, localstring[i])
		cnt <- cnt + 1.
		if(cnt >= nl) {
			if (web==TRUE)
				cat("\n", file = file, append = append)
			else
				etext <- paste(etext, "\n", sep="")
			cnt <- 0.
		}
	}
	if (web==TRUE)
		cat("</EM>\n<P>\n", file = file, append = append)
	else
		etext <- paste(etext, "\n\n")
	cnt <- 0.
	for(i in texstart:texend) {
		if (web==TRUE)
			cat(localstring[i], " ", file = file, append = append)
		else
			etext <- paste(etext, localstring[i])
		cnt <- cnt + 1.
		if(cnt >= nl) {
			if (web==TRUE)
				cat("\n", file = file, append = append)
			else
				etext <- paste(etext, "\n", sep="")
			cnt <- 0.
		}
	}
	#	cat("This is the text from EDM ", n, "\n", file = file, append
	#		 = append)
	if (web==TRUE)
		cat("<BR>\n", file = file, append = append)
	else
		etext <- paste(etext, "\n", sep="")
if (web==FALSE)
	return(etext)
}
