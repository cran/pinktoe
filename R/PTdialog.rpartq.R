"PTdialog.rpartq" <-
function(questiontext, qopts=NULL, qno=NULL, explaintext=NULL){
        tt <- tktoplevel()
        tkwm.title(tt,"Question")
	if (is.null(qopts))	{
		alt <- tclVar("yes")
		syes <- "yes"
		sno <- "no"
		}
	else	{
		alt <- tclVar(qopts)
		syes <- qopts
		sno <- qno
		}
	done <- tclVar(0)
	eqvar <- tclVar(0)

        submit.but <- tkbutton(tt, text="submit",
                               command=function()tclvalue(done)<-1)

        build <- function()
        {
            ## notice that tclvalue() is correct here, since it is the
            ## string representation of xvar and yvar that is being
            ## displayed in the entry fields

            a <- tclvalue(alt)
        }
        alt.rbuts <- tkframe(tt)

        tkpack(tklabel(alt.rbuts, text="Select answer below"))
        for ( i in c(syes, sno)){
            tmp<-tkradiobutton(alt.rbuts, text=i, variable=alt, value=i)
            tkpack(tmp,anchor="w")
        }

	if (!is.null(explaintext))
		tkgrid(tklabel(tt,text=explaintext), columnspan=1)
        tkgrid(tklabel(tt,text=questiontext),columnspan=1)
        tkgrid(alt.rbuts)
        tkgrid(submit.but)

        if (tclvalue(alt)=="") tclvalue(alt)<-syes

        ## capture destroy (e.g. from window controls
        ## otherwise the tkwait hangs with nowhere to go
        tkbind(tt, "<Destroy>", function()tclvalue(done)<-2)

        tkwait.variable(done)

        if(tclvalue(done)=="2") stop("aborted")

        tkdestroy(tt)
        cmd <- build()
        eval.parent(cmd)
    }
