"PTdialog.leaf" <-
function(leaftext){
        tt <- tktoplevel()
        tkwm.title(tt,"Leaf!")
	done <- tclVar(0)

        submit.but <- tkbutton(tt, text="OK",
                               command=function()tclvalue(done)<-1)

        build <- function()
        {
            ## notice that tclvalue() is correct here, since it is the
            ## string representation of xvar and yvar that is being
            ## displayed in the entry fields

        }
        alt.rbuts <- tkframe(tt)

        tkpack(tklabel(alt.rbuts, text=leaftext))

        tkgrid(tklabel(tt,text=leaftext),columnspan=1)
        tkgrid(submit.but)

        ## capture destroy (e.g. from window controls
        ## otherwise the tkwait hangs with nowhere to go
        tkbind(tt, "<Destroy>", function()tclvalue(done)<-2)

        tkwait.variable(done)

        if(tclvalue(done)=="2") stop("aborted")

        tkdestroy(tt)
    }
