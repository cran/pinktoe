\name{pinktoe-internal}
\title{Internal pinktoe functions}
[1] "genedmhtml"        "genericcommonhtml" "kyphosisprint"    
[4] "kyphosis.text"     "kyphosistittext"   "writehtml"        
[7] "writeperl"         "writeperloption"   "z.kyphosis" 

\alias{genedmhtml}
\alias{genericcommonhtml}
\alias{kyphosisprint}
\alias{kyphosis.text}
\alias{kyphosistittext}
\alias{writehtml}
\alias{writeperl}
\alias{writeperloption}
\alias{z.kyphosis}
\description{
  Internal pinktoe functions.
}
\usage{
genedmhtml(treeobj, noderow, edmtextfn, edmtittext, cgibindir, htmldir,
	localdir, stateprintfn, requirelib, commonhtml)

genericcommonhtml(file, append)

kyphosisprint(yval, file = "", append = F)

kyphosisprint(yval, file = "", append = F)

kyphosis.text(n, file = "", append = F)

kyphosistittext(label)

writehtml(edmlabel, uniqedmlab, edmtextfn, edmtittext, cgibindir, localdir, 
	commonhtml, numvar, numstring, treeobj)

writeperl(childleftrow, childrightrow, treeobj, perlname, edmlabel, htmldir, 
	edmix, stateprintfn, requirelib)

writeperloption(childvar, childrow, childnodeid, treeobj, perlname, append = T, 
	htmldir, stateprintfn)

}
\details{
  These are not to be called by the user.
}
\keyword{utilities}
