"writeperl" <-
function (childleftrow, childrightrow, treeobj, perlname, edmlabel, 
    htmldir, edmix, stateprintfn, requirelib) 
{
    cat("#!/usr/local/bin/perl\n\n", file = perlname, append = FALSE)
    cat("use Fcntl;\n", file = perlname, append = TRUE)
    cat("use File::Cat;\n", file = perlname, append = TRUE)
    cat("use POSIX qw(tmpnam);\n\n", file = perlname, append = TRUE)
    rlstring <- paste("require '", requirelib, "';\n", sep = "")
    cat(rlstring, file = perlname, append = TRUE)
    cat("if ($ENV{'REQUEST_METHOD'} eq 'GET') {\n", file = perlname, 
        append = TRUE)
    cat("	@pairs = split(/&/, $ENV{'QUERY_STRING'});\n", file = perlname, 
        append = TRUE)
    cat("} elsif ($ENV{'REQUEST_METHOD'} eq 'POST') {\n", file = perlname, 
        append = TRUE)
    cat("	read (STDIN, $buffer, $ENV{'CONTENT_LENGTH'});\n", 
        file = perlname, append = TRUE)
    cat("	@pairs = split(/&/, $buffer);\n", file = perlname, 
        append = TRUE)
    cat("} else {\n", file = perlname, append = TRUE)
    cat("	print \"Content-type: text/html\\n\\n\";\n", file = perlname, 
        append = TRUE)
    cat("	print \"<P>Use Post or Get\";\n", file = perlname, 
        append = TRUE)
    cat("}\n\n", file = perlname, append = TRUE)
    cat("foreach $pair (@pairs) {\n", file = perlname, append = TRUE)
    cat("	($key, $value) = split (/=/, $pair);\n", file = perlname, 
        append = TRUE)
    cat("	$key =~ tr/+/ /;\n", file = perlname, append = TRUE)
    cat("	$key =~ s/%([a-fA-F0-9] [a-fA-F0-9])/pack(\"C\", hex($1))/eg;\n", 
        file = perlname, append = TRUE)
    cat("	$value =~ tr/+/ /;\n", file = perlname, append = TRUE)
    cat("	$value =~ s/%([a-fA-F0-9] [a-fA-F0-9])/pack(\"C\", hex($1))/eg;\n", 
        file = perlname, append = TRUE)
    cat("	$value =~s/<!--(.|\\n)*-->//g;\n\n", file = perlname, 
        append = TRUE)
    cat("	if ($formdata{$key}) {\n", file = perlname, append = TRUE)
    cat("		$formdata{$key} .= \", $value\";\n", file = perlname, 
        append = TRUE)
    cat("	} else {\n", file = perlname, append = TRUE)
    cat("		$formdata{$key} = $value;\n", file = perlname, append = TRUE)
    cat("	}\n", file = perlname, append = TRUE)
    cat("}\n", file = perlname, append = TRUE)
    cat("print \"Content-type: text/html\\n\\n\";\n", file = perlname, 
        append = TRUE)
    cat("@edms = split(/,/, $formdata{'EDMQ'});\n", file = perlname, 
        append = TRUE)
    cat("undef %isedms;\n", file = perlname, append = TRUE)
    cat("foreach (@edms) { $isedms{$_} = 1 }\n", file = perlname, 
        append = TRUE)
    cat("\n\n", file = perlname, append = TRUE)
    childleftvar <- as.character(treeobj$frame[childleftrow, 
        "var"])
    childrightvar <- as.character(treeobj$frame[childrightrow, 
        "var"])
    childleftnodeid <- edmix[childleftrow]
    childrightnodeid <- edmix[childrightrow]
    cat(paste("if (exists $isedms{'", edmlabel, "'})	{\n", sep = ""), 
        file = perlname, append = TRUE)
    writeperloption(childrightvar, childrightrow, childrightnodeid, 
        treeobj, perlname, append = TRUE, htmldir = htmldir, 
        stateprintfn = stateprintfn)
    cat("}\n else	{\n", file = perlname, append = TRUE)
    writeperloption(childleftvar, childleftrow, childleftnodeid, 
        treeobj, perlname, append = TRUE, htmldir = htmldir, 
        stateprintfn = stateprintfn)
    cat("}\n", file = perlname, append = TRUE)
}
