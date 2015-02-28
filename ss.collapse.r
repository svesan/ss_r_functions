
ss.collapse <- function(x, y, old=NULL, new=OLD) {

if (class(x)!="factor") stop("x must be factor")

if (length(levels(x))!=length(old)) stop("Wrong number of factor levels in old")

if (length(old)<length(new)) stop("More levels in new than in old")

if ((levels(x) %in% old)==F) stop("Factor levels in OLD not correct")

y <- NA
for (i in 1:length(


summary(x)

}

ss.collapse(ana1$bmicat, gnu, old=c("0-18.5","18.5-25","25-30","30+"), new=c("s","d","g","sg","gg") )
