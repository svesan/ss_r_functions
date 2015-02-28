ordplt <- function(bar, area, ylab="Cumulative proportion", ...) {

# Author...: sven.sandin@meb.ki.se
# Date.....: 15-Dec-2004
#
# bar......: A factor. One bar is drawn for each factor level
# area.....: A factor. The segments within bars
     
tmp0 <- as.matrix(table(area, bar))
tmp  <- tmp0
print(tmp)

for (i in 1:NROW(tmp) ) {
for (j in 1:NCOL(tmp) ) tmp[i,j] <- tmp[i,j]/sum(tmp0[,j])
}
print(tmp)

barplot(tmp, angle = 15+10*1:5, density = 20, 
col = c("lightblue", "mistyrose", "lightcyan","lavender", "cornsilk"), legend = rownames(tmp), ylab=ylab, ...)
#title(main = list("Death Rates in Virginia", font = 4))
}

#ordplt(mort$physact, mort$cbmi, xlab="Physical Activity Score")
