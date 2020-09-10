#!/usr/bin/env Rscript

usage <- function() {
    cat('Usage: Rscript plot.R <Options> [Input report]\n');
    cat('    -a     Use arithmetic mean\n');
    quit();
}

"geometric.mean" = function(x, na.rm=TRUE){
    exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}

calcColMeans = function(frame, geom=FALSE, na.rm=TRUE) {
    if (geom) {
        l <- list();
        for (c in 1:ncol(frame))
            l[c] <- geometric.mean(frame[,c], na.rm=na.rm);
        names(l) <- colnames(frame);
	return(l);
    } else {
        return(as.list(colMeans(frame)));
    }
}

calcTimes = function(v1, v2) {
    if (v1 <= v2) {
        return(paste("speedup of ", round(v2 / v1, digit=2), " times", sep=""));
    } else {
        return(paste("slowdown of ", round(v1 / v2, digit=2), " times", sep=""));
    }
}

arith <- FALSE;
args <- commandArgs(trailingOnly=TRUE);
for (a in args) {
    if (startsWith(a, '-')) {
        if (a == '-a') {
            arith <- TRUE;
        } else {
            usage();
        }
      } else {
        if (exists('input')) {
            usage();
        } else {
            input <- a;
        }
    }
}

if (!exists('input'))
    usage();

data <- read.csv(input, header = TRUE, sep = ',');
data <- rbind(data, data.frame(
            benchmark=if (arith) "arithmetic mean" else "geometric mean",
            calcColMeans(data[,2:8], geom=!arith)));

process <- t(as.matrix(data[,c(-1)]));
colnames(process) <- data$benchmark;

for (r1 in 1:nrow(process)) {
    for (r2 in 1:nrow(process)) {
        if (r1 != r2) {
            cat(paste(rownames(process)[r1], " (", round(process[r1,ncol(process)], digit=2), ") vs ", sep=""));
            cat(paste(rownames(process)[r2], " (", round(process[r2,ncol(process)], digit=2), "): ", sep=""));
            cat(paste(calcTimes(process[r1,ncol(process)], process[r2,ncol(process)]), "\n", sep=""));
        }
    }
    cat("\n");
}
