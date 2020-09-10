#!/usr/bin/env Rscript

usage <- function() {
    cat('Usage: Rscript analysis.R <Options> [Input Report]\n');
    cat('    -a     Use arithmetic mean\n');
    quit();
}

proportion <- function(frame, base, top) {
    return(apply(frame, 1, function(x) {
        rate <- round((x[1] / x[2]) * 100, digits = 0);
        return(paste(rate, "%", sep = ""));
    }));
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
            calcColMeans(data[,c(2:4)], geom=!arith)));

data$unreached <- c(data[,c(2)] - rowSums(data[,c(3,4)]));

data$completeRate <- proportion(data[,c(3,2)]);
data$incompleteRate <- proportion(data[,c(4,2)]);
data$unreachedRate <- proportion(data[,c(5,2)]);

allCount <- sum(data[1:nrow(data)-1,c("all")]);
completeCount <- sum(data[1:nrow(data)-1,c("complete")]);
incompleteCount <- sum(data[1:nrow(data)-1,c("incomplete")]);
coverageCount <- completeCount + incompleteCount;
unreachedCount <- sum(data[1:nrow(data)-1,c("unreached")]);
coverageRate <- paste(round((coverageCount / allCount) * 100, digits = 0), "%", sep = "");

cat(paste(data[nrow(data), c("benchmark")], "\n", sep=""));
cat(paste("covered:      ", coverageCount, " of ", allCount, " (", coverageRate, ") control flow graphs\n", sep=""));
cat(paste("  complete:   ", completeCount, " of ", allCount, " (", data[nrow(data),c("completeRate")], ") control flow graphs\n", sep=""));
cat(paste("  incomplete: ", incompleteCount, " of ", allCount, " (", data[nrow(data),c("incompleteRate")], ") control flow graphs\n", sep=""));
cat(paste("unreached:    ", unreachedCount, " of ", allCount, " (", data[nrow(data),c("unreachedRate")], ") control flow graphs\n", sep=""));