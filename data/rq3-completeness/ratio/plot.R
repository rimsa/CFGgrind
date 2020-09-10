#!/usr/bin/env Rscript

usage <- function() {
    cat('Usage: Rscript plot.R <Options> [Input Report] <Output PDF>\n');
    cat('    -s     Graph only\n');
    cat('    -n     Normalize\n');
    cat('    -m     Exclude mean\n');
    cat('    -a     Use arithmetic mean\n');
    cat('    -r     Exclude rate\n');
    quit();
}

compound <- function(x) {
    last <- 0;
    unlist(
        lapply(x, function (v) {
            tmp <- v - last;
            last <<- v;
            return(tmp);
        })
    );
}

proportion <- function(frame, base, top) {
    return(apply(frame, 1, function(x) {
        rate <- round((x[c(base)] / x[c(top)]) * 100, digits = 0);
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

simple <- FALSE;
avg <- TRUE;
rate <- TRUE;
norm <- FALSE;
arith <- FALSE;
extra <- c();
args <- commandArgs(trailingOnly=TRUE);
for (a in args) {
    if (startsWith(a, '-')) {
        if (a == '-s') {
            simple <- TRUE;
        } else if (a == '-n') {
            norm <- TRUE;
        } else if (a == '-a') {
            if (!avg)
                stop("cannot combine -a with -m");
            arith <- TRUE;
        } else if (a == '-m') {
            if (arith)
                stop("cannot combine -m with -a");
            avg <- FALSE;
        } else if (a == '-r') {
            rate <- FALSE;
        } else {
            usage();
        }
      } else {
        extra <- append(extra, a);
    }
}

if (length(extra) < 1 || length(extra) > 2)
    usage();

file <- extra[1];
output <- extra[2];
if (is.na(output))
    output <- "output.pdf";

data <- read.csv(file, header = TRUE, sep = ',');
if (avg)
    data <- rbind(data, data.frame(
                benchmark="average",
                calcColMeans(data[,2:4], geom=!arith)));

data$unreached <- c(data[,c(2)] - rowSums(data[,c(3,4)]));

data$completeRate <- proportion(data[,c(3,2)], "complete", "all");
data$incompleteRate <- proportion(data[,c(4,2)], "incomplete", "all");
data$unreachedRate <- proportion(data[,c(5,2)], "unreached", "all");

if (norm) {
    process <- matrix(0, nrow = 3, ncol = nrow(data));
    colnames(process) <- data$benchmark;
    rownames(process) <- colnames(data)[c(5:3)];
    for (b in data$benchmark) {
        process[,b] <- unlist(subset(data, benchmark == b)[,5:3] * 100) / subset(data, benchmark == b)[,2];
    }
} else {
    process <- t(as.matrix(data[,c(5:3)]));
    colnames(process) <- data$benchmark;
}

pdf(file=output, width=12, height=5);
if (simple) {
    par(oma=c(1.5,4,2,0),mar=c(0,0,0,0));
} else {
    par(oma=c(0,1.5,1,0),mar=c(6.5,3.5,0,if (norm) 4.5 else 0));
}

colors <- c("#a9af03", "#d20000", "#3414ad");

if (norm) {
    lim <- range(pretty(c(0, 100)));
    p <- barplot(process, beside=!norm, col=colors, xlab="", ylim=lim, xaxt="n", las=1);
} else {
    lim <- c(1, max(process) * 2);
    p <- barplot(process, beside=!norm, col=colors, xlab="", ylim=lim, log="y", xaxt="n", las=1);
}

if (rate) {
    if (norm) {
        text(x=p, y=process[1,] / 2, label=data$unreachedRate, cex=0.5, offset=0, col="white");
        text(x=p, y=process[1,] + (process[2,] / 2), label=data$incompleteRate, cex=0.5, offset=0, col="black");
		text(x=p, y=process[1,] + process[2,] + (process[3,] / 2), label=data$completeRate, cex=0.5, offset=0, col="white");
    } else {
        labelvalues <- c(t(data[,8:6]));
        yvalues <- c(t(data[,5:3]));
        text(x=(p+0.45), y=yvalues, label=labelvalues, srt=90, pos=3, cex=0.70, offset=0.85, col="black");
    }
}

if (!simple) {
    yname = paste("Control Flow Graphs", (if (norm) " (%)" else " (#)"), sep="");
    mtext(text=yname, at=0.60, side=2, line=0, outer=TRUE);

    if (norm) {
        text(p, ((lim[2] / 1000) * -30), srt=45, adj=1, labels=colnames(process), xpd=T, cex=1, offset=10);
        legend(max(p) + ((max(p) / 100) * 2), lim[2] / 2, rev(rownames(process)), cex=0.8, fill=rev(colors), xpd=NA);
    } else {
        text(colMeans(p), 0.8, srt=45, adj=1, labels=colnames(process), xpd=T, cex=1, offset=10);
        legend(max(p) - (max(p) / 10), lim[2], rev(rownames(process)), cex=0.8, fill=rev(colors), xpd=NA);
    }
}
