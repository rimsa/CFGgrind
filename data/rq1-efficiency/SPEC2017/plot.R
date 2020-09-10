#!/usr/bin/env Rscript

usage <- function() {
    cat('Usage: Rscript plot.R <Options> [Input report] <Output PDF>\n');
    cat('    -s     Graph only\n');
    cat('    -a     Use arithmetic mean\n');
    cat('    -m     Exclude mean\n');
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

simple <- FALSE;
arith <- FALSE;
avg <- TRUE;
args <- commandArgs(trailingOnly=TRUE);
for (a in args) {
    if (startsWith(a, '-')) {
        if (a == '-s') {
            simple <- TRUE;
        } else if (a == '-a') {
            if (!avg)
                stop("cannot combine -a with -m");
            arith <- TRUE;
        } else if (a == '-m') {
            if (arith)
                stop("cannot combine -m with -a");
            avg <- FALSE;
        } else {
            usage();
        }
      } else {
        if (exists('input')) {
            if (exists('output')) {
                usage();
            } else {
                output <- a;
            }
        } else {
            input <- a;
        }
    }
}

if (!exists('input'))
    usage();

if (!exists('output'))
    output <- "output.pdf";

data <- read.csv(input, header = TRUE, sep = ',');
if (avg)
    data <- rbind(data, data.frame(
                benchmark="average",
                calcColMeans(data[,2:6], geom=!arith)));

process <- t(as.matrix(data[,c(2,3,4,6)]));
colnames(process) <- data$benchmark;

pdf(file=output, width=12, height=5);
if (simple) {
    par(oma=c(1.5,4,2,1),mar=c(0,0,0,0));
} else {
    par(oma=c(0,0,1,0),mar=c(6.5,4,0,6.5));
}

colors <- c("#14a1ad", "#a9af03", "#f95c3a", "#3414ad");
ptypes <- c(19, 19, 19, 19);

p = plot(1, 1, xlim=c(1,length(data$benchmark)), ylim=c(min(process),max(process)), type="n", xlab="", xaxt="n", las=1, ann=F, log="y");

for (n in 1:length(colors)) {
    points(process[c(n),], pch=ptypes[n], col=colors[n], cex=2);
}

if (!simple) {
    modes <- c("original", "nulgrind", "callgrind", "CFGgrind");

    mtext(text="Time (s)", at=0.60, side=2, line=-1.5, outer=TRUE);
    text(1:length(data$benchmark), min(process)-90, srt=45, adj=1, labels=colnames(process), xpd=T, cex=1.2, offset=10);

    legend(length(data$benchmark)+2.15, max(process)/15, modes, border=T, pch=ptypes, col=colors, cex=1, pt.cex=1.5, xpd=NA);
}
