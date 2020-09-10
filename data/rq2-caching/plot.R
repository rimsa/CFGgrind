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

calcRowMeans = function(frame, geom=FALSE, na.rm=TRUE) {
    if (geom) {
        l <- list();
        for (r in 1:nrow(frame))
            l[r] <- geometric.mean(frame[r,], na.rm=na.rm);
        names(l) <- rownames(frame);
	return(l);
    } else {
        return(as.list(rowMeans(frame)));
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

if (avg) {
    data[,c("average")] <- unlist(calcRowMeans(data[,2:11], geom=!arith));
    rel <- cbind(perlbench,gcc,mcf,omnetpp,xalancbmk,x264,deepsjeng,leela,exchange2,xz,average) ~ mode;
} else {
    rel <- cbind(perlbench,gcc,mcf,omnetpp,xalancbmk,x264,deepsjeng,leela,exchange2,xz) ~ mode;
}

bilan <- aggregate(rel, data=data, if (arith) mean else geometric.mean);
rownames(bilan) <- bilan[,1];
bilan <- as.matrix(bilan[,-1]);

for (r in 2:nrow(bilan)) {
    cat(paste(rownames(bilan)[r], " has a speedup of ", round(bilan[1,ncol(bilan)] / bilan[r,ncol(bilan)], digit=2), " times\n", sep=""));
}

pdf(file=output, width=12, height=5);
if (simple) {
    par(oma=c(2.5,3,2,0),mar=c(0,0,0,0))
} else {
    par(oma=c(2.5,4,2,0),mar=c(2,0,0,0))
}

colors <- c("#d20000", "#f95c3a", "#a9af03", "#458a26", "#33dcce", "#14a1ad", "#3414ad", "#b018e2", "#e21877");
barplot(bilan, beside=T, col=colors, ylim=range(pretty(c(0, max(bilan)))));

if (!simple) {
    modes <- c("no cache", "size 2", "size 4", "size 6", "size 8", "size 10", "size 12", "size 14", "size 16");
    legend(0, 6000, modes, cex=0.8, fill=colors, xpd=NA);
    mtext(text="Benchmarks",side=1,line=0.5,outer=TRUE);
    mtext(text="Time (s)",side=2,line=2.5,outer=TRUE);
}
