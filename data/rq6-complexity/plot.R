#!/usr/bin/env Rscript

usage <- function() {
    cat('Usage: Rscript plot.R <Options> [cBench data] [spec data] <Output PDF>\n');
    cat('    -s     Graph only\n');
    cat('    -e     Executed instructions\n');
    cat('    -r     Executed runtime\n');
    quit();
}

getrange <- function(from, to) {
    c(floor(log10(from)), ceiling(log10(to)));
}

plotaxis <- function(a, power) {
    at <- c(10 ^ (power[1]:power[2]));
    ly <- sapply(at, function(i) {
            as.expression(bquote(10^ .(log10(i))))
        });
    axis(a, at=at, labels=ly, las=1, cex.axis=1.3);
}

simple <- FALSE;
extra <- c();
args <- commandArgs(trailingOnly=TRUE);
for (a in args) {
    if (startsWith(a, '-')) {
        if (a == '-s') {
            simple <- TRUE;
        } else if (a == '-e') {
            type <- "executed";
        } else if (a == '-r') {
            type <- "nulgrind";
        } else {
            usage();
        }
    } else {
        extra <- append(extra, a);
    }
}

if (length(extra) < 2 || length(extra) > 3 || !exists('type'))
    usage();

file1 <- extra[1];
file2 <- extra[2];
output <- extra[3];
if (is.na(output))
    output <- "output.pdf";

pdf(file=output, width=8, height=6);
if (simple) {
    par(oma=c(2.5,2.5,1,1.5),mar=c(0,0,0,0));
} else {
    par(oma=c(2.5,2.5,1,1.5),mar=c(1.5,2,0,0.5));
}

data1 <- read.csv(file1, header = TRUE, sep = ',');
data2 <- read.csv(file2, header = TRUE, sep = ',');

matrix1 <- as.matrix(data1[,c(type, "cfggrind")]);
matrix2 <- as.matrix(data2[,c(type, "cfggrind")]);

xpower <- getrange(min(matrix1[,1], matrix2[,1]), max(matrix1[,1], matrix2[,1]));
ypower <- getrange(min(matrix1[,2], matrix2[,2]), max(matrix1[,2], matrix2[,2]));

plot(1, 1, xlab="", xlim=c(10 ** xpower[1], 10 ** xpower[2]),
          ylab="", ylim=c(10 ** ypower[1], 10 ** ypower[2]),
          log="xy", xaxt="n", yaxt="n");
plotaxis(1, xpower);
plotaxis(2, ypower);

colors <- c("#3414ad", "#d20000");
ptypes <- c(2, 1);

points(matrix1, pch=ptypes[1], col=colors[1], cex=1.5, lwd=2);
points(matrix2, pch=ptypes[2], col=colors[2], cex=1.5, lwd=2);

if (!simple) {
    if (type == "executed") {
        legend(10**(xpower[1]-0.12), 10**(ypower[2]+0.2)+0, c("cBench", "SPEC CPU2017"),
            cex=1.3, pch=ptypes,
            col=colors, pt.cex=1.5, pt.lwd=2, xpd=NA);
        mtext(text="Executed instructions (in billions)",
            side=1, cex=1.5, line=1, outer=TRUE);
    } else {
        legend(10**(xpower[1]-0.20), 10**(ypower[2]+0.2)+0, c("cBench", "SPEC CPU2017"),
            cex=1.3, pch=ptypes,
            col=colors, pt.cex=1.5, pt.lwd=2, xpd=NA);
        mtext(text="nulgrind runtime (in seconds)",
            side=1, cex=1.5, line=1, outer=TRUE);
    }
    mtext(text="CFGgrind runtime (in seconds)", side=2, cex=1.5, line=1, outer=TRUE);
}
