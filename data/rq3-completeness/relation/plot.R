#!/usr/bin/env Rscript

usage <- function() {
    cat('Usage: Rscript plot.R <Options> [Complete file] [Incomplete file] <Output PDF>\n');
    cat('    -i     Instructions [default]\n');
    cat('    -b     Blocks\n');
    cat('    -s     Graph only\n');
    quit();
}

instrs <- TRUE;
simple <- FALSE;
extra <- c();
args <- commandArgs(trailingOnly=TRUE);
for (a in args) {
    if (startsWith(a, '-')) {
        if (a == '-s') {
            simple <- TRUE;
        } else if (a == '-i') {
            instrs <- TRUE;
        } else if (a == '-b') {
            instrs <- FALSE;
        } else {
            usage();
        }
      } else {
        extra <- append(extra, a);
    }
}

if (length(extra) < 2 || length(extra) > 3)
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
    par(oma=c(2.5,2.5,1,1.5),mar=c(1.5,1.5,0,0.5));
}

complete <- read.csv(file1, header = TRUE, sep = ',');
incomplete <- read.csv(file2, header = TRUE, sep = ',');

xmax = if (instrs) 1000 else 100;
ymax = max(complete$count, incomplete$count);

colors <- c("#3414ad", "#d20000");
ptypes <- c(2, 1);

plot(0, 1, xlab="", xlim=range(0, xmax), ylim=c(1, ymax), xaxs="i", yaxs="i", ylab="", log="y", cex.axis=1.3);
points(as.matrix(complete), pch=ptypes[1], col=colors[1], cex=1.5, lwd=2);
points(as.matrix(incomplete), pch=ptypes[2], col=colors[2], cex=1.5, lwd=2);

if (!simple) {
    if (instrs) {
        legend(xmax-232, ymax, c("Complete", "Incomplete"), cex=1.3, pch=ptypes, col=colors, pt.cex=1.5, pt.lwd=2, xpd=NA);
        mtext(text="Instructions", side=1, cex=1.5, line=1, outer=TRUE);
    } else {
        legend(xmax-23.2, ymax, c("Complete", "Incomplete"), cex=1.3, pch=ptypes, col=colors, pt.cex=1.5, pt.lwd=2, xpd=NA);
        mtext(text="Block Nodes", side=1, cex=1.5, line=1, outer=TRUE);
    }
    mtext(text="Control Flow Graphs (#)", side=2, cex=1.5, line=1, outer=TRUE);
}
