#!/usr/bin/env Rscript

usage <- function() {
    cat('Usage: Rscript plot.R <Options> [Input report] <Output PDF>\n');
    cat('  -s   Graph only\n');
    cat('  -c   Plot CFGs [default]\n');
    cat('  -i   Plot instructions\n');
	cat('  -f   First half [default]\n');
	cat('  -l   Last half\n');
	quit();
}

simple <- FALSE;
type_cfg <- TRUE;
half_first <- TRUE;
args <- commandArgs(trailingOnly=TRUE);
for (a in args) {
    if (startsWith(a, '-')) {
        if (a == '-s') {
            simple <- TRUE;
        } else if (a == '-c') {
            type_cfg <- TRUE;
        } else if (a == '-i') {
            type_cfg <- FALSE;
		} else if (a == '-f') {
            half_first <- TRUE;
		} else if (a == '-l') {
            half_first <- FALSE;
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

benchmarks <- unique(data$benchmark);
process <- vector("list", length(benchmarks) / 2);

lowest = 100;
highest = 0;

n <- 1;
for (bench in benchmarks) {
	load <- if (half_first) n <= 16 else n > 16;
	if (load) {
		names(process)[if (half_first) n else n - 16] <- bench;
		mvalue <- subset(data, benchmark == bench & dataset == "max")[,c("count")];

		mds <- max(as.numeric(as.vector(subset(data, benchmark == bench & dataset != "max")[,c("dataset")])));
		stopifnot(mds > 0);
		for (c in 1:20) {
			c2 <- trunc(c * mds / 20);
			v <- (subset(data, benchmark == bench & dataset == c2)[,c("count")] / mvalue) * 100;

			lowest <- min(lowest, v);
			highest <- max(highest, v);

			process[[bench]][c] <- v;
		}
	}

	n <- n + 1;
}

pdf(file=output, width=10, height=6, bg="white");

if (simple) {
	par(oma=c(1.5,1.5,0,0),mar=c(1,1,1,1));
} else {
	par(oma=c(1.75,1.75,1,11.5),mar=c(1.75,1.75,0,0));
}

p = plot(1, 1, xlim=c(1, 20), ylim=c(lowest, highest), type="n", axes=F, ann=F);

axis(side=1, at=c(1:20), cex.axis = 1.0);
axis(side=2, at=pretty(c(lowest, highest)), cex.axis = 1.0);
box();

n_bms <- length(process);
colors <- rainbow(n_bms);
linetype <- c(1:n_bms);
plotchar <- seq(1, 1 + n_bms, 1);

n <- 1;
for (bench in names(process)) {
	lines(c(1:20), process[[bench]], type="b", lwd=1.5,
				lty=linetype[n], col=colors[n], pch=plotchar[n]);
	n <- n + 1;
}

if (!simple) {
	mtext(text="Data set", side=1, line=0.5, cex=1.1, outer=TRUE);
	mtext(text=if (type_cfg) "CFGs (%)" else "Instructions (%)", side=2, line=0.5, cex=1.1, outer=TRUE);
	legend("topright", inset=c(if (half_first) -0.32 else -0.31, 0.15), legend=names(process), cex=1.0, col=colors,
		pch=plotchar, lty=linetype, title="Benchmark", xpd=NA);
}
