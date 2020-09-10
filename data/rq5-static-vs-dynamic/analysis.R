#!/usr/bin/env Rscript

usage <- function() {
  cat('Usage: Rscript analysis.R [Input file]\n');
  quit();
}

args <- commandArgs(trailingOnly=TRUE);
if (length(args) != 1)
  usage();

data <- read.csv(args[1], header = TRUE, sep = ',');

cfgs <- sum(data$cfgsM) + sum(data$cfgsS) + sum(data$cfgsD);
total <- sum(data$cfgsT);

cat(paste("CFGs coverage: ", cfgs, " (", round(cfgs * 100 / total, digits=1), "%) of ", total, " (100%)\n\n", sep = ''));

info <- c("cfgs", "blocks", "edges", "instrs", "calls");
types <- c("cfggrind (A)", "dyninst (B)", "A∩B", "A\\B", "B\\A");

process <- matrix(0, nrow = length(types), ncol = length(info));
rownames(process) <- info;
colnames(process) <- types;

for (i in info) {
    matched <- sum(data[,paste(i, "M", sep = '')]);
    static <- sum(data[,paste(i, "S", sep = '')]);
    dynamic <- sum(data[,paste(i, "D", sep = '')]);
    process[i,] <- c(matched + dynamic, matched + static, matched, dynamic, static);
}

print(process);
cat("\n");

process2 <- matrix("", nrow = length(types), ncol = length(info) - 2);
rownames(process2) <- info;
colnames(process2) <- tail(types, -2);
for (i in info) {
    process2[i, types[3]] <- paste(round(process[i,types[3]] / process[i,types[1]] * 100, digits = 1), '%/', 
          round(process[i,types[3]] / process[i,types[2]] * 100, digits = 1), '%', sep = '');
    process2[i, types[4]] <- paste(round(process[i,types[4]] / process[i,types[1]] * 100, digits = 1), '%', sep = '');
    process2[i, types[5]] <- paste(round(process[i,types[5]] / process[i,types[2]] * 100, digits = 1), '%', sep = '');
}
print(process2);
