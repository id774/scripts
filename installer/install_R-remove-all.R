
# sudo R --no-save --no-restore < ~/scripts/installer/install_R-remove-all.R

mirror <- "http://cran.ism.ac.jp"
options(repos=mirror)
packs <- available.packages(contriburl=contrib.url(mirror))
remove.packages(packs[,1],lib="/usr/local/lib/R/site-library")
library()

