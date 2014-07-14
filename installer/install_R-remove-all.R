
# sudo R --no-save --no-restore < ~/scripts/installer/install_R-remove-all.R

options(repos="http://cran.ism.ac.jp")
packs <- available.packages(contriburl=contrib.url("http://cran.ism.ac.jp/"))
remove.packages(packs[,1],lib="/usr/local/lib/R/site-library")
library()

