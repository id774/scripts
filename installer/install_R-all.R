
# sudo R --no-save --no-restore < ~/scripts/installer/install_R-all.R

mirror <- "http://cran.ism.ac.jp"
options(repos=mirror)
packs <- available.packages(contriburl=contrib.url(mirror))
install.packages(packs[,1],contriburl=contrib.url(mirror))
library()

