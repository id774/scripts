
# sudo R --no-save --no-restore < ~/scripts/installer/install_R-all.R

options(repos="http://cran.ism.ac.jp")
packs <- available.packages(contriburl=contrib.url("http://cran.ism.ac.jp/"))
install.packages(packs[,1],contriburl=contrib.url("http://cran.ism.ac.jp/"))
library()

