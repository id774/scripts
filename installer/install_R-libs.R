
# sudo R --no-save --no-restore < ~/scripts/installer/install_R-libs.R

options(repos="http://cran.ism.ac.jp")
install.packages('sem')
install.packages('TTR')
install.packages('biOps')
install.packages('igraph')
install.packages('kernlab')
install.packages('lattice')
install.packages('mvtnorm')
install.packages('plotrix')
install.packages('pracma')
install.packages('quadprog')
install.packages('tseries')
install.packages('xts')
install.packages('zoo')

