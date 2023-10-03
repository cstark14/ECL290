# we recommend running this is a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
cmdstanr::install_cmdstan(dir="C:/Users/valen/")
#cmdstanr::set_cmdstan_path(path="C:/Users/valen/")

install.packages("coda")
install.packages("devtools")
install.packages("loo")
install.packages("mvtnorm")
install.packages("dagitty")
install.packages("shape")
install.packages("tidyverse")
devtools::install_github("rmcelreath/rethinking")