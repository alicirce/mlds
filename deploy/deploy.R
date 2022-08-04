# install from github to ensure remotes are handled correctly
devtools::install_github("alicirce/mlds", force = TRUE)
rsconnect::deployApp('inst/application', appName = "mlds")
