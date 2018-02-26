# An installation file for binder
packages <- readLines("00-install.R")
packages <- packages[grep("get_package[(]", packages)]
packages <- gsub("get_package", "install.packages", packages)
for (pkg in seq(packages)) {
  eval(parse(text = packages[pkg]))
}
