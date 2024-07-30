## _________________ Infos générales
## Nom : id.year.a.enlever.R
## Auteur : Arnaud Lheureux
## Email: arnaud.lheureux@sorbonne-universite.fr
##
## Date de création : 18 juin 2024
##
## Copyright (c) Arnaud Lheureux, 2024
## R version : 4.4
## _________________

## _________________ Notes
#
#' Fonction qui installe et charge les packages (CRAN et GitHub)
#' @param pkg Vecteur avec nom de packages en texte
#
## ---------------------------
Load.Install.pkg <- function(github.pkg, cran.pkg = NULL, upgrade = TRUE) {
  cat(
    "\n ---------------------------------------- \n",
    "Recherche de mise à jour de pak",
    "\n ---------------------------------------- \n"
  )
  pak::pak_update()

  cat(
    "\n ---------------------------------------- \n",
    "Installation et / ou mise à jour + chargement des libraries github",
    "\n ---------------------------------------- \n"
  )
  install_method_test <- paste("pak::pkg_install", "(c(", paste0("'", github.pkg, "'", collapse = ", "), "), ask = FALSE, upgrade = upgrade)", sep = "")
  tryCatch(
    eval(parse(text = install_method_test)),
    error = function(e) {
      return()
    }
  )
  spl.pkg <- strsplit(github.pkg, split = "/")
  github.pkg2 <- unlist(lapply(1:length(spl.pkg), function(x) spl.pkg[[x]][2]))
  invisible(sapply(github.pkg2, require, character.only = TRUE))



  cat(
    "\n ---------------------------------------- \n",
    "Installation et / ou mise à jour + chargement des libraries CRAN",
    "\n ---------------------------------------- \n"
  )
  install_method_test <- paste("pak::pkg_install", "(c(", paste0("'", cran.pkg, "'", collapse = ", "), "), ask = FALSE, upgrade = upgrade)", sep = "")
  tryCatch(
    eval(parse(text = install_method_test)),
    error = function(e) {
      return()
    }
  )

  invisible(sapply(cran.pkg, require, character.only = TRUE))
}

# ipak <- function(pkg) {
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]

#   if (length(new.pkg)) {
#     # install.packages(new.pkg, dependencies = TRUE)
#     pak::pkg_install(new.pkg)
#   }

#   sapply(pkg, require, character.only = TRUE)

#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]


#   if ("rnaturalearthhires" %in% new.pkg) {
#     install_github("ropensci/rnaturalearthhires")
#   }


#   if (length(new.pkg)) {
#     cat(new.pkg, ":\n", "--> Pas trouvé dans cran, j'installe avec githubinstall depuis github.", 
#         "\n --> Si jamais ÃƒÂ§a n'a pas marché, relancer cette ligne avec githubinstall.\n")
#     githubinstall::githubinstall(new.pkg)
#     sapply(new.pkg, require, character.only = TRUE)
#   }
# }












# ipak_dev function: install and load multiple R packages using 'devtools::install_' options.
# check to see if packages are installed. Install them if they are not, then load them into the R session.
#
# install_method = "github" is the default which creates "devtools::install_github('examplerepo/examplepackage')".
# "github" can be replaced with any of the default install options for devtools, such at c("git","bitbucket","cran","local" etc..
# NOTE not all of the options have been tested as I created this for my own personal use which is mainly through github.
#
# install_method = "base" is if you have a list of packages that are to be installed using the base install.packaes() function
#
# This script was adapted from the ipak function created by @stevenworthington

# Load.Install.pkg <- function(pkg, base.pkg = NULL){

#   if(is.null(base.pkg) == FALSE){
#     new.base.pkg <- base.pkg[!(base.pkg %in% installed.packages()[, "Package"])]
#     if (length(new.base.pkg))
#       #install.packages(new.base.pkg, dependencies = TRUE)
#       pak::pkg_install(new.base.pkg, upgrade = TRUE, ask = FALSE)

#     #split string to search for previouslly already installed packages
#     spl.pkg <- strsplit(pkg, split = "/")
#     dev.pkg <- unlist(lapply(1:length(pkg), function(x) spl.pkg[[x]][2]))
#     rep.pkg <- unlist(lapply(1:length(pkg), function(y) spl.pkg[[y]][1]))
#     df.pkg  <- data.frame(rep.pkg,dev.pkg, pkg)
#     lst.pkg <- dev.pkg[!(dev.pkg %in% installed.packages()[, "Package"])]

#     #list of packages to install
#     new.pkg <- df.pkg[df.pkg$dev.pkg %in% lst.pkg,3]

#     #create the devtools string for each package
#     install_method_tmp  <- paste("pak::pkg_install", sep="_")
#     install_method_test <- paste(install_method_tmp,"('", new.pkg,"', ask = FALSE, upgrade = TRUE)", sep="")

#     if (length(new.pkg) > 0){
#       #install and require
#       eval(parse(text=install_method_test))
#     }else{}
#     #combine both lists
#     all.pkg <- c(dev.pkg,base.pkg)
#     invisible(sapply(all.pkg, require, character.only = TRUE))

#   }else{
#     #split string to search for previouslly already installed packages
#     spl.pkg <- strsplit(pkg, split = "/")
#     dev.pkg <- unlist(lapply(1:length(pkg), function(x) spl.pkg[[x]][2]))
#     rep.pkg <- unlist(lapply(1:length(pkg), function(y) spl.pkg[[y]][1]))
#     df.pkg  <- data.frame(rep.pkg,dev.pkg, pkg)
#     lst.pkg <- dev.pkg[!(dev.pkg %in% installed.packages()[, "Package"])]

#     #list of packages to install
#     new.pkg <- df.pkg[df.pkg$dev.pkg == lst.pkg,3]

#     #create the devtools string for each package
#     install_method_tmp  <- paste("pak::pkg_install",install_method, sep="_")
#     install_method_test <- paste(install_method_tmp,"('", new.pkg,"')", sep="")

#     if (length(new.pkg)){
#       #install and require
#       eval(parse(text=install_method_test))}
#     invisible(sapply(dev.pkg, require, character.only = TRUE))

#   }

# }















#  download_url <- "https://raw.githubusercontent.com/hoxo-m/gepuro-task-views-copy/master/package_list.txt"
#   package_list <- fread(download_url, sep="\t", header = FALSE, stringsAsFactors = FALSE,
#                         colClasses = c("character", "character", "character"),
#                         col.names = c("username", "package_name", "title"),
#                         showProgress = FALSE, na.strings=NULL)





# which(package_list[,2] %like% packages)
