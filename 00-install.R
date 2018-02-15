get_package <- function(x){
    if (!require(x, character.only = TRUE)){
        if (x == "ggcompoplot"){
            if (!require("devtools")){
                install.packages("devtools", repos = "https://cran.rstudio.com")
                require("devtools")
            }
            install_github("zkamvar/ggcompoplot")
        }
        install.packages(x, repos = "https://cran.rstudio.com")
    }
}

get_package("poppr")
get_package("pegas")
get_package("igraph")
get_package("ggcompoplot")
get_package("tidyverse")
get_package("lubridate")
get_package("readxl")
get_package("here")
get_package("sessioninfo")
options(width = 200)
sessioninfo::session_info(include_base = TRUE)
