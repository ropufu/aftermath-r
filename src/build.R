

rm(list = ls()); # Clear environment.

build.local <- function(name) 
{
  setwd(paste("./", name, sep = ""));
  devtools::document();
  setwd("..");
  devtools::install(name);
}

# https://github.com/dgJacks0n/envDocument/blob/master/envDocument/R/getScriptPath.R
current_path <- as.character(sys.call(1))[2];
current_dir <- dirname(current_path);
setwd(current_dir);

#build.local("ropufu")
#build.local("changept")
