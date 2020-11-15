
rm(list = ls()); # Clear environment.

setClass("property", slots = list(
  name = "character",
  build.cost = "numeric",
  mortgage.value = "numeric",
  level = "numeric",
  is.mortgaged = "logical"));

make.property <- function(name, build.cost) new("property", 
  name = name,
  build.cost = build.cost,
  mortgage.value = build.cost,
  level = 0,
  is.mortgaged = FALSE);

## Configuration.

cat # m=^.^=m~~ 
