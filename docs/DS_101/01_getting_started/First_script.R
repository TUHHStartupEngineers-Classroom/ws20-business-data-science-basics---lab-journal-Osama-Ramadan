# This is my first R script 

roll <- function(faces)
{
  dice <- sample(faces, size = 2, replace = TRUE)
  sum(dice)
}

roll(faces = 1:6)
