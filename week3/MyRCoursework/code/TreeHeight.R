# This function calculates heights of trees given distance of each tree 
# from its base and angle to its top, using  the trigonometric formula 
#
# height = distance * tan(radians)
#
# ARGUMENTS
# degrees:   The angle of elevation of tree
# distance:  The distance from base of tree (e.g., meters)
#
# OUTPUT
# The heights of the tree, same units as "distance"

trees <- read.csv("../data/trees.csv")

TreeHeight <- function(degrees, distance) {
  radians <- degrees * pi / 180
  height <- distance * tan(radians)
  
  return (height)
}

TreeHeight(trees$Angle.degrees, trees$Distance)

trees$Tree.Height.m <- TreeHeight(trees$Angle.degrees, trees$Distance)


write.csv(trees, "../results/TreeHts.csv", row.names = FALSE)
