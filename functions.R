totalWeight <- function(weights) {
  return (sum(weights))
}

calcCapacity <- function(weights, percent) {
  return (totalWeight(weights) * percent)
}

evalFunc <- function(x) {
  current_solution_survivalpoints <- x %*% dataset$survivalpoints
  current_solution_weight <- x %*% dataset$weight
  
  if (current_solution_weight > weightlimit) 
    return(0) else return(-current_solution_survivalpoints)
}

summary.rbga <- function(object, echo=FALSE, ...) {
  # should do object type checking here
  rbga.object = object
  
  output = paste(
    "GA Settings", "\n",
    "  Type                  = ", rbga.object$type, "\n",
    "  Population size       = ", rbga.object$popSize, "\n",
    "  Number of Generations = ", rbga.object$iters, "\n",
    "  Elitism               = ", rbga.object$elitism, "\n",
    "  Mutation Chance       = ", rbga.object$mutationChance, "\n",
    "\n",
    "Search Domain", "\n", sep="")
  for (var in 1:length(rbga.object$stringMin)) {
    minVar = rbga.object$stringMin[var]
    maxVar = rbga.object$stringMax[var]
    output = paste(output,
                   "  Var ", var, " = [", minVar, ",", maxVar, "]\n", sep="");
  }
  output = paste(output, "\n", sep="");
  if (!is.null(rbga.object$suggestions)) {
    optionPart = paste("Suggestions", "\n", sep="");
    for (suggestion in 1:dim(rbga.object$suggestions)[1]) {
      optionPart = paste(optionPart,
                         "  ", suggestion, " = (", sep="");
      for (var in 1:(dim(rbga.object$suggestions)[2]-1)) {
        optionPart = paste(optionPart,
                           rbga.object$suggestions[suggestion,var], ", ",
                           sep="");
      }
      # and the last one
      optionPart = paste(optionPart,
                         rbga.object$suggestions[suggestion,dim(rbga.object$suggestions)[2]],
                         ")\n", sep="");
    }
    output = paste(output, optionPart, "\n", sep="");
  }
  if (!is.null(rbga.object$population)) {
    optionPart = paste("GA Results", "\n", "  Best Solution : ", sep="");
    # ok, deal with the situation that more than one object is best
    filter = rbga.object$evaluations == min(rbga.object$evaluations);
    
    bestSolution = rbga.object$population[filter, , drop= FALSE][1,];  
    # drop= FALSE: needed if one dimension is 1 (vars)
    # this statement takes all cases
    
    for (var in 1:length(bestSolution)) {
      optionPart = paste(optionPart,
                         bestSolution[var], " ",
                         sep="");
    }
    output = paste(output, optionPart, "\n", sep="");
  }
  if (echo) cat(output);
  invisible(output);
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
