library(genalg)
library(ggplot2)

setwd("~/Copy/Cesupa/CC7TA/Inteligência Artificial II/SBC - AG")
source("functions.R")

indSize <- 50

dataset <- data.frame(item = c("pocketknife", "beans", "potatoes", "unions","sleeping bag", "rope", "compass", "pen", "mouse", "cel", "card", "notebook", "cel1", "cel2", "cel3", "cel4", "cel5", "cel6", "cel7", "cel8", "cel9", "cel10", "cel11", "cel12", "cel13", "cel14", "ce15l", "cel16", "cel17", "cel18", "cel19", "cel20", "cel21", "cel22", "cel23", "cel24", "cel25", "cel26", "cel27", "cel28", "ce2l9", "cel30", "cel31", "cel32", "cel33", "cel34", "cel35", "cel36", "cel37", "cel38"), 
                      survivalpoints = sample(1:20, indSize, replace=T), 
                      weight = sample(1:30, indSize, replace=T))

percent <- 0.15
weightlimit <- calcCapacity(weights = dataset$weight, percent = percent)

#number of generations
iter <- 100
GAmodel <- rbga.bin(size = indSize, popSize = 50, iters = iter, mutationChance = 1, 
                    elitism = T, evalFunc = evalFunc)
cat(summary.rbga(GAmodel))

# Best Solution
solution = c(1, 1, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1)
dataset[solution == 1, ]

# solution vs abailable
cat(paste(solution %*% dataset$survivalpoints, "/", sum(dataset$survivalpoints)))

# First plot capacity 30
for (i in seq(1, iter)) {
  temp <- data.frame(Geração = c(seq(1, i), seq(1, i)), Variavel = c(rep("médio",i), 
                                                                     rep("melhor", i)), Fitness = c(-GAmodel$mean[1:i], -GAmodel$best[1:i]))
  
  p1 <- ggplot(temp, aes(x = Geração, y = Fitness, group = Variavel, 
                         colour = Variavel)) + geom_line() + scale_x_continuous(limits = c(0, iter)) + 
    scale_y_continuous(limits = c(0, 210)) + geom_hline(y = max(temp$Fitness),lty = 2) + 
    annotate("text", x = 1, y = max(temp$Fitness) + 2, hjust = 0, size = 3, color = "black", 
             label = paste("Melhor solução:", max(temp$Fitness))) + scale_colour_brewer(palette = "Set1") + 
    labs(title = "Média das execuções com capacidade = 30%")
  print(p1)
}

# Second plot capacity 15
for (i in seq(1, iter)) {
  temp <- data.frame(Geração = c(seq(1, i), seq(1, i)), Variavel = c(rep("médio",i), 
                                                                     rep("melhor", i)), Fitness = c(-GAmodel$mean[1:i], -GAmodel$best[1:i]))
  
  p2 <- ggplot(temp, aes(x = Geração, y = Fitness, group = Variavel, 
                         colour = Variavel)) + geom_line() + scale_x_continuous(limits = c(0, iter)) + 
    scale_y_continuous(limits = c(0, 210)) + geom_hline(y = max(temp$Fitness),lty = 2) + 
    annotate("text", x = 1, y = max(temp$Fitness) + 2, hjust = 0, size = 3, color = "black", 
             label = paste("Melhor solução:", max(temp$Fitness))) + scale_colour_brewer(palette = "Set1") + 
    labs(title = "Média das execuções com capacidade = 15%")
  print(p2)
}

# Third plot capacity 60
for (i in seq(1, iter)) {
  temp <- data.frame(Geração = c(seq(1, i), seq(1, i)), Variavel = c(rep("médio",i), 
                                                                     rep("melhor", i)), Fitness = c(-GAmodel$mean[1:i], -GAmodel$best[1:i]))
  
  p3 <- ggplot(temp, aes(x = Geração, y = Fitness, group = Variavel, 
                         colour = Variavel)) + geom_line() + scale_x_continuous(limits = c(0, iter)) + 
    scale_y_continuous(limits = c(0, 210)) + geom_hline(y = max(temp$Fitness),lty = 2) + 
    annotate("text", x = 1, y = max(temp$Fitness) + 2, hjust = 0, size = 3, color = "black", 
             label = paste("Melhor solução:", max(temp$Fitness))) + scale_colour_brewer(palette = "Set1") + 
    labs(title = "Média das execuções com capacidade = 60%")
  print(p3)
}

# Third plot capacity 80
for (i in seq(1, iter)) {
  temp <- data.frame(Geração = c(seq(1, i), seq(1, i)), Variavel = c(rep("médio",i), 
                                                                     rep("melhor", i)), Fitness = c(-GAmodel$mean[1:i], -GAmodel$best[1:i]))
  
  p4 <- ggplot(temp, aes(x = Geração, y = Fitness, group = Variavel, 
                         colour = Variavel)) + geom_line() + scale_x_continuous(limits = c(0, iter)) + 
    scale_y_continuous(limits = c(0, 210)) + geom_hline(y = max(temp$Fitness),lty = 2) + 
    annotate("text", x = 1, y = max(temp$Fitness) + 2, hjust = 0, size = 3, color = "black", 
             label = paste("Melhor solução:", max(temp$Fitness))) + scale_colour_brewer(palette = "Set1") + 
    labs(title = "Média das execuções com capacidade = 80%")
  print(p4)
}

multiplot(p1, p2, p3, p4, cols=2)


