library(tidyverse)

clasif_train <- read.csv("clasif_train_alumnos.csv", row.names = 1)


# Comparar LDA y QDA con wilcoxon

wilcox.response <- wilcox.test(clasif_train$out_train_lda, clasif_train$out_train_qda, paired = T)

# Como el p-value es 0.1769 y es mayor que 0.05, se acepta la hipótesis nula y se puede afirmar que no se aprecian diferencias significativas entre los dos algoritmos
wilcox.response

# Comparar todos los algoritmos con Friedman

# Como p-value > 0.05, se acepta la hipótesis nula y se puede afirmar que no hay diferencias significativas entre los algoritmos
test_friedman <- friedman.test(as.matrix(clasif_train))
test_friedman


# post hoc holm
# Aunque el test de friedman ha dado como resultado que no hay diferencias significativas, el resultado del test post-hoc de holm afirma
# que hay cierta diferencia entre el algoritmo 3 (QDA) y los algoritmos 1 y 2 (KNN y LDA) 

tam <- dim(clasif_train)
groups <- rep(1:tam[2], each=tam[1])
groups
pairwise.wilcox.test(as.matrix(clasif_train), groups, p.adjust = "holm", paired = TRUE)
