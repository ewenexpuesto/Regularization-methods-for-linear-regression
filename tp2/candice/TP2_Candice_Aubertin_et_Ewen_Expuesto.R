library(glmnet);
tab <- read.csv(file = "~/MRR21/TP2/Mexico_data.csv", header = TRUE, sep = ",")
Y <- as.numeric(tab$Total)
X<-as.matrix(tab[,-1])
Xs<-scale(X)
tabr<- data.frame(Y, Xs)



Y_matrix <- matrix(Y, length(Y), 1)
m_glm<-cv.glmnet(Xs, Y_matrix, alpha = 0)
print(m_glm$lambda.min)

m_adjusted<- glmnet(Xs, Y_matrix, alpha = 0, lambda = m_glm$lambda.min)

mg<- cv.glmnet(Xs, Y, alpha = 0)
plot(mg)
abline(v = log(mg$lambda.min), col = "blue")
rm<- glmnet(Xs, Y, alpha = 0)
plot(rm, xvar = "lambda", label = TRUE)

rm <- glmnet(Xs, Y, alpha = 0)
cr <- predict(rm, type = "coefficients", s = mg$lambda.min)
vr <- order(abs(cr), decreasing = TRUE)
head(vr)