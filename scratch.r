2+22+3
3+3

#make some data
height = rnorm(100, m=5, s=1)
weight = height*34+rnorm(100, 0,5)
df = data.frame(height, weight)
df$names = NA
df[which.max(df$height), "names"] = "tallest"
df[which.min(df$height), "names"] = "shortest"
plot(df[,1:2])
text(df[,1:2], df$names)

#center and scale data
df$height = df$height-mean(df$height)
df$weight = df$weight-mean(df$weight)
df$height=df$height/sd(df$height)
df$weight=df$weight/sd(df$weight)
plot(df[,1:2]) 
text(df[,1:2], df$names)



#compute principle components
pca = prcomp(df[,1:2], scale=F)
rot = pca$rotation

#which direction maximized variance?
lines(rbind(c(0,0),rot[,1]), col="red", lwd=3)

#plot samples along the first principal component
plot(pca$x[,1], rep(0, nrow(df)))
text(pca$x[,1], rep(0, nrow(df)), df$names)


pca2 = prcomp(mtcars)
plot(pca2$x[,1:2])
text(pca2$x[,1:2], rownames(mtcars))

library(ggfortify)
pca2$rotation
autoplot(pca2, colour="disp", label=T)
autoplot(pca2, colour="hp", label=T)
autoplot(pca2, label=T, loadings=T, loadings.label=T)
