library(factoextra)
library(viridis) 

data<-read.csv("clinical_data_40samplesBR_17feb23.txt", sep = "\t", header = T)
dim(data)
head(data)
mm<- data.matrix(data[,c(2,4,15)])
mm

res.pca <- prcomp(log(mm), scale = TRUE)

p3= fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 95))+
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text=element_text(size=18),
        plot.title = element_blank())
p3
p2=fviz_pca_var(res.pca, col.var="contrib",select.var = list(contrib = 5),repel = TRUE,arrowsize = 1)+
  scale_colour_gradient2(low="#00AFBB", mid="#E7B800", high="#FC4E07", midpoint = 16.1) +
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=18),
        legend.title = element_text(size = 18),
        legend.text=element_text(size=18),
        plot.title = element_blank())
p2

p1=fviz_pca_biplot(res.pca, 
                   # Individuals
                   geom.ind = "point",
                   fill.ind = as.factor(data$CM),
                   col.ind = as.factor(data$CM),
                   pointshape = 21, pointsize = 4, arrowsize = 1.4,
                   palette = "jco", addEllipses = TRUE, repel = TRUE,
                   legend.title = "Type")+
    theme(axis.text=element_text(size=23), 
        axis.title=element_text(size=23),
        legend.title = element_text(size = 23), 
        legend.text=element_text(size=23),
        plot.title = element_blank())

p1

ggarrange(p1, p2, p3, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)
