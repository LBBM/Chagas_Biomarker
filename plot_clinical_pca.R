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


######################## SCORE

data_arvc<-read.csv("data_qPCR_ARVC_validation_clinical_all_PCA_score.txt", sep = "\t", header = T)
dim(data_arvc)

mm<- data.matrix(data_arvc[,c(-1,-6,-7,-8)])
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
                   fill.ind = as.factor(data_arvc$type),
                   pointshape = 21, pointsize = 3,
                   arrowsize = 1,
                   palette = "jco",
                   addEllipses = TRUE,
                   # Variables
                   alpha.var ="contrib", col.var = "contrib",
                   gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
                   
)+
  labs(fill = "Cohort", color = "Contrib", alpha = "Contrib")+
  theme(axis.text=element_text(size=18), 
        axis.title=element_text(size=18),
        legend.title = element_text(size = 18), 
        legend.text=element_text(size=18),
        plot.title = element_blank())
p1

ggarrange(p1, p2, p3, 
          labels = c("A", "B", "C"),
          ncol = 3, nrow = 1)



