library(plotly)
p <- plot_ly(midwest, x = ~percollege, color = ~state, type = "box")
p


Isomap <- read.csv("Isomap.csv", header = FALSE)
Isomap <- read.csv("TSNE.csv", header = FALSE)
#Isomap <- read.csv("MDS.csv", header = FALSE)
ComplSet <- read.csv("TimAllData.csv", header = TRUE)
ComplSet <- ComplSet[,2:33]

Isomap <- Isomap %>%
  mutate(Cluster = ComplSet$Cluster, 
         Player = ComplSet$Player,
         Year = ComplSet$Year)

colors <- c('#FF3030', '#FFD700', 
            '#00CD66', '#00BFFF', 
            '#EE82EE')
c1 <- Isomap %>%
  filter(Cluster == 1)

cluster1 = list(
  type = 'sphere',
  xref ='x', yref='y',
  x0=min(c1$V1), y0=min(c1$V2), z0=min(c1$V3),
  x1=max(c1$V1), y1=max(c1$V1), z1=max(c1$V3),
  opacity=0.25,
  line = list(color="#835AF1"),
  fillcolor="#835AF1")

updatemenus <- list(
  list(
    active = -1,
    type = 'buttons',
    buttons = list(
      
      list(
        label = "None",
        method = "relayout",
        args = list(list(shapes = c()))),
      
      list(
        label = "Cluster 1",
        method = "relayout",
        args = list(list(shapes = list(cluster1, c(), c()))))
      )
    )
)

Isomap %>%
  plot_ly(x = ~V1, y = ~V2, z = ~V3, 
          color = ~Cluster, colors = colors,
          marker = list(opacity = 1,
                        size = 2.5), 
          mode = "markers",
          hoverinfo='text',
          text = ~paste('Player:', Player, 
                        '<br>  Year:', Year,
                        '<br> Group:', Cluster)) %>%
  add_markers() %>%
  layout(title = "Highlight Clusters", showlegend = FALSE,
         updatemenus = updatemenus)


