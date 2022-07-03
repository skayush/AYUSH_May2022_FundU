library(plotly)
## Treemaps

labels = c("A1", "A2", "A3", "A4", "A5", "B1", "B2")
parents = c("", "A1", "A2", "A3", "A4", "", "B1")
values = c("11", "12", "13", "14", "15", "20", "30")


fig1 <- plot_ly(
  type="treemap",
  labels=labels,
  parents=parents,
  values=values,
  textfont = list(family = "Arial"),
  marker=list(colors=c("#168aad", "#34a0a4", "#52b69a", "#76c893", "#99d98c","#168aad","#34a0a4"))) %>%
  layout(title = "Treemap", plot_bgcolor="Yellow") 

fig1


fig2 <- plot_ly(
  type="treemap",
  labels=labels,
  parents=parents,
  values=values,
  textfont = list(family = "Arial"),
  opacity = 0.9,
  marker = list(colors = c("#008000","#38b000", "#70e000", "#9ef01a", "ccff33","#008000", "#38b000"))) %>%
  layout(title = "Treemap", font = list(family = "Times New Roman")) 

fig2



fig3 <- plot_ly(
  type="treemap",
  labels=labels,
  parents=parents,
  values=values,
  textfont = list(family = "Arial"),
  marker=list(colors=c("#5390d9", "#48bfe3", "#64dfdf", "#72efdd", "#80ffdb","#5390d9","#48bfe3"))) %>%
  layout(title = "Treemap", font = "Arial") 

fig3



fig4 <- plot_ly(
  type="treemap",
  labels=labels,
  parents=parents,
  values=values,
  textfont = list(family = "Arial"),
  marker=list(colors=c("#c884a6", "#e7bbe3", "#ccd5ff", "#7cc6fe", "#23c9ff","#c884a6","#e7bbe3"))) %>%
  layout(title = "Treemap", font = list(family = "Times New Roman") )

fig4

fig5 <- plot_ly(
  type="treemap",
  labels=labels,
  parents=parents,
  values=values,
  textfont = list(family = "Arial"),
  marker=list(colorscale = "Reds")) %>%
  layout(title = "Treemap") 

fig5










