#library(ggcorrplot)

numericVars <- 
  select_if(st_drop_geometry(Model_clean), is.numeric)%>% na.omit()

ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c(low = "steelblue", mid = "white", high = "darkred"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation Matrix of variables") 
