library(tidyverse)
ggplot(data=stopdata) +
  geom_bar(mapping=aes(x=stop_district,fill=race_ethnicity))
