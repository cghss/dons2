
library(tidyverse)
library(segmented)

f <- read_csv(file.choose())
f %>% ggplot(aes(x = Year, y = Count)) + geom_point() + theme_bw() + geom_smooth(method = 'lm')

out.lm<-lm(Count~Year,data = f)
segmented(out.lm)
