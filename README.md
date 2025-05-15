# Left-handed-analysis

This analysis using R language is based on the Young People Survey collected by Statistics students at FSEV UK in 2013 on Kaggle (Sabo, 2015).

You can find the exact dataset and description of each item in this link: https://www.kaggle.com/datasets/miroslavsabo/young-people-survey?select=columns.csv 
(Sabo, 2015))

To run the analysis, download the dataset from kaggle and set the directory using the setwd() syntax here:
```{r}
library(tidyverse)
setwd("/Users/riveryu/Documents/Cornell/STSCI6020/final exam/young people dataset") #set your directory of the downloaded data csv here
all_data <- read.csv("responses.csv")
# names(all_data)
```

Roughly 10% of the human population is left-handed (Sha et al., 2021).
When you type "left-handed people are" in Google search bar, the first few search recommendations include "left-handed people are intelligent", "left-handed people are lucky" and "left-handed people are more artistic". It seems like left-handedness is related to a smart and creative impression. However, as the saying goes: "there's a fine line between genius and madness." Creative genius might suffer from some mental health concerns. In a word, can left-handedness be predicted using psychological traits and creative interest?

Based on this, this study generates the following research questions using:
1. Can creativity-related interests and psychological traits predict whether a young person (aged 18-25) is left-handed or right-handed?
2. Which specific factors are the strongest predictors of handedness in young adults?

By clarifying these relationships in this dataset, it can help understand left-handedness and possibly demystify it. It also has important implications to identify potential psychological and health-related needs. It can also fulfill our curiosity appetite!


Reference:
Sabo, M. (2015). Young People Survey [Data set]. Kaggle. https://www.kaggle.com/datasets/miroslavsabo/young-people-survey
Sha, Z., Pepe, A., Schijven, D., Carrión-Castillo, A., Roe, J.M., Westerhausen, R., Joliot, M., Fisher, S.E., Crivello, F., & Francks, C. (2021). Handedness and its genetic influences are associated with structural asymmetries of the cerebral cortex in 31,864 individuals. Proceedings of the National Academy of Sciences of the United States of America, 118.
