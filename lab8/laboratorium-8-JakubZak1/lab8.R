library(tidyverse)
patients <- read_tsv("D:/JZ/patient-data-cleaned.txt")
patients

p <- ggplot(patients, aes(Weight, BMI)) + 
  geom_smooth(method = lm, formula = y ~ splines::bs(x, 3), se = FALSE) + 
  geom_point(aes(color = factor(Height))) # zrobić w quarto bo teraz mi sie nie chce
  
p # nie wiem o co chodzi w estetykach globalych i lokalnych
