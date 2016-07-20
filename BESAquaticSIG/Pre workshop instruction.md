## BES Aquatic SIG ECR workshop
### General and generalised linear models

#### Please download the following before the workshop.

* R (the latest version)
* R packages glm
* Data files from: https://github.com/nhcooper123/TeachingMaterials/tree/master/BESAquaticSIG/Data OR https://drive.google.com/folderview?id=0B6WKgRBJw5LOQXJ4WXBPSks2V3M&usp=sharing

#### Before the workshop please ensure you can do the following. 
*If you can't don't panic, but please alert me at the start of the workshop so we can help.*

1. Open R on your computer.
2. Open a script in the R Editor OR script editor of your choice.
  * File > New Document
3. Get help for a particular function.
  * ?nameoffunction
  * Google!
4. Install and load packages in R.
  * install.packages("nameofpackage")
  * library(nameofpackage)
5. Load data into R using read.csv
  * mydata <- read.csv("PATH/dataset.csv")
6. Look at data you have loaded into R.
  * str(mydata)
7. Make simple boxplots and scatter plots
  * boxplot(Y ~ X, data = mydata)
  * plot(Y ~ X, data = mydata)