---
title: "Help"
author: "Christoph Glur"
date: "3.1.2016"
output: html_document
---

This app is a simple demonstration of the capabilities of the R ahp package. 

## AHP R Package

To install the package, from an R command-line type:

```
install.packages("ahp")
```

To get the latest from github, do

```
devtools::install_github("gluc/ahp", build_vignettes = TRUE)
```

Please star the package on [github](http://github.com/gluc/ahp/) if you like it.

## Running the demo App

From the package, you can run the demo app by typing

```
library(ahp)
RunGUI()
```

## Model Specification

In the navigation bar on top, go to `Model`, and specify your model using the ahp file format. You can load a model from file, save a model, or select from any of the predefined example models included in the package.

Go to `More > AHP File Format` to learn more about the ahp file format.

## Model Visualization

To visualize the hierarchy of your AHP model, click on `Visualize`. Note that the nodes have tool tips!
The output is the same as when you call `Visualize(ahpTree)` from R, albeit in R you have full flexibility on the styling options.

## Analyze

Once you have specified your model, you can click on `Analyze` to calculate and review the analysis of your model. Chose any of the different calculation methods. Also, you can drill down into each decision maker, if aplicable. Finally, you can filter the content of the table by setting filters.

In R, you can do the same thing using the function `AnalyzeTable(ahpTree)`.

