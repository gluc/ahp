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

## Model Analysis

Once you have specified your model, you can click on `Analysis` to calculate and review the analysis of your model. Chose any of the different calculation methods. Also, you can drill down into each decision maker, if aplicable. 

