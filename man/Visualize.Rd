% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.R
\name{Visualize}
\alias{Visualize}
\alias{GetGraph}
\title{Draw a diagram of the AHP tree.}
\usage{
Visualize(ahpTree, criteriaNodesStyle = list(style = "filled,rounded", shape =
  "box", color = "honeydew4", fillcolor = "honeydew", penwidth = 4, fontname =
  "helvetica", fontcolor = "black"), alternativeNodesStyle = list(style =
  "filled,rounded", shape = "box", color = "thistle4", fillcolor = "thistle",
  penwidth = 4, fontname = "helvetica"), criteriaEdgesStyle = list(arrowhead =
  "vee", color = "grey35", penwidth = 2), alternativeEdgesStyle = list(dir =
  "none", color = "grey35", penwidth = 2))

GetGraph(ahpTree, criteriaNodesStyle = list(style = "filled,rounded", shape =
  "box", color = "honeydew4", fillcolor = "honeydew", penwidth = 4, fontname =
  "helvetica"), alternativeNodesStyle = list(style = "filled,rounded", shape =
  "box", color = "thistle4", fillcolor = "thistle", penwidth = 4, fontname =
  "helvetica"), criteriaEdgesStyle = list(arrowhead = "vee", color = "grey35",
  penwidth = 2), alternativeEdgesStyle = list(dir = "none", color = "grey35",
  penwidth = 2))
}
\arguments{
\item{ahpTree}{The tree to be drawn}

\item{criteriaNodesStyle}{a list of graphviz / dot language styling elements}

\item{alternativeNodesStyle}{a list of graphviz / dot language styling elements}

\item{criteriaEdgesStyle}{a list of graphviz / dot language styling elements}

\item{alternativeEdgesStyle}{a list of graphviz / dot language styling elements}
}
\description{
The function uses graphviz via DiagrammeR. For details on styling, refer
to these.
}
