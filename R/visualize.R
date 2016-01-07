#' Draw a diagram of the AHP tree.
#' 
#' The function uses graphviz via DiagrammeR. For details on styling, refer
#' to these.
#' 
#' @param ahpTree The tree to be drawn
#' @param criteriaNodesStyle a list of graphviz / dot language styling elements
#' @param alternativeNodesStyle a list of graphviz / dot language styling elements
#' @param criteriaEdgesStyle a list of graphviz / dot language styling elements
#' @param alternativeEdgesStyle a list of graphviz / dot language styling elements
#' 
#' 
#' @export
Visualize <- function(ahpTree,
                 criteriaNodesStyle = list(style = "filled,rounded", shape = "box", color = "palevioletred"),
                 alternativeNodesStyle = list(style = "rounded", shape = "box"),
                 criteriaEdgesStyle = list(arrowhead = "vee"),
                 alternativeEdgesStyle = list(dir = "none", color = "grey85", penwidth = 0.5)
                 ) {
  graph <- GetGraph(ahpTree, criteriaNodesStyle, alternativeNodesStyle, criteriaEdgesStyle, alternativeEdgesStyle)
  render_graph(graph) 
}
  

#' @rdname Visualize
#' @import DiagrammeR
#' @export
GetGraph <- function(ahpTree,
                     criteriaNodesStyle = list(style = "filled,rounded", shape = "box", color = "palevioletred"),
                     alternativeNodesStyle = list(style = "rounded", shape = "box"),
                     criteriaEdgesStyle = list(arrowhead = "vee"),
                     alternativeEdgesStyle = list(dir = "none", color = "grey85", penwidth = 0.5)
                    ) {

    
  critNodes <- create_nodes(nodes = ahpTree$Get("name", filterFun = isNotLeaf))
  for (style in names(criteriaNodesStyle)) critNodes[ , style] <- criteriaNodesStyle[style]
  
  altNodes <- create_nodes(nodes = unique(Get(ahpTree$leaves, "name")))
  for (style in names(alternativeNodesStyle)) altNodes[ , style] <- alternativeNodesStyle[style]

  nodes <- merge(critNodes, altNodes, all = TRUE)
  
  nodes <- replace(nodes, is.na(nodes), "")
  
  graph <- create_graph(nodes)
  
  edges <- ToDataFrameNetwork( Clone(ahpTree, pruneFun = isNotLeaf)) 
  for (style in names(criteriaEdgesStyle)) edges[ , style] <- criteriaEdgesStyle[style]
  
  graph <- add_edges(graph, edges)
  
  edges <- ToDataFrameTable(ahpTree,
                            from = function(x) x$parent$name, 
                            to = function(x) x$name)
  for (style in names(alternativeEdgesStyle)) edges[ , style] <- alternativeEdgesStyle[style]
  graph <- add_edges(graph, edges)
  
  return (graph)
  
}

