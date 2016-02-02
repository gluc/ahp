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
#' @import yaml
#' @export
Visualize <- function(ahpTree,
                      criteriaNodesStyle = list(style = "filled,rounded", shape = "box", color = "honeydew4", fillcolor = "honeydew", penwidth = 4, fontname="helvetica"),
                      alternativeNodesStyle = list(style = "filled,rounded", shape = "box", color = "thistle4", fillcolor = "thistle", penwidth = 4, fontname="helvetica"),
                      criteriaEdgesStyle = list(arrowhead = "vee", color = "grey35", penwidth = 2),
                      alternativeEdgesStyle = list(dir = "none", color = "grey35", penwidth = 2)
                 ) {
  graph <- GetGraph(ahpTree, criteriaNodesStyle, alternativeNodesStyle, criteriaEdgesStyle, alternativeEdgesStyle)
  render_graph(graph) 
}
  

#' @rdname Visualize
#' @import DiagrammeR
#' @export
GetGraph <- function(ahpTree,
                     criteriaNodesStyle = list(style = "filled,rounded", shape = "box", color = "honeydew4", fillcolor = "honeydew", penwidth = 4, fontname="helvetica"),
                     alternativeNodesStyle = list(style = "filled,rounded", shape = "box", color = "thistle4", fillcolor = "thistle", penwidth = 4, fontname="helvetica"),
                     criteriaEdgesStyle = list(arrowhead = "vee", color = "grey35", penwidth = 2),
                     alternativeEdgesStyle = list(dir = "none", color = "grey35", penwidth = 2)
                    ) {

  # criteria  
  tr <- Traverse(ahpTree, filterFun = isNotLeaf)
  critNodes <- create_nodes(nodes = Get(tr, "name"), 
                            tooltip = Get(tr, GetTooltip)
                            #tooltip = Get(tr, "name") 
                            )
  for (style in names(criteriaNodesStyle)) critNodes[ , style] <- criteriaNodesStyle[style]
  
  alt <- create_nodes(nodes = "Alternatives")
  for (style in names(alternativeNodesStyle)) alt[ , style] <- alternativeNodesStyle[style]
  
  altNodes <- as.data.frame(ahpTree$leaves[[1]]$parent, 
                            nodes = "name", 
                            tooltip = GetTooltip,
                            filterFun = isLeaf)[,-1]
  
  alternatives <- unique(Get(ahpTree$leaves, "name"))
  for (style in names(alternativeNodesStyle)) altNodes[ , style] <- alternativeNodesStyle[style]
  
  nodes <- combine_nodes(critNodes, alt, altNodes)
  
  #nodes <- nodes[!names(nodes)=="tooltip"]
  
  edges <- ToDataFrameNetwork( Clone(ahpTree, pruneFun = isNotLeaf)) 
  for (style in names(criteriaEdgesStyle)) edges[ , style] <- criteriaEdgesStyle[style]
  
  edgesAlt <- as.data.frame(ahpTree, 
                               from = function(x) x$name, 
                               to = function(x) "Alternatives", 
                               filterFun = function(x) x$height == 2)[ , -1]
  for (style in names(criteriaEdgesStyle)) edgesAlt[ , style] <- criteriaEdgesStyle[style]
  
  edgesAlts <- create_edges(from = rep("Alternatives", length(alternatives)), to = alternatives)
  for (style in names(alternativeEdgesStyle)) edgesAlts[ , style] <- alternativeEdgesStyle[style]
  
  edges <- combine_edges(edges, edgesAlt, edgesAlts)
  
  graph <- create_graph(nodes, edges)
  
  return (graph)
  
}


GetTooltip <- function(x) {
  
  tt <- ""
  if (x$isRoot) {
    dm <- GetDecisionMakers(x)
    if (length(dm) > 1) {
      tt <- paste(dm, collapse = ", ")
      tt <- paste0("Decision Makers: ", tt, "\n")
    }
  } 
  if (!is.null(x$parent$priority)) {
    prio <- x$parent$priority["Total", x$name]
    tt <- paste0("priority: ", percent1(prio), "\n")
  }
  #browser()
  myfields <- x$fields[!x$fields %in% c("preferences", 
                                        "weightContribution", 
                                        "name", 
                                        "consistency", 
                                        "priority", 
                                        "preferenceFunction",
                                        "decision-makers",
                                        "score")]
  if (length(myfields) > 0) {
    mylist <- as.list.environment(x)[myfields] 
    props <- as.yaml(mylist)
    tt <- paste0(tt, props)
  }
  return (tt)
}
