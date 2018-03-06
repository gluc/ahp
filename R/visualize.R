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
#' @importFrom yaml as.yaml
#' @export
Visualize <- function(ahpTree,
                      criteriaNodesStyle = list(style = "filled,rounded", shape = "box", color = "honeydew4", fillcolor = "honeydew", penwidth = 4, fontname="helvetica", fontcolor = "black"),
                      alternativeNodesStyle = list(style = "filled,rounded", shape = "box", color = "thistle4", fillcolor = "thistle", penwidth = 4, fontname="helvetica"),
                      criteriaEdgesStyle = list(arrowhead = "vee", color = "grey35", penwidth = 2),
                      alternativeEdgesStyle = list(dir = "none", color = "grey35", penwidth = 2)
                 ) {
  graph <- GetGraph(ahpTree, criteriaNodesStyle, alternativeNodesStyle, criteriaEdgesStyle, alternativeEdgesStyle)
  DiagrammeR::render_graph(graph) 
}
  

#' @rdname Visualize
#' @export
GetGraph <- function(ahpTree,
                     criteriaNodesStyle = list(style = "filled,rounded", shape = "box", color = "honeydew4", fillcolor = "honeydew", penwidth = 4, fontname="helvetica"),
                     alternativeNodesStyle = list(style = "filled,rounded", shape = "box", color = "thistle4", fillcolor = "thistle", penwidth = 4, fontname="helvetica"),
                     criteriaEdgesStyle = list(arrowhead = "vee", color = "grey35", penwidth = 2),
                     alternativeEdgesStyle = list(dir = "none", color = "grey35", penwidth = 2)
                    ) {


  
  # nodes
  # =====
  
  # criteria  
  trCriteria <- Traverse(ahpTree, filterFun = isNotLeaf)
  Set(trCriteria, `.id` = 1:length(trCriteria))
  critNodes <- DiagrammeR::create_node_df(n = length(trCriteria), label = Get(trCriteria, "name"), 
                            tooltip = Get(trCriteria, GetTooltip)
                            )
  for (style in names(criteriaNodesStyle)) critNodes[ , style] <- criteriaNodesStyle[style]
  
  # alternative hard-coded box
  alt <- DiagrammeR::create_node_df(n = 1, label = "Alternatives")
  for (style in names(alternativeNodesStyle)) alt[ , style] <- alternativeNodesStyle[style]
  
  # alternatives
  trAlternatives <- ahpTree$leaves[[1]]$parent$leaves
  altNodes <- DiagrammeR::create_node_df(n = length(trAlternatives), 
                                         label = Get(trAlternatives, "name"),
                                         tooltip = Get(trAlternatives, GetTooltip))

  for (style in names(alternativeNodesStyle)) altNodes[ , style] <- alternativeNodesStyle[style]
  
  nodes <- DiagrammeR::combine_ndfs(critNodes, alt, altNodes)
  
  # edges
  # =====
  
  #tree
  edges <- ToDataFrameNetwork( Clone(ahpTree, pruneFun = isNotLeaf), fromId = function(node) node$parent$`.id`, toId = ".id") 
  edges <- DiagrammeR::create_edge_df(edges$fromId, edges$toId)
  
  for (style in names(criteriaEdgesStyle)) edges[ , style] <- criteriaEdgesStyle[style]
  
  # to hard-coded alternatives
  edgesAlt <- as.data.frame(ahpTree, 
                               from = function(x) x$`.id`, 
                               to = function(x) length(trCriteria) + 1, 
                               filterFun = function(x) x$height == 2)[ , -1]
  edgesAlt <- DiagrammeR::create_edge_df(edgesAlt$from, edgesAlt$to)
  for (style in names(criteriaEdgesStyle)) edgesAlt[ , style] <- criteriaEdgesStyle[style]
  
  # from hc alternatives to alternatives
  
  edgesAlts <- DiagrammeR::create_edge_df(from = rep(length(trCriteria) + 1, length(trAlternatives)), to = 1:length(trAlternatives) + length(trCriteria) + 1)
  for (style in names(alternativeEdgesStyle)) edgesAlts[ , style] <- alternativeEdgesStyle[style]
  
  edges <- DiagrammeR::combine_edfs(edges, edgesAlt, edgesAlts)
  
  graph <- DiagrammeR::create_graph(nodes, edges, attr_theme = NULL)
  graph <- DiagrammeR::add_global_graph_attrs(graph, "rankdir", "TB", "graph")
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
