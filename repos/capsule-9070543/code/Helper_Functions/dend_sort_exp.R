dend_sort_exp = function (d, exp) 
{
  #require(dendextend)
  if (!inherits(d, "dendrogram") && !inherits(d, "hclust")) {
    stop("d variable must be a dendrogram or hclust object")
  }
  dend = d
  if (inherits(d, "hclust")) {
    dend = as.dendrogram(d)
  }
  #set exp attribute
  dend = set_node_exp(dend, exp)
  n = sort_exp(dend)
  if (inherits(d, "hclust")) {
    n = as.hclust(n)
  }
  return(n)
}

set_node_exp = function (d, exp_list)
{
  if (class(d) != "dendrogram") {
    stop("d variable must be a dendrogram")
  }
  left <- d[[1]]
  right <- d[[2]]
  if (is.leaf(left) && is.leaf(right)) {
    left.name = attributes(left)$label
    right.name = attributes(right)$label
    attr(left, "exp") = exp_list[[left.name]]
    attr(right, "exp") = exp_list[[right.name]]
    n = merge(left, right, height = attributes(d)$height)
    attr(n, "exp") = mean(c(attributes(left)$exp, attributes(right)$exp))
  }
  else if (!is.leaf(left) && is.leaf(right)) { #left tree, right leaf
    right.name = attributes(right)$label
    attr(right, "exp") = exp_list[[right.name]]
    left2 = set_node_exp(left, exp_list)
    n = merge(left2, right, height = attributes(d)$height)
    attr(n, "exp") = mean(c(attributes(left2)$exp, attributes(right)$exp))
  }
  else if (is.leaf(left) && !is.leaf(right)) {#left leaf, right tree
    left.name = attributes(left)$label
    attr(left, "exp") = exp_list[[left.name]]
    right2 = set_node_exp(right, exp_list)
    n = merge(left, right2, height = attributes(d)$height)
    attr(n, "exp") = mean(c(attributes(left)$exp, attributes(right2)$exp))
  }
  else {
    lft = set_node_exp(left, exp_list)
    rght = set_node_exp(right, exp_list)
    n = merge(lft, rght, height = attributes(d)$height)
    attr(n, "exp") <- mean(c(attributes(lft)$exp, attributes(rght)$exp))
  }
  return(n)
}
