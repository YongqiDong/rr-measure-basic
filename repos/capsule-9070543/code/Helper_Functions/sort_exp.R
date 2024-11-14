sort_exp = function (d) 
{
  if (class(d) != "dendrogram") {
    stop("d variable must be a dendrogram")
  }
  a <- attributes(d)
  left <- d[[1]]
  right <- d[[2]]
  left.exp = attributes(left)$exp
  right.exp = attributes(right)$exp
  if (is.leaf(left) && is.leaf(right)) {
    if(left.exp >= right.exp){
      n = merge(left, right, height = a$height)
    } else{
      n = merge(right, left, height = a$height)
    }
    attr(n, "exp") <- mean(left.exp, right.exp)
  }
  else if (!is.leaf(left) && is.leaf(right)) { #left tree, right leaf
    #TODO: define exp for this node
    if(left.exp >= right.exp){
      n = merge(sort_exp(left), right, height = a$height)
    } else{
      n = merge(right, sort_exp(left), height = a$height)
    }
    attr(n, "exp") <- mean(left.exp, right.exp)
  }
  else if (is.leaf(left) && !is.leaf(right)) {#left leaf, right tree
    #TODO: define exp for this node. 
    if(left.exp >= right.exp){
      n = merge(left, sort_exp(right), height = a$height)
    } else{
      n = merge(sort_exp(right), left, height = a$height)
    }
    attr(n, "exp") <- mean(left.exp, right.exp)
  }
  else {
    #TODO: define exp for this node
    lft <- sort_exp(left)
    rght <- sort_exp(right)
    if (left.exp >= right.exp) {
      n <- merge(lft, rght, height = a$height)
    }
    else {
      n <- merge(rght, lft, height = a$height)
    }
    attr(n, "exp") <- min(left.exp, right.exp)
  }
  return(n)
}