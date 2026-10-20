cluster_winners <- function(res, w, h = NULL, k = NULL, plot = TRUE) {
  
  # Your weights as a named vector (first column, tickers as row names)
  wv <- setNames(as.numeric(w[[1]]), rownames(w))
  wv <- wv[!is.na(wv) & wv > 0]
  
  # Cluster membership
  groups <- if (!is.null(h)) cutree(res$hc, h = h)
  else if (!is.null(k)) cutree(res$hc, k = k)
  else res$groups
  if (is.null(groups)) stop("Give h or k, or run stock_cluster() with k/h first.")
  
  out <- do.call(rbind, lapply(sort(unique(groups)), function(g) {
    members <- names(groups)[groups == g]
    held    <- members[members %in% names(wv)]
    
    if (length(held) == 0)
      return(data.frame(cluster = g, n_members = length(members), n_held = 0,
                        top_weight = NA, n_winners = 0, winners = "(none held)",
                        other_held = "", stringsAsFactors = FALSE))
    
    top <- held[abs(wv[held] - max(wv[held])) < 1e-9]       # every stock tied at the max
    oth <- setdiff(held, top)
    oth <- oth[order(-wv[oth])]
    
    data.frame(cluster    = g,
               n_members  = length(members),
               n_held     = length(held),
               top_weight = max(wv[held]),
               n_winners  = length(top),
               winners    = paste(top, collapse = " "),
               other_held = paste(sprintf("%s(%g)", oth, wv[oth]), collapse = " "),
               stringsAsFactors = FALSE)
  }))
  
  # Held stocks that are not in the tree (dropped by filters / no price data)
  not_in_tree <- setdiff(names(wv), names(groups))
  
  # Dendrogram: winners in bold red, other held stocks black, rest grey
  if (plot) {
    win <- unlist(strsplit(out$winners[out$n_winners > 0], " "))
    dend <- dendrapply(as.dendrogram(res$hc), function(n) {
      if (is.leaf(n)) {
        lab <- attr(n, "label")
        attr(n, "nodePar") <- list(pch = NA, lab.cex = 0.7,
                                   lab.col  = if (lab %in% win) "red3" else if (lab %in% names(wv)) "black" else "grey70",
                                   lab.font = if (lab %in% win) 2 else 1)
      }
      n
    })
    op <- par(mar = c(3, 1, 3, 6)); on.exit(par(op), add = TRUE)
    plot(dend, horiz = TRUE, main = "Highest-weight stock(s) per cluster (red)")
    if (!is.null(h)) abline(v = h, lty = 2, col = "grey40")
  }
  
  print(out, row.names = FALSE, right = FALSE)
  if (length(not_in_tree))
    cat("\nHeld but not in the tree (no cluster):",
        paste(not_in_tree, collapse = " "), "\n")
  
  invisible(list(winners = out, not_in_tree = not_in_tree))
}
res <- stock_cluster(prices, date_col = "Date", last_n = 156, h = 0.5,
                     min_obs = 0.5, max_zero = 0.5, plot = FALSE)
