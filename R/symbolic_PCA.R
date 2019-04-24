sym_interval_pca <- function(x = NULL, method = c("classic", "tops", "centers",
                                         "principal.curves", "optimized.distance",
                                         "optimized.variance")) {
  method <- match.arg(method)
  switch(method,
         classic={
           return(sym_interval_pca_classic(x))
         },
         centers={
           return(sym_interval_pca_centers(x))
         },
         {
           stop("error")
         }
  )
}


sym_interval_pca_classic <- function(x, ...) {
  .names <- x$concept
  x <- dplyr::select(x, -concept)
  centers <- as.data.frame(map_df(x,~(Re(vec_data(.)) + Im(vec_data(.)))/2))
  rownames(centers) <- .names
  FactoMineR::PCA(X = centers, scale.unit = T, ncp = ncol(x), graph = F)
}

sym_interval_pca_centers <- function(x, ...){
  var_names <- colnames(x)
  ind_names <- x$concept
  x <- dplyr::select(x, -concept)

  centers <- sapply(x, function(x) (Re(vec_data(x)) + Im(vec_data(x)))/2)
  .sd <- apply(centers, 2, stats::sd)*sqrt((nrow(centers)-1)/nrow(centers))
  .mean <- colMeans(centers)
  centers.stan <- t((t(centers)-.mean)/.sd)
  min.stan <- t((t(sapply(x, function(x) sapply(x, min)))-.mean)/.sd)
  max.stan <- t((t(sapply(x, function(x) sapply(x, max)))-.mean)/.sd)

  R <- t(centers.stan) %*% centers.stan
  svd <- eigen(R)

  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      smin <- 0
      smax <- 0
      for (k in seq_len(ncol(x))) {
        if (svd$vectors[k, j] < 0) {
          smin <- smin + max.stan[i, k] * svd$vectors[k,
                                                      j]
        }
        else {
          smin <- smin + min.stan[i, k] * svd$vectors[k,
                                                      j]
        }
        if (svd$vectors[k, j] < 0) {
          smax <- smax + min.stan[i, k] * svd$vectors[k,
                                                      j]
        }
        else {
          smax <- smax + max.stan[i, k] * svd$vectors[k,
                                                      j]
        }
      }
      x[i,j] <- sym_interval(c(smin,smax))
    }
  }

  svdV <- matrix(0, nrow(x), ncol(x))
  for (i in seq_len(nrow(x))) {
    for (j in seq_len(ncol(x))) {
      ss <- 0
      for (k in seq_len(ncol(x))) {
        ss <- ss + centers.stan[i, k] * svd$vectors[k,
                                                    j]
      }
      svdV[i, j] <- (1/sqrt(svd$values[j])) * ss
    }
  }
  IPrinCorre <- matrix(0, ncol(x), 2 * ncol(x))
  for (i in seq_len(ncol(x))) {
    pcol <- 1
    for (j in seq_len(ncol(x))) {
      smin <- 0
      smax <- 0
      for (k in seq_len(nrow(x))) {
        if (svdV[k, j] < 0) {
          smin <- smin + (1/sqrt(nrow(x))) * max.stan[k,
                                                      i] * svdV[k, j]
        }
        else {
          smin <- smin + (1/sqrt(nrow(x))) * min.stan[k,
                                                      i] * svdV[k, j]
        }
        if (svdV[k, j] < 0) {
          smax <- smax + (1/sqrt(nrow(x))) * min.stan[k,
                                                      i] * svdV[k, j]
        }
        else {
          smax <- smax + (1/sqrt(nrow(x))) * max.stan[k,
                                                      i] * svdV[k, j]
        }
      }
      IPrinCorre[i, pcol] <- smin
      IPrinCorre[i, pcol + 1] <- smax
      pcol <- pcol + 2
    }
  }
  colnames(IPrinCorre)  <- paste0("V", seq_len(ncol(IPrinCorre)))
  IPrinCorre <- tibble::as_tibble(IPrinCorre)
  IPrinCorre <- tibble::add_column(.data = IPrinCorre, variables = .names,.before = 0)
  colnames(x) <- paste0("C", seq_len(ncol(x)))
  x <- tibble::add_column(x, individuals = ind_names,.before = 0)

  list(Sym.Components = x, Sym.Prin.Correlations = IPrinCorre)
}

