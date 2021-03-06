#######################################################################
# seriation - Infrastructure for seriation
# Copyrigth (C) 2011 Michael Hahsler, Christian Buchta and Kurt Hornik
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 2 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License along
# with this program; if not, write to the Free Software Foundation, Inc.,
# 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.

## Criterion for the quality of a permutation of a dissimilarity
## matrix

criterion.dist <- function(x, order = NULL, method = NULL, ...) {
    
    ## check order and x
    if(!is.null(order)) {
        if(!inherits(order, "ser_permutation"))
            order <- ser_permutation(order)
        .check_dist_perm(x, order)
    }
    
    ## check dist (most C code only works with lower-triangle version) 
    if(attr(x, "Diag") || attr(x, "Upper"))
        x <- as.dist(x, diag = FALSE, upper = FALSE)
    if(!is.double(x)) mode(x) <- "double"

    ## get methods
    if(is.null(method)) method <- list_criterion_methods("dist") 
    method <- lapply(method, function(m) get_criterion_method("dist", m))
    
    sapply(method, 
        function(m) structure(m$fun(x, order, ...), names=m$name))
}

criterion.default <- criterion.dist

## Wrapper to computing the length of the order under a distance matrix,
## e.g. a tour where the leg between the first and last city is omitted.
## that this is a (Hamilton) path.
##
## Note that this corresponds to the sum of distances along the first
## off diagonal of the ordered distance matrix.

criterion_path_length <- function(x, order = NULL, ...) {
    if (is.null(order)) order <- 1:attr(x, "Size")
    else order <- get_order(order)
    .Call("order_length", x, order, PACKAGE = "seriation")
}


## Least squares criterion. measures the difference between the 
## dissimilarities between two elements and the rank distance
## (PermutMatrix).

criterion_least_squares <- function(x, order = NULL, ...) {
    if(is.null(order)) order <- 1:attr(x, "Size") 
    else order <- get_order(order)
    .Call("least_squares_criterion", x, order, PACKAGE = "seriation")
}

## inertia around the diagonal (see PermutMatrix)
criterion_inertia <- function(x, order = NULL, ...) {
    if(is.null(order)) order <- 1:attr(x, "Size") 
    else order <- get_order(order)
    .Call("inertia_criterion", x, order, PACKAGE = "seriation")
}

## anti-Robinson loss functions (Streng and Schoenfelder 1978, Chen
## 2002)
## method: 1...i, 2...s, 3...w
.ar <- function(x, order = NULL, method = 1L) {
    if(is.null(order)) order <- 1:attr(x, "Size") 
    else order <- get_order(order)
    .Call("ar", x, order, as.integer(method), PACKAGE = "seriation")
}

criterion_ar_events <- function(x, order, ...) .ar(x, order, 1L)
criterion_ar_deviations <- function(x, order, ...) .ar(x, order, 2L)
#criterion_ar_weighted <- function(x, order, ...) .ar(x, order, 3L)

criterion_rgar <- function(x, order, w=NULL, ...) {
  if(is.null(order)) order <- 1:attr(x, "Size") 
  else order <- get_order(order)
  if(is.null(w)) w <- length(order)-1L
  if(w<1 || w>=length(order)) stop("Window w needs to be 1<=w<length(order)!")
  .Call("rgar", x, order, as.integer(w)+1L, PACKAGE = "seriation")
} 

criterion_gradient_raw <- function(x, order, ...) {
    if(is.null(order)) order <- 1:attr(x, "Size")
    else order <- get_order(order)
    .Call("gradient", x, order, 1L, PACKAGE = "seriation")
}

criterion_gradient_weighted <- function(x, order, ...) {
    if(is.null(order)) order <- 1:attr(x, "Size")
    else order <- get_order(order)
    .Call("gradient", x, order, 2L, PACKAGE = "seriation")
}

.A_2SUM <- function(n)
  outer(1:n,1:n, FUN = function(i,j) (i-j)^2)

criterion_2SUM <- function(x, order, ...) {
    if(is.null(order)) order <- 1:attr(x, "Size")
    else order <- get_order(order)
    
    qap::qap.obj(.A_2SUM(attr(x, "Size")), 1/(1+as.matrix(x)), order)
}

.A_LS <- function(n)
  outer(1:n,1:n, FUN = function(i,j) n-abs(i-j))

criterion_LS <- function(x, order, ...) {
    if(is.null(order)) order <- 1:attr(x, "Size")
    else order <- get_order(order)
    
    qap::qap.obj(.A_LS(attr(x, "Size")), as.matrix(x), order)
}


criterion_ME_dist <- function(x, order, ...)
    criterion(as.matrix(x), c(order, order), "ME")

criterion_Moore_stress_dist  <- function(x, order, ...)
    criterion(as.matrix(x), c(order, order), "Moore_stress")

criterion_Neumann_stress_dist  <- function(x, order, ...)
    criterion(as.matrix(x), c(order, order), "Neumann_stress")


### register methods
set_criterion_method("dist", "AR_events" , criterion_ar_events, 
    "Anti-Robinson events", FALSE)
set_criterion_method("dist", "AR_deviations", criterion_ar_deviations,
    "Anti-Robinson deviations", FALSE)
## set_criterion_method("dist", "AR_weighted", criterion_ar_weighted)
set_criterion_method("dist", "RGAR", criterion_rgar,
  "Relative generalized anti-Robinson events", FALSE)
set_criterion_method("dist", "Gradient_raw" , criterion_gradient_raw,
    "Gradient measure", TRUE)
set_criterion_method("dist", "Gradient_weighted", criterion_gradient_weighted,
    "Gradient measure (weighted)", TRUE)
set_criterion_method("dist", "Path_length", criterion_path_length,
    "Hamiltonian path length", FALSE)
set_criterion_method("dist", "Inertia", criterion_inertia,
    "Inertia criterion", TRUE)
set_criterion_method("dist", "Least_squares", criterion_least_squares,
    "Least squares criterion", FALSE)
set_criterion_method("dist", "ME", criterion_ME_dist,
    "Measure of effectiveness", TRUE)
set_criterion_method("dist", "Moore_stress", criterion_Moore_stress_dist,
    "Stress (Moore neighborhood)", FALSE)
set_criterion_method("dist", "Neumann_stress", criterion_Neumann_stress_dist,
    "Stress (Neumann neighborhood)", FALSE)
set_criterion_method("dist", "2SUM", criterion_2SUM,
    "2-SUM objective value (QAP)", FALSE)
set_criterion_method("dist", "LS", criterion_LS,
    "Linear seriation objective value (QAP)", FALSE)
