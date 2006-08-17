#include <R.h>
#include <Rinternals.h>
#include <math.h>

#include "lt.h"


/*
 * path length can be found in optimal.c
 */


/* 
 * least-square criterion
 */

SEXP least_square_criterion(SEXP R_dist, SEXP R_order) {

    double sum = 0.0;
    int p = INTEGER(getAttrib(R_dist, install("Size")))[0];
    int *o = INTEGER(R_order);
    double x = 0; 
    SEXP R_out;

    /* since d ist symmetric we only need to sum up half the matrix */
    for (int i = 1; i <= p; i++) {
        for (int j = 1; j < i; j++) {
            x = (REAL(R_dist)[LT_POS(p, o[i-1], o[j-1])] - abs(i-j));
            sum += x*x;
        }
    }
    sum *= 2;

    PROTECT(R_out = allocVector(REALSXP, 1));
    REAL(R_out)[0] = sum; 
    UNPROTECT(1);

    return(R_out);
}

/* 
 * inertia criterion
 */


SEXP inertia_criterion(SEXP R_dist, SEXP R_order) {

    double sum = 0.0;
    int p = INTEGER(getAttrib(R_dist, install("Size")))[0];
    int *o = INTEGER(R_order);
    double x = 0; 
    SEXP R_out;

    /* since d ist symmetric we only need to sum up half the matrix */
    for (int i = 1; i <= p; i++) {
        for (int j = 1; j < i; j++) {
            x = abs(i-j);
            sum += REAL(R_dist)[LT_POS(p, o[i-1], o[j-1])] * x*x;
        }
    }
    sum *= 2;

    PROTECT(R_out = allocVector(REALSXP, 1));
    REAL(R_out)[0] = sum; 
    UNPROTECT(1);

    return(R_out);
}


SEXP ar(SEXP R_dist, SEXP R_order, SEXP R_which) {

    /* 
     * which indicates the weighing scheme
     * 1 ... no weighting (i)
     * 2 ... abs. deviations (s)
     * 3 ... weighted abs. deviations (w)
     */
    
    int p = INTEGER(getAttrib(R_dist, install("Size")))[0];
    int *o = INTEGER(R_order);
    int which = INTEGER(R_which)[0];
    
    double sum = 0.0;
    double d_ij = 0.0;
    double d_ik = 0.0;
    
    SEXP R_out;

    
    
    /* sum_i=1^p sum_j<k<i I(d_ij < d_ik) * weight */
    for (int i = 3; i <= p; i++) {
        for(int k = 2; k < i; k++) {
            d_ik = REAL(R_dist)[LT_POS(p, o[i-1], o[k-1])];
            
            for(int j = 1; j < k; j++) {
                d_ij = REAL(R_dist)[LT_POS(p, o[i-1], o[j-1])];

                if(d_ij < d_ik) {
                    if(which == 1) { 
                        sum++;
                    }else if(which == 2) {
                        sum += fabs(d_ij - d_ik);
                    }else if(which == 3) 
                        sum += fabs(o[j-1]-o[k-1]) * fabs(d_ij - d_ik);
                }    
            }
        }
    }
                    
    /* sum_i=1^p sum_i<j<k I(d_ij > d_ik) * weight */
    for (int i = 1; i < (p-1); i++) {
        for(int j = i+1; j < p; j++) {
            d_ij = REAL(R_dist)[LT_POS(p, o[i-1], o[j-1])];
            for(int k = j+1; k <= p; k++) {
                d_ik = REAL(R_dist)[LT_POS(p, o[i-1], o[k-1])];

                if(d_ij > d_ik) {
                    if(which == 1) {
                        sum++;
                    }else if(which == 2) {
                        sum += fabs(d_ij - d_ik);
                    }else if(which == 3) 
                        sum += fabs(o[j-1]-o[k-1]) * fabs(d_ij - d_ik);
                }    
            }
        }
    }

    PROTECT(R_out = allocVector(REALSXP, 1));
    REAL(R_out)[0] = sum; 
    UNPROTECT(1);

    return(R_out);
}


