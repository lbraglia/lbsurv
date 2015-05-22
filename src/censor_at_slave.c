#include <R.h>
#include <Rinternals.h>


SEXP censor_at_slave(SEXP time, SEXP status, SEXP censoring_time)
{
    int n = length(time);	/* vectors length */
    int i;			/* a counter */
    /* results list: elements and container list) */
    SEXP rval_time = PROTECT(allocVector(REALSXP, n));
    SEXP rval_status = PROTECT(allocVector(INTSXP, n));
    SEXP rval = PROTECT(allocVector(VECSXP, 2));
    
    for (i = 0; i < n; i++){
	if (ISNA(REAL(time)[i]) || INTEGER(status)[i] == NA_INTEGER) {
	    REAL(rval_time)[i] = NA_REAL;
	    INTEGER(rval_status)[i] = NA_INTEGER;
	} else if(REAL(time)[i] <= REAL(censoring_time)[i]){
	    REAL(rval_time)[i] = REAL(time)[i];
	    INTEGER(rval_status)[i] = INTEGER(status)[i];
	} else {
	    REAL(rval_time)[i] = REAL(censoring_time)[i];
	    INTEGER(rval_status)[i] = 0;
	}
    }

    SET_VECTOR_ELT(rval, 0, rval_time);
    SET_VECTOR_ELT(rval, 1, rval_status);
	
    UNPROTECT(3);
    return rval;
}
