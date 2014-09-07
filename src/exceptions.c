/*
 * Copyright (C) 1999-2003 The Omega Project for Statistical Computing.
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 */

#include "rsqlite.h"

void setException(SQLiteConnection *con, int err_no, 
                            const char *err_msg) {

  RS_SQLite_exception* ex = con->exception;
  if (!ex) {
    // Create new exception object
    ex = malloc(sizeof(RS_SQLite_exception));
    if (!ex) {
      error("could not allocate SQLite exception object");
    }
  } else {
    // Reuse existing
    free(ex->errorMsg);
  }

  ex->errorNum = err_no;
  if (err_msg) {
    ex->errorMsg = RS_DBI_copyString(err_msg);
  } else { 
    ex->errorMsg = NULL;
  }
  
  con->exception = ex;
  return;
}

void freeException(SQLiteConnection *con) {
  RS_SQLite_exception *ex = con->exception;

  if (!ex) 
    return;
  if (ex->errorMsg) 
    free(ex->errorMsg);
  free(ex);
  
  con->exception = NULL;
  return;
}

SEXP RS_SQLite_getException(SEXP conHandle) {
  SQLiteConnection* con = get_connection(conHandle);
  if (!con->drvConnection)
    error("internal error: corrupt connection handle");

  RS_SQLite_exception* err = (RS_SQLite_exception *) con->exception;
  
  SEXP output = PROTECT(allocVector(VECSXP, 2));
  SEXP output_nms = PROTECT(allocVector(STRSXP, 2));
  SET_NAMES(output, output_nms);
  UNPROTECT(1);
  
  SET_STRING_ELT(output_nms, 0, mkChar("errorNum"));
  SET_VECTOR_ELT(output, 0, ScalarInteger(err->errorNum));
  
  SET_STRING_ELT(output_nms, 1, mkChar("errorMsg"));
  SET_VECTOR_ELT(output, 1, mkString(err->errorMsg));

  UNPROTECT(1);
  return output;
}