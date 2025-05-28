/*  File src/rle_utils.c in package rle, currently hosted at https://github.com/statnet/rle .
 *
 *  This software is distributed under the GNU General Public License Version 3 or later.
 *  A copy of this license may be found at https://www.gnu.org/licenses/gpl-3.0.en.html .
 *
 *  Copyright 2017-2020 Pavel N. Krivitsky and others (see inst/COPYRIGHT).
 */
#include<R.h>
#include<Rmath.h>
#include<Rinternals.h>

SEXP sync_RLEs(SEXP lens1, SEXP lens2){
  const char *names[] = {"lengths", "val1i", "val2i", "nruns", ""};
  SEXP out = PROTECT(mkNamed(VECSXP, names));
  SEXP lengths = PROTECT(allocVector(INTSXP, length(lens1)+length(lens2)));
  SEXP v1i = PROTECT(allocVector(INTSXP, length(lens1)+length(lens2)));
  SEXP v2i = PROTECT(allocVector(INTSXP, length(lens1)+length(lens2)));
  SEXP nruns = PROTECT(allocVector(INTSXP, 1));

  // Leftover lengths of the current run in each vector:
  unsigned int left1 = 0, left2 = 0;
  // Positions in the RLE length vectors:
  unsigned int opos = 0, pos1 = 0, pos2 = 0;

  /*
    Iteratate through the length vectors. Note that we increment the
    position as soon as we read from a vector, so it's already in "R"
    indexing (from 1).
*/
  while(pos1 < length(lens1)){
    left1 += INTEGER(lens1)[pos1];
    pos1++;

    while(left1 > left2){
      /*
	Now, we know that left1 > left2.

	So, we flush from both rle1 and rle2 by the left2's amount,
	and add on to left2 until left2 is the one "sticking out".
      */

      if(left2 != 0){
	INTEGER(lengths)[opos] = left2;
	INTEGER(v1i)[opos] = pos1;
	INTEGER(v2i)[opos] = pos2;
	left1 -= left2;
	left2 = 0; // I.e., subtract left2 from left2.
	opos++;
      }

      if(pos2 >= length(lens2)) break;
      left2 += INTEGER(lens2)[pos2];
      pos2++;
    }

    /*
       Now, left1 <= left2. (If lens2 has run out, then this should
       still hold, since the uncompressed lengths of the input vectors
       must be equal.)

       So, we flush from both by the left1's amount.
    */

    if(left1 != 0){
      INTEGER(lengths)[opos] = left1;
      INTEGER(v1i)[opos] = pos1;
      INTEGER(v2i)[opos] = pos2;
      left2 -= left1;
      left1 = 0; // I.e., subtract left1 from left1.
      opos++;
    }
  }

  *INTEGER(nruns) = opos;

  SET_VECTOR_ELT(out, 0, lengths);
  SET_VECTOR_ELT(out, 1, v1i);
  SET_VECTOR_ELT(out, 2, v2i);
  SET_VECTOR_ELT(out, 3, nruns);

  UNPROTECT(5);
  return(out);
}


SEXP compress_RLE(SEXP lR, SEXP vR, SEXP remapR){
  Rboolean remap = asLogical(remapR);
  unsigned int *l = (unsigned int *) INTEGER(lR), *v = (unsigned int *) INTEGER(vR);

  const char *names[] = {"lengths", "vali", "nruns", ""};
  SEXP outR = PROTECT(mkNamed(VECSXP, names));
  SEXP lengthsR = PROTECT(allocVector(INTSXP, length(lR)));
  SEXP voR = PROTECT(allocVector(TYPEOF(vR), length(lR)));
  SEXP nrunsR = PROTECT(allocVector(INTSXP, 1));

  unsigned int *lengths = (unsigned int *) INTEGER(lengthsR), *vo = (unsigned int *) INTEGER(voR), *nruns = (unsigned int *) INTEGER(nrunsR);

  lengths[0] = l[0];
  vo[0] = remap? 1 : v[0];
  unsigned int o = 0;
  for(unsigned int i = 1; i < length(lR); i++){
    unsigned int nextl = l[i];
    if(lengths[o] > INT_MAX-nextl || // If cumulative run is too long or
       (remap? v[vo[o]-1] : vo[o]) != v[i]){ // the value is not the same as the current one...
      // advance the output vector;
      o++;
      lengths[o] = nextl;
      vo[o] = remap ? i+1 : v[i];
    }else{ // otherwise...
      // stay put and add to the run length.
      lengths[o] += nextl;
    }
  }

  *nruns = o+1;

  SET_VECTOR_ELT(outR, 0, lengthsR);
  SET_VECTOR_ELT(outR, 1, voR);
  SET_VECTOR_ELT(outR, 2, nrunsR);

  UNPROTECT(4);
  return(outR);
}
