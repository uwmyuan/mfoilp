struct SCIP_ProbData
{
   SCIP_VAR**   vars;          /**< variables in the problem */
   int          nvars;         /**< number of variables in the problem */
   int          vars_len;      /**< length of vars array */
   SCIP_CONS*   atomcount_cons;
};


#define VAR_BLOCKSIZE 10;
