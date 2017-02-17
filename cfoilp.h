struct SCIP_ProbData
{
   SCIP_VAR**   vars;          /**< variables in the problem */
   int          nvars;         /**< number of variables in the problem */
   int          vars_len;      /**< length of vars array */
};


#define VAR_BLOCKSIZE 10;
