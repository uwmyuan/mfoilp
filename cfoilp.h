#define VAR_BLOCKSIZE 10;

struct SCIP_ProbData
{
   SCIP_VAR**   vars;          /**< variables in the problem */
   int          nvars;         /**< number of variables in the problem */
   int          vars_len;      /**< length of vars array */
   MR_AtomStore atom_store;    /**< Mercury bimap (+ next index) between atoms and their indices */
};

EXTERN
SCIP_RETCODE addNewVars(
   SCIP*           scip,               /**< SCIP pointer */
   MR_FloatList    objectives,
   MR_StringList   varnames,
   SCIP_Bool       initial
   );



