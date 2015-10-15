#define VAR_BLOCKSIZE 10
#define MAX_CLAUSELENGTH 100

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
   SCIP_PROBDATA*  probdata,           /**< problem data */
   MR_FloatList    objectives,         /**< objectives values for new variables */
   MR_StringList   varnames,           /**< names for new variables */
   SCIP_Bool       initial             /**< whether new variables should be 'initial' */
   );

EXTERN
SCIP_RETCODE makeclause(
   SCIP* scip,                /**< SCIP pointer */
   SCIP_PROBDATA*  probdata,  /**< problem data */
   MR_IntList neglits,        /**< indices for negative literals */
   MR_IntList poslits,        /**< indices for positive literals */
   int* nvars,                /**< pointer to number of literals in the clause */
   SCIP_VAR** clausevars,     /**< temporary storage for SCIP variables in clause */
   int* once_only             /**< if clause is of length < 3 and a lit is marked as occuring only in 
                                   this clause then its index in clausevars, else -1 
                                   if two such lits the index of the first is given */
   );


