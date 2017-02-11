/**@file   cons_folinear.c
 * @brief  constraint handler for folinear constraints
 * @author James Cussens
 */

/*---+----1----+----2----+----3----+----4----+----5----+----6----+----7----+----8----+----9----+----0----+----1----+----2*/
/*#define SCIP_DEBUG*/
#include <assert.h>
#include <string.h>

#include <cons_folinear.h>
#include <cfoilp.h>


/* fundamental constraint handler properties */
#define CONSHDLR_NAME          "folinear"
#define CONSHDLR_DESC          "first order linear constraint handler"
#define CONSHDLR_ENFOPRIORITY         0 /**< priority of the constraint handler for constraint enforcing */
#define CONSHDLR_CHECKPRIORITY        0 /**< priority of the constraint handler for checking feasibility */
#define CONSHDLR_EAGERFREQ          100 /**< frequency for using all instead of only the useful constraints in separation,
                                              *   propagation and enforcement, -1 for no eager evaluations, 0 for first only */
#define CONSHDLR_NEEDSCONS         TRUE /**< should the constraint handler be skipped, if no constraints are available? */

/* optional constraint handler properties */
/* TODO: remove properties which are never used because the corresponding routines are not supported */
#define CONSHDLR_SEPAPRIORITY         100 /**< priority of the constraint handler for separation */
#define CONSHDLR_SEPAFREQ             1 /**< frequency for separating cuts; zero means to separate only in the root node */
#define CONSHDLR_DELAYSEPA        FALSE /**< should separation method be delayed, if other separators found cuts? */

#define CONSHDLR_PROPFREQ            -1 /**< frequency for propagating domains; zero means only preprocessing propagation */
#define CONSHDLR_DELAYPROP        FALSE /**< should propagation method be delayed, if other propagators found reductions? */
#define CONSHDLR_PROP_TIMING       SCIP_PROPTIMING_BEFORELP/**< propagation timing mask of the constraint handler*/

#define CONSHDLR_MAXPREROUNDS        -1 /**< maximal number of presolving rounds the constraint handler participates in (-1: no limit) */
#define CONSHDLR_DELAYPRESOL      FALSE /**< should presolving method be delayed, if other presolvers found reductions? */

/*
 * Data structures
 */

/** constraint data for folinear constraints */
/* struct SCIP_ConsData */
/* { */
/* }; */

/** constraint handler data */
/* struct SCIP_ConshdlrData */
/* {  */
/* };  */


/*
 * Local methods
 */

/* put your local methods here, and declare them static */

/* complete this later */

static
SCIP_RETCODE sol2mercury(
   SCIP* scip,
   SCIP_SOL* sol,
   MR_IntList* indices_ptr,
   MR_FloatList* values_ptr
   )
{
   SCIP_PROBDATA* probdata;

   int i;
   SCIP_VAR* var;
   SCIP_Real val;

   probdata = SCIPgetProbData(scip);
   assert( probdata != NULL );

   SCIPdebugMessage("Translating solution for Mercury\n");

   MR_sol2mercury_init(indices_ptr,values_ptr);
   
   /* *indices_ptr = MR_list_empty(); */
   /* *values_ptr = MR_list_empty(); */

   for( i = 0; i < probdata->nvars; ++i )
   {
      var = probdata->vars[i];
      val = SCIPgetSolVal(scip, sol, var);
#ifdef SCIP_DEBUG
      SCIPdebug( SCIPprintVar(scip, var, NULL) );
      SCIPdebugMessage("has index %d and solution value <%g>\n", i, val);
#endif
      if( !SCIPisZero(scip, val))
      {
         MR_sol2mercury_cons(i,val,*indices_ptr,*values_ptr,indices_ptr,values_ptr);

         /* *indices_ptr = MR_list_cons( i, *indices_ptr); */
         /* *values_ptr = MR_list_cons( MR_float_to_word(val), *values_ptr); */
      }
   }
   return SCIP_OKAY;
}


/** separate first order clauses */
static
SCIP_RETCODE FOLinearSeparate(
   SCIP*                 scip,               /**< SCIP pointer */
   SCIP_CONSHDLR*        conshdlr,           /**< constraint handler */
   const char*           consname,           /**< name of first-order clause */
   SCIP_CONS*            cons,               /**< constraint being separated */
   MR_IntList indices,                       /**< indices of variables with non-zero value in solution */
   MR_FloatList values,                      /**< values of variables with non-zero value in solution */
   int*                  nGen,               /**< output: pointer to store number of added rows */
   SCIP_Bool*            cutoff              /**< output: pointer to store whether we detected a cutoff */
   )
{

   SCIP_PROBDATA* probdata;

   MR_FloatList objectives;
   MR_StringList varnames;
   MR_IntListList neglitss;
   MR_IntListList poslitss;
   MR_IntList neglits;
   MR_IntList poslits;

   MR_Integer equality;
      
   SCIP_VAR* var;

   MR_Integer mr_down;
   MR_Integer mr_up;

   SCIP_VAR* clausevars[100];
   int nvars;
   SCIP_VAR* negvar;
   SCIP_ROW* row;

   assert( scip != NULL );
   assert( nGen != NULL );
   assert( cutoff != NULL );

   probdata = SCIPgetProbData(scip);
   assert( probdata != NULL );

   *cutoff = FALSE;

   /* get cuts, if any, and any new variables */

   MR_findcuts((MR_String) consname, &equality, indices, values, &neglitss, &poslitss, &objectives, &varnames); 
   
   /* create any new binary variables in constraints using "objectives" list */
   /* this same code occurs in cfoilp.c ! */

   while ( !MR_list_is_empty(objectives) ) 
   {
      SCIP_CALL( 
         SCIPcreateVar(scip, &var, 
            (char *) MR_list_head(varnames), 
            0.0, 1.0, 
            (SCIP_Real) MR_word_to_float(MR_list_head(objectives)), 
            SCIP_VARTYPE_BINARY, FALSE, FALSE, NULL, NULL, NULL, NULL, NULL) );
      SCIP_CALL( SCIPaddVar(scip, var) );
      
      /* lock the variable */

      MR_locks( (MR_String) consname, probdata->nvars, &mr_down, &mr_up);
      SCIP_CALL( SCIPlockVarCons(scip, var, cons, (SCIP_Bool) mr_down, (SCIP_Bool) mr_up) ); 

#ifdef SCIP_DEBUG
      SCIPdebugMessage("New variable:\n");
      SCIPdebug( SCIPprintVar(scip, var, NULL) );
      SCIPdebugMessage("Down lock = <%d>, Up lock = <%d>\n", (SCIP_Bool) mr_down, (SCIP_Bool) mr_up);
#endif
      
      /* increase size of probdata->vars if necessary */
      if( !(probdata->nvars < probdata->vars_len) )
      {
         probdata->vars_len += VAR_BLOCKSIZE;
         SCIP_CALL( SCIPreallocMemoryArray(scip, &(probdata->vars), probdata->vars_len) );
      }
      
      /* record variable in array */
      /* value of probdata->nvars will correspond with that of Mercury's atomstore */
      probdata->vars[probdata->nvars++] = var;
      
      objectives = MR_list_tail(objectives);
      varnames = MR_list_tail(varnames);
      
   }
   
   /* now add the cuts */
   
   while ( !MR_list_is_empty(neglitss)  )
   {
      neglits =  MR_list_head(neglitss);
      poslits =  MR_list_head(poslitss);
      
      nvars = 0;
      
      while ( !MR_list_is_empty(neglits) )
      {
         var = probdata->vars[(int) MR_list_head(neglits)];
         SCIP_CALL( SCIPgetNegatedVar(scip,var,&negvar) );
         clausevars[nvars++] = negvar;
#ifdef SCIP_DEBUG
         SCIPdebugMessage("Variable in cut:\n");
         SCIPdebug( SCIPprintVar(scip, negvar, NULL) );
#endif
         neglits =  MR_list_tail(neglits);
      }

      while ( !MR_list_is_empty(poslits) )
      {
         var = probdata->vars[(int) MR_list_head(poslits)];
         clausevars[nvars++] = var;
#ifdef SCIP_DEBUG
         SCIPdebugMessage("Variable in cut:\n");
         SCIPdebug( SCIPprintVar(scip, var, NULL) );
#endif
         
         poslits =  MR_list_tail(poslits);
      }

      SCIP_CALL( SCIPcreateEmptyRowCons(scip, &row, conshdlr, "cut", 1.0, (SCIP_Bool) equality ? 1.0 : SCIPinfinity(scip), FALSE, FALSE, TRUE) );
      SCIP_CALL( SCIPaddVarsToRowSameCoef(scip, row, nvars, clausevars, 1.0) );
#ifdef SCIP_DEBUG
      SCIPdebug( SCIPprintRow(scip, row, NULL) );
#endif
      SCIP_CALL( SCIPaddCut(scip, NULL, row, FALSE, cutoff) );
      SCIP_CALL( SCIPreleaseRow(scip, &row));
      (*nGen)++;

      if ( *cutoff )
      {
         SCIPdebugMessage("Cut generated a cutoff.\n");
         break;
      }
      
      neglitss = MR_list_tail(neglitss);
      poslitss = MR_list_tail(poslitss);
   }
   return SCIP_OKAY;
}

/*
 * Callback methods of constraint handler
 */

/* TODO: Implement all necessary constraint handler methods. The methods with #if 0 ... #else #define ... are optional */

/** copy method for constraint handler plugins (called when SCIP copies plugins) */
#if 0
static
SCIP_DECL_CONSHDLRCOPY(conshdlrCopyFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define conshdlrCopyFolinear NULL
#endif

/** destructor of constraint handler to free constraint handler data (called when SCIP is exiting) */
#if 0
static
SCIP_DECL_CONSFREE(consFreeFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consFreeFolinear NULL
#endif


/** initialization method of constraint handler (called after problem was transformed) */
#if 0
static
SCIP_DECL_CONSINIT(consInitFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consInitFolinear NULL
#endif


/** deinitialization method of constraint handler (called before transformed problem is freed) */
#if 0
static
SCIP_DECL_CONSEXIT(consExitFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consExitFolinear NULL
#endif


/** presolving initialization method of constraint handler (called when presolving is about to begin) */
#if 0
static
SCIP_DECL_CONSINITPRE(consInitpreFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consInitpreFolinear NULL
#endif


/** presolving deinitialization method of constraint handler (called after presolving has been finished) */
#if 0
static
SCIP_DECL_CONSEXITPRE(consExitpreFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consExitpreFolinear NULL
#endif


/** solving process initialization method of constraint handler (called when branch and bound process is about to begin) */
#if 0
static
SCIP_DECL_CONSINITSOL(consInitsolFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consInitsolFolinear NULL
#endif


/** solving process deinitialization method of constraint handler (called before branch and bound process data is freed) */
#if 0
static
SCIP_DECL_CONSEXITSOL(consExitsolFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consExitsolFolinear NULL
#endif


/** frees specific constraint data */
static
SCIP_DECL_CONSDELETE(consDeleteFolinear)
{  /*lint --e{715}*/

   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( cons != NULL );

   SCIPdebugMessage("deleting first order linear constraint <%s>.\n", SCIPconsGetName(cons));

   return SCIP_OKAY;
}



/** transforms constraint data into data belonging to the transformed problem */
#if 0
static
SCIP_DECL_CONSTRANS(consTransFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consTransFolinear NULL
#endif


/** LP initialization method of constraint handler (called before the initial LP relaxation at a node is solved) */
#if 0
static
SCIP_DECL_CONSINITLP(consInitlpFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consInitlpFolinear NULL
#endif


/** separation method of constraint handler for LP solutions */
static
SCIP_DECL_CONSSEPALP(consSepalpFolinear)
{  /*lint --e{715}*/

   int c;
   int nGen = 0;

   MR_IntList indices;         /**< indices of variables with non-zero value in solution */
   MR_FloatList values;        /**< values of variables with non-zero value in solution  */

   SCIP_CONS* cons;
   SCIP_Bool cutoff;

   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( conss != NULL );
   assert( result != NULL );

   /* get solution values for all problem variables */

   sol2mercury(scip, NULL, &indices, &values);

   *result = SCIP_DIDNOTRUN;

   /* loop through all constraints */
   for (c = 0; c < nconss; ++c)
   {
      cons = conss[c];
      assert( cons != NULL );
      SCIPdebugMessage("separating LP solution for first order linear constraint <%s>.\n", SCIPconsGetName(cons));

      *result = SCIP_DIDNOTFIND;
      SCIP_CALL( FOLinearSeparate(scip, conshdlr, SCIPconsGetName(cons), cons, indices, values, &nGen, &cutoff) );

      if ( cutoff )
      {
         SCIPdebugMessage("cutoff while separating LP solution for first order linear constraint <%s>.\n", SCIPconsGetName(cons));
         *result = SCIP_CUTOFF;
         return SCIP_OKAY;
      }
   }
   
   if( nGen > 0 )
      *result = SCIP_SEPARATED;
   SCIPdebugMessage("separated %d cuts.\n", nGen);

   return SCIP_OKAY;

}



/** separation method of constraint handler for arbitrary primal solutions */
static
SCIP_DECL_CONSSEPASOL(consSepasolFolinear)
{ 

   int c;
   int nGen = 0;

   MR_IntList indices;         /**< indices of variables with non-zero value in solution */
   MR_FloatList values;        /**< values of variables with non-zero value in solution  */

   SCIP_CONS* cons;
   SCIP_Bool cutoff;

   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( conss != NULL );
   assert( result != NULL );

   /* get solution values for all problem variables */

   sol2mercury(scip, sol, &indices, &values);

   *result = SCIP_DIDNOTRUN;

   /* loop through all constraints */
   for (c = 0; c < nconss; ++c)
   {
      cons = conss[c];
      assert( cons != NULL );
      SCIPdebugMessage("separating solution for first order linear constraint <%s>.\n", SCIPconsGetName(cons));

      *result = SCIP_DIDNOTFIND;
      SCIP_CALL( FOLinearSeparate(scip, conshdlr, SCIPconsGetName(cons), cons, indices, values, &nGen, &cutoff) );

      if ( cutoff )
      {
         SCIPdebugMessage("cutoff while separating solution for first order linear constraint <%s>.\n", SCIPconsGetName(cons));
         *result = SCIP_CUTOFF;
         return SCIP_OKAY;
      }
   }
   
   if( nGen > 0 )
      *result = SCIP_SEPARATED;
   SCIPdebugMessage("separated %d cuts.\n", nGen);

   return SCIP_OKAY;


}



/** constraint enforcing method of constraint handler for LP solutions */
static
SCIP_DECL_CONSENFOLP(consEnfolpFolinear)
{  /*lint --e{715}*/


   int c;
   int nGen = 0;

   MR_IntList indices;         /**< indices of variables with non-zero value in solution */
   MR_FloatList values;        /**< values of variables with non-zero value in solution  */

   SCIP_CONS* cons;
   SCIP_Bool cutoff;

   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( conss != NULL );
   assert( result != NULL );

   /* get solution values for all problem variables */

   sol2mercury(scip, NULL, &indices, &values);

   *result = SCIP_DIDNOTRUN;

   /* loop through all constraints */
   for (c = 0; c < nconss; ++c)
   {
      cons = conss[c];
      assert( cons != NULL );
      SCIPdebugMessage("enforcing LP solution for first order linear constraint <%s>.\n", SCIPconsGetName(cons));

      *result = SCIP_DIDNOTFIND;
      SCIP_CALL( FOLinearSeparate(scip, conshdlr, SCIPconsGetName(cons), cons, indices, values, &nGen, &cutoff) );

      if ( cutoff )
      {
         SCIPdebugMessage("cutoff while enforcing LP solution for first order linear constraint <%s>.\n", SCIPconsGetName(cons));
         *result = SCIP_CUTOFF;
         return SCIP_OKAY;
      }
   }
   
   if( nGen > 0 )
   {
      *result = SCIP_SEPARATED;
      SCIPdebugMessage("separated %d cuts.\n", nGen);
      return SCIP_OKAY;
   }
   
   SCIPdebugMessage("all first-order linear constraints are feasible.\n");
   *result = SCIP_FEASIBLE;
   return SCIP_OKAY;
}

/** constraint enforcing method of constraint handler for pseudo solutions */
static
SCIP_DECL_CONSENFOPS(consEnfopsFolinear)
{  /*lint --e{715}*/

   int c;

   SCIP_CONS* cons;

   MR_IntList indices;
   MR_FloatList values;

   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( conss != NULL );
   assert( result != NULL );

   /* get solution values for all problem variables */
      
   sol2mercury(scip, NULL, &indices, &values);

   /* loop through all constraints */
   for (c = 0; c < nconss; ++c)
   {
      cons = conss[c];
      assert( cons != NULL );
      SCIPdebugMessage("enforcing pseudo solution for first order linear constraint <%s>.\n", SCIPconsGetName(cons));
    

      /* ask Mercury whether there exists a ground instance of this constraint
         which does not satisfy the solution 
      */

      if( MR_existscut((MR_String) SCIPconsGetName(cons),indices,values) )
      {
         SCIPdebugMessage("constraint <%s> infeasible.\n", SCIPconsGetName(cons));
         *result = SCIP_INFEASIBLE;
         return SCIP_OKAY;
      }

   }
   SCIPdebugMessage("all first-order linear constraints are feasible.\n");
   *result = SCIP_FEASIBLE;
   return SCIP_OKAY;

}


/** feasibility check method of constraint handler for integral solutions */
static
SCIP_DECL_CONSCHECK(consCheckFolinear)
{  /*lint --e{715}*/

   int c;

   SCIP_CONS* cons;

   MR_IntList indices;
   MR_FloatList values;

   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( conss != NULL );
   assert( result != NULL );

   /* get solution values for all problem variables */
      
   sol2mercury(scip, sol, &indices, &values);

   /* loop through all constraints */
   for (c = 0; c < nconss; ++c)
   {
      cons = conss[c];
      assert( cons != NULL );
      SCIPdebugMessage("checking first order linear constraint <%s>.\n", SCIPconsGetName(cons));


      /* ask Mercury whether there exists a ground instance of this constraint
         which does not satisfy the solution 
      */

      if( MR_existscut((MR_String) SCIPconsGetName(cons),indices,values) )
      {
         SCIPdebugMessage("first order linear constraint <%s> violated by solution.\n", SCIPconsGetName(cons));
         *result = SCIP_INFEASIBLE;
         return SCIP_OKAY;
      }
   }
   
   SCIPdebugMessage("all first order linear constraint satisfied by solution.\n");
   *result = SCIP_FEASIBLE;
   return SCIP_OKAY;
}


/** domain propagation method of constraint handler */
#if 0
static
SCIP_DECL_CONSPROP(consPropFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consPropFolinear NULL
#endif


/** presolving method of constraint handler */
#if 0
static
SCIP_DECL_CONSPRESOL(consPresolFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consPresolFolinear NULL
#endif


/** propagation conflict resolving method of constraint handler */
#if 0
static
SCIP_DECL_CONSRESPROP(consRespropFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consRespropFolinear NULL
#endif


/** variable rounding lock method of constraint handler */
static
SCIP_DECL_CONSLOCK(consLockFolinear)
{  /*lint --e{715}*/

   SCIP_PROBDATA* probdata;

   int i;
   SCIP_VAR* var;
   const char* consname = SCIPconsGetName(cons);

   MR_Integer mr_down;
   MR_Integer mr_up;

   assert( scip != NULL );
   assert( conshdlr != NULL );
   assert( strcmp(SCIPconshdlrGetName(conshdlr), CONSHDLR_NAME) == 0 );
   assert( cons != NULL );

   SCIPdebugMessage("locking variables in first order linear constraint <%s>.\n", consname);

   probdata = SCIPgetProbData(scip);
   assert( probdata != NULL );
   assert( probdata->vars != NULL );

   /* check every variable in the problem instance ... */

   for( i = 0; i < probdata->nvars; ++i )
   {
      var = probdata->vars[i];
      MR_locks((MR_String) consname, i, &mr_down, &mr_up);

      if( (SCIP_Bool) mr_up )
      {
         
         SCIPdebugMessage("adding up lock for variable <%s>\n", SCIPvarGetName(var));
         
         if( (SCIP_Bool) mr_down )
         {
            SCIPdebugMessage("adding down lock for variable <%s>\n", SCIPvarGetName(var));            
            SCIPaddVarLocks(scip, var, nlockspos + nlocksneg, nlockspos + nlocksneg);
         }
         else
            SCIPaddVarLocks(scip, var, nlocksneg, nlockspos);
      }
      else if( (SCIP_Bool) mr_down )
      {
         SCIPaddVarLocks(scip, var, nlockspos, nlocksneg);
         SCIPdebugMessage("adding down lock for variable <%s>\n", SCIPvarGetName(var));            
      }

   }
   
   return SCIP_OKAY;
}


/** constraint activation notification method of constraint handler */
#if 0
static
SCIP_DECL_CONSACTIVE(consActiveFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consActiveFolinear NULL
#endif


/** constraint deactivation notification method of constraint handler */
#if 0
static
SCIP_DECL_CONSDEACTIVE(consDeactiveFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consDeactiveFolinear NULL
#endif


/** constraint enabling notification method of constraint handler */
#if 0
static
SCIP_DECL_CONSENABLE(consEnableFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consEnableFolinear NULL
#endif


/** constraint disabling notification method of constraint handler */
#if 0
static
SCIP_DECL_CONSDISABLE(consDisableFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consDisableFolinear NULL
#endif

/** variable deletion of constraint handler */
#if 0
static
SCIP_DECL_CONSDELVARS(consDelvarsFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consDelvarsFolinear NULL
#endif


/** constraint display method of constraint handler */
#if 0
static
SCIP_DECL_CONSPRINT(consPrintFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consPrintFolinear NULL
#endif


/** constraint copying method of constraint handler */
#if 0
static
SCIP_DECL_CONSCOPY(consCopyFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consCopyFolinear NULL
#endif


/** constraint parsing method of constraint handler */
#if 0
static
SCIP_DECL_CONSPARSE(consParseFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consParseFolinear NULL
#endif


/** constraint method of constraint handler which returns the variables (if possible) */
#if 0
static
SCIP_DECL_CONSGETVARS(consGetVarsFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear power constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consGetVarsFolinear NULL
#endif

/** constraint method of constraint handler which returns the number of variables (if possible) */
#if 0
static
SCIP_DECL_CONSGETNVARS(consGetNVarsFolinear)
{  /*lint --e{715}*/
   SCIPerrorMessage("method of folinear power constraint handler not implemented yet\n");
   SCIPABORT(); /*lint --e{527}*/

   return SCIP_OKAY;
}
#else
#define consGetNVarsFolinear NULL
#endif


/*
 * constraint specific interface methods
 */

/** creates the handler for folinear constraints and includes it in SCIP */
SCIP_RETCODE SCIPincludeConshdlrFolinear(
   SCIP*                 scip                /**< SCIP data structure */
   )
{

   SCIP_CONSHDLR* conshdlr;

   conshdlr = NULL;

   /* include constraint handler */

   /* use SCIPincludeConshdlrBasic() plus setter functions if you want to set callbacks one-by-one and your code should
    * compile independent of new callbacks being added in future SCIP versions
    */
   SCIP_CALL( SCIPincludeConshdlrBasic(scip, &conshdlr, CONSHDLR_NAME, CONSHDLR_DESC,
         CONSHDLR_ENFOPRIORITY, CONSHDLR_CHECKPRIORITY, CONSHDLR_EAGERFREQ, CONSHDLR_NEEDSCONS,
         consEnfolpFolinear, consEnfopsFolinear, consCheckFolinear, consLockFolinear,
         NULL) );
   assert(conshdlr != NULL);

   /* set non-fundamental callbacks via specific setter functions */
   /* SCIP_CALL( SCIPsetConshdlrActive(scip, conshdlr, consActiveFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrCopy(scip, conshdlr, conshdlrCopyFolinear, consCopyFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrDeactive(scip, conshdlr, consDeactiveFolinear) ); */
   SCIP_CALL( SCIPsetConshdlrDelete(scip, conshdlr, consDeleteFolinear) );
   /* SCIP_CALL( SCIPsetConshdlrDelvars(scip, conshdlr, consDelvarsFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrDisable(scip, conshdlr, consDisableFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrEnable(scip, conshdlr, consEnableFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrExit(scip, conshdlr, consExitFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrExitpre(scip, conshdlr, consExitpreFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrExitsol(scip, conshdlr, consExitsolFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrFree(scip, conshdlr, consFreeFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrGetVars(scip, conshdlr, consGetVarsFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrGetNVars(scip, conshdlr, consGetNVarsFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrInit(scip, conshdlr, consInitFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrInitpre(scip, conshdlr, consInitpreFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrInitsol(scip, conshdlr, consInitsolFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrInitlp(scip, conshdlr, consInitlpFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrParse(scip, conshdlr, consParseFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrPresol(scip, conshdlr, consPresolFolinear, CONSHDLR_MAXPREROUNDS, CONSHDLR_DELAYPRESOL) ); */
   /* SCIP_CALL( SCIPsetConshdlrPrint(scip, conshdlr, consPrintFolinear) ); */
   /* SCIP_CALL( SCIPsetConshdlrProp(scip, conshdlr, consPropFolinear, CONSHDLR_PROPFREQ, CONSHDLR_DELAYPROP, */
   /*       CONSHDLR_PROP_TIMING) ); */
   /* SCIP_CALL( SCIPsetConshdlrResprop(scip, conshdlr, consRespropFolinear) ); */
   SCIP_CALL( SCIPsetConshdlrSepa(scip, conshdlr, consSepalpFolinear, consSepasolFolinear, CONSHDLR_SEPAFREQ, CONSHDLR_SEPAPRIORITY, CONSHDLR_DELAYSEPA) ); 
   /* SCIP_CALL( SCIPsetConshdlrTrans(scip, conshdlr, consTransFolinear) ); */

   /* add folinear constraint handler parameters */
   /* TODO: (optional) add constraint handler specific parameters with SCIPaddTypeParam() here */

   return SCIP_OKAY;
}

/** creates and captures a folinear constraint
 *
 *  @note the constraint gets captured, hence at one point you have to release it using the method SCIPreleaseCons()
 */
SCIP_RETCODE SCIPcreateConsFolinear(
   SCIP*                 scip,               /**< SCIP data structure */
   SCIP_CONS**           cons,               /**< pointer to hold the created constraint */
   const char*           name,               /**< name of constraint */
   SCIP_Bool             initial,            /**< should the LP relaxation of constraint be in the initial LP?
                                              *   Usually set to TRUE. Set to FALSE for 'lazy constraints'. */
   SCIP_Bool             separate,           /**< should the constraint be separated during LP processing?
                                              *   Usually set to TRUE. */
   SCIP_Bool             enforce,            /**< should the constraint be enforced during node processing?
                                              *   TRUE for model constraints, FALSE for additional, redundant constraints. */
   SCIP_Bool             check,              /**< should the constraint be checked for feasibility?
                                              *   TRUE for model constraints, FALSE for additional, redundant constraints. */
   SCIP_Bool             propagate,          /**< should the constraint be propagated during node processing?
                                              *   Usually set to TRUE. */
   SCIP_Bool             local,              /**< is constraint only valid locally?
                                              *   Usually set to FALSE. Has to be set to TRUE, e.g., for branching constraints. */
   SCIP_Bool             modifiable,         /**< is constraint modifiable (subject to column generation)?
                                              *   Usually set to FALSE. In column generation applications, set to TRUE if pricing
                                              *   adds coefficients to this constraint. */
   SCIP_Bool             dynamic,            /**< is constraint subject to aging?
                                              *   Usually set to FALSE. Set to TRUE for own cuts which
                                              *   are separated as constraints. */
   SCIP_Bool             removable,          /**< should the relaxation be removed from the LP due to aging or cleanup?
                                              *   Usually set to FALSE. Set to TRUE for 'lazy constraints' and 'user cuts'. */
   SCIP_Bool             stickingatnode      /**< should the constraint always be kept at the node where it was added, even
                                              *   if it may be moved to a more global node?
                                              *   Usually set to FALSE. Set to TRUE to for constraints that represent node data. */
   )
{

   SCIP_CONSHDLR* conshdlr;

   /* find the folinear constraint handler */
   conshdlr = SCIPfindConshdlr(scip, CONSHDLR_NAME);
   if( conshdlr == NULL )
   {
      SCIPerrorMessage("folinear constraint handler not found\n");
      return SCIP_PLUGINNOTFOUND;
   }

   /* create constraint */
   SCIP_CALL( SCIPcreateCons(scip, cons, name, conshdlr, NULL, initial, separate, enforce, check, propagate,
         local, modifiable, dynamic, removable, stickingatnode) );

   return SCIP_OKAY;
}

/** creates and captures a folinear constraint with all its constraint flags set to their
 *  default values
 *
 *  @note the constraint gets captured, hence at one point you have to release it using the method SCIPreleaseCons()
 */
SCIP_RETCODE SCIPcreateConsBasicFolinear(
   SCIP*                 scip,               /**< SCIP data structure */
   SCIP_CONS**           cons,               /**< pointer to hold the created constraint */
   const char*           name                /**< name of constraint */
   )
{


   SCIP_CALL( SCIPcreateConsFolinear(scip, cons, name,
         TRUE, TRUE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE, FALSE, FALSE) );

   return SCIP_OKAY;
}
