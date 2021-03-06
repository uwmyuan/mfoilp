

#-----------------------------------------------------------------------------
# paths
#-----------------------------------------------------------------------------

SCIPDIR         =       scip

#-----------------------------------------------------------------------------
# include default project Makefile from SCIP
#-----------------------------------------------------------------------------

include $(SCIPDIR)/make/make.project

#-----------------------------------------------------------------------------
# Main Program
#-----------------------------------------------------------------------------

# overwrite where SCIP puts src, obj and bin

SRCDIR		=	.
OBJDIR		=	.
BINDIR		=	.

MAINNAME	=	mfoilp
MAINOBJ		=	cfoilp.o cons_folinear.o pricer_dummy.o
MAINSRC		=	$(addprefix $(SRCDIR)/,$(MAINOBJ:.o=.c))
MAINDEP		=	$(SRCDIR)/depend.cmain.$(OPT)

MAIN		=	$(MAINNAME).$(BASE).$(LPS)$(EXEEXTENSION)
MAINFILE	=	$(BINDIR)/$(MAIN)
MAINSHORTLINK	=	$(BINDIR)/$(MAINNAME)
MAINOBJFILES	=	$(addprefix $(OBJDIR)/,$(MAINOBJ))


# suppress warning produced by Mercury generated C code

GCCWARN		=	-Wno-long-long -Wall -W -Wpointer-arith -Wcast-align -Wwrite-strings -Wshadow \
			-Wno-unknown-pragmas -Wno-unused-parameter \
			-Wdisabled-optimization \
			-Wsign-compare -Wstrict-prototypes \
			-Wmissing-declarations -Wmissing-prototypes -Wdeclaration-after-statement



# Mercury stuff

MMC = mmc
GRADEOPT =
MMCFLAGS = 
#MMCFLAGS = --inhibit-warnings --no-warn-target-code
MERCURY_LINKAGE = --mercury-linkage static
MERCURY_LIB_LDFLAGS = libmfoilp.a
# next line commented out, use default C compiler
#CC = $(shell $(MMC) --output-cc)

CFLAGS_FOR_GRADE = $(shell $(MMC) $(GRADEOPT) --output-grade-defines)
CFLAGS_FOR_INCLUDES = $(shell $(MMC) $(GRADEOPT) --output-c-include-dir-flags)
CMFLAGS = $(CFLAGS_FOR_GRADE) $(CFLAGS_FOR_INCLUDES)
#LD = $(shell $(MMC) --output-link-command)
#LINKCXX = $(shell $(MMC) --output-link-command)

LIB_LDFLAGS = $(shell $(MMC) $(GRADEOPT) $(MERCURY_LINKAGE) --output-library-link-flags)


#-----------------------------------------------------------------------------
# Rules
#-----------------------------------------------------------------------------




ifeq ($(VERBOSE),false)
.SILENT:	$(MAINFILE) $(MAINOBJFILES) $(MAINSHORTLINK) 
endif

.PHONY: all
all:            $(SCIPDIR) $(MAINFILE) $(MAINSHORTLINK)

.PHONY: lint
lint:		$(MAINSRC)
		-rm -f lint.out
		$(SHELL) -ec 'for i in $^; \
			do \
			echo $$i; \
			$(LINT) -I$(SCIPDIR) lint/main-gcc.lnt +os\(lint.out\) -u -zero \
			$(FLAGS) -UNDEBUG -UWITH_READLINE -UROUNDING_FE $$i; \
			done'

.PHONY: scip
scip:
		@$(MAKE) -C $(SCIPDIR) libs $^

.PHONY: doc
doc:
		@-(cd doc && ln -fs ../$(SCIPDIR)/doc/scip.css);
		@-(cd doc && ln -fs ../$(SCIPDIR)/doc/pictures/scippy.png);
		@-(cd doc && ln -fs ../$(SCIPDIR)/doc/pictures/miniscippy.png);
		@-(cd doc && ln -fs ../$(SCIPDIR)/doc/scipfooter.html footer.html);
		cd doc; $(DOXY) $(MAINNAME).dxy

$(MAINSHORTLINK):	$(MAINFILE)
		@rm -f $@
		cd $(dir $@) && ln -s $(notdir $(MAINFILE)) $(notdir $@)

# $(OBJDIR):
# 		@-mkdir -p $(OBJDIR)

# $(BINDIR):
# 		@-mkdir -p $(BINDIR)

.PHONY: clean
clean:		$(OBJDIR)
ifneq ($(OBJDIR),)
		@-(rm -f $(OBJDIR)/*.o );
		@echo "-> remove main objective files"
endif
		@-rm -f $(MAINFILE) $(MAINLINK) $(MAINSHORTLINK)
		@echo "-> remove binary"
		@-$(MMC) --make mfoilp.realclean
		@/bin/rm -f mfoilp_int.[cho] cfoilp.o mfoilp Deep.data
		@/bin/rm -rf Mercury


.PHONY: test
test:           $(MAINFILE)
		@-(cd check && ln -fs ../$(SCIPDIR)/check/evalcheck.sh);
		@-(cd check && ln -fs ../$(SCIPDIR)/check/evalcheck_cluster.sh);
		@-(cd check && ln -fs ../$(SCIPDIR)/check/check.awk);
		@-(cd check && ln -fs ../$(SCIPDIR)/check/getlastprob.awk);
		@-(cd check && ln -fs ../$(SCIPDIR)/check/configuration_set.sh);
		@-(cd check && ln -fs ../$(SCIPDIR)/check/configuration_logfiles.sh);
		@-(cd check && ln -fs ../$(SCIPDIR)/check/configuration_tmpfile_setup_scip.sh);
		@-(cd check && ln -fs ../$(SCIPDIR)/check/run.sh);
		cd check; \
		$(SHELL) ./check.sh $(TEST) $(MAINFILE) $(SETTINGS) $(notdir $(MAINFILE)) $(TIME) $(NODES) $(MEM) $(THREADS) $(FEASTOL) $(DISPFREQ) $(CONTINUE) $(LOCK) "example" $(LPS) $(VALGRIND) $(CLIENTTMPDIR) $(OPTCOMMAND);

.PHONY: tags
tags:
		rm -f TAGS; ctags -e src/*.c src/*.h $(SCIPDIR)/src/scip/*.c $(SCIPDIR)/src/scip/*.h;

.PHONY: depend
depend:		$(SCIPDIR)
		$(SHELL) -ec '$(DCC) $(FLAGS) $(DFLAGS) $(MAINSRC) \
		| sed '\''s|^\([0-9A-Za-z\_]\{1,\}\)\.o *: *$(SRCDIR)/\([0-9A-Za-z\_]*\).c|$$\(OBJDIR\)/\2.o: $(SRCDIR)/\2.c|g'\'' \
		>$(MAINDEP)'

-include	$(MAINDEP)

.PHONY: solution
solution:	$(MAINSHORTLINK)
		mfoilp

# main target
$(MAINFILE):	$(BINDIR) $(OBJDIR) $(SCIPLIBFILE) $(LPILIBFILE) $(NLPILIBFILE) $(MAINOBJFILES) mfoilp_int.o mfoilp.init
		@echo "-> linking $@"
ifeq ($(VERBOSE),true)
		$(LINKCXX) $(MAINOBJFILES) $(MERCURY_LIB_LDFLAGS) mfoilp_int.o \
		$(LIB_LDFLAGS) \
		$(LINKCXX_L)$(SCIPDIR)/lib $(LINKCXX_l)$(SCIPLIB)$(LINKLIBSUFFIX) \
                $(LINKCXX_l)$(OBJSCIPLIB)$(LINKLIBSUFFIX) $(LINKCXX_l)$(LPILIB)$(LINKLIBSUFFIX) $(LINKCXX_l)$(NLPILIB)$(LINKLIBSUFFIX) \
                $(OFLAGS) $(LPSLDFLAGS) \
		$(LDFLAGS) $(LINKCXX_o)$@
else
		@$(LINKCXX) $(MAINOBJFILES) $(MERCURY_LIB_LDFLAGS) mfoilp_int.o \
		$(LIB_LDFLAGS) \
		$(LINKCXX_L)$(SCIPDIR)/lib $(LINKCXX_l)$(SCIPLIB)$(LINKLIBSUFFIX) \
                $(LINKCXX_l)$(OBJSCIPLIB)$(LINKLIBSUFFIX) $(LINKCXX_l)$(LPILIB)$(LINKLIBSUFFIX) $(LINKCXX_l)$(NLPILIB)$(LINKLIBSUFFIX) \
                $(OFLAGS) $(LPSLDFLAGS) \
		$(LDFLAGS) $(LINKCXX_o)$@
endif

mfoilp.init: mfoilp.m prob.m
	$(MMC) $(MMCFLAGS) $(GRADEOPT) --make  libmfoilp

# The following rule creates the stand-alone interface to the mfoilp
# library, Mercury standard library and Mercury runtime.  Since we haven't
# installed mfoilp all the relevant files will have been built in
# this directory; with an installed library we would need to use the
# `--mld' option to specify its location.
#
mfoilp_int.o: mfoilp.init
	$(MMC) $(GRADEOPT) --ml  mfoilp \
		--generate-standalone-interface mfoilp_int

# c_main.o: c_main.c mercury_lib.init mercury_lib_int.o
# 	$(CC) $(CFLAGS) -c c_main.c

# c_main: c_main.o mercury_lib_int.o mercury_lib.init
# 	$(LD) -o c_main c_main.o $(MERCURY_LIB_LDFLAGS) mercury_lib_int.o $(LIB_LDFLAGS)



$(OBJDIR)/%.o:	$(SRCDIR)/%.c mfoilp.init mfoilp_int.o
		@echo "-> compiling $@"
		$(CC) $(FLAGS) $(OFLAGS) $(BINOFLAGS) $(CFLAGS) $(CMFLAGS) -c $< $(CC_o)$@

$(OBJDIR)/%.o:	$(SRCDIR)/%.cpp
		@echo "-> compiling $@"
		$(CXX) $(FLAGS) $(OFLAGS) $(BINOFLAGS) $(CXXFLAGS) -c $< $(CXX_o)$@

#---- EOF --------------------------------------------------------------------
