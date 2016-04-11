ROOT := ../../..

INCDIRS := $(ROOT)/src
ALL_EXE := public public2
EVERYTHING_EXE := geneanet clavier connex hist selroy chkimg consmoy lune titres gwck nbdesc probot
TOFIX_EXE := cleopatre popule modif
LIBS = $(LIBUNIX)

probot.opt probot.out: LIBS += str gramlib
probot.opt probot.out: LINKFLAGS += -I $(CAMLP5D)
probot_PREPEND_OBJS := $(ROOT)/src/gwlib

include $(ROOT)/tools/Makefile.config
