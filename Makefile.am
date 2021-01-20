include $(top_srcdir)/Makefile.tool.am

#----------------------------------------------------------------------------
# Headers, etc
#----------------------------------------------------------------------------

pkginclude_HEADERS =

bin_SCRIPTS = \
	cfggrind_asmmap \
	cfggrind_info

noinst_HEADERS = \
	global.h

#----------------------------------------------------------------------------
# cfggrind-<platform>
#----------------------------------------------------------------------------

noinst_PROGRAMS  = cfggrind-@VGCONF_ARCH_PRI@-@VGCONF_OS@
if VGCONF_HAVE_PLATFORM_SEC
noinst_PROGRAMS += cfggrind-@VGCONF_ARCH_SEC@-@VGCONF_OS@
endif

CFGGRIND_SOURCES_COMMON = \
	bb.c \
	callstack.c \
	cfg.c \
	clo.c \
	debug.c \
	fdesc.c \
	fn.c \
	instrs.c \
	main.c \
	smarthash.c \
	smartlist.c \
	threads.c

cfggrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_SOURCES      = \
	$(CFGGRIND_SOURCES_COMMON)
cfggrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
cfggrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS       = $(LTO_CFLAGS) \
	$(AM_CFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
cfggrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_PRI_CAPS@)
cfggrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_PRI_CAPS@)
cfggrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_PRI_CAPS@)
cfggrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_PRI@ \
	$(LINK) \
	$(cfggrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_CFLAGS) \
	$(cfggrind_@VGCONF_ARCH_PRI@_@VGCONF_OS@_LDFLAGS)

if VGCONF_HAVE_PLATFORM_SEC
cfggrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_SOURCES      = \
	$(CFGGRIND_SOURCES_COMMON)
cfggrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CPPFLAGS     = \
	$(AM_CPPFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
cfggrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS       = $(LTO_CFLAGS) \
	$(AM_CFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
cfggrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_DEPENDENCIES = \
	$(TOOL_DEPENDENCIES_@VGCONF_PLATFORM_SEC_CAPS@)
cfggrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDADD        = \
	$(TOOL_LDADD_@VGCONF_PLATFORM_SEC_CAPS@)
cfggrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS      = \
	$(TOOL_LDFLAGS_@VGCONF_PLATFORM_SEC_CAPS@)
cfggrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LINK = \
	$(top_builddir)/coregrind/link_tool_exe_@VGCONF_OS@ \
	@VALT_LOAD_ADDRESS_SEC@ \
	$(LINK) \
	$(cfggrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_CFLAGS) \
	$(cfggrind_@VGCONF_ARCH_SEC@_@VGCONF_OS@_LDFLAGS)
endif
