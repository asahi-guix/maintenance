bin_SCRIPTS =

# Handle substitution of fully-expanded Autoconf variables.
do_subst = $(SED)					\
  -e 's,[@]GUILE[@],$(GUILE),g'				\
  -e 's,[@]guilemoduledir[@],$(guilemoduledir),g'	\
  -e 's,[@]guileobjectdir[@],$(guileobjectdir),g'	\
  -e 's,[@]localedir[@],$(localedir),g'

nodist_noinst_SCRIPTS = pre-inst-env

GOBJECTS = $(SOURCES:%.scm=%.go)

moddir=$(prefix)/share/guile/site/$(GUILE_EFFECTIVE_VERSION)
godir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache
ccachedir=$(libdir)/guile/$(GUILE_EFFECTIVE_VERSION)/site-ccache

nobase_mod_DATA = $(SOURCES) $(NOCOMP_SOURCES)
nobase_go_DATA = $(GOBJECTS)

# Make sure source files are installed first, so that the mtime of
# installed compiled files is greater than that of installed source
# files.  See
# <http://lists.gnu.org/archive/html/guile-devel/2010-07/msg00125.html>
# for details.
guile_install_go_files = install-nobase_goDATA
$(guile_install_go_files): install-nobase_modDATA

EXTRA_DIST = $(SOURCES) $(NOCOMP_SOURCES)
GUILE_WARNINGS = -Wunbound-variable -Warity-mismatch -Wformat
SUFFIXES = .scm .go
.scm.go:
	$(AM_V_GEN)$(top_builddir)/pre-inst-env $(GUILE_TOOLS) compile $(GUILE_WARNINGS) -L modules -o "$@" "$<"

NOCOMP_SOURCES = \
	modules/asahi/guix/maintenance/files/authorized-keys/apple-m1.pub \
	modules/asahi/guix/maintenance/files/ssh/authorized-keys/ed25519/roman.pub \
	modules/asahi/guix/maintenance/files/ssh/authorized-keys/rsa/root.pub \
	modules/asahi/guix/maintenance/patches/cuirass-disable-jit.patch

SOURCES = \
	modules/asahi/guix/maintenance/channels.scm \
	modules/asahi/guix/maintenance/hconfig.scm \
	modules/asahi/guix/maintenance/machines/server.scm \
	modules/asahi/guix/maintenance/packages/ci.scm \
	modules/asahi/guix/maintenance/packages/guile-xyz.scm \
	modules/asahi/guix/maintenance/services/admin.scm \
	modules/asahi/guix/maintenance/services/avahi.scm \
	modules/asahi/guix/maintenance/services/base.scm \
	modules/asahi/guix/maintenance/services/certbot.scm \
	modules/asahi/guix/maintenance/services/cuirass.scm \
	modules/asahi/guix/maintenance/services/databases.scm \
	modules/asahi/guix/maintenance/services/networking.scm \
	modules/asahi/guix/maintenance/services/security.scm \
	modules/asahi/guix/maintenance/services/ssh.scm \
	modules/asahi/guix/maintenance/services/virtualization.scm \
	modules/asahi/guix/maintenance/services/web.scm \
	modules/asahi/guix/maintenance/services/website.scm \
	modules/asahi/guix/maintenance/systems/server.scm

TESTS =

TEST_EXTENSIONS = .scm
SCM_LOG_DRIVER =                                \
  $(top_builddir)/pre-inst-env                  \
  $(GUILE) --no-auto-compile -e main -L modules     \
      $(top_srcdir)/build-aux/test-driver.scm

# Tell 'build-aux/test-driver.scm' to display only source file names,
# not indivdual test names.
AM_SCM_LOG_DRIVER_FLAGS = --brief=yes

AM_SCM_LOG_FLAGS = --no-auto-compile -L "$(top_srcdir)"

AM_TESTS_ENVIRONMENT = abs_top_srcdir="$(abs_top_srcdir)"

info_TEXINFOS = doc/asahi-guix-maintenance.texi
dvi: # Don't build dvi docs

EXTRA_DIST += doc/NOTES.org \
              README.org \
              README \
              COPYING \
              .github/FUNDING.yml \
              .github/workflows/test.yml \
              .gitignore \
              .guix-authorizations \
              .guix-channel \
              .guix/manifest.scm \
              .guix/modules/asahi-guix-maintenance-package.scm \
              build-aux/test-driver.scm \
              $(TESTS)

ACLOCAL_AMFLAGS = -I m4

clean-go:
	-$(RM) $(GOBJECTS)
.PHONY: clean-go

CLEANFILES =					\
  $(GOBJECTS)					\
  $(TESTS:tests/%.scm=%.log)
