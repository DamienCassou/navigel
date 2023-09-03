ELPA_DEPENDENCIES=package-lint tablist let-alist

ELPA_ARCHIVES=melpa gnu

TEST_ERT_FILES=$(wildcard test/*.el)
LINT_CHECKDOC_FILES=$(wildcard *.el) $(wildcard test/*.el)
LINT_PACKAGE_LINT_FILES=$(wildcard *.el)
LINT_COMPILE_FILES=$(wildcard *.el) $(wildcard test/*.el)

LINT_CHECKDOC_OPTIONS=--eval "(setq checkdoc-arguments-in-order-flag nil)"

makel.mk:
	# Download makel
	@if [ -f ../makel/makel.mk ]; then \
		ln -s ../makel/makel.mk .; \
	else \
		curl \
		--fail --silent --show-error --insecure --location \
		--retry 9 --retry-delay 9 \
		-O https://github.com/DamienCassou/makel/raw/v0.5.3/makel.mk; \
	fi

# Include makel.mk if present
-include makel.mk
