

.PHONY: all test dialyzer

all: test dialyzer

test:
	(cd test && ct_run -dir . -log_dir test_results -suite yaemep_SUITE)

dialyzer:
	test -f ./.dialyzer_plt || dialyzer --build_plt --apps erts kernel stdlib mnesia --output_plt ./.dialyzer_plt
	dialyzer --plt ./.dialyzer_plt emacs_erlang_yaemep_support.erl
