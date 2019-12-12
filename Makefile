

.PHONY: test


test:
	(cd test && ct_run -dir . -log_dir test_results -suite yaemep_SUITE)
