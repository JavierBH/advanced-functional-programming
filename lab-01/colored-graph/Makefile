default : run
MODULE = kcolor

run: compile
	@erl -noshell -eval "$(MODULE):run()" -s init stop
compile:
	@erlc +debug_info $(MODULE).erl
dialyzer: compile
	@dialyzer -Wunmatched_returns $(MODULE).beam
eunit: compile
	@erl -noshell -eval "eunit:test($(MODULE))" -s init stop
clean:
	$(RM) *.beam erl_crash.dump