REBAR = ./rebar

get_deps:
	@$(REBAR) get-deps

compile: get_deps
	@$(REBAR) compile

test: compile
	@$(REBAR) ct skip_deps=true