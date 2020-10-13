COMPILING = \n\033[1;34m[Compiling ...]\033[0m   ┬─┬⃰͡ (ᵔᵕᵔ͜ )\n

SUCCESS = \n(☞ﾟヮﾟ)☞ \033[0;32m[SUCCESS]\033[0m ☜(ﾟヮﾟ☜)\n

FAIL = \n\033[1;31m[... Fail]\033[0m \t(╯°□°)╯ ┻━┻\n

SUPPR = \n \t(∩｀-´)⊃━☆ﾟ.*･｡ﾟ \n\n \t\t\t cleaning done

PURGED = \n \t(∩｀-´)⊃━☆ﾟ.*･｡ﾟ \n\n \t\t\t DIS IS PURGED

package =	EvalStack

stack_yaml = STACK_YAML="EvalStack/stack.yaml"
stack = $(stack_yaml) stack

path = $(shell $(stack) path --local-install-root)

exe = $(package)-exe

bin = calcExpr

all:
	@echo -e "$(COMPILING)"
	@rm -f $(package)/$(package).cabal
	@$(stack) build $(package) \
		&& echo -e "$(SUCCESS)" \
                || echo -e "$(FAIL)"
	@cp $(path)/bin/$(exe) ./
	@mv $(exe) $(bin)

clean:
	@echo -e "\nCleaning ...\n"
	@$(stack) clean
	@echo -e "	$(SUPPR)\n"

fclean:
	@echo -e "\nPURGE THEM ALL ...\n"
	@rm -f $(bin)
	@$(stack) purge
	@echo -e "	$(PURGED)\n"

re:	fclean all

.PHONY: all clean fclean re
