##
## EPITECH PROJECT, 2021
## funEvalExpr
## File description:
## Makefile
##

NAME		=	hal
BIN_PATH	=	$(shell stack path --local-install-root)/bin/$(NAME)-exe


all:
	stack build
	cp $(BIN_PATH) ./$(NAME)

clean:

fclean: clean
	rm -f $(NAME)

re: fclean all

.PHONY: all clean fclean re
