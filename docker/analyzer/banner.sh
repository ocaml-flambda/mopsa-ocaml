#!/bin/bash

echo -e "\e[0;37m┏┳┓┏━┓┏━┓\e[0;97m┏━┓┏━┓\e[0m"
echo -e "\e[0;36m┃┃┃┃\e[1;37m▿\e[0;36m┃┣━┛┗━┓\e[0;96m┣━┫\e[0m"
echo -e "\e[0;94m╹ ╹┗━┛╹  ┗━┛╹ ╹\e[0m"
echo
echo -e "\e[1;35mWelcome to MOPSA!\e[0m"
echo -e -n "\e[0;90m"
mopsa -v
echo -e "\e[0m"
echo -e "Run MOPSA for instance with \e[1;33mmopsa-c <some-file.c>\e[0m"
echo -e "Source code is available at \e[1;34mhttps://gitlab.com/mopsa/mopsa-analyzer/\e[0m"
echo -e "Documentation is availble at \e[1;34mhttps://mopsa.gitlab.io/mopsa-manual/user-manual/\e[0m"
echo
