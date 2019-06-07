# Compilar o cliente e o servidor por defeito
compile:
	erlc -o Server/ Server/scores.erl
	erlc -o Server/ Server/login.erl
	erlc -o Server/ Server/user.erl
	erlc -o Server/ Server/server.erl
