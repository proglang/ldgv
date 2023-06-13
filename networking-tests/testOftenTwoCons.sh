for i in {1..2000}; do
    clear; echo "$i TwoCons"; (trap 'kill 0' SIGINT; stack run ldgv -- interpret < ../networking-examples/twoCons/server.ldgvnw & stack run ldgv -- interpret < ../networking-examples/twoCons/client.ldgvnw & wait);
done